// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "vm.h"

#include <cstdlib>
#include "array.h"
#include "block.h"
#include "function.h"
#include "handle.h"
#include "heap.h"
#include "interpreter.h"
#include "memory.h"
#include "package.h"
#include "platform.h"
#include "roots.h"
#include "stack.h"
#include "string.h"

using namespace std;

namespace codeswitch {
namespace internal {

VM* VM::currentVM_;

VM::VM(Flags flags)
    : flags_(flags),
      heap_(new Heap(this)),
      roots_(new Roots),
      handleStorage_(new HandleStorage) {
  AllowAllocationScope allowAllocation(heap_.get(), true);
  roots_->initialize(heap());
  {
    HandleScope handleScope(handleStorage_.get());
    Local<Stack> stack = Stack::create(heap(), Stack::kDefaultSize);
    stack_ = Persistent<Stack>(stack);
  }
  if (hasFlags(kVerifyHeap))
    heap()->verify();

  const char* envSearchPaths = getenv("CS_PACKAGE_PATH");
  if (envSearchPaths != nullptr)
    addPackageSearchPaths(envSearchPaths);
  #ifdef CS_PACKAGE_PATH
    if (packageSearchPaths_.empty())
      addPackageSearchPaths(CS_PACKAGE_PATH);
  #endif
}


VM::~VM() {
  if (hasFlags(kVerifyHeap))
    heap()->verify();
}


VM* VM::fromAddress(void* address) {
  Chunk* page = Chunk::fromAddress(address);
  return page->vm();
}


Persistent<Package> VM::findPackage(const Handle<PackageName>& name) {
  for (auto& package : packages_) {
    if (name->equals(package->name()))
      return package;
  }
  return Persistent<Package>();
}


Persistent<Package> VM::loadPackage(const Handle<PackageDependency>& dependency) {
  HandleScope handleScope(this);
  auto package = findPackage(handle(dependency->name()));
  if (package && dependency->isSatisfiedBy(*package))
    return package;

  auto packagePath = searchForPackage(dependency);
  if (!packagePath.empty())
    return loadPackage(packagePath);

  return Persistent<Package>();
}


Persistent<Package> VM::loadPackage(const string& fileName) {
  HandleScope handleScope(this);
  Persistent<Package> package(Package::loadFromFile(this, fileName.c_str()));
  loadPackageDependenciesAndInitialize(package);
  return package;
}


void VM::addPackage(const Handle<Package>& package) {
  packages_.push_back(Persistent<Package>(package));
}


void VM::addPackageSearchPaths(const string& paths) {
  size_t begin = 0;
  auto end = paths.find(':');
  while (end != string::npos) {
    auto len = end - begin;
    if (len > 0) {
      auto path = paths.substr(begin, len);
      packageSearchPaths_.push_back(path);
    }
    begin = end + 1;
    end = paths.find(':', begin);
  }
  auto len = paths.size() - begin;
  if (len > 0) {
    auto path = paths.substr(begin, len);
    packageSearchPaths_.push_back(path);
  }
}


string VM::searchForPackage(const Handle<PackageDependency>& dependency) {
  AllowAllocationScope allowAllocation(heap(), true);
  auto packageName = PackageName::toString(heap(), handle(dependency->name()))
      ->toUtf8StlString();
  size_t minNameLength = packageName.size() + 6;
  for (auto path : packageSearchPaths_) {
    vector<string> files;
    try {
      files = listDirectory(path);
    } catch (Error e) {
      continue;
    }

    Persistent<PackageVersion> bestVersion;
    string bestFile;
    for (auto file : files) {
      HandleScope handleScope(this);
      if (file.size() < minNameLength ||
          file.substr(file.size() - 4, 4) != ".csp" ||
          file.substr(0, packageName.size()) != packageName) {
        continue;
      }
      auto dashPos = file.find('-');
      if (dashPos != packageName.size())
        continue;
      auto dotPos = file.size() - 4;
      auto versionStlStr = file.substr(dashPos + 1, dotPos - dashPos - 1);
      auto versionStr = String::fromUtf8String(heap(), versionStlStr);
      auto version = PackageVersion::fromString(heap(), versionStr);
      if (version &&
          (dependency->minVersion() == nullptr ||
           version->compare(dependency->minVersion()) >= 0) &&
          (dependency->maxVersion() == nullptr ||
           version->compare(dependency->maxVersion()) <= 0) &&
          (!bestVersion ||
           version->compare(*bestVersion) > 0)) {
        bestVersion = version;
        bestFile = file;
      }
    }

    if (bestFile.size() > 0) {
      auto bestPath = pathJoin(path, bestFile);
      return bestPath;
    }
  }

  return "";
}


void VM::loadPackageDependenciesAndInitialize(const Handle<Package>& package) {
  struct LoadState {
    Local<Package> package;
    length_t currentDepIndex;

    explicit LoadState(const Handle<Package>& package)
        : package(package), currentDepIndex(0) { }
  };

  // Load the dependencies in depth-first order, using an explicit stack, `loadingPackages`.
  // Packages are popped from this stack and pushed onto `loadedPackages` when all their
  // dependencies have been satisfied.
  HandleScope handleScope(this);
  vector<LoadState> loadingPackages{LoadState(package)};
  vector<Local<Package>> loadedPackages;
  while (!loadingPackages.empty()) {
    auto& last = loadingPackages.back();
    if (last.currentDepIndex == last.package->dependencies()->length()) {
      loadedPackages.push_back(last.package);
      loadingPackages.pop_back();
    } else {
      auto index = last.currentDepIndex++;
      auto dep = handle(last.package->dependencies()->get(index));

      // Check if this package is already loaded.
      auto depPackage = findPackage(handle(dep->name()));

      // If not, check if we have already started loading the package, and its dependencies
      // are satisfied.
      if (!depPackage) {
        for (auto& loaded : loadedPackages) {
          if (dep->name()->equals(loaded->name())) {
            depPackage = loaded;
            break;
          }
        }
      }

      // If we did find a package, make sure it's a suitable version. We don't allow loading
      // multiple versions of the same package.
      if (depPackage && !dep->isSatisfiedBy(*depPackage)) {
        throw Error("package is already loaded with bad version");
      }

      // If not, check if we are still trying to satisfy the dependencies. This indicates a
      // circular dependency, which is not allowed.
      if (!depPackage) {
        for (auto& state : loadingPackages) {
          if (dep->isSatisfiedBy(*state.package)) {
            throw Error("circular package dependency");
          }
        }

        // Search the file system for the package and load it.
        auto depFileName = searchForPackage(dep);
        if (depFileName.empty()) {
          throw Error("could not find package");
        }

        depPackage = Package::loadFromFile(this, depFileName);
        loadingPackages.push_back(LoadState(depPackage));
      }

      dep->setPackage(*depPackage);
    }
  }

  // Initialize all the packages we loaded (if they need to be initialized). Note that we
  // appended packages to `loadedPackages` in post-order, so dependencies will be initialized
  // before their dependents.
  Interpreter interpreter(this, stack_);
  for (auto& package : loadedPackages) {
    Package::link(heap(), package);
    if (package->initFunctionIndex() != kIndexNotSet) {
      Persistent<Function> init(package->initFunction());
      if (init->parameterTypes()->length() > 0) {
        throw Error("init function has arguments");
      }
      interpreter.call(init);
    }
  }

  // Insert all the loaded packages into the main package list. We don't do this until all of
  // the initializers have run, since one of them could throw an exception.
  for (auto& package : loadedPackages) {
    packages_.push_back(Persistent<Package>(package));
  }
}

}
}

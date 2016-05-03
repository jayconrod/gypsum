// Copyright 2014-2016 Jay Conrod. All rights reserved.

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
#include "name.h"
#include "package.h"
#include "platform.h"
#include "roots.h"
#include "stack.h"
#include "string.h"
#include "thread-bindle.h"

using namespace std;

namespace codeswitch {
namespace internal {

VM::VM(const VMOptions& vmOptions)
    : apiPtr_(nullptr),
      heap_(new Heap(this)),
      roots_(new Roots),
      handleStorage_(new HandleStorage),
      packageSearchPaths_(vmOptions.packageSearchPaths),
      nativeFunctionSearchOrder_(vmOptions.nativeFunctionSearchOrder),
      shouldVerifyHeap_(vmOptions.verifyHeap) {
  #ifdef DEBUG
    shouldVerifyHeap_ = true;
  #endif

  AllowAllocationScope allowAllocation(heap_.get(), true);
  roots_->initialize(heap());
  {
    HandleScope handleScope(handleStorage_.get());
    Local<Stack> stack = Stack::create(heap(), Stack::kDefaultSize);
    stack_ = Persistent<Stack>(stack);
    Local<ThreadBindle> threadBindle = ThreadBindle::create(heap());
    ThreadBindle::createExceptions(threadBindle);
    threadBindle_ = Persistent<ThreadBindle>(threadBindle);
  }

  for (auto entry : vmOptions.nativeFunctions) {
    pair<string, string> key(get<0>(entry), get<1>(entry));
    union {
      NativeFunction nativeFunctionAddr;
      void (*nativeFunctionPtr)();
    } nativeFunctionUnion;
    nativeFunctionUnion.nativeFunctionPtr = get<2>(entry);
    registeredNativeFunctions_[key] = nativeFunctionUnion.nativeFunctionAddr;
  }

  #ifdef CS_PACKAGE_PATH
    for (auto& path : split(CS_PACKAGE_PATH, ':')) {
      addPackageSearchPath(path);
    }
  #endif

  if (shouldVerifyHeap())
    heap()->verify();
}


VM::~VM() {
  if (shouldVerifyHeap())
    heap()->verify();
}


VM* VM::fromAddress(void* address) {
  Chunk* page = Chunk::fromAddress(address);
  return page->vm();
}


Persistent<Package> VM::findPackage(const Handle<Name>& name) {
  for (auto& package : packages_) {
    if (name->equals(package->name()))
      return package;
  }
  return Persistent<Package>();
}


Persistent<Package> VM::loadPackage(const Handle<Name>& name,
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  auto dep = PackageDependency::create(heap(), name,
      Persistent<PackageVersion>(), Persistent<PackageVersion>(),
      0, 0, 0, 0, 0, 0);
  return loadPackage(dep, nativeFunctionSearchOrder);
}


Persistent<Package> VM::loadPackage(const Handle<PackageDependency>& dependency,
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  HandleScope handleScope(this);
  auto package = findPackage(handle(dependency->name()));
  if (package && dependency->isSatisfiedBy(*package))
    return package;

  auto packagePath = searchForPackage(dependency);
  if (!packagePath.empty())
    return loadPackage(packagePath, nativeFunctionSearchOrder);

  return Persistent<Package>();
}


Persistent<Package> VM::loadPackage(const string& fileName,
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  HandleScope handleScope(this);
  Persistent<Package> package;
  if (nativeFunctionSearchOrder.empty()) {
    package = Package::loadFromFile(this, fileName, nativeFunctionSearchOrder_);
  } else {
    package = Package::loadFromFile(this, fileName, nativeFunctionSearchOrder);
  }
  loadPackageDependenciesAndInitialize(package);
  return package;
}


void VM::addPackage(const Handle<Package>& package) {
  loadPackageDependenciesAndInitialize(package);
}


NativeFunction VM::loadRegisteredFunction(Name* packageName, Name* functionName) {
  return loadRegisteredFunction(packageName->toStlString(), functionName->toStlString());
}


NativeFunction VM::loadRegisteredFunction(const std::string& packageName,
    const std::string& functionName) {
  pair<string, string> key(packageName, functionName);
  auto it = registeredNativeFunctions_.find(key);
  return it == registeredNativeFunctions_.end() ? nullptr : it->second;
}


string VM::searchForPackage(const Handle<PackageDependency>& dependency) {
  AllowAllocationScope allowAllocation(heap(), true);
  auto packageName = Name::toString(heap(), handle(dependency->name()))
      ->toUtf8StlString();
  string extension(".csp");
  size_t minNameLength = packageName.size() + extension.size();
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
          file.substr(file.size() - extension.size(), extension.size()) != extension ||
          file.substr(0, packageName.size()) != packageName) {
        continue;
      }
      if (file.size() == packageName.size() + extension.size()) {
        if (!bestVersion) {
          bestFile = file;
        }
        continue;
      }
      auto dashPos = file.find('-');
      if (dashPos != packageName.size()) {
        continue;
      }
      auto dotPos = file.size() - extension.size();
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
          throw Error("could not find package: " + dep->name()->toStlString());
        }

        depPackage = Package::loadFromFile(this, depFileName, nativeFunctionSearchOrder_);
        loadingPackages.push_back(LoadState(depPackage));
      }

      dep->setPackage(*depPackage);
    }
  }

  // Initialize all the packages we loaded (if they need to be initialized). Note that we
  // appended packages to `loadedPackages` in post-order, so dependencies will be initialized
  // before their dependents.
  Interpreter interpreter(this, stack_, threadBindle_);
  for (auto& package : loadedPackages) {
    Package::link(package);
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

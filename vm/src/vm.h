// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef vm_h
#define vm_h

#include "codeswitch.h"

#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "handle.h"
#include "platform.h"
#include "roots.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Block;
class Heap;
class Name;
class Stack;
class ThreadBindle;
class Package;
class PackageDependency;

typedef std::pair<std::string, std::string> NativeFunctionKey;
struct HashNativeFunctionKey: public std::unary_function<NativeFunctionKey, std::size_t> {
  std::size_t operator () (const NativeFunctionKey& key) const {
    std::hash<std::string> hashString;
    return hashMix(hashString(key.first)) ^ hashString(key.second);
  }
};


class VM {
 public:
  VM() : VM(VMOptions()) { }
  explicit VM(const VMOptions& vmOptions);
  ~VM();

  static VM* fromAddress(void* addr);

  ::codeswitch::VM* apiPtr() const { return apiPtr_; }
  void setApiPtr(::codeswitch::VM* apiPtr) { apiPtr_ = apiPtr; }
  Heap* heap() { return heap_.get(); }
  Roots* roots() { return roots_.get(); }
  HandleStorage& handleStorage() { return *handleStorage_; }
  const Persistent<Stack>& stack() { return stack_; }
  const Persistent<ThreadBindle>& threadBindle() { return threadBindle_; }

  Persistent<Package> findPackage(const Handle<Name>& name);
  Persistent<Package> loadPackage(const Handle<Name>& name,
      const std::vector<NativeFunctionSearch>& nativeFunctionSearchOrder);
  Persistent<Package> loadPackage(const Handle<PackageDependency>& dependency,
      const std::vector<NativeFunctionSearch>& nativeFunctionSearchOrder);
  Persistent<Package> loadPackage(const std::string& fileName,
      const std::vector<NativeFunctionSearch>& nativeFunctionSearchOrder);
  void addPackage(const Handle<Package>& package);
  const std::vector<Persistent<Package>>& packages() { return packages_; }

  bool shouldVerifyHeap() const { return shouldVerifyHeap_; }

  NativeFunction loadRegisteredFunction(Name* packageName, Name* functionName);
  NativeFunction loadRegisteredFunction(const std::string& packageName,
      const std::string& functionName);

  template <class Callback>
  void visitPointers(Callback callback) {
    roots_->visitPointers(callback);
    handleStorage_->visitPointers(callback);
  }

 private:
  std::string searchForPackage(const Handle<PackageDependency>& dependency);
  void loadPackageDependenciesAndInitialize(const Handle<Package>& package);

  ::codeswitch::VM* apiPtr_;
  std::unique_ptr<Heap> heap_;
  std::unique_ptr<Roots> roots_;
  std::unique_ptr<HandleStorage> handleStorage_;
  Persistent<Stack> stack_;
  Persistent<ThreadBindle> threadBindle_;
  std::vector<std::string> packageSearchPaths_;
  std::vector<Persistent<Package>> packages_;
  std::vector<NativeFunctionSearch> nativeFunctionSearchOrder_;
  std::unordered_map<NativeFunctionKey, NativeFunction, HashNativeFunctionKey>
      registeredNativeFunctions_;
  bool shouldVerifyHeap_;

  friend class GC;
};

}
}

#endif

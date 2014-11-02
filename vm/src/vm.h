// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef vm_h
#define vm_h

#include <memory>
#include <vector>
#include "handle.h"
#include "roots.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Block;
class Heap;
class Stack;
class Package;

class VM {
 public:
  typedef u32 Flags;
  static const Flags kVerifyHeap = 1;

#ifdef DEBUG
  static const Flags kDefaultFlags = kVerifyHeap;
#else
  static const Flags kDefaultFlags = 0;
#endif

  explicit VM(Flags flags = kDefaultFlags);
  ~VM();

  static VM* current() { return currentVM_; }
  static void setCurrent(VM* vm) { currentVM_ = vm; }
  class Scope {
   public:
    Scope(VM* vm)
        : oldVM_(VM::current()) {
      VM::setCurrent(vm);
    }
    ~Scope() { VM::setCurrent(oldVM_); }
   private:
    VM* oldVM_;
  };

  static VM* fromAddress(void* addr);

  Flags flags() { return flags_; };
  bool hasFlags(Flags flags) { return (flags_ & flags) == flags; }

  Heap* heap() { return heap_.get(); }
  Roots* roots() { return roots_.get(); }
  HandleStorage& handleStorage() { return *handleStorage_; }
  const Persistent<Stack>& stack() { return stack_; }

  void addPackage(const Persistent<Package>& package);
  const std::vector<Persistent<Package>>& package() { return packages_; }

  template <class Callback>
  void visitPointers(Callback callback) {
    roots_->visitPointers(callback);
    handleStorage_->visitPointers(callback);
  }

 private:
  static VM* currentVM_;

  Flags flags_;

  std::unique_ptr<Heap> heap_;
  std::unique_ptr<Roots> roots_;
  std::unique_ptr<HandleStorage> handleStorage_;
  Persistent<Stack> stack_;
  std::vector<Persistent<Package> > packages_;

  friend class GC;
};

}
}

#endif

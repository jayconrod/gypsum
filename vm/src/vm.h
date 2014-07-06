// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef vm_h
#define vm_h

#include <memory>
#include "handle.h"
#include "list.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Block;
class Heap;
class Roots;
class Stack;
class HandleStorage;
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

  static inline VM* fromAddress(void* addr);

  Flags flags() { return flags_; };
  bool hasFlags(Flags flags) { return (flags_ & flags) == flags; }

  Heap* heap() { return heap_.get(); }
  Roots* roots() { return roots_.get(); }
  HandleStorage& handleStorage() { return *handleStorage_; }
  Handle<Stack> stack() { return stack_; }

  void addPackage(Handle<Package> package);
  const List<Handle<Package> >& packages() { return packages_; }

 private:
  static VM* currentVM_;

  Flags flags_;
  std::unique_ptr<Heap> heap_;
  std::unique_ptr<Roots> roots_;
  std::unique_ptr<HandleStorage> handleStorage_;
  Handle<Stack> stack_;
  List<Handle<Package> > packages_;

  friend class GC;
};

}
}

#endif

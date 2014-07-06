// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "vm.h"

#include "block-inl.h"
#include "handle-inl.h"
#include "heap-inl.h"
#include "package.h"
#include "stack-inl.h"

namespace codeswitch {
namespace internal {

VM* VM::currentVM_;

VM::VM(Flags flags)
    : flags_(flags),
      heap_(new Heap(this)),
      roots_(new Roots),
      handleStorage_(new HandleStorage) {
  roots_->initialize(heap());
  stack_ = Handle<Stack>(Stack::allocate(heap(), Stack::kDefaultSize));
  if (hasFlags(kVerifyHeap))
    heap()->verify();
}


VM::~VM() {
  if (hasFlags(kVerifyHeap))
    heap()->verify();
}


void VM::addPackage(Handle<Package> package) {
  packages_.add(package);
}

}
}

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "vm.h"

#include "block.h"
#include "handle.h"
#include "heap.h"
#include "package.h"
#include "roots.h"
#include "stack.h"

namespace codeswitch {
namespace internal {

VM* VM::currentVM_;

VM::VM(Flags flags)
    : flags_(flags),
      heap_(new Heap(this)),
      roots_(new Roots),
      handleStorage_(new HandleStorage) {
  roots_->initialize(heap());
  {
    HandleScope handleScope(handleStorage_.get());
    Local<Stack> stack = Stack::create(heap(), Stack::kDefaultSize);
    stack_ = Persistent<Stack>(stack);
  }
  if (hasFlags(kVerifyHeap))
    heap()->verify();
}


VM::~VM() {
  if (hasFlags(kVerifyHeap))
    heap()->verify();
}


void VM::addPackage(const Persistent<Package>& package) {
  packages_.push_back(package);
}

}
}

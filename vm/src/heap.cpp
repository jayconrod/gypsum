// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "heap-inl.h"

#include <algorithm>
#include <vector>
#include "array.h"
#include "block-inl.h"
#include "block-visitor.h"
#include "bytecode.h"
#include "error.h"
#include "function.h"
#include "gc.h"
#include "handle-inl.h"
#include "package.h"
#include "stack.h"

#include <iostream>   // debug

using namespace std;

namespace codeswitch {
namespace internal {

Heap::Heap(VM* vm)
    : vm_(vm),
      newSpace_(vm),
      packageSpace_(vm, PACKAGE_SPACE),
      allocatingPage_(nullptr),
      top_(0),
      limit_(0) {
  newSpace_.expand();
  setAllocationRange(newSpace_.toSpace()->pages().front().get());
}


Space* Heap::getSpaceById(SpaceId id) {
  switch (id) {
    case NEW_SPACE: return newSpace_.toSpace();
    case PACKAGE_SPACE: return &packageSpace_;
    default:
      UNREACHABLE();
      return nullptr;
  }
}


#ifdef DEBUG
bool Heap::contains(Block* block) {
  return any_of(begin(), end(), [block](Space* space) { return space->contains(block); });
}


void Heap::verify() {
  class VerifyPointerVisitor: public BlockVisitorBase<VerifyPointerVisitor> {
   public:
    explicit VerifyPointerVisitor(Heap* heap)
        : heap_(heap) { }
    void visitPointer(Block** p) {
      ASSERT(*p == nullptr || heap_->contains(*p));
    }
   private:
    Heap* heap_;
  };
  VerifyPointerVisitor visitor(this);

  allocatingPage_->setAllocationPtr(top_);
  for (Space* space : *this) {
    for (Page* page : *space) {
      for (Block* block : *page) {
        ASSERT(contains(block));
        visitor.visit(block);
      }
    }
  }
}
#endif   // DEBUG


void Heap::collectGarbage() {
  GC gc(this);
  gc.collectGarbage();
}


Address Heap::allocateRawSlow(word_t size) {
  return 0;
}


void Heap::allocationRange(Page** page, Address* top, Address* limit) {
  if (page != nullptr)
    *page = allocatingPage_;
  if (top != nullptr)
    *top = top_;
  if (limit != nullptr)
    *limit = limit_;
}


void Heap::setAllocationRange(Page* page) {
  setAllocationRange(page, page->allocationPtr(), page->limit());
}


void Heap::setAllocationRange(Page* page, Address top, Address limit) {
  if (allocatingPage_ != nullptr) {
    allocatingPage_->setAllocationPtr(top_);
  }
  ASSERT(page->inRange(top) && page->inRange(limit - 1) && top <= limit);
  allocatingPage_ = page;
  top_ = top;
  limit_ = limit;
}

}
}

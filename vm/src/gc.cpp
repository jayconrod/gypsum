// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "gc.h"

#include <cstring>
#include "block-inl.h"
#include "block-visitor.h"
#include "heap-inl.h"
#include "remembered-set-inl.h"

using namespace std;

namespace codeswitch {
namespace internal {

GC::GC(Heap* heap)
    : heap_(heap) { }


class PointerUpdatingVisitor: public BlockVisitorBase<PointerUpdatingVisitor> {
 public:
  explicit PointerUpdatingVisitor(GC& gc)
      : gc_(gc) { }

  void visitPointer(Block** block) {
    gc_.copyAndUpdate(block);
  }

  void operator () (Block** block) {
    visitPointer(block);
  }

 private:
  GC& gc_;
};


void GC::collectGarbage() {
#ifdef DEBUG
  heap_->verify();
#endif

  // Swap To and From space. We will be copying live objects out of From space into To space.
  heap_->newSpace_.swap();
  heap_->setAllocationRange(heap_->newSpace()->toSpace()->pages().front().get());

  // Scan all of the roots. This copies an initial set of objects into To space.
  PointerUpdatingVisitor visitor(*this);
  heap_->visitRoots(visitor);

  // Also scan pointers into new space from objects in other spaces. We recorded these pointers
  // in remembered sets with the write barrier, so we don't have to scan a lot of memory to
  // find them. We need to rebuild this information for To space.
  for (Page* page : *heap_->newSpace()->fromSpace()) {
    for (Block** slot : *page->rememberedSet()) {
      if (heap_->newSpace_.fromSpace()->contains(*slot)) {
        copyAndUpdate(slot);
        Heap::recordWrite(slot, *slot);
      }
    }
    page->rememberedSet()->clear();
  }

  // Scan outward recursively from the roots. Stop when we have copied and updated all
  // reachable objects.
  Address scan = heap_->newSpace()->toSpace()->pages().front()->allocationBase();
  while (scan != heap_->top_) {
    Block* block = reinterpret_cast<Block*>(scan);
    word_t size = block->sizeOfBlock();
    scan += size;

    visitor.visit(block);
  }

#ifdef DEBUG
  // Fill From space with garbage.
  for (Page* page : *heap_->newSpace()->fromSpace()) {
    memset(reinterpret_cast<char*>(page->allocationBase()), kGarbageByte,
           page->limit() - page->allocationBase());
  }

  heap_->verify();
#endif
}


void GC::copyAndUpdate(Block** p) {
  if (!heap_->newSpace_.fromSpace()->contains(*p))
    return;

  Address to_address;
  if ((*p)->gcBits() != kIsForwardingAddress) {
    // Object has not yet been moved. Move it and leave a forwarding address.
    word_t size = (*p)->sizeOfBlock();
    to_address = heap_->allocateRaw(size);
    memcpy(reinterpret_cast<void*>(to_address),
           reinterpret_cast<void*>(*p),
           size);
    if ((*p)->meta()->needsRelocation()) {
      // probably not the right time to do this.
      ASSERT((*p)->isStack());
      word_t delta = to_address - reinterpret_cast<Address>(*p);
      reinterpret_cast<Stack*>(to_address)->relocateStack(delta);
    }
    mem<Address>(*p) = to_address | kIsForwardingAddress;
  } else {
    // Object has already been moved. Update the pointer using the forwarding address.
    to_address = mem<Address>(*p) & ~Block::kGCBitMask;
  }
  *p = reinterpret_cast<Block*>(to_address);
}

}
}

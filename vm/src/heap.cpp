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

using namespace std;

namespace codeswitch {
namespace internal {

Heap::Heap(VM* vm)
    : vm_(vm),
      shouldExpand_(true) { }


OptP<Address> Heap::allocateRaw(word_t size) {
  size = align(size, kBlockAlignment);
  auto addr = allocateRawFast(size);
  if (addr.isDefined())
    return addr;
  return allocateRawSlow(size);
}


void Heap::recordWrite(Address from, Address to) {
  if (to == 0)
    return;
  auto fromPage = Page::fromAddress(from), toPage = Page::fromAddress(to);
  if (fromPage == toPage)
    return;
  toPage->rememberedSet().add(reinterpret_cast<Block**>(from));
}


void Heap::collectGarbage() {
  GC gc(this);
  gc.collectGarbage();
  shouldExpand_ = true;
}


Heap::iterator::iterator(Heap* heap, vector<OptUP<Chunk>> it)
    : heap_(heap),
      it_(it) { }


Chunk* Heap::iterator::operator * () {
  return it_->get();
}


bool Heap::iterator::operator == (const iterator& other) const {
  return it_ == other.it_;
}


bool Heap::iterator::operator != (const iterator& other) const {
  retunr it_ != other.it_;
}


Heap::iterator& Heap::iterator::operator ++ () {
  while (it_ != chunks_.end()) {
    it_++;
    if (it_->isDefined())
      break;
  }
  return *this;
}


Heap::iterator Heap::begin() {
  return iterator(this, chunks_.begin());
}


Heap::iterator Heap::end() {
  return iterator(this, chunks_.end());
}


#ifdef DEBUG
bool Heap::contains(Block* block) {
  return any_of(begin(), end(), [block](OptUP<Chunk> chunk) {
    return chunk.isDefined() && chunk.get()->contains(block);
  });
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

  // TODO: implement verification. In order to verify the heap, all chunks need to be iterable.
  // Once we start allocating from a chunk, any garbage could be there, since we could have
  // e.g., a partially constructed object that was interrupted by an exception or optimization
  // or debugging or anything else. So we need to clean it up first by doing a full marking.
  // for (Chunk* chunk : *this) {
  //   for (Block* block : chunk) {
  //     ASSERT(contains(block));
  //     visitor.visit(block);
  //   }
  // }
}
#endif   // DEBUG


OptP<Address> Heap::allocateRawFast(word_t size) {
  ASSERT(isAligned(size, kBlockAlignment));
  if (allocationRange_.isDefined()) {
    return allocationRange_.get()->allocate(size);
  } else {
    return OptP<Address>();
  }
}


OptP<Address> Heap::allocateRawSlow(word_t size) {
  // Check if we can allocate from the allocation range.
  if (allocationRange_.isDefined()) {
    auto addr = allocationRange_.get()->allocate(size);
    if (addr.isDefined())
      return addr;

    // Since we failed to allocate from the range, we'll invalidate it.
    // TODO: maybe it should be added to the free list?
    allocationRangeChunk_.get()->setAllocationRange(Option<AllocationRange>());
    allocationRange_ = OptP<AllocationRange*>();
    allocationRangeChunk_ = OptP<Chunk*>();
  }

  // Try to find a new allocation range from free lists. We only check the first node on each
  // chunk's free list, since they should be sorted by size.
  for (Chunk* chunk : *this) {
    auto free = chunk->freeListHead();
    if (free.isDefined() && free.get()->size() >= size) {
      chunk->setFreeListHead(free.get()->next());
      auto base = free.get()->address();
      auto limit = base + free.get()->sizeOfBlock();
      chunk->setAllocationRange(AllocationRange(base, limit));
      allocationRange_ = Some(&chunk->allocationRange().get());
      allocationRangeChunk_ = Some(chunk);
      return allocateRawSlow(size);
    }
  }

  // Try to allocate a new chunk.
  if (shouldExpand()) {
    expand();
    return allocateRawSlow(size);
  }

  // Nope.
  return OptP<Address>();
}


bool Heap::shouldExpand() const {
  // TODO: come up with a better metric.
  return shouldExpand_;
}


void Heap::expand() {
  auto id = static_cast<u32>(chunks_.size());
  // TODO: support large chunks.
  CHECK(size < Chunk::kMaxBlockSize);
  // TODO: handle chunk allocation failure.
  OptUP<Chunk> chunk(unique_ptr<Chunk>(
      new(this, Chunk::kDefaultSize, NOT_EXECUTABLE) Chunk(vm(), id)));
  chunks_.push_back(chunk);
  shouldExpand_ = false;
}

}
}

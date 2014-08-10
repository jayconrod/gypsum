// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "heap.h"

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
      shouldExpand_(true) {
  expand();
}


Address Heap::allocateUninitialized(word_t size) {
  size = align(size, kBlockAlignment);
  auto addr = allocateFast(size);
  if (addr == 0)
    return allocateSlow(size);
  return addr;
}


Address Heap::allocate(word_t size) {
  size = align(size, kBlockAlignment);
  auto addr = allocateUninitialized(size);
  if (addr != 0)
    fill_n(reinterpret_cast<word_t*>(addr), size / kWordSize, 0);
  return addr;
}


void Heap::recordWrite(Address from, Address to) {
  if (to == 0)
    return;
  auto fromChunk = Chunk::fromAddress(from), toChunk = Chunk::fromAddress(to);
  if (fromChunk == toChunk)
    return;
  toChunk->rememberedSet().add(reinterpret_cast<Block**>(from));
}


void Heap::collectGarbage() {
  allocator_.release();
  GC gc(this);
  gc.collectGarbage();
  shouldExpand_ = true;
}


Chunk* Heap::iterator::operator * () {
  return it_->get();
}


bool Heap::iterator::operator == (const iterator& other) const {
  return it_ == other.it_;
}


Heap::iterator& Heap::iterator::operator ++ () {
  while (it_ != end_) {
    it_++;
    if (*it_)
      break;
  }
  return *this;
}


Heap::iterator Heap::begin() {
  return iterator(chunks_.begin(), chunks_.end());
}


Heap::iterator Heap::end() {
  return iterator(chunks_.end(), chunks_.end());
}


#ifdef DEBUG
bool Heap::contains(Block* block) {
  auto addr = block->address();
  return any_of(begin(), end(), [addr](Chunk* chunk) {
    return chunk && chunk->contains(addr);
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


Heap::Allocator::Allocator(Chunk* chunk)
    : range_(&chunk->allocationRange()),
      chunk_(chunk) {
  ASSERT(range_->isValid());
}


Address Heap::Allocator::allocate(word_t size) {
  ASSERT(isAligned(size, kBlockAlignment));
  if (!range_)
    return 0;
  return range_->allocate(size);
}


void Heap::Allocator::release() {
  if (chunk_ == nullptr)
    return;
  // TODO: maybe the rest should be added to the free list?
  chunk_->setAllocationRange(AllocationRange::empty());
  range_ = nullptr;
  chunk_ = nullptr;
}


Address Heap::allocateFast(word_t size) {
  ASSERT(isAligned(size, kBlockAlignment));
  return allocator_.allocate(size);
}


Address Heap::allocateSlow(word_t size) {
  ASSERT(isAligned(size, kBlockAlignment));

  // Check if we can allocate from the allocation range. `allocateFast` already does this, but
  // we will recurse after creating a new allocation range.
  auto addr = allocator_.allocate(size);
  if (addr != 0)
    return addr;

  // Since we failed to allocate from the range, we'll invalidate it.
  allocator_.release();

  // Try to find a new allocation range from free lists. We only check the first node on each
  // chunk's free list, since they should be sorted by size.
  for (Chunk* chunk : *this) {
    auto free = chunk->freeListHead();
    if (free && free->size() >= size) {
      chunk->setFreeListHead(free->next());
      auto base = free->address();
      auto limit = base + free->size();
      chunk->setAllocationRange(AllocationRange(base, limit));
      allocator_ = Allocator(chunk);
      return allocateSlow(size);
    }
  }

  // Try to allocate a new chunk.
  if (shouldExpand()) {
    // TODO: support large chunks.
    CHECK(size < Chunk::kMaxBlockSize);
    expand();
    return allocateSlow(size);
  }

  // Nope. Caller must call `collectGarbage` or give up. We won't call it here, since we don't
  // know if the caller is in a good state for GC.
  return 0;
}


bool Heap::shouldExpand() const {
  // TODO: come up with a better metric.
  return shouldExpand_;
}


void Heap::expand() {
  auto id = static_cast<u32>(chunks_.size());
  // TODO: handle chunk allocation failure.
  unique_ptr<Chunk> chunk(new(Chunk::kDefaultSize, NOT_EXECUTABLE) Chunk(vm(), id));
  chunks_.push_back(move(chunk));
  shouldExpand_ = false;
}

}
}

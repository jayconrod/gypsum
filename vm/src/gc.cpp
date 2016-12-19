// Copyright 2014,2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "gc.h"

#include <algorithm>
#include <vector>
#include <cstring>
#include "block.h"
#include "block-visitor.h"
#include "heap.h"
#include "tagged.h"

using namespace std;

namespace codeswitch {
namespace internal {

GC::GC(Heap* heap)
    : heap_(heap) { }


class PointerMarkingVisitor: public BlockVisitorBase<PointerMarkingVisitor> {
 public:
  PointerMarkingVisitor(GC& gc, vector<Block*>& markingStack)
      : gc_(gc),
        markingStack_(markingStack) { }

  void visitPointer(Block** p) {
    auto block = *p;
    if (block != nullptr && Tagged<Block>::isPointer(block) && !gc_.isMarked(block)) {
      gc_.mark(block);
      markingStack_.push_back(block);
    }
  }

  void operator () (Block** p) { visitPointer(p); }

 private:
  GC& gc_;
  vector<Block*>& markingStack_;
};


void GC::collectGarbage() {
  ASSERT(heap_->isGcAllowed());

  #ifdef DEBUG
  for (auto chunk : *heap_) {
    ASSERT(!chunk->isMarked());
  }
  #endif

  markLiveObjects();
  #ifdef DEBUG
  // Live object iteration (needed to perform verification) can only be done after marking.
  heap_->verify();
  #endif

  for (auto chunk : *heap_) {
    sweepChunk(chunk);
    chunk->getBitmap().clear();
    chunk->setIsMarked(false);
  }

  #ifdef DEBUG
  heap_->verify();
  #endif
}


void GC::markLiveObjects() {
  // Create a marking stack.
  // TODO: have a bounded size for this. Very long chains of pointers (e.g., linked lists) will
  // cause us to use too much memory.
  vector<Block*> markingStack;

  // Mark the roots.
  PointerMarkingVisitor marker(*this, markingStack);
  heap_->vm()->visitPointers(marker);

  // Recursively mark everything reachable from the roots.
  while (!markingStack.empty()) {
    auto block = markingStack.back();
    markingStack.pop_back();
    ASSERT(isMarked(block));
    marker.visit(block);
  }

  // Set flags indicating the heap has been marked.
  for (auto chunk : *heap_) {
    chunk->setIsMarked(true);
  }
}


bool GC::isMarked(Block* block) {
  ASSERT(block != nullptr && isAligned(reinterpret_cast<Address>(block), kWordSize));
  auto chunk = Chunk::fromAddress(block);
  auto bitmap = chunk->getBitmap();
  auto index = chunk->bitIndexForAddress(block);
  return bitmap.at(index);
}


void GC::mark(Block* block) {
  ASSERT(block != nullptr && isAligned(reinterpret_cast<Address>(block), kWordSize));
  auto chunk = Chunk::fromAddress(block);
  auto bitmap = chunk->getBitmap();
  auto index = chunk->bitIndexForAddress(block);
  bitmap.set(index, true);
}


Address prevLive = 0;

void GC::sweepChunk(Chunk* chunk) {
  // Collect free blocks in this chunk.
  vector<Free*> freeBlocks;
  Address endOfPrevLive = chunk->storageBase();
  for (Address live : *chunk) {
    ASSERT(live >= endOfPrevLive);
    auto free = maybeFreeRange(endOfPrevLive, live);
    if (free)
      freeBlocks.push_back(free);
    auto blockSize = reinterpret_cast<Block*>(live)->sizeOfBlock();
    auto alignedSize = align(blockSize, kWordSize);
    endOfPrevLive = live + alignedSize;
    prevLive = live;
  }
  auto free = maybeFreeRange(endOfPrevLive, chunk->storageLimit());
  if (free)
    freeBlocks.push_back(free);

  // Sort by size, descending.
  sort(freeBlocks.begin(), freeBlocks.end(),
       [](Free* left, Free* right) {
         return left->size() > right->size();
       });

  // Link the free blocks together.
  for (size_t i = 0; i < freeBlocks.size() - 1; i++) {
    freeBlocks[i]->setNext(freeBlocks[i + 1]);
  }
  chunk->setFreeListHead(freeBlocks.empty() ? nullptr : freeBlocks.front());
}


Free* GC::maybeFreeRange(Address begin, Address end) {
  word_t freeSize = end - begin;
  ASSERT(isAligned(freeSize, kWordSize));
  if (freeSize < kMinFreeSize)
    return nullptr;
  auto free = new(reinterpret_cast<void*>(begin), freeSize) Free(nullptr);
  return free;
}

}
}

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "handle-inl.h"

#include <sys/mman.h>
#include "block-inl.h"
#include "error.h"
#include "heap-inl.h"

using namespace std;

namespace codeswitch {
namespace internal {

HandleDataBlock* HandleDataBlock::allocate() {
  // Allocate extra memory from the system to ensure alignment.
  auto mapSize = 2 * kSize;
  auto memory = mmap(nullptr, mapSize,
                     PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS,
                      -1, 0);
  if (memory == MAP_FAILED) {
    throw std::bad_alloc();
  }

  // Free extra memory before and after the aligned block.
  auto begin = reinterpret_cast<Address>(memory);
  auto end = begin + mapSize;
  auto blockBegin = align(begin, kSize);
  auto blockEnd = blockBegin + kSize;
  if (begin != blockBegin) {
    munmap(reinterpret_cast<void*>(begin), blockBegin - begin);
  }
  if (end != blockEnd) {
    munmap(reinterpret_cast<void*>(blockEnd), end - blockEnd);
  }
  auto block = reinterpret_cast<HandleDataBlock*>(blockBegin);

  // Initialize the block header.
  block->setFreeListHead(0);
  block->setFreeCount(kCount);
  block->at(0)->freeNext = kFreeListFreeAfter;
  return block;
}


HandleStorage::HandleStorage()
    : allocationIndex_(0) {
  blocks_.push_back(unique_ptr<HandleDataBlock>(HandleDataBlock::allocate()));
}


HandleData* HandleStorage::allocateHandleSlow(Block* block) {
  // Look for a block which has some free handles.
  auto oldAllocationIndex = allocationIndex_;
  auto n = blocks_.size();
  for (allocationIndex_ = (allocationIndex_ + 1) % n;
       allocationIndex_ != oldAllocationIndex &&
           blocks_[allocationIndex_]->freeCount() == 0;
       allocationIndex_ = (allocationIndex_ + 1) % n);

  // If all blocks are full, create a new empty block.
  if (allocationIndex_ == oldAllocationIndex) {
    blocks_.push_back(unique_ptr<HandleDataBlock>(HandleDataBlock::allocate()));
    allocationIndex_ = blocks_.size() - 1;
  }

  // Allocate from the block we found.
  HandleData* handle = blocks_[allocationIndex_]->allocateHandle(block);
  return handle;
}


}
}

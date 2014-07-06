// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <memory>
#include "block-inl.h"
#include "handle-inl.h"
#include "heap-inl.h"

using namespace std;
using namespace codeswitch::internal;

TEST(HandleDataValidity) {
  HandleData handle;
  handle.refCount = 1;
  ASSERT_TRUE(handle.isValid());
  handle.invalidate();
  ASSERT_EQ(0, handle.refCount);
  ASSERT_FALSE(handle.isValid());
}


TEST(HandleDataBlockOperations) {
  // Check that the block is aligned.
  unique_ptr<HandleDataBlock> block(HandleDataBlock::allocate());
  ASSERT_TRUE(isAligned(reinterpret_cast<Address>(block.get()),
              HandleDataBlock::kSize));
  HandleData* handle = block->allocateHandle(nullptr);
  ASSERT_EQ(block.get(), HandleDataBlock::fromHandle(handle));
  block->freeHandle(handle);

  // Allocate all of the handles.
  ASSERT_EQ(HandleDataBlock::kCount, block->freeCount());
  Block* fakeBlock = reinterpret_cast<Block*>(0x1000);
  for (word_t i = 0; i < HandleDataBlock::kCount; i++) {
    HandleData* handle = block->allocateHandle(fakeBlock);
    ASSERT_EQ(1, handle->refCount);
    ASSERT_EQ(fakeBlock, handle->block);
  }
  ASSERT_EQ(0, block->freeCount());

  // Free some of the handles to create a free list.
  for (word_t i = 0; i < HandleDataBlock::kCount; i += 2) {
    block->freeHandle(block->at(i));
  }

  // Reallocate those handles;
  while (block->freeCount() > 0) {
    block->allocateHandle(nullptr);
  }
}


TEST(HandleStorageOperations) {
  HandleStorage storage;

  // Allocate lots of handles.
  for (word_t i = 0; i < 2 * HandleDataBlock::kCount; i++) {
    storage.allocateHandle(nullptr);
  }

  // Iterate over them.
  word_t count = 0;
  for (HandleData& handle : storage) {
    USE(handle);
    count++;
  }
  ASSERT_EQ(2 * HandleDataBlock::kCount, count);
}

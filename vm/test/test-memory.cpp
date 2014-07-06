// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <memory>
#include <unistd.h>
#include "array.h"
#include "heap-inl.h"
#include "memory-inl.h"
#include "utils.h"
#include "vm-inl.h"

using namespace std;
using namespace codeswitch::internal;

TEST(MemoryChunkAlignment) {
  const auto systemPageSize = static_cast<word_t>(sysconf(_SC_PAGESIZE));

  // Small allocation with large alignment.
  {
    unique_ptr<MemoryChunk> chunk(
        MemoryChunk::allocate(systemPageSize, Page::kSize, MemoryChunk::READABLE));
    ASSERT_EQ(systemPageSize, chunk->size());
    ASSERT_TRUE(isAligned(chunk->base(), Page::kSize));
    ASSERT_EQ(MemoryChunk::READABLE, chunk->protection());
  }

  // Large allocation with small alignment.
  {
    unique_ptr<MemoryChunk> chunk(
        MemoryChunk::allocate(Page::kSize, systemPageSize, MemoryChunk::READABLE));
    ASSERT_EQ(Page::kSize, chunk->size());
    ASSERT_TRUE(isAligned(chunk->base(), systemPageSize));
    ASSERT_EQ(MemoryChunk::READABLE, chunk->protection());
  }
}


TEST(MemoryChunkShrink) {
  unique_ptr<MemoryChunk> chunk(
      MemoryChunk::allocate(Page::kSize, Page::kSize, MemoryChunk::READABLE));
  ASSERT_EQ(Page::kSize, chunk->size());
  chunk->shrink(Page::kSize / 2);
  ASSERT_EQ(Page::kSize / 2, chunk->size());
}


TEST(PageIteration) {
  VM vm;
  auto heap = vm.heap();

  // Create a new page.
  unique_ptr<Page> page(Page::allocate(&vm));

  // Verify it's empty.
  ASSERT_TRUE(page->begin() == page->end());

  // Allocate some objects.
  vector<I32Array*> arrays;
  {
    Heap::AllocationRangeScope scope(heap, page.get());
    for (int i = 0; i < 3; i++) {
      I32Array* array = I32Array::tryAllocate(heap, i);
      arrays.push_back(array);
    }
  }

  // Verify iterating over the page gives the same objects.
  Page::iterator it = page->begin();
  for (I32Array* array : arrays) {
    ASSERT_TRUE(it != page->end());
    ASSERT_EQ(array, *it);
    ++it;
  }
  ASSERT_TRUE(it == page->end());
}


TEST(SpaceIteration) {
  VM vm;

  // Create a space.
  Space space(&vm, PACKAGE_SPACE);

  // Make sure it's empty.
  ASSERT_EQ(0, space.pages().size());
  ASSERT_TRUE(space.begin() == space.end());

  // Add a few pages.
  for (word_t i = 0; i < 5; i++) {
    space.expand();
    ASSERT_EQ(i + 1, space.pages().size());
  }

  // Make sure we can iterate over them.
  Space::iterator it = space.begin();
  for (unique_ptr<Page>& page : space.pages()) {
    ASSERT_TRUE(it != space.end());
    ASSERT_EQ(page.get(), *it);
    ++it;
  }
  ASSERT_TRUE(it == space.end());
}

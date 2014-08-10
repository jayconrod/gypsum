// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <memory>
#include "handle-inl.h"
#include "heap.h"
#include "memory.h"
#include "remembered-set.h"
#include "utils.h"

using namespace std;
using namespace codeswitch::internal;

TEST(RememberedSetIteration) {
  RememberedSet set;
  word_t slots[] = { 0x2000, 0x1000, 0x2000, 0x3000, 0x4000, 0x1000 };
  for (auto i = 0U; i < ARRAY_LENGTH(slots); i++)
    set.add(reinterpret_cast<Block**>(slots[i]));

  word_t expectedSlots[] = { 0x1000, 0x2000, 0x3000, 0x4000 };
  int n = ARRAY_LENGTH(expectedSlots);
  int i = 0;
  for (Block** slot : set) {
    ASSERT_EQ(reinterpret_cast<Block**>(expectedSlots[i++]), slot);
  }
  ASSERT_EQ(n, i);
}


TEST(WriteBarrierSameChunk) {
  unique_ptr<Chunk> chunk(new(Chunk::kDefaultSize, NOT_EXECUTABLE) Chunk(nullptr, 0));
  auto slot = reinterpret_cast<Block**>(chunk->storageBase());
  *slot = reinterpret_cast<Block*>(chunk->storageBase());
  Heap::recordWrite(slot, *slot);
  ASSERT_EQ(0, chunk->rememberedSet().length());
}


// TODO: test write barrier for different chunks.

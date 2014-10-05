// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <cstring>
#include "block.h"
#include "handle.h"
#include "heap.h"
#include "object.h"
#include "vm.h"

using namespace codeswitch::internal;

TEST(ObjectsAreZeroInitialized) {
  VM vm;
  Heap* heap = vm.heap();

  word_t objectSize = 16;
  auto meta = new(heap, 0, objectSize, 0) Meta(OBJECT_BLOCK_TYPE);
  auto dummy = new(heap, meta) Object;
  ASSERT_EQ(objectSize, dummy->sizeOfBlock());
  memset(reinterpret_cast<char*>(dummy) + objectSize, 0xef, 100);
  auto obj = new(heap, meta) Object;
  auto value = mem<word_t>(obj, kWordSize);
  ASSERT_EQ(0, value);
}

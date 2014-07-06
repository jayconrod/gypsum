// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include "array.h"
#include "heap-inl.h"
#include "object.h"
#include "vm-inl.h"

using namespace codeswitch::internal;

TEST(HeapIteration) {
  VM vm;
  Heap* heap = vm.heap();

  Heap::iterator it = heap->begin();
  ASSERT_EQ(heap->newSpace()->toSpace(), *it);
  ++it;
  ASSERT_EQ(heap->packageSpace(), *it);
  ++it;
  ASSERT_EQ(heap->end(), it);
}


TEST(AllocateUnaligned) {
  VM vm;
  Heap* heap = vm.heap();

  const word_t length = 7;
  auto first = I8Array::tryAllocate(heap, length);
  ASSERT_TRUE(isAligned(reinterpret_cast<word_t>(first), Heap::kBlockAlignment));
  auto size = first->sizeOfBlock();
  ASSERT_EQ(I8Array::kHeaderSize + length, size);
  auto second = I8Array::tryAllocate(heap, length);
  ASSERT_EQ(align(reinterpret_cast<word_t>(first) + size, Heap::kBlockAlignment),
            reinterpret_cast<word_t>(second));
}

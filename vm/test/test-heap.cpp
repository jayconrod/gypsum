// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include "handle.h"
#include "heap.h"
#include "vm.h"

using namespace codeswitch::internal;

TEST(HeapIteration) {
  VM vm;
  Heap* heap = vm.heap();
  Heap::iterator it = heap->begin();
  ASSERT_EQ(heap->begin(), it);
  ASSERT_NE(heap->end(), it);
  ++it;
  ASSERT_NE(heap->begin(), it);
  ASSERT_EQ(heap->end(), it);
}


TEST(AllocateUnaligned) {
  VM vm;
  auto heap = vm.heap();
  auto addr = heap->allocate(7);
  ASSERT_NE(0, addr);
}

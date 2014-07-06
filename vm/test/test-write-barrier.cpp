// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <memory>
#include "handle-inl.h"
#include "heap-inl.h"
#include "memory-inl.h"
#include "remembered-set-inl.h"
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


TEST(WriteBarrierSamePage) {
  VM vm(0);
  unique_ptr<Page> page(Page::allocate(&vm));
  page->setIdentity(NEW_SPACE);
  auto slot = reinterpret_cast<Block**>(page->allocationBase());
  *slot = reinterpret_cast<Block*>(page->allocationBase());
  Heap::recordWrite(slot, *slot);
  ASSERT_EQ(0, page->rememberedSet()->length());
}


TEST(WriteBarrierFromNewToPackage) {
  VM vm(0);
  unique_ptr<Page> newPage(Page::allocate(&vm));
  newPage->setIdentity(NEW_SPACE);
  unique_ptr<Page> packagePage(Page::allocate(&vm));
  packagePage->setIdentity(PACKAGE_SPACE);
  auto slot = reinterpret_cast<Block**>(newPage->allocationBase());
  auto block = reinterpret_cast<Block*>(packagePage->allocationBase());
  *slot = block;
  Heap::recordWrite(slot, block);
  ASSERT_EQ(0, newPage->rememberedSet()->length());
  ASSERT_EQ(0, packagePage->rememberedSet()->length());
}


TEST(WriteBarrierFromPackageToNew) {
  VM vm(0);
  unique_ptr<Page> newPage(Page::allocate(&vm));
  newPage->setIdentity(NEW_SPACE);
  unique_ptr<Page> packagePage(Page::allocate(&vm));
  packagePage->setIdentity(PACKAGE_SPACE);
  auto slot = reinterpret_cast<Block**>(packagePage->allocationBase());
  auto block = reinterpret_cast<Block*>(newPage->allocationBase());
  *slot = block;
  Heap::recordWrite(slot, block);
  ASSERT_EQ(1, newPage->rememberedSet()->length());
  ASSERT_EQ(slot, *newPage->rememberedSet()->begin());
  ASSERT_EQ(0, packagePage->rememberedSet()->length());
}

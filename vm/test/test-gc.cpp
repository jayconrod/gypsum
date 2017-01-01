// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include "array.h"
#include "block.h"
#include "gc.h"
#include "handle.h"
#include "memory.h"
#include "stack.h"
#include "vm.h"

using namespace codeswitch::internal;

TEST(NotMarkedBeforeGC) {
  VM vm;
  auto block = vm.roots()->metaMeta();
  ASSERT_FALSE(GC::isMarked(block));
  auto chunk = Chunk::fromAddress(block);
  ASSERT_FALSE(chunk->isMarked());
}


TEST(RootsAreMarked) {
  VM vm;
  GC gc(vm.heap());
  gc.markLiveObjects();
  auto root = vm.roots()->metaMeta();
  ASSERT_TRUE(GC::isMarked(root));
  auto chunk = Chunk::fromAddress(root);
  ASSERT_TRUE(chunk->isMarked());
}


TEST(HandlesAreMarked) {
  VM vm;
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);
  Local<BlockArray<Block>> localArray = BlockArray<Block>::create(vm.heap(), 10);
  Persistent<BlockArray<Block>> persistentArray(new(vm.heap(), 10) BlockArray<Block>);
  GC gc(vm.heap());
  gc.markLiveObjects();
  ASSERT_TRUE(GC::isMarked(*localArray));
  ASSERT_TRUE(GC::isMarked(*persistentArray));
}


TEST(OldHandlesAreUnmarked) {
  VM vm;
  AllowAllocationScope allowAllocation(vm.heap(), true);
  BlockArray<Block>* rawArray = nullptr;
  {
    HandleScope handleScope(&vm);
    auto array = BlockArray<Block>::create(vm.heap(), 10);
    rawArray = *array;
  }
  GC gc(vm.heap());
  gc.markLiveObjects();
  ASSERT_FALSE(GC::isMarked(rawArray));
}


TEST(EscapedHandlesAreMarked) {
  VM vm;
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope outerScope(&vm);
  BlockArray<Block>* rawArray = nullptr;
  {
    HandleScope innerScope(&vm);
    auto innerArray = BlockArray<Block>::create(vm.heap(), 10);
    auto outerArray = innerScope.escape(*innerArray);
    rawArray = *outerArray;
  }
  GC gc(vm.heap());
  gc.markLiveObjects();
  ASSERT_TRUE(GC::isMarked(rawArray));
}


TEST(IndirectObjectsAreMarked) {
  VM vm;
  AllowAllocationScope allowAllocation(vm.heap(), true);
  auto a = new(vm.heap(), 2) BlockArray<Block>;
  auto b = new(vm.heap(), 2) BlockArray<Block>;
  auto c = new(vm.heap(), 2) BlockArray<Block>;
  auto d = new(vm.heap(), 2) BlockArray<Block>;
  auto e = new(vm.heap(), 2) BlockArray<Block>;
  a->set(0, b);
  a->set(1, c);
  b->set(0, c);
  c->set(0, b);
  d->set(0, e);
  Persistent<BlockArray<Block>> h(a);
  GC gc(vm.heap());
  gc.markLiveObjects();
  ASSERT_TRUE(GC::isMarked(a));
  ASSERT_TRUE(GC::isMarked(b));
  ASSERT_TRUE(GC::isMarked(c));
  ASSERT_FALSE(GC::isMarked(d));
  ASSERT_FALSE(GC::isMarked(e));
}


TEST(StackIsMarked) {
  VM vm;
  GC gc(vm.heap());
  gc.markLiveObjects();
  ASSERT_TRUE(GC::isMarked(*vm.stack()));
}


TEST(Sweep) {
  VM vm;
  AllowAllocationScope allowAllocation(vm.heap(), true);
  const int kLength = 128;
  Block* a = new(vm.heap(), kLength) I8Array;
  Persistent<I8Array> b(new(vm.heap(), kLength) I8Array);
  Block* c = new(vm.heap(), kLength) I8Array;
  Block* d = new(vm.heap(), 125) I8Array;  // unaligned
  USE(d);
  Persistent<I8Array> e(new(vm.heap(), kLength) I8Array);
  const int kExpectedArraySize = align(kWordSize + sizeof(length_t), kWordSize) + kLength;

  GC gc(vm.heap());
  gc.collectGarbage();
  ASSERT_EQ(FREE_BLOCK_TYPE, a->blockType());
  ASSERT_EQ(kExpectedArraySize, a->sizeOfBlock());
  ASSERT_EQ(kExpectedArraySize - sizeof(Free), block_cast<Free>(a)->size());
  ASSERT_EQ(nullptr, block_cast<Free>(a)->next());
  ASSERT_EQ(FREE_BLOCK_TYPE, c->blockType());
  ASSERT_EQ(2 * kExpectedArraySize, c->sizeOfBlock());
  ASSERT_EQ(a, block_cast<Free>(c)->next());
  auto chunk = Chunk::fromAddress(a);
  ASSERT_EQ(c, chunk->freeListHead()->next());
}

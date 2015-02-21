// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "memory.h"

#include <algorithm>
#include <utility>
#include <unistd.h>
#include <sys/fcntl.h>
#include "bitmap.h"
#include "block.h"
#include "error.h"
#include "handle.h"
#include "heap.h"
#include "platform.h"
#include "vm.h"

using namespace std;

namespace codeswitch {
namespace internal {

bool AllocationRange::canAllocate(size_t size) const {
  ASSERT(isValid());
  auto newBase = base_ + size;
  bool overflow = newBase < base_;
  return !overflow && newBase <= limit_;
}


Address AllocationRange::allocate(size_t size) {
  STATIC_ASSERT(sizeof(base_) == sizeof(size));
  ASSERT(isValid());
  auto newBase = base_ + size;
  // We can safely check for overflow because unsigned addition is defined to wrap.
  bool overflow = newBase < base_;
  if (overflow || newBase > limit_)
    return 0;
  auto addr = base_;
  base_ = newBase;
  return addr;
}


void* Chunk::operator new (size_t unused, size_t size, Executable executable) {
  ASSERT(isAligned(size, PAGESIZE));
  // TODO: support large allocations
  ASSERT(size == kDefaultSize);

  // We can't guarantee the kernel will give us an aligned chunk, so we ask for some extra
  // and align the chunk within the region given to us.
  // TODO: ASLR
  auto alignedSize = size + kDefaultSize;
  auto prot = kReadable | kWritable | (executable == EXECUTABLE ? kExecutable : 0);
  auto base = allocateMemory(alignedSize, prot);

  // Free the extra memory at the beginning and end.
  auto start = align(base, kDefaultSize);
  auto end = start + size;
  auto extraBefore = start - base;
  if (extraBefore > 0)
    releaseMemory(base, extraBefore);
  auto extraAfter = (base + alignedSize) - end;
  if (extraAfter > 0)
    releaseMemory(end, extraAfter);

  Chunk* chunk = reinterpret_cast<Chunk*>(start);
  chunk->size_ = size;
  chunk->executable_ = executable;

  return reinterpret_cast<void*>(start);
}


Chunk::Chunk(VM* vm, u32 id)
    : vm_(vm),
      id_(id) {
  // We don't initialize the marking bitmap or the contents of the page, since the kernel will
  // zero-initialize pages before giving them to us.
  auto freePlace = reinterpret_cast<void*>(storageBase());
  auto free = new(freePlace, storageSize()) Free(nullptr);
  setFreeListHead(free);
}


void Chunk::operator delete (void* addr) {
  auto size = reinterpret_cast<Chunk*>(addr)->size();
  releaseMemory(reinterpret_cast<Address>(addr), size);
}


Bitmap Chunk::getBitmap() {
  return Bitmap(reinterpret_cast<word_t*>(bitmapBase()), bitmapSize() * 8);
}


Address Chunk::bitmapBase() const {
  return align(base() + sizeof(Chunk), kWordSize);
}


size_t Chunk::bitmapSize() const {
  auto nonHeaderSize = alignDown(size() - sizeof(Chunk), kWordSize);
  auto nonHeaderWords = nonHeaderSize / kWordSize;
  auto bitmapWords = (nonHeaderWords + WORDSIZE) / (WORDSIZE + 1);
  ASSERT(bitmapWords * WORDSIZE - (nonHeaderWords - bitmapWords) < WORDSIZE);
  return bitmapWords * kWordSize;
}


word_t Chunk::bitIndexForAddress(Address addr) const {
  ASSERT(isAligned(addr, kWordSize));
  return (addr - storageBase()) / kWordSize;
}


Address Chunk::storageBase() const {
  ASSERT(isAligned(bitmapSize(), kWordSize));
  return bitmapBase() + bitmapSize();
}


Address Chunk::storageLimit() const {
  return limit();
}


Chunk::iterator::iterator(Chunk* chunk, word_t index)
    : storageBase_(chunk->storageBase()),
      bitmap_(chunk->getBitmap()),
      index_(index) { }


Address Chunk::iterator::operator * () {
  return storageBase_ + index_ * kWordSize;
}


bool Chunk::iterator::operator == (const iterator& other) const {
  ASSERT(storageBase_ == other.storageBase_);
  return index_ == other.index_;
}


Chunk::iterator& Chunk::iterator::operator ++ () {
  index_ = findNextIndex(bitmap_, index_ + 1, bitmap_.bitCount());
  return *this;
}


word_t Chunk::iterator::findNextIndex(Bitmap bitmap, word_t from, word_t limit) {
  word_t i;
  for (i = from; i < limit; i++) {
    if (bitmap[i])
      break;
  }
  return i;
}


Chunk::iterator Chunk::begin() {
  return iterator(this, iterator::findNextIndex(getBitmap(), 0, getBitmap().bitCount()));
}


Chunk::iterator Chunk::end() {
  return iterator(this, getBitmap().bitCount());
}

}
}

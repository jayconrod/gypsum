// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "memory.h"

#include <algorithm>
#include <utility>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/fcntl.h>
#include "bitmap.h"
#include "block-inl.h"
#include "error.h"
#include "handle-inl.h"
#include "heap-inl.h"
#include "remembered-set-inl.h"
#include "vm.h"

using namespace std;

namespace codeswitch {
namespace internal {

OptP<Address> AllocationRange::allocate(size_t size) {
  STATIC_ASSERT(sizeof(base_) == sizeof(size));
  ASSERT(isValid());
  auto newBase = base_ + size;
  // We can safely check for overflow because unsigned addition is defined to wrap.
  bool overflow = newBase < base_;
  if (overflow || newBase > limit_)
    return OptP<Address>();
  auto addr = base_;
  base_ = newBase;
  return Some(addr);
}


void* Chunk::operator new (size_t unused, size_t size, Executable executable) {
  ASSERT(isAligned(size, PAGESIZE));
  // TODO: support large allocations
  ASSERT(size == kDefaultSize);

  // TODO: ASLR
  Address base = 0;

  // We can't guarantee the kernel will give us an aligned chunk, so we ask for some extra
  // and align the chunk within the region given to us.
  auto alignedSize = size + kDefaultSize;
  auto prot = PROT_READ | PROT_WRITE | (executable == EXECUTABLE ? PROT_EXEC : 0);
  auto addr = mmap(reinterpret_cast<void*>(base), alignedSize, prot,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (addr == MAP_FAILED) {
    throw Error("could not allocate memory");
  }
  base = reinterpret_cast<Address>(addr);

  // Free the extra memory at the beginning and end.
  auto start = align(base, kDefaultSize);
  auto end = start + size;
  auto extraBefore = start - base;
  if (extraBefore > 0)
    munmap(reinterpret_cast<void*>(base), extraBefore);
  auto extraAfter = (base + alignedSize) - end;
  if (extraAfter > 0)
    munmap(reinterpret_cast<void*>(end), extraAfter);

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
  setFreeListHead(Some(free));
}


void Chunk::operator delete (void* addr) {
  auto size = reinterpret_cast<Chunk*>(addr)->size();
  munmap(addr, size);
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


Address Chunk::storageBase() const {
  ASSERT(isAligned(bitmapSize(), kWordSize));
  return bitmapBase() + bitmapSize();
}


Address Chunk::storageLimit() const {
  return limit();
}


MemoryChunk* MemoryChunk::allocate(word_t size, word_t alignment, Protection protection) {
  // We can't guarantee the kernel will give us an aligned chunk, so we ask for some extra
  // and align the chunk within the region given to us.
  Address base = randomAddress(alignment);
  word_t alignedSize = size + alignment;
  int prot = PROT_READ | PROT_WRITE |   // initially permissive prot, so we can write header
             (protection & EXECUTABLE ? PROT_EXEC : 0);
  void* addr = mmap(reinterpret_cast<void*>(base), alignedSize, prot,
                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (addr == MAP_FAILED) {
    throw Error("could not allocate memory");
  }
  base = reinterpret_cast<Address>(addr);

  // Free the extra memory at the beginning and end.
  Address start = align(base, alignment);
  Address end = start + size;
  size_t extraBefore = start - base;
  if (extraBefore > 0)
    munmap(reinterpret_cast<void*>(base), extraBefore);
  size_t extraAfter = (base + alignedSize) - end;
  if (extraAfter > 0)
    munmap(reinterpret_cast<void*>(end), extraAfter);

  // Write the header.
  MemoryChunk* chunk = reinterpret_cast<MemoryChunk*>(start);
  mem<word_t>(chunk, kSizeOffset) = size;
  mem<word_t>(chunk, kAlignmentOffset) = alignment;
  mem<word_t>(chunk, kProtectionOffset) = static_cast<word_t>(protection);

  // Change the protection in case the client wanted something more restrictive.
  int client_prot = (protection & READABLE   ? PROT_READ  : 0) |
                    (protection & WRITABLE   ? PROT_WRITE : 0) |
                    (protection & EXECUTABLE ? PROT_EXEC  : 0);
  if (client_prot != prot) {
    mprotect(reinterpret_cast<void*>(start), size, client_prot);
  }

  return chunk;
}


void MemoryChunk::operator delete (void* chunk) {
  auto self = reinterpret_cast<MemoryChunk*>(chunk);
  munmap(reinterpret_cast<void*>(chunk), self->size());
}


void MemoryChunk::shrink(word_t newSize) {
  ASSERT(newSize <= size());
  ASSERT(newSize > 0);
  if (newSize == size())
    return;
  munmap(reinterpret_cast<void*>(base() + newSize), size() - newSize);
  int prot = (protection() & READABLE   ? PROT_READ  : 0) |
             (protection() & WRITABLE   ? PROT_WRITE : 0) |
             (protection() & EXECUTABLE ? PROT_EXEC  : 0);
  if ((prot & PROT_WRITE) == 0) {
    mprotect(reinterpret_cast<void*>(this), PAGESIZE, prot | PROT_WRITE);
  }
  mem<word_t>(this, kSizeOffset) = newSize;
  if ((prot & PROT_WRITE) == 0) {
    mprotect(reinterpret_cast<void*>(this), PAGESIZE, prot);
  }
}


Address MemoryChunk::randomAddress(word_t alignment) {
  int ret;
  int fd = open("/dev/urandom", O_RDONLY);
  if (fd == -1)
    goto fail;

  Address addr;
  ret = read(fd, reinterpret_cast<char*>(&addr), sizeof(addr));
  if (ret != sizeof(addr))
    goto fail;

#if WORDSIZE == 64
  addr &= 0x3ffffffffffff000 & ~(alignment - 1);
#else
  addr &= 0x3ffff000 & ~(alignment - 1);
  addr += 0x20000000;
#endif
  return addr;

 fail:
  if (fd != -1)
    close(fd);
  return 0;
}


Page* Page::allocate(VM* vm) {
  MemoryChunk* chunk = MemoryChunk::allocate(kSize, kSize,
                                             static_cast<Protection>(READABLE | WRITABLE));
  Page* page = reinterpret_cast<Page*>(chunk);
  page->setVm(vm);
  page->setHeap(vm->heap());
  page->setAllocationPtr(reinterpret_cast<Address>(page) + kPageHeaderSize);
  page->setRememberedSet(new RememberedSet());
  return page;
}


Page::~Page() {
  if (rememberedSet() != nullptr)
    delete rememberedSet();
}


void Page::shrinkToAllocationPtr() {
  word_t usedSize = align(allocationPtr() - base(), PAGESIZE);
  shrink(usedSize);
}


void Space::expand() {
  unique_ptr<Page> page(Page::allocate(vm_));
  page->setIdentity(identity_);
  pages_.push_back(move(page));
}


NewSpace::NewSpace(VM* vm)
    : vm_(vm),
      semiSpace1_(vm, NEW_SPACE),
      semiSpace2_(vm, NEW_SPACE),
      toSpace_(&semiSpace1_),
      fromSpace_(&semiSpace2_) { }


void NewSpace::expand() {
  toSpace_->expand();
  fromSpace_->expand();
}


void NewSpace::swap() {
  std::swap(toSpace_, fromSpace_);
  for (unique_ptr<Page>& page : toSpace_->pages()) {
    page->setAllocationPtr(page->allocationBase());
  }
}

}
}

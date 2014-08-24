// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef memory_h
#define memory_h

#include <iterator>
#include <memory>
#include <vector>
#include "list.h"
#include "remembered-set.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Bitmap;
class Block;
class Free;
class Heap;
class VM;

enum SpaceId {
  NEW_SPACE,
  PACKAGE_SPACE,
  SPACE_COUNT
};
static_assert(SPACE_COUNT <= 16, "space id must fit in 4 bits");


/** An AllocationRange is a contiguous range of memory currently being used for allocation.
 *  `base` indicates where the next allocation should occur. `limit` indicates the end of
 *  the range.
 */
class AllocationRange {
 public:
  AllocationRange()
      : base_(0), limit_(0) { }
  AllocationRange(Address base, Address limit)
      : base_(base), limit_(limit) { }

  static AllocationRange empty() { return AllocationRange(); }
  bool isValid() const { return base_ != 0 && limit_ != 0; }

  Address base() const { return base_; }
  Address limit() const { return limit_; }
  Address size() const { return limit_ - base_; }

  /** Returns true if an allocation of the given size will succeed. */
  bool canAllocate(size_t size) const;

  /** Increment `base` by `size` bytes and return the old value of `base`. If `base` exceeds
   *  `limit`, the allocation fails, and 0 is returned.
   */
  Address allocate(size_t size);

  bool operator == (const AllocationRange& other) const {
    return base_ == other.base_ && limit_ == other.limit_;
  }
  bool operator != (const AllocationRange& other) const { return !(*this == other); }

 private:
  Address base_;
  Address limit_;
};


enum Executable {
  NOT_EXECUTABLE,
  EXECUTABLE
};


/** A Chunk is the basic unit of memory allocated from the operating system. The garbage
 *  collected heap is composed of chunks. Each chunk contains some header information, a
 *  marking bitmap (for the garbage collector), and an area where blocks can be allocated.
 */
class Chunk {
 public:
  static const word_t kDefaultSize = 1 * MB;
  static const word_t kMaxBlockSize = kDefaultSize - 16 * KB;

  void* operator new (size_t unused, size_t size, Executable executable);
  explicit Chunk(VM* vm, u32 id);
  void operator delete (void* addr);

  static Chunk* fromAddress(Address addr) {
    return reinterpret_cast<Chunk*>(alignDown(addr, kDefaultSize));
  }
  static Chunk* fromAddress(void* addr) {
    return fromAddress(reinterpret_cast<Address>(addr));
  }

  Address base() const { return reinterpret_cast<Address>(this); }
  word_t size() const { return kDefaultSize; }
  Address limit() const { return base() + size(); }
  bool contains(Address addr) const { return base() <= addr && addr < limit(); }

  bool isExecutable() const { return executable_ == EXECUTABLE; }
  Executable executable() const { return executable_; }

  VM* vm() const { return vm_; }
  u32 id() const { return id_; }

  AllocationRange& allocationRange() { return allocationRange_; }
  void setAllocationRange(AllocationRange range) { allocationRange_ = range; }

  Free* freeListHead() const { return freeListHead_; }
  void setFreeListHead(Free* free) { freeListHead_ = free; }

  RememberedSet& rememberedSet() { return rememberedSet_; }

  Bitmap getBitmap();
  Address bitmapBase() const;
  size_t bitmapSize() const;

  Address storageBase() const;
  Address storageLimit() const;
  size_t storageSize() const { return storageLimit() - storageBase(); }
  bool inStorageRange(Address addr) const {
    return storageBase() <= addr && addr < storageLimit();
  }

 private:
  word_t size_;
  Executable executable_;
  VM* vm_;
  u32 id_;
  AllocationRange allocationRange_;
  Free* freeListHead_;
  RememberedSet rememberedSet_;
};

}
}

#endif

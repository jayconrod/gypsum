// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef memory_h
#define memory_h

#include <iterator>
#include <memory>
#include <vector>
#include "list.h"
#include "option.h"
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
  constexpr AllocationRange(Address base, Address limit)
      : base_(base), limit_(limit) { }

  static AllocationRange getInvalid() { return AllocationRange(0, 0); }
  bool isValid() const { return base_ != 0 && limit_ != 0; }
  static bool isValid(const AllocationRange& range) { return range.isValid(); }

  Address base() const { return base_; }
  Address limit() const { return limit_; }
  Address size() const { return limit_ - base_; }

  /** Increment `base` by `size` bytes and return the old value of `base`. If `base` exceeds
   *  `limit`, the allocation fails, and `None` is returned.
   */
  OptP<Address> allocate(size_t size);

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

  Address base() const { return reinterpret_cast<Address>(this); }
  word_t size() const { return kDefaultSize; }
  Address limit() const { return base() + size(); }
  bool contains(Address addr) const { return base() <= addr && addr < limit(); }

  bool isExecutable() const { return executable_ == EXECUTABLE; }
  Executable executable() const { return executable_; }

  VM* vm() const { return vm_; }
  u32 id() const { return id_; }

  Option<AllocationRange>& allocationRange() { return allocationRange_; }
  void setAllocationRange(Option<AllocationRange> range) { allocationRange_ = range; }

  OptP<Free*> freeListHead() const { return freeListHead_; }
  void setFreeListHead(OptP<Free*> free) { freeListHead_ = free; }

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
  Option<AllocationRange> allocationRange_;
  OptP<Free*> freeListHead_;
  RememberedSet rememberedSet_;
};


class MemoryChunk {
 public:
  enum Protection {
    READABLE = 1,
    WRITABLE = 2,
    EXECUTABLE = 4,
  };

  static MemoryChunk* allocate(word_t size, word_t alignment, Protection protection);
  void operator delete(void* chunk);

  Address base() { return reinterpret_cast<Address>(this); }
  word_t size() { return mem<word_t>(this, kSizeOffset); }
  Address limit() { return base() + size(); }
  Protection protection() {
    return static_cast<Protection>(mem<word_t>(this, kProtectionOffset));
  }

  bool inRange(Address addr) { return base() <= addr && addr < limit(); }

  void shrink(word_t newSize);

  static const word_t kSizeOffset = 0;
  static const word_t kAlignmentOffset = kSizeOffset + kWordSize;
  static const word_t kProtectionOffset = kAlignmentOffset + kWordSize;
  static const word_t kMemoryChunkHeaderSize = kProtectionOffset + kWordSize;

 private:
  static Address randomAddress(word_t alignment);
};


class VM;
class Heap;

class Page: public MemoryChunk {
 public:
  static Page* allocate(VM* vm);
  ~Page();

  static inline Page* fromAddress(void* addr);
  static inline Page* fromAddress(Address addr);

  DEFINE_INL_ACCESSORS(VM*, vm, setVm, kVmOffset)
  DEFINE_INL_ACCESSORS(Heap*, heap, setHeap, kHeapOffset)
  DEFINE_INL_ACCESSORS(Address, allocationPtr, setAllocationPtr, kAllocationPtrOffset)
  DEFINE_INL_ACCESSORS(RememberedSet*, rememberedSet, setRememberedSet, kRememberedSetOffset)
  DEFINE_INL_ACCESSORS(u64, flags, setFlags, kFlagsOffset)
  DEFINE_INL_BIT_ACCESSORS(SpaceId, identity, setIdentity,
                           kFlagsOffset, kIdentityWidth, kIdentityShift)

  Address allocationBase() { return reinterpret_cast<Address>(this) + kPageHeaderSize; }

  inline bool contains(Block* block);

  void shrinkToAllocationPtr();

  class iterator: public std::iterator<std::input_iterator_tag, Block*> {
   public:
    explicit inline iterator(Address pos);
    inline Block* operator * ();
    inline bool operator == (const iterator& other) const;
    inline bool operator != (const iterator& other) const {
      return !(*this == other);
    }
    inline iterator& operator ++ ();
   private:
    Address pos_;
  };

  inline iterator begin();
  inline iterator end();

  static const word_t kSize = 1 * MB;

  static const word_t kVmOffset = kMemoryChunkHeaderSize;
  static const word_t kHeapOffset = kVmOffset + kWordSize;
  static const word_t kAllocationPtrOffset = kHeapOffset + kWordSize;
  static const word_t kRememberedSetOffset = kAllocationPtrOffset + kWordSize;
  static const word_t kFlagsOffset = kRememberedSetOffset + kWordSize;
  static const word_t kPageHeaderSize = kFlagsOffset + sizeof(u64);

  static const word_t kIdentityWidth = 4;
  static const word_t kIdentityShift = 0;

  static const word_t kAllocatableSize = kSize - kPageHeaderSize;
};


class Space {
 public:
  explicit Space(VM* vm, SpaceId identity)
      : vm_(vm),
        identity_(identity) { }
  Space(const Space&) = delete;
  Space& operator = (const Space&) = delete;

  SpaceId identity() { return identity_; }
  std::vector<std::unique_ptr<Page>>& pages() { return pages_; }

  inline bool contains(Block* block);
  void expand();
  void releasePage(Page* page);

  class iterator: public std::iterator<std::input_iterator_tag, Space*> {
   public:
    explicit inline iterator(std::vector<std::unique_ptr<Page>>::iterator it);
    inline Page* operator * ();
    inline bool operator == (const iterator& other) const;
    inline bool operator != (const iterator& other) const {
      return !(*this == other);
    }
    inline iterator& operator ++ ();
   private:
    std::vector<std::unique_ptr<Page>>::iterator it_;
  };

  inline iterator begin();
  inline iterator end();

 private:
  VM* vm_;
  SpaceId identity_;
  std::vector<std::unique_ptr<Page>> pages_;
};


class NewSpace {
 public:
  explicit NewSpace(VM* vm);

  Space* toSpace() { return toSpace_; }
  Space* fromSpace() { return fromSpace_; }

  inline bool contains(Block* block);
  void expand();
  void swap();

 private:
  VM* vm_;
  Space semiSpace1_, semiSpace2_;
  Space* toSpace_;
  Space* fromSpace_;
  NON_COPYABLE(NewSpace)
};

}
}

#endif

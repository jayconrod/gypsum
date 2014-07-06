// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef memory_h
#define memory_h

#include <iterator>
#include <memory>
#include <vector>
#include "list.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Block;
class Heap;
class RememberedSet;

enum SpaceId {
  NEW_SPACE,
  PACKAGE_SPACE,
  SPACE_COUNT
};
static_assert(SPACE_COUNT <= 16, "space id must fit in 4 bits");


/** A MemoryChunk is an aligned region of contiguous memory obtained from the system using
 *  anonymous mapping. Its size must be a multiple of the system page size. Some information
 *  about the memory chunk is stored at the beginning of the chunk itself. Note that this is
 *  a pointer object: "this" always points to the beginning of the actual chunk.
 */
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

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef heap_h
#define heap_h

#include <exception>
#include <iterator>
#include "builtins.h"
#include "error.h"
#include "memory.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Function;
class VM;

#define DEFINE_NEW(clas, type)                                                        \
  void* operator new (size_t, Heap* heap) {                                           \
    auto _block = reinterpret_cast<clas*>(heap->allocateUninitialized(sizeof(clas))); \
    _block->setMetaWord(type);                                                        \
    return _block;                                                                    \
  }                                                                                   \


// TODO: remove when no longer used
#define ALLOCATE_WITH_GC(name, heap, type, call) do { \
  type* _value = call;                                \
  if (_value == nullptr) {                            \
    (heap)->collectGarbage();                         \
    _value = call;                                    \
    if (_value == nullptr) {                          \
      throw Error("out of memory");                   \
    }                                                 \
  }                                                   \
  name = Local<type>(_value);                         \
} while (false)                                       \


// TODO: remove when no longer used
#define DEFINE_ALLOCATION(heap, type, call)      \
  Local<type> _result;                           \
  ALLOCATE_WITH_GC(_result, heap, type, call);   \
  return _result;                                \


/** Thrown when memory can't be allocated from the heap. Contains a flag that indicates whether
 *  allocation should be reattempted after garbage collection.
 */
class AllocationError: public std::exception {
 public:
  explicit AllocationError(bool shouldRetryAfterGC = true)
      : shouldRetryAfterGC_(shouldRetryAfterGC) { }

  bool shouldRetryAfterGC() const { return shouldRetryAfterGC_; }
  virtual const char* what() const noexcept override { return "allocation error"; }

 private:
  bool shouldRetryAfterGC_;
};


/** The main class that manages memory for the VM. There is exactly one heap in each VM.
 *  Memory is managed as a list of `Chunk`s, which are obtained from the kernel using `mmap` or
 *  some similar mechanism. Memory is allocated using a contiguous `AllocationRange` on one of
 *  the chunks by bumping the pointer at the beginning of the range. When the range is
 *  exhausted, the chunks' free lists are searched for a new range. If there is no free memory,
 *  the heap can be expanded by allocating a new chunk, or garbage can be collected.
 */
class Heap {
 public:
  explicit Heap(VM* vm);

  /** Blocks are always aligned to 8 bytes on both 32 and 64-bit architectures. This matches
   *  the largest primitive data types, so we can lay out objects with predictable alignment.
   *  This also lets us make other assumptions. Since pointers are aligned, we can use the low
   *  bits for other things. For marking, we know blocks will never be closer than 8 bytes.
   */
  static const word_t kBlockAlignment = 8;

  /** We will never allocate blocks below this address. Values below here can signal failures
   *  or tagged values.
   */
  static const word_t kMinAddress = PAGESIZE;

  bool isAllocationAllowed() const { return isAllocationAllowed_; }
  void setIsAllocationAllowed(bool allowed) { isAllocationAllowed_ = allowed; }

  /** Reserves raw memory on the heap without initializing it. Note that all words in a block
   *  should be initialized before storing a pointer to the block in any other reachable block.
   *  Throws `AllocationError` on failure.
   */
  Address allocateUninitialized(word_t size);

  /** Reserves memory on the heap and zero-initializes it. See safety comments for
   *  `allocateUninitialized`. Throws `AllocationError` on failure.
   */
  Address allocate(word_t size);

  /** Notify the garbage collector when a pointer is written into a block. This must be called
   *  for any pointer-write with one exception: if the block being written is freshly allocated,
   *  i.e., nothing else has been allocated later, and no pointer to that block has been stored,
   *  the write barrier is not necessary.
   */
  template <class T>
  static void recordWrite(T** from, T* to);
  static void recordWrite(Address from, Address to);

  /** Reclaim memory used by blocks which are not reachable anymore. */
  void collectGarbage();

  VM* vm() { return vm_; }

  class iterator: public std::iterator<std::input_iterator_tag, Chunk*> {
   public:
    Chunk* operator * ();
    bool operator == (const iterator& other) const;
    bool operator != (const iterator& other) const {
      return !(*this == other);
    }
    iterator& operator ++ ();
   private:
    iterator(const std::vector<std::unique_ptr<Chunk>>::iterator& it,
             const std::vector<std::unique_ptr<Chunk>>::iterator& end)
        : it_(it), end_(end) { }

        std::vector<std::unique_ptr<Chunk>>::iterator it_, end_;

    friend class Heap;
  };
  iterator begin();
  iterator end();

#ifdef DEBUG
  bool contains(Block* block);
  void verify();
#endif

 private:
  class Allocator {
   public:
    Allocator()
        : range_(nullptr), chunk_(nullptr) { }
    explicit Allocator(Chunk* chunk);
    bool canAllocate(word_t size) const;
    Address allocate(word_t size);
    void release();

   private:
    AllocationRange* range_;
    Chunk* chunk_;
  };

  Address allocateFast(word_t size);
  Address allocateSlow(word_t size);

  bool shouldExpand() const;
  void expand();

  template <class Callback>
  void visitRoots(Callback callback);

  VM* vm_;
  std::vector<std::unique_ptr<Chunk>> chunks_;
  Allocator allocator_;
  bool shouldExpand_;
  bool isAllocationAllowed_;

  friend class GC;
  friend class VM;

  NON_COPYABLE(Heap)
};


template <class T>
void Heap::recordWrite(T** from, T* to) {
  recordWrite(reinterpret_cast<Address>(from), reinterpret_cast<Address>(to));
}


class AllowAllocationScope {
 public:
  AllowAllocationScope(Heap* heap, bool allow)
      : heap_(heap),
        wasAllocationAllowed_(heap->isAllocationAllowed()) {
    heap_->setIsAllocationAllowed(allow);
  }

  ~AllowAllocationScope() {
    heap_->setIsAllocationAllowed(wasAllocationAllowed_);
  }

 private:
  Heap* heap_;
  bool wasAllocationAllowed_;
};

}
}

#endif

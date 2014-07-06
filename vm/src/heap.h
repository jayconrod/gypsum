// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef heap_h
#define heap_h

#include <iterator>
#include "builtins.h"
#include "error.h"
#include "list.h"
#include "memory.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Function;
class VM;

#define ALLOCATE_WITH_GC(name, heap, type, call) do { \
  type* _value = call;                                \
  if (_value == nullptr) {                            \
    (heap)->collectGarbage();                         \
    _value = call;                                    \
    if (_value == nullptr) {                          \
      throw Error("out of memory");                   \
    }                                                 \
  }                                                   \
  name = Handle<type>(_value);                        \
} while (false)                                       \


#define DEFINE_ALLOCATION(heap, type, call)      \
  Handle<type> _result;                          \
  ALLOCATE_WITH_GC(_result, heap, type, call);   \
  return _result;                                \


class Heap {
 public:
  Heap(VM* vm);

  static const int kGCBitCount = 2;
  static const int kGCBitMask = (1 << kGCBitCount) - 1;

  inline Address allocateRaw(word_t size);

  template <class T>
  static inline void recordWrite(T** from, T* to);
  static inline void recordWrite(Address from, Address to);

  void collectGarbage();

  VM* vm() { return vm_; }
  NewSpace* newSpace() { return &newSpace_; }
  Space* packageSpace() { return &packageSpace_; }

  Space* getSpaceById(SpaceId);

  class iterator: public std::iterator<std::input_iterator_tag, Space*> {
   public:
    inline iterator(Heap* heap, SpaceId id);
    inline Space* operator * ();
    inline bool operator == (const iterator& other) const;
    inline bool operator != (const iterator& other) const {
      return !(*this == other);
    }
    inline iterator& operator ++ ();
   private:
    Heap* heap_;
    SpaceId id_;
  };
  inline iterator begin();
  inline iterator end();

  class AllocationRangeScope {
   public:
    AllocationRangeScope(Heap* heap, Page* page)
        : heap_(heap) {
      heap_->allocationRange(&page_, &top_, &limit_);
      heap_->setAllocationRange(page);
    }
    ~AllocationRangeScope() {
      heap_->setAllocationRange(page_, top_, limit_);
    }
   private:
    Heap* heap_;
    Page* page_;
    Address top_;
    Address limit_;
  };

#ifdef DEBUG
  bool contains(Block* block);
  void verify();
#endif

  static const word_t kBlockAlignment = 8;
  static const word_t kMinAddress = PAGESIZE;

 private:
  inline Address allocateRawFast(word_t size);
  Address allocateRawSlow(word_t size);

  void allocationRange(Page** page, Address* top, Address* limit);
  void setAllocationRange(Page* page);
  void setAllocationRange(Page* page, Address top, Address limit);
  template <class Callback>
  void visitRoots(Callback callback);

  VM* vm_;
  NewSpace newSpace_;
  Space packageSpace_;
  Page* allocatingPage_;
  Address top_;
  Address limit_;

  friend class GC;
  friend class Package;
  friend class VM;

  NON_COPYABLE(Heap)
};

}
}

#endif

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
#include "option.h"
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

  OptP<Address> allocateRaw(word_t size);

  template <class T>
  static void recordWrite(T** from, T* to);
  static void recordWrite(Address from, Address to);

  void collectGarbage();

  VM* vm() { return vm_; }

  class iterator: public std::iterator<std::input_iterator_tag, Chunk*> {
   public:
    iterator(Heap* heap, std::vector<OptUP<Chunk>> it);
    Chunk* operator * ();
    bool operator == (const iterator& other) const;
    bool operator != (const iterator& other) const {
      return !(*this == other);
    }
    iterator& operator ++ ();
   private:
    Heap* heap_;
    std::vector<OptUP<Chunk>>::iterator it_;
  };
  iterator begin();
  iterator end();

#ifdef DEBUG
  bool contains(Block* block);
  void verify();
#endif

  static const word_t kBlockAlignment = 8;
  static const word_t kMinAddress = PAGESIZE;

 private:
  OptP<Address> allocateRawFast(word_t size);
  OptP<Address> allocateRawSlow(word_t size);

  bool shouldExpand() const;
  void expand();

  template <class Callback>
  void visitRoots(Callback callback);

  VM* vm_;
  std::vector<OptUP<Chunk>> chunks_;
  OptP<AllocationRange*> allocationRange_;
  OptP<Chunk*> allocationRangeChunk_;
  bool shouldExpand_;

  friend class GC;
  friend class VM;

  NON_COPYABLE(Heap)
};


template <class T>
void Heap::recordWrite(T** from, T* to) {
  recordWrite(reinterpret_cast<Address>(from), reinterpret_cast<Address>(to));
}

}
}

#endif

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef array_h
#define array_h

#include "block.h"
#include "heap-inl.h"
#include "tagged.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class Handle;

template <class T>
class Array: public Block {
 public:
  static word_t sizeForLength(word_t length) {
    return align(kHeaderSize + kElementSize * length, kWordSize);
  }

  DEFINE_INL_ACCESSORS(word_t, length, setLength, kLengthOffset)

  T* elements() {
    Address elementsBase = reinterpret_cast<Address>(this) + kHeaderSize;
    return reinterpret_cast<T*>(elementsBase);
  }

  T get(word_t index) {
    ASSERT(index < length());
    return elements()[index];
  }

  void set(word_t index, T value) {
    ASSERT(index < length());
    elements()[index] = value;
    recordWrite(&elements()[index], value);
  }

  void setAll(T value) {
    for (word_t i = 0, n = length(); i < n; i++)
      set(i, value);
  }

  void recordWrite(T* slot, T value) {
    // subclasses containing pointers must override this
  }

  static const int kLengthOffset = kBlockHeaderSize;
  static const int kHeaderSize = kLengthOffset + kWordSize;
  static const int kElementSize = sizeof(T);

  class iterator: public std::iterator<std::random_access_iterator_tag, T> {
   public:
    explicit iterator(T* elem)
        : elem_(elem) { }

    bool operator == (const iterator& other) { return elem_ == other.elem_; }
    bool operator != (const iterator& other) { return !operator==(other); }
    bool operator < (const iterator& other) { return elem_ < other.elem_; }
    bool operator <= (const iterator& other) { return elem_ <= other.elem_; }
    bool operator > (const iterator& other) { return elem_ > other.elem_; }
    bool operator >= (const iterator& other) { return elem_ >= other.elem_; }

    T operator * () { return *elem_; }
    T operator -> () { return *elem_; }
    T operator [] (int index) { return elem_[index]; }

    iterator& operator ++ () {
      ++elem_;
      return *this;
    }
    iterator operator ++ (int unused) {
      iterator it(*this);
      ++elem_;
      return it;
    }
    iterator& operator -- () {
      --elem_;
      return *this;
    }
    iterator operator -- (int unused) {
      iterator it(*this);
      --elem_;
      return it;
    }

    iterator operator + (int n) {
      return iterator(elem_ + n);
    }
    iterator& operator += (int n) {
      elem_ += n;
      return *this;
    }
    iterator operator - (int n) {
      return iterator(elem_ + n);
    }
    iterator& operator -= (int n) {
      elem_ -= n;
      return *this;
    }
    ptrdiff_t operator - (iterator other) {
      return elem_ - other.elem_;
    }

   private:
    T* elem_;
  };

  iterator begin() { return iterator(elements()); }
  iterator end() { return iterator(elements() + length()); }

 protected:
  static Array<T>* allocateBase(Heap* heap, word_t meta, word_t length) {
    word_t size = sizeForLength(length);
    Array<T>* array = reinterpret_cast<Array<T>*>(heap->allocateRaw(size));
    if (array == nullptr)
      return array;

    array->setMetaWord(meta);
    array->setLength(length);
    return array;
  }
};


template <class T>
class PointerArray: public Array<T> {
 public:
  void recordWrite(T* slot, T value) {
    Heap::recordWrite(slot, value);
  }
};


class I8Array: public Array<i8> {
 public:
  static I8Array* tryAllocate(Heap* heap, word_t length);
  static Handle<I8Array> allocate(Heap* heap, word_t length);
  DEFINE_CAST(I8Array)
  void printI8Array(FILE* out);
};


class I32Array: public Array<i32> {
 public:
  static I32Array* tryAllocate(Heap* heap, word_t length);
  static Handle<I32Array> allocate(Heap* heap, word_t length);
  DEFINE_CAST(I32Array)
  void printI32Array(FILE* out);
};


class I64Array: public Array<i64> {
 public:
  static I64Array* tryAllocate(Heap* heap, word_t length);
  static Handle<I64Array> allocate(Heap* heap, word_t length);
  DEFINE_CAST(I64Array)
  void printI64Array(FILE* out);
};


class BlockArray: public PointerArray<Block*> {
 public:
  static BlockArray* tryAllocate(Heap* heap, word_t length,
                                 bool fill = false, Block* fillValue = nullptr);
  static Handle<BlockArray> allocate(Heap* heap, word_t length,
                                     bool fill = false, Block* fillValue = nullptr);
  DEFINE_CAST(BlockArray)
  void printBlockArray(FILE* out);
};


class TaggedArray: public Array<Tagged<Block>> {
 public:
  static TaggedArray* tryAllocate(Heap* heap, word_t length);
  static Handle<TaggedArray> allocate(Heap* heap, word_t length);
  DEFINE_CAST(TaggedArray)
  void printTaggedArray(FILE* out);

  void recordWrite(Tagged<Block>* slot, Tagged<Block> value) {
    if (value.isPointer()) {
      Heap::recordWrite(reinterpret_cast<Block**>(slot), value.getPointer());
    }
  }
};

}
}

#endif

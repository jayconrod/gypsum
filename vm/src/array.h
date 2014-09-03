// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef array_h
#define array_h

#include "block.h"
#include "gc.h"
#include "heap.h"
#include "tagged.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class Handle;

template <class T>
class Array: public Block {
 public:
  void* operator new(size_t, Heap* heap, word_t length) {
    auto size = sizeForLength(length);
    auto array = reinterpret_cast<Array<T>*>(heap->allocate(size));
    array->length_ = length;
    return array;
  }

  void* operator new(size_t, Heap* heap, word_t length, T fillValue) {
    auto size = sizeForLength(length);
    auto array = reinterpret_cast<Array<T>*>(heap->allocate(size));
    array->length_ = length;
    for (word_t i = 0; i < length; i++)
      array->elements()[i] = fillValue;
    return array;
  }

  static word_t sizeForLength(word_t length) {
    return sizeof(Array) + sizeof(T) * length;
  }

  word_t length() const { return length_; }

  T* elements() {
    return &mem<T>(this, sizeof(Array));
  }
  const T* elements() const {
    return &mem<const T>(this, sizeof(Array));
  }

  T get(word_t index) const {
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
  explicit Array(BlockType blockType)
      : Block(blockType) { }

  word_t length_;
};


#define DEFINE_ARRAY_CREATE(Name, T)                                     \
static Local<Name> create(Heap* heap, word_t length) {                   \
  RETRY_WITH_GC(return Local<Name>(new(heap, length) Name));             \
}                                                                        \
static Local<Name> create(Heap* heap, word_t length, T fillValue) {      \
  RETRY_WITH_GC(return Local<Name>(new(heap, length, fillValue) Name));  \
}


template <class T>
class BlockArray: public Array<T*> {
 public:
  BlockArray() : Array<T*>(BLOCK_ARRAY_BLOCK_TYPE) {
    CHECK_SUBTYPE_VALUE(Block*, this->get(0));
  }

  DEFINE_ARRAY_CREATE(BlockArray, T)

  void printBlockArray(FILE* out) {
    fprintf(out, "BlockArray @%p\n", reinterpret_cast<void*>(this));
    fprintf(out, "  length: %d\n", static_cast<int>(this->length()));
    for (word_t i = 0; i < this->length(); i++) {
      fprintf(out, "  %3d: %p\n", static_cast<int>(i), reinterpret_cast<void*>(this->get(i)));
    }
  }

  void recordWrite(T** slot, T* value) {
    Heap::recordWrite(slot, value);
  }
};


template <class T, BlockType blockType>
class DataArray: public Array<T> {
 public:
  DataArray() : Array<T>(blockType) { }
};


class I8Array: public DataArray<i8, I8_ARRAY_BLOCK_TYPE> {
 public:
  DEFINE_ARRAY_CREATE(I8Array, i8)
  DEFINE_CAST(I8Array)
  void printI8Array(FILE* out);
};


class I32Array: public DataArray<i32, I32_ARRAY_BLOCK_TYPE> {
 public:
  DEFINE_ARRAY_CREATE(I32Array, i32)
  DEFINE_CAST(I32Array)
  void printI32Array(FILE* out);
};


class I64Array: public DataArray<i64, I64_ARRAY_BLOCK_TYPE> {
 public:
  DEFINE_ARRAY_CREATE(I64Array, i64)
  DEFINE_CAST(I64Array)
  void printI64Array(FILE* out);
};


template <class T>
class TaggedArray: public Array<Tagged<T>> {
 public:
  TaggedArray() : Array<Tagged<T>>(TAGGED_ARRAY_BLOCK_TYPE) { }

  DEFINE_ARRAY_CREATE(TaggedArray, Tagged<T>)
  DEFINE_CAST(TaggedArray)

  void printTaggedArray(FILE* out) {
    UNIMPLEMENTED();
  }

  void recordWrite(Tagged<T>* slot, Tagged<T> value) {
    if (value.isPointer()) {
      Heap::recordWrite(reinterpret_cast<Block**>(slot), value.getPointer());
    }
  }
};


#undef DEFINE_ARRAY_CREATE

}
}

#endif

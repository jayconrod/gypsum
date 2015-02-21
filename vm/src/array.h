// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef array_h
#define array_h

#include <iomanip>
#include <iostream>
#include "block.h"
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
  void* operator new(size_t, Heap* heap, length_t length) {
    ASSERT(length <= kMaxLength);
    auto size = sizeForLength(length);
    auto array = reinterpret_cast<Array<T>*>(heap->allocate(size));
    array->length_ = length;
    return array;
  }

  void* operator new(size_t, Heap* heap, length_t length, T fillValue) {
    ASSERT(length <= kMaxLength);
    auto size = sizeForLength(length);
    auto array = reinterpret_cast<Array<T>*>(heap->allocate(size));
    array->length_ = length;
    for (length_t i = 0; i < length; i++)
      array->elements()[i] = fillValue;
    return array;
  }

  static word_t sizeForLength(length_t length) {
    ASSERT(length <= kMaxLength);
    return sizeof(Array) + sizeof(T) * length;
  }

  length_t length() const { return length_; }
  bool isEmpty() const { return length_ == 0; }

  T* elements() {
    return &mem<T>(this, sizeof(Array));
  }
  const T* elements() const {
    return &mem<const T>(this, sizeof(Array));
  }

  T get(length_t index) const {
    ASSERT(index < length());
    return elements()[index];
  }

  void set(length_t index, T value) {
    ASSERT(index < length());
    elements()[index] = value;
    recordWrite(&elements()[index], value);
  }

  void setAll(T value) {
    for (length_t i = 0, n = length(); i < n; i++)
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

  length_t length_;

  friend class Roots;
};


#define DEFINE_ARRAY_CREATE(Name, T)                                           \
static Local<Name> create(Heap* heap, length_t length) {                       \
  RETRY_WITH_GC(heap, return Local<Name>(new(heap, length) Name));             \
}                                                                              \
static Local<Name> create(Heap* heap, length_t length, T fillValue) {          \
  RETRY_WITH_GC(heap, return Local<Name>(new(heap, length, fillValue) Name));  \
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
    for (length_t i = 0; i < this->length(); i++) {
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
  static const BlockType kBlockType = blockType;
  DataArray() : Array<T>(blockType) { }
};


class I8Array: public DataArray<i8, I8_ARRAY_BLOCK_TYPE> {
 public:
  DEFINE_ARRAY_CREATE(I8Array, i8)
  void printI8Array(FILE* out);
};


class I32Array: public DataArray<i32, I32_ARRAY_BLOCK_TYPE> {
 public:
  DEFINE_ARRAY_CREATE(I32Array, i32)
  void printI32Array(FILE* out);
};


typedef I32Array LengthArray;
typedef I32Array IdArray;


class I64Array: public DataArray<i64, I64_ARRAY_BLOCK_TYPE> {
 public:
  DEFINE_ARRAY_CREATE(I64Array, i64)
  void printI64Array(FILE* out);
};


template <class T>
class TaggedArray: public Array<Tagged<T>> {
 public:
  static const BlockType kBlockType = TAGGED_ARRAY_BLOCK_TYPE;

  TaggedArray() : Array<Tagged<T>>(TAGGED_ARRAY_BLOCK_TYPE) { }

  DEFINE_ARRAY_CREATE(TaggedArray, Tagged<T>)

  void printTaggedArray(FILE* out) {
    UNIMPLEMENTED();
  }

  void recordWrite(Tagged<T>* slot, Tagged<T> value) {
    if (value.isPointer()) {
      Heap::recordWrite(reinterpret_cast<Block**>(slot), value.getPointer());
    }
  }
};


template <class A>
void printBlockElements(std::ostream& os, const A* array) {
  for (length_t i = 0; i < array->length(); i++) {
    os << "\n  " << std::setw(5) << i << ": " << brief(array->get(i));
  }
}


template <class A>
void printRawElements(std::ostream& os, const A* array) {
  for (length_t i = 0; i < array->length(); i++) {
    os << "\n  " << std::setw(5) << i << ": " << array->get(i);
  }
}


template <class T>
std::ostream& operator << (std::ostream& os, const BlockArray<T>* array) {
  os << brief(array);
  printBlockElements(os, array);
  return os;
}


inline std::ostream& operator << (std::ostream& os, const I8Array* array) {
  os << brief(array);
  printRawElements(os, array);
  return os;
}


inline std::ostream& operator << (std::ostream& os, const I32Array* array) {
  os << brief(array);
  printRawElements(os, array);
  return os;
}


inline std::ostream& operator << (std::ostream& os, const I64Array* array) {
  os << brief(array);
  printRawElements(os, array);
  return os;
}


template <class T>
std::ostream& operator << (std::ostream& os, const TaggedArray<T>* array) {
  os << brief(array);
  printRawElements(os, array);
  return os;
}


#undef DEFINE_ARRAY_CREATE

}
}

#endif

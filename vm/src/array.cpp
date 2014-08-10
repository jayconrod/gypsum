// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "array.h"

#include "block-inl.h"
#include "handle.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

I8Array* I8Array::tryAllocate(Heap* heap, word_t length) {
  auto mw = static_cast<word_t>(I8_ARRAY_BLOCK_TYPE) << kGCBitCount;
  return reinterpret_cast<I8Array*>(
      allocateBase(heap, mw, length));
}


Local<I8Array> I8Array::allocate(Heap* heap, word_t length) {
  DEFINE_ALLOCATION(heap, I8Array, tryAllocate(heap, length))
}


I32Array* I32Array::tryAllocate(Heap* heap, word_t length) {
  auto mw = static_cast<word_t>(I32_ARRAY_BLOCK_TYPE) << kGCBitCount;
  return reinterpret_cast<I32Array*>(
      allocateBase(heap, mw, length));
}


Local<I32Array> I32Array::allocate(Heap* heap, word_t length) {
  DEFINE_ALLOCATION(heap, I32Array, tryAllocate(heap, length))
}


void I32Array::printI32Array(FILE* out) {
  fprintf(out, "I32Array @%p\n", reinterpret_cast<void*>(this));
  fprintf(out, "  length: %d\n", static_cast<int>(length()));
  for (word_t i = 0; i < length(); i++) {
    fprintf(out, "  %3d: %d\n", static_cast<int>(i), static_cast<int>(get(i)));
  }
}


I64Array* I64Array::tryAllocate(Heap* heap, word_t length) {
  auto mw = static_cast<word_t>(I64_ARRAY_BLOCK_TYPE) << kGCBitCount;
  return reinterpret_cast<I64Array*>(
      allocateBase(heap, mw, length));
}


Local<I64Array> I64Array::allocate(Heap* heap, word_t length) {
  DEFINE_ALLOCATION(heap, I64Array, tryAllocate(heap, length))
}


void I64Array::printI64Array(FILE* out) {
  fprintf(out, "I64Array @%p\n", reinterpret_cast<void*>(this));
  fprintf(out, "  length: %d\n", static_cast<int>(length()));
  for (word_t i = 0; i < length(); i++) {
    fprintf(out, "  %3d: %ld\n", static_cast<int>(i), static_cast<long>(get(i)));
  }
}


BlockArray* BlockArray::tryAllocate(Heap* heap, word_t length, bool fill, Block* fillValue) {
  auto mw = static_cast<word_t>(BLOCK_ARRAY_BLOCK_TYPE) << kGCBitCount;
  auto array = reinterpret_cast<BlockArray*>(allocateBase(heap, mw, length));
  if (array != nullptr && fill) {
    for (word_t i = 0; i < length; i++)
      array->set(i, fillValue);
  }
  return array;
}


Local<BlockArray> BlockArray::allocate(Heap* heap, word_t length,
                                        bool fill, Block* fillValue) {
  DEFINE_ALLOCATION(heap, BlockArray, tryAllocate(heap, length, fill, fillValue))
}


void BlockArray::printBlockArray(FILE* out) {
  fprintf(out, "BlockArray @%p\n", reinterpret_cast<void*>(this));
  fprintf(out, "  length: %d\n", static_cast<int>(length()));
  for (word_t i = 0; i < length(); i++) {
    fprintf(out, "  %3d: %p\n", static_cast<int>(i), reinterpret_cast<void*>(get(i)));
  }
}


TaggedArray* TaggedArray::tryAllocate(Heap* heap, word_t length) {
  auto mw = static_cast<word_t>(TAGGED_ARRAY_BLOCK_TYPE) << kGCBitCount;
  return reinterpret_cast<TaggedArray*>(allocateBase(heap, mw, length));
}


Local<TaggedArray> TaggedArray::allocate(Heap* heap, word_t length) {
  DEFINE_ALLOCATION(heap, TaggedArray, tryAllocate(heap, length))
}

}
}

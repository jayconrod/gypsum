// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "block-inl.h"

#include "array.h"
#include "function-inl.h"
#include "handle-inl.h"
#include "heap-inl.h"
#include "stack-inl.h"

namespace codeswitch {
namespace internal {

word_t Block::sizeOfBlock() {
  if (!meta()->hasCustomSize()) {
    word_t length = meta()->hasElements() ? elementsLength() : 0;
    word_t size = meta()->objectSize() + length * meta()->elementSize();
    return size;
  } else {
    word_t size;
    switch (meta()->type()) {
      case META_BLOCK_TYPE:
        size = Meta::cast(this)->sizeOfMeta();
        break;
      case FUNCTION_BLOCK_TYPE:
        size = Function::cast(this)->sizeOfFunction();
        break;
      default:
        UNREACHABLE();
        size = kNotFound;
    }
    return size;
  }
  return kNotFound;
}


void Block::print(FILE* out) {
  switch (meta()->type()) {
    case META_BLOCK_TYPE: Meta::cast(this)->printMeta(out); break;
    case STACK_BLOCK_TYPE: Stack::cast(this)->printStack(out); break;
    case FUNCTION_BLOCK_TYPE: Function::cast(this)->printFunction(out); break;
    case I32_ARRAY_BLOCK_TYPE: I32Array::cast(this)->printI32Array(out); break;
    case I64_ARRAY_BLOCK_TYPE: I64Array::cast(this)->printI64Array(out); break;
    default:
      fprintf(out, "invalid object\n");
  }
}


void Block::relocate(word_t delta) {
  switch (meta()->type()) {
    case STACK_BLOCK_TYPE: Stack::cast(this)->relocateStack(delta); break;
    default:
      UNREACHABLE();
  }
}


Meta* Meta::tryAllocate(Heap* heap, word_t dataLength, u32 objectSize, u32 elementSize) {
  word_t size = sizeForMeta(dataLength, objectSize, elementSize);
  Meta* meta = reinterpret_cast<Meta*>(heap->allocateRaw(size));
  if (meta == nullptr)
    return meta;

  meta->setMeta(META_BLOCK_TYPE);
  meta->setDataLength(dataLength);
  return meta;
}


Handle<Meta> Meta::allocate(Heap* heap, word_t dataLength, u32 objectSize, u32 elementSize) {
  DEFINE_ALLOCATION(heap, Meta, tryAllocate(heap, dataLength, objectSize, elementSize));
}


void Meta::initialize(BlockType type, Class* clas, u32 objectSize, u32 elementSize) {
  word_t flags = bitInsert(0, type, kTypeWidth, kTypeShift);
  setFlags(flags);
  setClass(clas);
  setObjectSize(objectSize);
  setElementSize(elementSize);
  objectPointerMap().clear();
  elementPointerMap().clear();
}


word_t Meta::sizeForMeta(word_t dataLength, u32 objectSize, u32 elementSize) {
  word_t dataSize = dataLength * kWordSize;
  word_t objectWords = align(objectSize, kWordSize) / kWordSize;
  word_t elementWords = align(elementSize, kWordSize) / kWordSize;
  return kHeaderSize + dataSize + Bitmap::sizeFor(objectWords) + Bitmap::sizeFor(elementWords);
}


word_t Meta::sizeOfMeta() {
  return sizeForMeta(dataLength(), objectSize(), elementSize());
}


void Meta::printMeta(FILE* out) {
  const char* typeStr;
  switch (type()) {
#define TYPE_STR(Name, NAME) case NAME##_BLOCK_TYPE: typeStr = #Name; break;
BLOCK_TYPE_LIST(TYPE_STR)
#undef TYPE_STR
    default: typeStr = "Unknown";
  }
  fprintf(out, "Meta @%p\n", reinterpret_cast<void*>(this));
  fprintf(out, "  length: %d\n", static_cast<int>(dataLength()));
  fprintf(out, "  type: %s\n", typeStr);
  fprintf(out, "  custom size: %s\n", hasCustomSize() ? "yes" : "no");
  fprintf(out, "  custom pointers: %s\n", hasCustomPointers() ? "yes" : "no");
  fprintf(out, "  object size: %d\n", static_cast<int>(objectSize()));
  fprintf(out, "  element size: %d\n", static_cast<int>(elementSize()));
}

}
}

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "block.h"

#include "array.h"
#include "function.h"
#include "gc.h"
#include "handle.h"
#include "heap.h"
#include "roots.h"
#include "stack.h"

namespace codeswitch {
namespace internal {

word_t Block::sizeOfBlock() const {
  word_t size = kNotFound;
  if (!meta()->hasCustomSize()) {
    if (meta()->hasElements()) {
      size = elementsOffset() + elementsSize();
    } else {
      size = meta()->objectSize();
    }
  } else {
    switch (meta()->blockType()) {
      case META_BLOCK_TYPE:
        size = Meta::cast(this)->sizeOfMeta();
        break;
      default:
        UNREACHABLE();
    }
  }
  ASSERT(size != kNotFound && size > 0);
  return size;
}


void Block::print(FILE* out) {
  switch (meta()->blockType()) {
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
  switch (meta()->blockType()) {
    case STACK_BLOCK_TYPE: Stack::cast(this)->relocateStack(delta); break;
    default:
      UNREACHABLE();
  }
}


Meta* Block::meta() const {
  return metaWord().isPointer()
       ? metaWord().getPointer()
       : getVM()->roots()->getMetaForBlockType(metaWord().getBlockType());
}


void Block::setMeta(Meta* meta) {
  metaWord_.setPointer(meta);
  Heap::recordWrite(reinterpret_cast<Address>(&metaWord_), reinterpret_cast<Address>(meta));
}


BlockType Block::blockType() const {
  return metaWord().isBlockType()
       ? metaWord().getBlockType()
       : metaWord().getPointer()->blockType();
}


word_t Block::elementsLength() const {
  ASSERT(meta()->elementSize() > 0 && meta()->lengthOffset() > 0);
  word_t len;
  if (meta()->hasWordSizeLength()) {
    len = mem<word_t>(this, meta()->lengthOffset());
    ASSERT(len != kNotSet);
  } else {
    len = static_cast<word_t>(mem<length_t>(this, meta()->lengthOffset()));
    ASSERT(len != kLengthNotSet);
  }
  return len;
}


void Block::setElementsLength(word_t length) {
  ASSERT(meta()->elementSize() > 0 && meta()->lengthOffset() > 0);
  ASSERT(length != kNotSet);
  if (meta()->hasWordSizeLength()) {
    mem<word_t>(this, meta()->lengthOffset()) = length;
  } else {
    ASSERT(length <= kMaxLength);
    auto len = static_cast<length_t>(length);
    ASSERT(len == length);
    mem<word_t>(this, meta()->lengthOffset()) = len;
  }
}


ptrdiff_t Block::elementsOffset() const {
  ASSERT(meta()->hasElements());
  return align(meta()->objectSize(), kWordSize);
}


Address Block::elementsBase() const {
  ASSERT(meta()->hasElements());
  return address() + elementsOffset();
}


word_t Block::elementsSize() const {
  ASSERT(meta()->hasElements());
  return static_cast<word_t>(elementsLength()) * meta()->elementSize();
}


VM* Block::getVM() const {
  Chunk* page = Chunk::fromAddress(this);
  return page->vm();
}


Heap* Block::getHeap() const {
  return getVM()->heap();
}


#define META_POINTER_LIST(F) \
  F(Meta, clas_)             \

DEFINE_POINTER_MAP(Meta, META_POINTER_LIST)
#undef META_POINTER_LIST


void* Meta::operator new (size_t, Heap* heap,
                          length_t dataLength,
                          u32 objectSize,
                          u32 elementSize) {
  ASSERT(dataLength <= kMaxLength);
  auto size = sizeForMeta(dataLength, objectSize, elementSize);
  // Heap::allocate zero-initializes the block for us. This is needed since the constructor
  // doesn't initialize most of the body.
  auto meta = reinterpret_cast<Meta*>(heap->allocate(size));
  meta->dataLength_ = dataLength;
  meta->objectSize_ = objectSize;
  meta->elementSize_ = elementSize;
  return meta;
}


Local<Meta> Meta::create(Heap* heap,
                         length_t dataLength,
                         u32 objectSize,
                         u32 elementSize,
                         BlockType blockType) {
  ASSERT(dataLength <= kMaxLength);
  RETRY_WITH_GC(heap, return Local<Meta>(
      new(heap, dataLength, objectSize, elementSize) Meta(blockType)));
}


word_t Meta::sizeForMeta(length_t dataLength, u32 objectSize, u32 elementSize) {
  ASSERT(dataLength <= kMaxLength);
  auto headerSize = align(sizeof(Meta), kWordSize);
  auto dataSize = dataLength * kWordSize;
  auto objectWords = align(objectSize, kWordSize) / kWordSize;
  auto elementWords = align(elementSize, kWordSize) / kWordSize;
  return headerSize + dataSize + Bitmap::sizeFor(objectWords) + Bitmap::sizeFor(elementWords);
}


word_t Meta::sizeOfMeta() const {
  return sizeForMeta(dataLength(), objectSize(), elementSize());
}


void Meta::printMeta(FILE* out) {
  const char* typeStr;
  switch (meta()->blockType()) {
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


Block* Meta::getData(length_t index) const {
  ASSERT(index < dataLength());
  return dataBase()[index];
}


void Meta::setData(length_t index, Block* value) {
  ASSERT(index < dataLength());
  Block** p = dataBase() + index;
  *p = value;
  Heap::recordWrite(p, value);
}


word_t* Meta::rawObjectPointerMap() {
  Address elementsEnd = elementsBase() + elementsSize();
  return reinterpret_cast<word_t*>(elementsEnd);
}


Bitmap Meta::objectPointerMap() {
  auto objectWordCount = align(objectSize(), kWordSize) / kWordSize;
  return Bitmap(rawObjectPointerMap(), objectWordCount);
}


word_t* Meta::rawElementPointerMap() {
  Address elementsEnd = elementsBase() + elementsSize();
  auto objectWordCount = align(objectSize(), kWordSize) / kWordSize;
  auto objectPointerMapSize = Bitmap::sizeFor(objectWordCount);
  return reinterpret_cast<word_t*>(elementsEnd + objectPointerMapSize);
}


Bitmap Meta::elementPointerMap() {
  auto elementWordCount = align(elementSize(), kWordSize) / kWordSize;
  return Bitmap(rawElementPointerMap(), elementWordCount);
}


void* Free::operator new (size_t unused, Heap* heap, word_t size) {
  auto free = reinterpret_cast<Free*>(heap->allocate(size));
  if (free == nullptr)
    return free;

  free->setMeta(FREE_BLOCK_TYPE);
  free->size_ = size - sizeof(Free);
  return free;
}


void* Free::operator new (size_t unused, void* place, word_t size) {
  auto free = reinterpret_cast<Free*>(place);
  free->setMeta(FREE_BLOCK_TYPE);
  free->size_ = size - sizeof(Free);
  return free;
}

}
}

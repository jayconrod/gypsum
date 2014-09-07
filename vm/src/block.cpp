// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "block.h"

#include "array.h"
#include "function.h"
#include "gc.h"
#include "handle.h"
#include "heap.h"
#include "roots-inl.h"
#include "stack.h"

namespace codeswitch {
namespace internal {

word_t Block::sizeOfBlock() const {
  word_t size = kNotFound;
  if (!meta()->hasCustomSize()) {
    word_t length = meta()->hasElements() ? elementsLength() : 0;
    size = meta()->objectSize() + length * meta()->elementSize();
  } else {
    switch (meta()->blockType()) {
      case META_BLOCK_TYPE:
        size = Meta::cast(this)->sizeOfMeta();
        break;
      case FREE_BLOCK_TYPE:
        size = Free::cast(this)->size();
        break;
      case FUNCTION_BLOCK_TYPE:
        size = Function::cast(this)->sizeOfFunction();
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
  ASSERT(meta()->hasElements());
  return mem<word_t>(this, sizeof(Block));
}


VM* Block::getVM() const {
  Chunk* page = Chunk::fromAddress(this);
  return page->vm();
}


Heap* Block::getHeap() const {
  return getVM()->heap();
}


void* Meta::operator new (size_t, Heap* heap,
                          word_t dataLength,
                          u32 objectSize,
                          u32 elementSize) {
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
                         word_t dataLength,
                         u32 objectSize,
                         u32 elementSize,
                         BlockType blockType) {
  RETRY_WITH_GC(heap, return Local<Meta>(
      new(heap, dataLength, objectSize, elementSize) Meta(blockType)));
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


Block* Meta::getData(word_t index) const {
  ASSERT(index < dataLength());
  return dataBase()[index];
}


void Meta::setData(word_t index, Block* value) {
  ASSERT(index < dataLength());
  Block** p = dataBase() + index;
  *p = value;
  Heap::recordWrite(p, value);
}


word_t* Meta::rawObjectPointerMap() {
  auto dataSize = dataLength() * kWordSize;
  return &mem<word_t>(this, sizeof(Meta) + dataSize);
}


Bitmap Meta::objectPointerMap() {
  auto objectWordCount = align(objectSize(), kWordSize) / kWordSize;
  return Bitmap(rawObjectPointerMap(), objectWordCount);
}


word_t* Meta::rawElementPointerMap() {
  auto dataSize = dataLength() * kWordSize;
  auto objectWordCount = align(objectSize(), kWordSize) / kWordSize;
  auto objectPointerMapSize = Bitmap::sizeFor(objectWordCount);
  return &mem<word_t>(this, sizeof(Meta) + dataSize + objectPointerMapSize);
}


Bitmap Meta::elementPointerMap() {
  auto elementWordCount = align(elementSize(), kWordSize) / kWordSize;
  return Bitmap(rawElementPointerMap(), elementWordCount);
}


word_t Meta::sizeForMeta(word_t dataLength, u32 objectSize, u32 elementSize) {
  auto headerSize = sizeof(Meta);
  auto dataSize = dataLength * kWordSize;
  auto objectWords = align(objectSize, kWordSize) / kWordSize;
  auto elementWords = align(elementSize, kWordSize) / kWordSize;
  return headerSize + dataSize + Bitmap::sizeFor(objectWords) + Bitmap::sizeFor(elementWords);
}


void* Free::operator new (size_t unused, Heap* heap, size_t size) {
  auto free = reinterpret_cast<Free*>(heap->allocate(size));
  if (free == nullptr)
    return free;

  free->setMeta(FREE_BLOCK_TYPE);
  free->size_ = size;
  return free;
}


void* Free::operator new (size_t unused, void* place, size_t size) {
  auto free = reinterpret_cast<Free*>(place);
  free->setMeta(FREE_BLOCK_TYPE);
  free->size_ = size;
  return free;
}

}
}

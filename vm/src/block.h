// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef block_h
#define block_h

#include <cstdio>
#include "bitmap-inl.h"
#include "heap.h"
#include "utils.h"
#include "vm.h"

namespace codeswitch {
namespace internal {

#define BLOCK_TYPE_LIST(V)             \
V(Meta, META)                          \
V(Free, FREE)                          \
V(Package, PACKAGE)                    \
V(Stack, STACK)                        \
V(Function, FUNCTION)                  \
V(Class, CLASS)                        \
V(Field, FIELD)                        \
V(TypeParameter, TYPE_PARAMETER)       \
V(I8Array, I8_ARRAY)                   \
V(I32Array, I32_ARRAY)                 \
V(I64Array, I64_ARRAY)                 \
V(BlockArray, BLOCK_ARRAY)             \
V(TaggedArray, TAGGED_ARRAY)           \
V(Object, OBJECT)                      \
V(Type, TYPE)                          \
V(String, STRING)                      \


#define ENUM_BLOCK_TYPE(Name, NAME) NAME##_BLOCK_TYPE,
enum BlockType {
  BLOCK_TYPE_LIST(ENUM_BLOCK_TYPE)
  LAST_BLOCK_TYPE
};
#undef ENUM_BLOCK_TYPE


#define DECLARE_BLOCK_CLASSES(Name, NAME) class Name;
BLOCK_TYPE_LIST(DECLARE_BLOCK_CLASSES)
#undef DECLARE_BLOCK_CLASSES


#define DEFINE_CAST(Name) \
static Name* cast(Block* block) { \
  ASSERT(block->is##Name()); \
  return reinterpret_cast<Name*>(block); \
}


class Bitmap;
template <class T>
class Handle;
class Heap;


class Block {
 public:
  Block() { }
  Block(const Block&) = delete;
  Block(const Block&&) = delete;

#define DECLARE_TYPE_CHECK(Name, NAME) \
  inline bool is##Name();
BLOCK_TYPE_LIST(DECLARE_TYPE_CHECK)
#undef DECLARE_TYPE_CHECK

  Address address() { return reinterpret_cast<Address>(this); }
  word_t sizeOfBlock();
  void print(FILE* out = stderr);
  void relocate(word_t delta);

  DECLARE_ACCESSORS(Meta*, meta, setMeta)
  inline void setMeta(BlockType type);
  inline bool hasEncodedMeta();
  inline BlockType getMetaEncoding();
  DEFINE_INL_ACCESSORS(word_t, metaWord, setMetaWord, kMetaOffset)
  DECLARE_ACCESSORS(u8, gcBits, setGcBits)

  DEFINE_INL_ACCESSORS(word_t, elementsLength, setElementsLength, kBlockHeaderSize)

  inline VM* getVM();
  inline Heap* getHeap();

  static const int kMetaOffset = 0;
  static const word_t kGCBitCount = 2;
  static const word_t kGCBitMask = (1 << kGCBitCount) - 1;
  static const int kBlockHeaderSize = kMetaOffset + kWordSize;
};


class Meta: public Block {
 public:
  static Meta* tryAllocate(Heap* heap, word_t dataLength, u32 objectSize, u32 elementSize);
  static Handle<Meta> allocate(Heap* heap, word_t dataLength, u32 objectSize, u32 elementSize);
  void initialize(BlockType type, Class* clas, u32 objectSize, u32 elementSize);
  static word_t sizeForMeta(word_t dataLength, u32 objectSize, u32 elementSize);
  word_t sizeOfMeta();
  DEFINE_CAST(Meta)

  void printMeta(FILE* out);

  DEFINE_INL_ACCESSORS(word_t, dataLength, setDataLength, kDataLengthOffset)
  DEFINE_INL_ACCESSORS(word_t, flags, setFlags, kFlagsOffset)
  DEFINE_INL_BIT_ACCESSORS(BlockType, type, setType, kFlagsOffset, kTypeWidth, kTypeShift)
  DEFINE_INL_BOOL_ACCESSORS(hasCustomSize, setHasCustomSize,
                            kFlagsOffset, kHasCustomSizeShift)
  DEFINE_INL_BOOL_ACCESSORS(hasPointers, setHasPointers,
                            kFlagsOffset, kHasPointersShift)
  DEFINE_INL_BOOL_ACCESSORS(hasElementPointers, setHasElementPointers,
                            kFlagsOffset, kHasElementPointersShift)
  DEFINE_INL_BOOL_ACCESSORS(hasCustomPointers, setHasCustomPointers,
                            kFlagsOffset, kHasCustomPointersShift)
  DEFINE_INL_BOOL_ACCESSORS(needsRelocation, setNeedsRelocation,
                            kFlagsOffset, kNeedsRelocationShift)
  DEFINE_INL_PTR_ACCESSORS(Class*, clas, setClass, kClassOffset)
  DEFINE_INL_ACCESSORS(u32, objectSize, setObjectSize, kObjectSizeOffset)
  DEFINE_INL_ACCESSORS(u32, elementSize, setElementSize, kElementSizeOffset)

  inline Block** dataBase() {
    return &mem<Block*>(this, kHeaderSize);
  }
  inline Block* getData(word_t index) { return mem<Block*>(this, kHeaderSize, index); }
  inline void setData(word_t index, Block* value) {
    mem<Block*>(this, kHeaderSize, index) = value;
  }

  bool hasElements() { return elementSize() > 0; }
  inline word_t* rawObjectPointerMap() {
    word_t dataSize = dataLength() * kWordSize;
    return &mem<word_t>(this, kHeaderSize + dataSize);
  }
  inline Bitmap objectPointerMap() {
    word_t objectWordCount = align(objectSize(), kWordSize) / kWordSize;
    return Bitmap(rawObjectPointerMap(), objectWordCount);
  }
  inline word_t* rawElementPointerMap() {
    word_t dataSize = dataLength() * kWordSize;
    word_t objectWordCount = align(objectSize(), kWordSize) / kWordSize;
    word_t objectPointerMapSize = Bitmap::sizeFor(objectWordCount);
    return &mem<word_t>(this, kHeaderSize + dataSize + objectPointerMapSize);
  }
  inline Bitmap elementPointerMap() {
    word_t elementWordCount = align(elementSize(), kWordSize) / kWordSize;
    return Bitmap(rawElementPointerMap(), elementWordCount);
  }

  static const int kDataLengthOffset = kBlockHeaderSize;
  static const int kFlagsOffset = kDataLengthOffset + kWordSize;
  static const int kTypeWidth = 8;
  static const int kTypeShift = 0;
  static const int kHasCustomSizeShift = kTypeShift + kTypeWidth;
  static const int kHasPointersShift = kHasCustomSizeShift + 1;
  static const int kHasElementPointersShift = kHasPointersShift + 1;
  static const int kHasCustomPointersShift = kHasElementPointersShift + 1;
  static const int kNeedsRelocationShift = kHasCustomPointersShift + 1;
  static const int kClassOffset = kFlagsOffset + kWordSize;
  static const int kObjectSizeOffset = kClassOffset + kWordSize;
  static const int kElementSizeOffset = kObjectSizeOffset + sizeof(u32);
  static const int kHeaderSize = kElementSizeOffset + sizeof(u32);
  static const int kElementSize = kWordSize;

  static const word_t kPointerMap = 0;
  static const word_t kElementPointerMap = 1;
};


/** A Free block is a range of free memory. It contains a size and a pointer to another Free.
 *  It can be used by the heap as a node in a free list.
 */
class Free: public Block {
 public:
  void* operator new (size_t unused, Heap* heap, size_t size);
  void* operator new (size_t unused, void* place, size_t size);
  Free(OptP<Free*> next)
      : next_(next) { }

  DEFINE_CAST(Free)

  word_t size() const { return size_; }
  OptP<Free*> next() const { return next_; }

 private:
  word_t size_;
  OptP<Free*> next_;
};


#define DEFINE_TYPE_CHECK(Name, NAME) \
bool Block::is##Name() { \
  auto type = hasEncodedMeta() ? getMetaEncoding() : meta()->type(); \
  return type == NAME##_BLOCK_TYPE; \
}
BLOCK_TYPE_LIST(DEFINE_TYPE_CHECK)
#undef DEFINE_TYPE_CHECK

}
}

#endif

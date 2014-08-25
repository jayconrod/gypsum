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
} \
static const Name* cast(const Block* block) { \
  ASSERT(block->is##Name()); \
  return reinterpret_cast<const Name*>(block); \
}


class Bitmap;
template <class T>
class Handle;
class Heap;
class Meta;


/** Encodes the first word of each `Block`. The first word is called the "meta word".
 *  It contains either a pointer to a `Meta`, which describes the structure of the `Block`, or
 *  a `BlockType`, which can be used to find a standard `Meta` in `Roots`. Since all `Block`s,
 *  including `Meta`s are at least word-aligned on the heap, the two low bits of the `Meta`
 *  pointer are guaranteed to be 0. `BlockType` does not need all bits either. So we reserve
 *  these bits for use by the GC. These bits are masked out when we access the Meta.
 */
class MetaWord {
 public:
  MetaWord(Meta* meta, word_t gcBits = 0)
      : bits_(reinterpret_cast<word_t>(meta) | gcBits) { }
  MetaWord(BlockType type, word_t gcBits = 0)
      : bits_((static_cast<word_t>(type) << kGcBitCount) | gcBits) { }

  bool isPointer() const {
    return (bits_ >> kGcBitCount) >= Heap::kMinAddress;
  }
  Meta* getPointer() const {
    ASSERT(isPointer());
    return reinterpret_cast<Meta*>(bits_ & ~kGcBitMask);
  }
  void setPointer(Meta* meta) {
    bits_ = reinterpret_cast<word_t>(meta) | gcBits();
  }
  bool isBlockType() const {
    return !isPointer();
  }
  BlockType getBlockType() const {
    ASSERT(isBlockType());
    return static_cast<BlockType>(bits_ >> kGcBitCount);
  }
  void setBlockType(BlockType type) {
    bits_ = static_cast<word_t>(type) | gcBits();
  }

  word_t gcBits() const { return bits_ & kGcBitMask; }
  void setGcBits(word_t gcBits) {
    bits_ = (bits_ & ~kGcBitMask) | gcBits;
  }

  static const word_t kGcBitCount = 2;
  static const word_t kGcBitMask = (1 << kGcBitCount) - 1;

 private:
  // We don't use bit fields since their order is implementation-defined. We will likely need
  // to access the meta word in jitcode, so they need to be in a well-defined order.
  word_t bits_;
};


/** Block is the base class for every kind of block allocated on the garbage-collected heap.
 *  It just contains one "meta word": every block begins with one of these. See `MetaWord`
 *  for details on this.
 */
class Block {
 public:
  explicit Block(MetaWord metaWord)
      : metaWord_(metaWord) { }

#define DECLARE_TYPE_CHECK(Name, NAME) \
  inline bool is##Name() const;
BLOCK_TYPE_LIST(DECLARE_TYPE_CHECK)
#undef DECLARE_TYPE_CHECK

  Address address() const { return reinterpret_cast<Address>(this); }
  word_t sizeOfBlock() const;
  void print(FILE* out = stderr);
  void relocate(word_t delta);

  const MetaWord& metaWord() const { return metaWord_; }
  Meta* meta() const;
  void setMeta(Meta* meta);
  BlockType blockType() const;

  word_t elementsLength() const;

  VM* getVM() const;
  Heap* getHeap() const;

  // TODO: remove these methods when no longer needed for compatibility.
  void setMeta(BlockType blockType) { metaWord_ = MetaWord(blockType); }
  void setMetaWord(word_t mw) {
    metaWord_ = MetaWord(static_cast<BlockType>(mw >> MetaWord::kGcBitCount));
  }

 private:
  MetaWord metaWord_;
};


// TODO: remove this when no longer needed for compatibility.
static const int kBlockHeaderSize = sizeof(Block);


/** Contains shared meta-data for blocks that point to it. Metas contain:
 *  - the size of the block and the size of each element if the block has elements
 *  - the `BlockType`
 *  - an optional pointer to a `Class`
 *  - various flags
 *  - pointer maps for the block and its elements, to be used by the GC
 *  - an array of pointers, useful for methods or static class members.
 *  Each `Class` typically has its own `Meta` for instances. Built-in classes have their own
 *  `Meta`s, too.
 */
class Meta: public Block {
 public:
  void* operator new (size_t, Heap* heap, word_t dataLength, u32 objectSize, u32 elementSize);
  explicit Meta(BlockType blockType)
      : Block(META_BLOCK_TYPE),
        blockType_(blockType) { }

  word_t sizeOfMeta() const;
  DEFINE_CAST(Meta)

  void printMeta(FILE* out);

  word_t dataLength() const { return dataLength_; }
  BlockType blockType() const { return blockType_; }
  bool hasCustomSize() const { return hasCustomSize_; }
  bool hasPointers() const { return hasPointers_; }
  bool hasElementPointers() const { return hasElementPointers_; }
  bool hasCustomPointers() const { return hasCustomPointers_; }
  bool needsRelocation() const { return needsRelocation_; }
  DEFINE_INL_PTR_ACCESSORS2(Class*, clas, setClass)
  u32 objectSize() const { return objectSize_; }
  u32 elementSize() const { return elementSize_; }

  Block* getData(word_t index) const;
  void setData(word_t index, Block* value);

  bool hasElements() { return elementSize() > 0; }
  word_t* rawObjectPointerMap();
  Bitmap objectPointerMap();
  word_t* rawElementPointerMap();
  Bitmap elementPointerMap();

  static const word_t kElementSize = kWordSize;

 private:
  static word_t sizeForMeta(word_t dataLength, u32 objectSize, u32 elementSize);
  Block** dataBase() { return &mem<Block*>(this, sizeof(Meta)); }
  Block* const* dataBase() const { return &mem<Block*>(this, sizeof(Meta)); }

  word_t dataLength_;
  BlockType blockType_ : 8;
  bool hasCustomSize_ : 1;
  bool hasPointers_ : 1;
  bool hasElementPointers_ : 1;
  bool hasCustomPointers_ : 1;
  bool needsRelocation_ : 1;
  alignas(word_t) Class* clas_;
  u32 objectSize_;
  u32 elementSize_;

  static const word_t kPointerMap = 0x8;
  static const word_t kElementPointerMap = 0x1;

  friend class Class;
  friend class Roots;
};


/** A Free block is a range of free memory. It contains a size and a pointer to another Free.
 *  It can be used by the heap as a node in a free list.
 */
class Free: public Block {
 public:
  void* operator new (size_t, Heap* heap, size_t size);
  void* operator new (size_t, void* place, size_t size);
  explicit Free(Free* next)
      : Block(FREE_BLOCK_TYPE),
        next_(next) { }

  DEFINE_CAST(Free)

  word_t size() const { return size_; }
  Free* next() const { return next_; }

 private:
  word_t size_;
  Free* next_;
};


#define DEFINE_TYPE_CHECK(Name, NAME)                   \
bool Block::is##Name() const {                          \
  return blockType() == NAME##_BLOCK_TYPE;              \
}
BLOCK_TYPE_LIST(DEFINE_TYPE_CHECK)
#undef DEFINE_TYPE_CHECK

}
}

#endif

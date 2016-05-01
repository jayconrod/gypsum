// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef block_h
#define block_h

#include <iostream>
#include <type_traits>
#include "bitmap.h"
#include "heap.h"
#include "ptr.h"
#include "utils.h"
#include "vm.h"

namespace codeswitch {
namespace internal {

#define BLOCK_TYPE_LIST(V)                                                     \
V(Meta, META)                                                                  \
V(Free, FREE)                                                                  \
V(Package, PACKAGE)                                                            \
V(PackageVersion, PACKAGE_VERSION)                                             \
V(PackageDependency, PACKAGE_DEPENDENCY)                                       \
V(Stack, STACK)                                                                \
V(Name, NAME)                                                                  \
V(Global, GLOBAL)                                                              \
V(Function, FUNCTION)                                                          \
V(Class, CLASS)                                                                \
V(Field, FIELD)                                                                \
V(Trait, TRAIT)                                                                \
V(TraitTable, TRAIT_TABLE)                                                     \
V(TypeParameter, TYPE_PARAMETER)                                               \
V(I8Array, I8_ARRAY)                                                           \
V(I32Array, I32_ARRAY)                                                         \
V(I64Array, I64_ARRAY)                                                         \
V(BlockArray, BLOCK_ARRAY)                                                     \
V(TaggedArray, TAGGED_ARRAY)                                                   \
V(BlockHashMapTable, BLOCK_HASH_MAP_TABLE)                                     \
V(BlockHashMap, BLOCK_HASH_MAP)                                                \
V(Object, OBJECT)                                                              \
V(Type, TYPE)                                                                  \
V(ExternTypeInfo, EXTERN_TYPE_INFO)                                            \
V(String, STRING)                                                              \
V(ThreadBindle, THREAD_BINDLE)                                                 \


#define ENUM_BLOCK_TYPE(Name, NAME) NAME##_BLOCK_TYPE,
enum BlockType {
  BLOCK_TYPE_LIST(ENUM_BLOCK_TYPE)
  LAST_BLOCK_TYPE
};
#undef ENUM_BLOCK_TYPE


class Bitmap;
class Block;
class Class;
template <class T>
class Handle;
class Heap;
template <class T>
class Local;
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
  void relocate(word_t delta);

  const MetaWord& metaWord() const { return metaWord_; }
  Meta* meta() const;
  void setMeta(Meta* meta);
  BlockType blockType() const;

  word_t elementsLength() const;
  ptrdiff_t elementsOffset() const;
  static u32 elementsOffset(u32 objectSize, u32 elementSize);
  Address elementsBase() const;
  word_t elementsSize() const;

  VM* getVM() const;
  Heap* getHeap() const;

  // TODO: remove these methods when no longer needed for compatibility.
  void setMeta(BlockType blockType) { metaWord_ = MetaWord(blockType); }
  void setMetaWord(word_t mw) {
    metaWord_ = MetaWord(static_cast<BlockType>(mw >> MetaWord::kGcBitCount));
  }

 protected:
  void setElementsLength(word_t length);

 private:
  MetaWord metaWord_;
};


/** Convenience struct for printing the short version of a block. Example:
 *    cout << brief(block)
 *  This is useful for implementing other << operators. In general, while we're printing a
 *  block, we don't want to print another block verbosely. This could result in LOTS of output
 *  and possibly infinite recursion.
 */
struct brief {
 public:
  // We bypass the type system here to avoid a lot of extra includes. Whenever possible, we
  // forward-declare a class instead of including its header. We don't want to include headers
  // just to use `brief`.
  explicit brief(const void* block)
      : block_(reinterpret_cast<const Block*>(block)) { }
  const Block* block_;
};

std::ostream& operator << (std::ostream& os, brief b);


#ifdef DEBUG
/** Prints debugging information about any block on stderr. This is intended to be called
 *  directly from the debugger.
 */
void dump(const Block* block);
#endif


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
  static const BlockType kBlockType = META_BLOCK_TYPE;

  /** Attempts to allocate raw memory for a `Meta`. Throws AllocationError on failure. The
   *  memory will be zero-initialized (except for size-related members), so explicit
   *  zero-initialization is not necessary.
   */
  void* operator new (size_t, Heap* heap, length_t dataLength, u32 objectSize, u32 elementSize);
  explicit Meta(BlockType blockType)
      : Block(META_BLOCK_TYPE),
        blockType_(blockType) { }
  static Local<Meta> create(Heap* heap,
                            length_t dataLength,
                            u32 objectSize,
                            u32 elementSize,
                            BlockType blockType);

  static word_t sizeForMeta(length_t dataLength, u32 objectSize, u32 elementSize);
  word_t sizeOfMeta() const;

  length_t dataLength() const { return dataLength_; }
  BlockType blockType() const { return blockType_; }
  bool hasCustomSize() const { return hasCustomSize_; }
  bool hasPointers() const { return hasPointers_; }
  bool hasElementPointers() const { return hasElementPointers_; }
  bool hasCustomPointers() const { return hasCustomPointers_; }
  bool needsRelocation() const { return needsRelocation_; }
  bool hasWordSizeLength() const { return hasWordSizeLength_; }
  u8 lengthOffset() const { return lengthOffset_; }
  Class* clas() const { return clas_.get(); }
  void setClass(Class* newClass) { clas_.set(this, newClass); }
  u32 objectSize() const { return objectSize_; }
  u32 elementSize() const { return elementSize_; }

  Block* getData(length_t index) const;
  void setData(length_t index, Block* value);

  bool hasElements() { return elementSize() > 0; }
  word_t* rawObjectPointerMap();
  Bitmap objectPointerMap();
  word_t* rawElementPointerMap();
  Bitmap elementPointerMap();

  static const word_t kElementSize = kWordSize;

 private:
  DECLARE_POINTER_MAP()

  static const word_t kElementPointerMap = 0x1;

  Block** dataBase() { return reinterpret_cast<Block**>(elementsBase()); }
  Block* const* dataBase() const { return reinterpret_cast<Block**>(elementsBase()); }

  length_t dataLength_;
  BlockType blockType_ : 8;
  bool hasCustomSize_ : 1;
  bool hasPointers_ : 1;
  bool hasElementPointers_ : 1;
  bool hasCustomPointers_ : 1;
  bool needsRelocation_ : 1;
  bool hasWordSizeLength_ : 1;
  u8 lengthOffset_;
  Ptr<Class> clas_;
  u32 objectSize_;
  u32 elementSize_;
  // Update META_POINTER_LIST if pointers change.

  friend class Class;
};


/** A Free block is a range of free memory. It contains a size and a pointer to another Free.
 *  It can be used by the heap as a node in a free list.
 */
class Free: public Block {
 public:
  static const BlockType kBlockType = FREE_BLOCK_TYPE;

  void* operator new (size_t, Heap* heap, word_t size);
  void* operator new (size_t, void* place, word_t size);
  explicit Free(Free* next)
      : Block(FREE_BLOCK_TYPE),
        next_(next) { }

  word_t size() const { return size_; }
  Free* next() const { return next_; }
  void setNext(Free* next) { next_ = next; }

 private:
  word_t size_;
  Free* next_;

  friend class Roots;
};

STATIC_ASSERT(isAligned(sizeof(Free), kWordSize));


/** Returns true if the block is an instance of the template class. Concrete sub-classes of
 *  Block must define a static kBlockType member for this to work.
 */
template <class T>
bool isa(const Block* block) {
  return block->meta()->blockType() == T::kBlockType;
}


/** A checked cast for a block pointer to a given type. Note that the type argument must be a
 *  pointer type to a concrete Block subclass.
 */
template <class T>
T* block_cast(Block* block) {
  ASSERT(isa<T>(block));
  return reinterpret_cast<T*>(block);
}


template <class T>
const T* const_block_cast(const Block* block) {
  ASSERT(isa<T>(block));
  return reinterpret_cast<const T*>(block);
}

}
}

#endif

// Copyright 2014-2015 Jay Conrod. All rights reserved.

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

using namespace std;

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
        size = const_block_cast<Meta>(this)->sizeOfMeta();
        break;
      default:
        UNREACHABLE();
    }
  }
  ASSERT(size != kNotFound && size > 0);
  return size;
}


static const char* kBlockTypeStrings[] = {
  #define BLOCK_TYPE_STRING(Name, NAME) #Name,
  BLOCK_TYPE_LIST(BLOCK_TYPE_STRING)
  #undef BLOCK_TYPE_STRING
  "Unknown"
};


ostream& operator << (std::ostream& os, brief b) {
  if (b.block_ == nullptr) {
    os << "null";
  } else {
    os << kBlockTypeStrings[b.block_->meta()->blockType()]
       << " @" << static_cast<const void*>(b.block_);
  }
  return os;
}


#ifdef DEBUG
// Forward declarations for `dump`, so that we don't need to include every header.
// Unfortunately, we can't use macros to declare these because some of them are templated.
class Package;
ostream& operator << (ostream&, const Package*);
class PackageName;
ostream& operator << (ostream&, const PackageName*);
class PackageVersion;
ostream& operator << (ostream&, const PackageVersion*);
class PackageDependency;
ostream& operator << (ostream&, const PackageDependency*);
class Stack;
ostream& operator << (ostream&, const Stack*);
class Global;
ostream& operator << (ostream&, const Global*);
class Function;
ostream& operator << (ostream&, const Function*);
class Class;
ostream& operator << (ostream&, const Class*);
class Field;
ostream& operator << (ostream&, const Field*);
class TypeParameter;
ostream& operator << (ostream&, const TypeParameter*);
class Object;
ostream& operator << (ostream&, const Object*);
class Type;
ostream& operator << (ostream&, const Type*);
class String;
ostream& operator << (ostream&, const String*);


void dump(const Block* block) {
  switch (block->blockType()) {
    case META_BLOCK_TYPE: cerr << reinterpret_cast<const Meta*>(block); break;
    case FREE_BLOCK_TYPE: cerr << reinterpret_cast<const Free*>(block); break;
    case PACKAGE_BLOCK_TYPE: cerr << reinterpret_cast<const Package*>(block); break;
    case PACKAGE_NAME_BLOCK_TYPE: cerr << reinterpret_cast<const PackageName*>(block); break;
    case PACKAGE_VERSION_BLOCK_TYPE: cerr << reinterpret_cast<const PackageVersion*>(block); break;
    case PACKAGE_DEPENDENCY_BLOCK_TYPE: cerr << reinterpret_cast<const PackageDependency*>(block); break;
    case STACK_BLOCK_TYPE: cerr << reinterpret_cast<const Stack*>(block); break;
    case GLOBAL_BLOCK_TYPE: cerr << reinterpret_cast<const Global*>(block); break;
    case FUNCTION_BLOCK_TYPE: cerr << reinterpret_cast<const Function*>(block); break;
    case CLASS_BLOCK_TYPE: cerr << reinterpret_cast<const Class*>(block); break;
    case FIELD_BLOCK_TYPE: cerr << reinterpret_cast<const Field*>(block); break;
    case TYPE_PARAMETER_BLOCK_TYPE: cerr << reinterpret_cast<const TypeParameter*>(block); break;
    case I8_ARRAY_BLOCK_TYPE: cerr << reinterpret_cast<const I8Array*>(block); break;
    case I32_ARRAY_BLOCK_TYPE: cerr << reinterpret_cast<const I32Array*>(block); break;
    case I64_ARRAY_BLOCK_TYPE: cerr << reinterpret_cast<const I64Array*>(block); break;
    case BLOCK_ARRAY_BLOCK_TYPE: cerr << reinterpret_cast<const BlockArray<Block>*>(block); break;
    case TAGGED_ARRAY_BLOCK_TYPE: cerr << reinterpret_cast<const TaggedArray<Block>*>(block); break;
    case OBJECT_BLOCK_TYPE: cerr << reinterpret_cast<const Object*>(block); break;
    case TYPE_BLOCK_TYPE: cerr << reinterpret_cast<const Type*>(block); break;
    case STRING_BLOCK_TYPE: cerr << reinterpret_cast<const String*>(block); break;
    default:
      cerr << brief(block);
  }
  cerr << endl;
}
#endif


void Block::relocate(word_t delta) {
  switch (meta()->blockType()) {
    case STACK_BLOCK_TYPE: block_cast<Stack>(this)->relocateStack(delta); break;
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


#ifdef DEBUG
void Block::dump() const {
  cerr << brief(this) << endl;
}
#endif


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


ostream& operator << (ostream& os, const Meta* meta) {
  os << brief(meta)
     << "\n  length: " << meta->dataLength()
     << "\n  type: " << kBlockTypeStrings[meta->blockType()]
     << "\n  has custom size: " << meta->hasCustomSize()
     << "\n  has pointers: " << meta->hasPointers()
     << "\n  has element pointers: " << meta->hasElementPointers()
     << "\n  has custom pointers: " << meta->hasCustomPointers()
     << "\n  needs relocation: " << meta->needsRelocation()
     << "\n  has word size length: " << meta->hasWordSizeLength()
     << "\n  length offset: " << meta->lengthOffset()
     << "\n  clas: " << brief(meta->clas())
     << "\n  object size: " << meta->objectSize()
     << "\n  element size: " << meta->elementSize();
  return os;
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


ostream& operator << (ostream& os, const Free* free) {
  os << brief(free)
     << "\n  size: " << free->size()
     << "\n  next: " << brief(free->next());
  return os;
}

}
}

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef class_h
#define class_h

#include "block.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Function;
class Package;
class TaggedArray;
class Type;

class Class: public Block {
 public:
  static Class* tryAllocate(Heap* heap);
  static Handle<Class> allocate(Heap* heap);
  void initialize(u32 flags,
                  Type* supertype,
                  BlockArray* fields,
                  Type* elementType,
                  WordArray* constructors,
                  WordArray* methods,
                  Package* package,
                  Meta* instanceMeta);

  void printClass(FILE* out);
  DEFINE_CAST(Class)

  DEFINE_INL_ACCESSORS(u32, flags, setFlags, kFlagsOffset)
  DEFINE_INL_PTR_ACCESSORS(Type*, supertype, setSupertype, kSupertypeOffset)
  DEFINE_INL_PTR_ACCESSORS(BlockArray*, fields, setFields, kFieldsOffset)
  word_t findFieldIndex(word_t offset);
  word_t findFieldOffset(word_t index);
  DEFINE_INL_PTR_ACCESSORS(Type*, elementType, setElementType, kElementTypeOffset)
  DEFINE_INL_PTR_ACCESSORS(WordArray*, constructors, setConstructors, kConstructorsOffset)
  inline Function* getConstructor(word_t index);
  DEFINE_INL_PTR_ACCESSORS(WordArray*, methods, setMethods, kMethodsOffset)
  inline Function* getMethod(word_t index);
  DEFINE_INL_PTR_ACCESSORS(Package*, package, setPackage, kPackageOffset)
  DEFINE_INL_PTR_ACCESSORS(Meta*, instanceMeta, setInstanceMeta, kInstanceMetaOffset)
  inline Meta* getOrTryBuildInstanceMeta(Heap* heap);
  Meta* tryBuildInstanceMeta(Heap* heap);
  inline Handle<Meta> getOrBuildInstanceMeta(Heap* heap);
  Handle<Meta> buildInstanceMeta(Heap* heap);

  bool isSubclassOf(Class* other);

  static const int kFlagsOffset = kBlockHeaderSize;
  static const int kSupertypeOffset = kFlagsOffset + kWordSize;
  static const int kFieldsOffset = kSupertypeOffset + kWordSize;
  static const int kElementTypeOffset = kFieldsOffset + kWordSize;
  static const int kConstructorsOffset = kElementTypeOffset + kWordSize;
  static const int kMethodsOffset = kConstructorsOffset + kWordSize;
  static const int kPackageOffset = kMethodsOffset + kWordSize;
  static const int kInstanceMetaOffset = kPackageOffset + kWordSize;
  static const int kSize = kInstanceMetaOffset + kWordSize;

  static const word_t kPointerMap = 0x1FC;

 private:
  void computeSizeAndPointerMap(u32* size, bool* hasPointers, BitSet* pointerMap);
  void computeSizeAndPointerMapForType(Type* type, u32* size,
                                       bool* hasPointers, BitSet* pointerMap);
};

}
}

#endif

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
  void initialize(Type* supertype,
                  BlockArray* fieldTypes,
                  Type* elementType,
                  WordArray* constructors,
                  WordArray* methods,
                  Package* package,
                  Meta* instanceMeta);

  void printClass(FILE* out);
  DEFINE_CAST(Class)

  DEFINE_INL_PTR_ACCESSORS(Type*, supertype, setSupertype, kSupertypeOffset)
  DEFINE_INL_PTR_ACCESSORS(BlockArray*, fieldTypes, setFieldTypes, kFieldTypesOffset)
  inline Type* getFieldType(word_t index);
  word_t findFieldIndex(word_t offset);
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

  static const int kSupertypeOffset = kBlockHeaderSize;
  static const int kFieldTypesOffset = kSupertypeOffset + kWordSize;
  static const int kElementTypeOffset = kFieldTypesOffset + kWordSize;
  static const int kConstructorsOffset = kElementTypeOffset + kWordSize;
  static const int kMethodsOffset = kConstructorsOffset + kWordSize;
  static const int kPackageOffset = kMethodsOffset + kWordSize;
  static const int kInstanceMetaOffset = kPackageOffset + kWordSize;
  static const int kSize = kInstanceMetaOffset + kWordSize;

  static const word_t kPointerMap = 0xFE;

 private:
  void computeSizeAndPointerMap(BlockArray* types, u32* size,
                                bool* hasPointers, BitSet* pointerMap);
  void computeSizeAndPointerMapForType(Type* type, u32* size,
                                       bool* hasPointers, BitSet* pointerMap);
};

}
}

#endif

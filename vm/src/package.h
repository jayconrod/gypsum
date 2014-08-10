// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef package_h
#define package_h

#include <cstdio>
#include "block.h"
#include "handle.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class BlockArray;
class Function;
class Heap;
class String;
class TypeParameter;

class Package: public Block {
 public:
  static Package* tryAllocate(Heap* heap);
  static Local<Package> allocate(Heap* heap);
  DEFINE_CAST(Package)

  void printPackage(FILE* out);

  static Local<Package> loadFromFile(VM* vm, const char* fileName);
  static Local<Package> loadFromBytes(VM* vm, const u8* bytes, word_t size);
  static Local<Package> loadFromStream(VM* vm, std::istream& stream);

  DEFINE_INL_ACCESSORS(u64, flags, setFlags, kFlagsOffset)
  DEFINE_INL_PTR_ACCESSORS(BlockArray*, strings, setStrings, kStringsOffset)
  inline String* getString(word_t index);
  DEFINE_INL_PTR_ACCESSORS(BlockArray*, functions, setFunctions, kFunctionsOffset)
  inline Function* getFunction(word_t index);
  DEFINE_INL_PTR_ACCESSORS(BlockArray*, classes, setClasses, kClassesOffset)
  inline Class* getClass(word_t index);
  DEFINE_INL_PTR_ACCESSORS(BlockArray*,
                           typeParameters,
                           setTypeParameters,
                           kTypeParametersOffset)
  inline TypeParameter* getTypeParameter(word_t index);
  DEFINE_INL_ACCESSORS(word_t, entryFunctionIndex, setEntryFunctionIndex,
                       kEntryFunctionIndexOffset)
  inline Function* entryFunction();

  static const u32 kMagic = 0x676b7073;
  static const word_t kFlagsOffset = kBlockHeaderSize;
  static const word_t kStringsOffset = kFlagsOffset + sizeof(u64);
  static const word_t kFunctionsOffset = kStringsOffset + kWordSize;
  static const word_t kClassesOffset = kFunctionsOffset + kWordSize;
  static const word_t kTypeParametersOffset = kClassesOffset + kWordSize;
  static const word_t kEntryFunctionIndexOffset = kTypeParametersOffset + kWordSize;
  static const word_t kSize = kEntryFunctionIndexOffset + kWordSize;

  static const word_t kPointerMap = 0x3c;
};

}
}

#endif

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

template <class T>
class BlockArray;
class Function;
class Heap;
class String;
class TypeParameter;


class Package: public Block {
 public:
  DEFINE_NEW(Package, PACKAGE_BLOCK_TYPE)
  explicit Package(VM* vm);
  static Local<Package> create(VM* vm);
  DEFINE_CAST(Package)

  void printPackage(FILE* out);

  static Local<Package> loadFromFile(VM* vm, const char* fileName);
  static Local<Package> loadFromBytes(VM* vm, const u8* bytes, word_t size);
  static Local<Package> loadFromStream(VM* vm, std::istream& stream);

  DEFINE_INL_ACCESSORS2(u64, flags, setFlags)
  DEFINE_INL_PTR_ACCESSORS2(BlockArray<String>*, strings, setStrings)
  String* getString(word_t index);
  DEFINE_INL_PTR_ACCESSORS2(BlockArray<Function>*, functions, setFunctions)
  Function* getFunction(word_t index);
  DEFINE_INL_PTR_ACCESSORS2(BlockArray<Class>*, classes, setClasses)
  Class* getClass(word_t index);
  DEFINE_INL_PTR_ACCESSORS2(BlockArray<TypeParameter>*, typeParameters, setTypeParameters)
  TypeParameter* getTypeParameter(word_t index);
  DEFINE_INL_ACCESSORS2(word_t, entryFunctionIndex, setEntryFunctionIndex)
  Function* entryFunction();

  static const u32 kMagic = 0x676b7073;
  static const word_t kPointerMap = 0x3c;

 private:
  u64 flags_;
  BlockArray<String>* strings_;
  BlockArray<Function>* functions_;
  BlockArray<Class>* classes_;
  BlockArray<TypeParameter>* typeParameters_;
  word_t entryFunctionIndex_;
};

}
}

#endif

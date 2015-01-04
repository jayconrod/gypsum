// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef package_h
#define package_h

#include <iostream>
#include "block.h"
#include "handle.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class BlockArray;
class Function;
class Global;
class Heap;
class String;
class TypeParameter;


class Package: public Block {
 public:
  static const BlockType kBlockType = PACKAGE_BLOCK_TYPE;

  DEFINE_NEW(Package, PACKAGE_BLOCK_TYPE)
  explicit Package(VM* vm);
  static Local<Package> create(Heap* heap);

  static Local<Package> loadFromFile(VM* vm, const char* fileName);
  static Local<Package> loadFromBytes(VM* vm, const u8* bytes, word_t size);
  static Local<Package> loadFromStream(VM* vm, std::istream& stream);

  DEFINE_INL_ACCESSORS2(u64, flags, setFlags)
  String* name() const { return name_.get(); }
  void setName(String* newName) { name_.set(this, newName); }
  String* version() const { return version_.get(); }
  void setVersion(String* newVersion) { version_.set(this, newVersion); }
  BlockArray<String>* strings() const { return strings_.get(); }
  void setStrings(BlockArray<String>* newStrings) { strings_.set(this, newStrings); }
  String* getString(length_t index);
  BlockArray<Global>* globals() const { return globals_.get(); }
  void setGlobals(BlockArray<Global>* newGlobals) { globals_.set(this, newGlobals); }
  Global* getGlobal(length_t index);
  BlockArray<Function>* functions() const { return functions_.get(); }
  void setFunctions(BlockArray<Function>* newFunctions) { functions_.set(this, newFunctions); }
  Function* getFunction(length_t index);
  BlockArray<Class>* classes() const { return classes_.get(); }
  void setClasses(BlockArray<Class>* newClasses) { classes_.set(this, newClasses); }
  Class* getClass(length_t index);
  BlockArray<TypeParameter>* typeParameters() const { return typeParameters_.get(); }
  void setTypeParameters(BlockArray<TypeParameter>* newTypeParameters) {
    typeParameters_.set(this, newTypeParameters);
  }
  TypeParameter* getTypeParameter(length_t index);
  DEFINE_INL_ACCESSORS2(length_t, entryFunctionIndex, setEntryFunctionIndex)
  Function* entryFunction();
  DEFINE_INL_ACCESSORS2(length_t, initFunctionIndex, setInitFunctionIndex)
  Function* initFunction();

 private:
  DECLARE_POINTER_MAP()

  u64 flags_;
  Ptr<String> name_;
  Ptr<String> version_;
  Ptr<BlockArray<String>> strings_;
  Ptr<BlockArray<Global>> globals_;
  Ptr<BlockArray<Function>> functions_;
  Ptr<BlockArray<Class>> classes_;
  Ptr<BlockArray<TypeParameter>> typeParameters_;
  length_t entryFunctionIndex_;
  length_t initFunctionIndex_;
  // Update PACKAGE_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const Package* pkg);

}
}

#endif

// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef function_h
#define function_h

#include <iostream>
#include <new>
#include <vector>
#include "array.h"
#include "block.h"
#include "defnid.h"
#include "platform.h"
#include "type.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Bitmap;
class Class;
class ObjectTypeDefn;
class Name;
class Package;
class StackPointerMap;
class TypeParameter;

class Function: public Block {
 public:
  static const BlockType kBlockType = FUNCTION_BLOCK_TYPE;

  void* operator new(size_t, Heap* heap, length_t instructionsSize);
  Function(DefnId id,
           Name* name,
           String* sourceName,
           u32 flags,
           BlockArray<TypeParameter>* typeParameters,
           Type* returnType,
           BlockArray<Type>* parameterTypes,
           ObjectTypeDefn* definingClass,
           word_t localsSize,
           const std::vector<u8>& instructions,
           LengthArray* blockOffsets,
           Package* package,
           StackPointerMap* stackPointerMap,
           NativeFunction nativeFunction);
  static Local<Function> create(Heap* heap, DefnId id);
  static Local<Function> create(Heap* heap,
                                DefnId id,
                                const Handle<Name>& name,
                                const Handle<String>& sourceName,
                                u32 flags,
                                const Handle<BlockArray<TypeParameter>>& typeParameters,
                                const Handle<Type>& returnType,
                                const Handle<BlockArray<Type>>& parameterTypes,
                                const Handle<ObjectTypeDefn>& definingClass,
                                word_t localsSize,
                                const std::vector<u8>& instructions,
                                const Handle<LengthArray>& blockOffsets,
                                const Handle<Package>& package,
                                NativeFunction nativeFunction);

  static word_t sizeForFunction(length_t instructionsSize);

  DefnId id() const { return id_; }
  Name* name() const { return name_.get(); }
  void setName(Name* newName) { name_.set(this, newName); }
  String* sourceName() const { return sourceName_.get(); }
  void setSourceName(String* newSourceName) { sourceName_.set(this, newSourceName); }
  u32 flags() const { return flags_; }
  void setFlags(u32 newFlags) { flags_ = newFlags; }

  BuiltinId builtinId() const {
    ASSERT(hasBuiltinId());
    return builtinId_;
  }
  void setBuiltinId(BuiltinId id) { builtinId_ = id; }
  bool hasBuiltinId() const { return builtinId_ != 0; }

  BlockArray<TypeParameter>* typeParameters() const { return typeParameters_.get(); }
  void setTypeParameters(BlockArray<TypeParameter>* newTypeParameters) {
    typeParameters_.set(this, newTypeParameters);
  }
  TypeParameter* typeParameter(length_t index) const { return typeParameters()->get(index); }

  Type* returnType() const { return returnType_.get(); }
  void setReturnType(Type* newReturnType) { returnType_.set(this, newReturnType); }

  BlockArray<Type>* parameterTypes() const { return parameterTypes_.get(); }
  void setParameterTypes(BlockArray<Type>* newParameterTypes) {
    parameterTypes_.set(this, newParameterTypes);
  }
  word_t parametersSize() const;
  ptrdiff_t parameterOffset(length_t index) const;

  ObjectTypeDefn* definingClass() const { return definingClass_.get(); }
  void setDefiningClass(ObjectTypeDefn* newDefiningClass) {
    definingClass_.set(this, newDefiningClass);
  }

  word_t localsSize() const { return localsSize_; }
  void setLocalsSize(word_t newLocalsSize) { localsSize_ = newLocalsSize; }

  length_t instructionsSize() const { return instructionsSize_; }
  u8* instructionsStart() const;

  LengthArray* blockOffsets() const { return blockOffsets_.get(); }
  length_t blockOffset(length_t index) const { return blockOffsets()->get(index); }

  Package* package() const { return package_.get(); }
  void setPackage(Package* newPackage) { package_.set(this, newPackage); }

  StackPointerMap* stackPointerMap() const { return stackPointerMap_.get(); }
  void setStackPointerMap(StackPointerMap* newStackPointerMap) {
    stackPointerMap_.set(this, newStackPointerMap);
  }
  bool hasPointerMapAtPcOffset(length_t pcOffset) const;

  bool isNative() const;
  NativeFunction nativeFunction() const { return nativeFunction_; }
  void setNativeFunction(NativeFunction newNativeFunction) {
    nativeFunction_ = newNativeFunction;
  }
  void ensureNativeFunction();
  NativeFunction ensureAndGetNativeFunction();

 private:
  DECLARE_POINTER_MAP()
  DefnId id_;
  Ptr<Name> name_;
  Ptr<String> sourceName_;
  u32 flags_;
  BuiltinId builtinId_;
  Ptr<BlockArray<TypeParameter>> typeParameters_;
  Ptr<Type> returnType_;
  Ptr<BlockArray<Type>> parameterTypes_;
  Ptr<ObjectTypeDefn> definingClass_;
  word_t localsSize_;
  length_t instructionsSize_;
  Ptr<LengthArray> blockOffsets_;
  Ptr<Package> package_;
  Ptr<StackPointerMap> stackPointerMap_;
  NativeFunction nativeFunction_;
  // Update FUNCTION_POINTER_LIST if pointer members change.
};

std::ostream& operator << (std::ostream& os, const Function* fn);


class StackPointerMap: public WordArray {
  // NOTE: We shouldn't say StackPointerMap "is a" WordArray. It is implemented using a
  // WordArray, so private inheritance might be more appropriate. However, it definitely
  // "is a" block. Multiple inheritance will likely mess up everything here, but at least
  // there aren't any virtual methods.
 public:
  static Local<StackPointerMap> buildFrom(Heap* heap, const Local<Function>& function);

  Bitmap bitmap();
  void getParametersRegion(word_t* paramOffset, word_t* paramCount);
  void getLocalsRegion(length_t pc, word_t* localsOffset, word_t* localsCount);
  word_t searchLocalsRegion(length_t pc);
  bool hasLocalsRegion(length_t pc) { return searchLocalsRegion(pc) != kNotSet; }

  DEFINE_INL_INDEX_ACCESSORS(word_t, bitmapLength, setBitmapLength, kBitmapLengthIndex)
  DEFINE_INL_INDEX_ACCESSORS(word_t, entryCount, setEntryCount, kEntryCountIndex)
  DEFINE_INL_ENTRY_ACCESSORS(word_t, pcOffset, setPcOffset,
                             kHeaderLength, kEntryLength, kPcOffsetEntryIndex)
  DEFINE_INL_ENTRY_ACCESSORS(word_t, mapOffset, setMapOffset,
                             kHeaderLength, kEntryLength, kMapOffsetEntryIndex)
  DEFINE_INL_ENTRY_ACCESSORS(word_t, mapCount, setMapCount,
                             kHeaderLength, kEntryLength, kMapCountEntryIndex)

  static const int kBitmapLengthIndex = 0;
  static const int kEntryCountIndex = kBitmapLengthIndex + 1;
  static const int kHeaderLength = kEntryCountIndex + 1;

  static const int kPcOffsetEntryIndex = 0;
  static const int kMapOffsetEntryIndex = kPcOffsetEntryIndex + 1;
  static const int kMapCountEntryIndex = kMapOffsetEntryIndex + 1;
  static const int kEntryLength = kMapCountEntryIndex + kWordSize;
};

}
}

#endif

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef function_h
#define function_h

#include <new>
#include <vector>
#include "array.h"
#include "block.h"
#include "type.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Bitmap;
class Package;
class StackPointerMap;
class TypeParameter;

class Function: public Block {
 public:
  void* operator new(size_t, Heap* heap, length_t instructionsSize);
  Function(u32 flags,
           TaggedArray<TypeParameter>* typeParameters,
           BlockArray<Type>* types,
           word_t localsSize,
           const std::vector<u8>& instructions,
           LengthArray* blockOffsets,
           Package* package,
           StackPointerMap* stackPointerMap);
  static Local<Function> create(Heap* heap,
                                u32 flags,
                                const Handle<TaggedArray<TypeParameter>>& typeParameters,
                                const Handle<BlockArray<Type>>& types,
                                word_t localsSize,
                                const std::vector<u8>& instructions,
                                const Handle<LengthArray>& blockOffsets,
                                const Handle<Package>& package);

  static word_t sizeForFunction(length_t instructionsSize);
  word_t sizeOfFunction() const;
  void printFunction(FILE* out);
  DEFINE_CAST(Function)

  u32 flags() const { return flags_; }

  BuiltinId builtinId() const {
    ASSERT(hasBuiltinId());
    return builtinId_;
  }
  void setBuiltinId(BuiltinId id) { builtinId_ = id; }
  bool hasBuiltinId() const { return builtinId_ != 0; }

  TaggedArray<TypeParameter>* typeParameters() const { return typeParameters_; }
  TypeParameter* typeParameter(length_t index) const;
  length_t typeParameterCount() const { return typeParameters_->length(); }

  BlockArray<Type>* types() const { return types_; }
  Type* returnType() const { return types_->get(0); }
  length_t parameterCount() const { return types_->length() - 1; }
  word_t parametersSize() const;
  ptrdiff_t parameterOffset(length_t index) const;
  Type* parameterType(length_t index) const { return types_->get(index + 1); }

  word_t localsSize() const { return localsSize_; }

  length_t instructionsSize() const { return instructionsSize_; }
  u8* instructionsStart() const;

  LengthArray* blockOffsets() const { return blockOffsets_; }
  length_t blockOffset(length_t index) const { return blockOffsets_->get(index); }

  Package* package() const { return package_; }
  DEFINE_INL_PTR_ACCESSORS2(StackPointerMap*, stackPointerMap, setStackPointerMap)

 private:
  DECLARE_POINTER_MAP()
  u32 flags_;
  BuiltinId builtinId_;
  TaggedArray<TypeParameter>* typeParameters_;
  BlockArray<Type>* types_;
  word_t localsSize_;
  length_t instructionsSize_;
  LengthArray* blockOffsets_;
  Package* package_;
  StackPointerMap* stackPointerMap_;
  // Update FUNCTION_POINTER_LIST if pointer members change.
};


class StackPointerMap: public WordArray {
 public:
  static StackPointerMap* tryBuildFrom(Heap* heap, Function* function);
  static Local<StackPointerMap> buildFrom(Heap* heap, Local<Function> function);
  static StackPointerMap* cast(Block* block);

  Bitmap bitmap();
  void getParametersRegion(word_t* paramOffset, word_t* paramCount);
  void getLocalsRegion(length_t pc, word_t* localsOffset, word_t* localsCount);

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

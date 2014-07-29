// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef function_h
#define function_h

#include "array.h"
#include "block.h"
#include "type.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Bitmap;
class Package;
class StackPointerMap;

class Function: public Block {
 public:
  static Function* tryAllocate(Heap* heap, word_t instructionsSize);
  static Handle<Function> allocate(Heap* heap, word_t instructionsSize);
  void initialize(u32 flags,
                  TaggedArray* typeParameters,
                  BlockArray* types,
                  word_t localsSize,
                  const vector<u8>& instructions,
                  WordArray* blockOffsets,
                  Package* package,
                  StackPointerMap* stackPointerMap);

  static inline word_t sizeForFunction(word_t instructionsSize);
  inline word_t sizeOfFunction();
  void printFunction(FILE* out);
  DEFINE_CAST(Function)

  DEFINE_INL_ACCESSORS(u32, flags, setFlags, kFlagsOffset)
  inline BuiltinId builtinId();
  inline void setBuiltinId(BuiltinId id);
  inline bool hasBuiltinId();

  DEFINE_INL_PTR_ACCESSORS(TaggedArray*,
                           typeParameters,
                           setTypeParameters,
                           kTypeParametersOffset)
  inline word_t typeParameterCount();
  inline TypeParameter* typeParameter(word_t index);

  DEFINE_INL_PTR_ACCESSORS(BlockArray*, types, setTypes, kTypesOffset)
  inline Type* returnType();
  inline word_t parameterCount();
  inline word_t parametersSize();
  inline Type* parameterType(word_t index);

  DEFINE_INL_ACCESSORS(word_t, localsSize, setLocalsSize, kLocalsSizeOffset)

  DEFINE_INL_ACCESSORS(word_t, instructionsSize, setInstructionsSize, kInstructionsSizeOffset)
  inline u8* instructionsStart();

  DEFINE_INL_PTR_ACCESSORS(WordArray*, blockOffsets, setBlockOffsets, kBlockOffsetsOffset)
  inline word_t blockOffset(word_t index);

  DEFINE_INL_PTR_ACCESSORS(Package*, package, setPackage, kPackageOffset)
  DEFINE_INL_PTR_ACCESSORS(StackPointerMap*, stackPointerMap,
                           setStackPointerMap, kStackPointerMapOffset)

  static const int kFlagsOffset = kBlockHeaderSize;
  static const int kBuiltinIdOffset = kFlagsOffset + sizeof(u32);
  static const int kTypeParametersOffset = align(kBuiltinIdOffset + sizeof(u8), kWordSize);
  static const int kTypesOffset = kTypeParametersOffset + kWordSize;
  static const int kLocalsSizeOffset = kTypesOffset + kWordSize;
  static const int kInstructionsSizeOffset = kLocalsSizeOffset + kWordSize;
  static const int kBlockOffsetsOffset = kInstructionsSizeOffset + kWordSize;
  static const int kPackageOffset = kBlockOffsetsOffset + kWordSize;
  static const int kStackPointerMapOffset = kPackageOffset + kWordSize;
  static const int kHeaderSize = kStackPointerMapOffset + kWordSize;

  static const word_t kPointerMap = 0x1cc;
};


class StackPointerMap: public WordArray {
 public:
  static StackPointerMap* tryBuildFrom(Heap* heap, Function* function);
  static Handle<StackPointerMap> buildFrom(Heap* heap, Handle<Function> function);
  static inline StackPointerMap* cast(Block* block);

  inline Bitmap bitmap();
  inline void getParametersRegion(word_t* paramOffset, word_t* paramCount);
  void getLocalsRegion(word_t pc, word_t* localsOffset, word_t* localsCount);

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

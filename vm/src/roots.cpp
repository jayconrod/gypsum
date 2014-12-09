// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "roots.h"

#include "array.h"
#include "block.h"
#include "field.h"
#include "function.h"
#include "handle.h"
#include "package.h"
#include "stack.h"
#include "string.h"
#include "type.h"
#include "type-parameter.h"

namespace codeswitch {
namespace internal {

void Roots::initialize(Heap* heap) {
  auto metaMeta = new(heap, 0, sizeof(Meta), Meta::kElementSize) Meta(META_BLOCK_TYPE);
  basicRoots_[META_META_ROOT_INDEX] = metaMeta;
  metaMeta->hasPointers_ = true;
  metaMeta->hasCustomSize_ = true;
  metaMeta->hasElementPointers_ = true;
  metaMeta->lengthOffset_ = offsetof(Meta, dataLength_);
  metaMeta->objectPointerMap().setWord(0, Meta::kPointerMap);
  metaMeta->elementPointerMap().setWord(0, Meta::kElementPointerMap);

  auto freeMeta = new(heap, 0, sizeof(Free), 1) Meta(FREE_BLOCK_TYPE);
  freeMeta->hasWordSizeLength_ = true;
  freeMeta->lengthOffset_ = offsetof(Free, size_);
  basicRoots_[FREE_META_ROOT_INDEX] = freeMeta;

  auto packageMeta = new(heap, 0, sizeof(Package), 0) Meta(PACKAGE_BLOCK_TYPE);
  packageMeta->hasPointers_ = true;
  packageMeta->objectPointerMap().setWord(0, Package::kPointerMap);
  basicRoots_[PACKAGE_META_ROOT_INDEX] = packageMeta;

  auto stackMeta = new(heap, 0, sizeof(Stack), 1) Meta(STACK_BLOCK_TYPE);
  stackMeta->hasPointers_ = true;
  stackMeta->hasCustomPointers_ = true;
  stackMeta->needsRelocation_ = true;
  stackMeta->hasWordSizeLength_ = true;
  stackMeta->lengthOffset_ = offsetof(Stack, stackSize_);
  basicRoots_[STACK_META_ROOT_INDEX] = stackMeta;

  auto functionMeta = new(heap, 0, sizeof(Function), 1) Meta(FUNCTION_BLOCK_TYPE);
  functionMeta->hasPointers_ = true;
  functionMeta->lengthOffset_ = offsetof(Function, instructionsSize_);
  functionMeta->objectPointerMap().setWord(0, Function::kPointerMap);
  basicRoots_[FUNCTION_META_ROOT_INDEX] = functionMeta;

  auto classMeta = new(heap, 0, sizeof(Class), 0) Meta(CLASS_BLOCK_TYPE);
  classMeta->hasPointers_ = true;
  classMeta->objectPointerMap().setWord(0, Class::kPointerMap);
  basicRoots_[CLASS_META_ROOT_INDEX] = classMeta;

  auto fieldMeta = new(heap, 0, sizeof(Field), 0) Meta(FIELD_BLOCK_TYPE);
  fieldMeta->hasPointers_ = true;
  fieldMeta->objectPointerMap().setWord(0, Field::kPointerMap);
  basicRoots_[FIELD_META_ROOT_INDEX] = fieldMeta;

  auto typeParameterMeta =
      new(heap, 0, sizeof(TypeParameter), 0) Meta(TYPE_PARAMETER_BLOCK_TYPE);
  typeParameterMeta->hasPointers_ = true;
  typeParameterMeta->objectPointerMap().setWord(0, TypeParameter::kPointerMap);
  basicRoots_[TYPE_PARAMETER_META_ROOT_INDEX] = typeParameterMeta;

  auto i8ArrayMeta = new(heap, 0, sizeof(I8Array), sizeof(i8)) Meta(I8_ARRAY_BLOCK_TYPE);
  i8ArrayMeta->lengthOffset_ = offsetof(I8Array, length_);
  basicRoots_[I8_ARRAY_META_ROOT_INDEX] = i8ArrayMeta;

  auto i32ArrayMeta =
      new(heap, 0, sizeof(I32Array), sizeof(i32)) Meta(I32_ARRAY_BLOCK_TYPE);
  i32ArrayMeta->lengthOffset_ = offsetof(I32Array, length_);
  basicRoots_[I32_ARRAY_META_ROOT_INDEX] = i32ArrayMeta;

  auto i64ArrayMeta =
      new(heap, 0, sizeof(I64Array), sizeof(i64)) Meta(I64_ARRAY_BLOCK_TYPE);
  i64ArrayMeta->lengthOffset_ = offsetof(I64Array, length_);
  basicRoots_[I64_ARRAY_META_ROOT_INDEX] = i64ArrayMeta;

  auto blockArrayMeta =
      new(heap, 0, sizeof(BlockArray<Block>), kWordSize) Meta(BLOCK_ARRAY_BLOCK_TYPE);
  blockArrayMeta->hasPointers_ = true;
  blockArrayMeta->hasElementPointers_ = true;
  blockArrayMeta->lengthOffset_ = offsetof(BlockArray<Block>, length_);
  blockArrayMeta->objectPointerMap().setWord(0, 0);
  blockArrayMeta->elementPointerMap().setWord(0, 1);
  basicRoots_[BLOCK_ARRAY_META_ROOT_INDEX] = blockArrayMeta;

  auto taggedArrayMeta =
      new(heap, 0, sizeof(TaggedArray<Block>), kWordSize) Meta(TAGGED_ARRAY_BLOCK_TYPE);
  taggedArrayMeta->hasPointers_ = true;
  taggedArrayMeta->hasCustomPointers_ = true;
  taggedArrayMeta->lengthOffset_ = offsetof(TaggedArray<Block>, length_);
  basicRoots_[TAGGED_ARRAY_META_ROOT_INDEX] = taggedArrayMeta;

  auto emptyI8Array = new(heap, 0) I8Array;
  basicRoots_[EMPTY_I8_ARRAY_ROOT_INDEX] = emptyI8Array;

  auto emptyI32Array = new(heap, 0) I32Array;
  basicRoots_[EMPTY_I32_ARRAY_ROOT_INDEX] = emptyI32Array;

  auto emptyI64Array = new(heap, 0) I64Array;
  basicRoots_[EMPTY_I64_ARRAY_ROOT_INDEX] = emptyI64Array;

  auto emptyTaggedArray = new(heap, 0) TaggedArray<Block>;
  basicRoots_[EMPTY_TAGGED_ARRAY_ROOT_INDEX] = emptyTaggedArray;

  auto emptyBlockArray = new(heap, 0) BlockArray<Block>;
  basicRoots_[EMPTY_BLOCK_ARRAY_ROOT_INDEX] = emptyBlockArray;

  initializeBuiltins(heap);

  auto nullType = new(heap, 1) Type(getBuiltinClass(BUILTIN_NOTHING_CLASS_ID),
                                    Type::NULLABLE_FLAG);
  basicRoots_[NULL_TYPE_ROOT_INDEX] = nullType;

  auto erasedType = new(heap, 0) Type(Type::ERASED_TYPE, Type::NO_FLAGS);
  basicRoots_[ERASED_TYPE_ROOT_INDEX] = erasedType;

  Meta* typeMeta = getBuiltinClass(BUILTIN_TYPE_CLASS_ID)->instanceMeta();
  typeMeta->blockType_ = TYPE_BLOCK_TYPE;
  basicRoots_[TYPE_META_ROOT_INDEX] = typeMeta;

  Meta* stringMeta = getBuiltinClass(BUILTIN_STRING_CLASS_ID)->instanceMeta();
  stringMeta->blockType_ = STRING_BLOCK_TYPE;
  basicRoots_[STRING_META_ROOT_INDEX] = stringMeta;

  auto trueString = String::rawFromUtf8CString(heap, "true");
  basicRoots_[TRUE_STRING_ROOT_INDEX] = trueString;

  auto falseString = String::rawFromUtf8CString(heap, "false");
  basicRoots_[FALSE_STRING_ROOT_INDEX] = falseString;
}


Meta* Roots::getMetaForBlockType(int type) {
  switch (type) {
    case META_BLOCK_TYPE: return metaMeta();
    case FREE_BLOCK_TYPE: return freeMeta();
    case PACKAGE_BLOCK_TYPE: return packageMeta();
    case STACK_BLOCK_TYPE: return stackMeta();
    case FUNCTION_BLOCK_TYPE: return functionMeta();
    case CLASS_BLOCK_TYPE: return classMeta();
    case FIELD_BLOCK_TYPE: return fieldMeta();
    case TYPE_PARAMETER_BLOCK_TYPE: return typeParameterMeta();
    case I8_ARRAY_BLOCK_TYPE: return i8ArrayMeta();
    case I32_ARRAY_BLOCK_TYPE: return i32ArrayMeta();
    case I64_ARRAY_BLOCK_TYPE: return i64ArrayMeta();
    case BLOCK_ARRAY_BLOCK_TYPE: return blockArrayMeta();
    case TAGGED_ARRAY_BLOCK_TYPE: return taggedArrayMeta();
    case TYPE_BLOCK_TYPE: return typeMeta();
    case STRING_BLOCK_TYPE: return stringMeta();
    default:
      UNREACHABLE();
      return nullptr;
  }
}


Class* Roots::getBuiltinClass(BuiltinId id) const {
  auto index = builtinIdToIndex(id);
  ASSERT(index < builtinClasses_.size());
  return builtinClasses_[index];
}


Meta* Roots::getBuiltinMeta(BuiltinId id) const {
  auto index = builtinIdToIndex(id);
  ASSERT(index < builtinMetas_.size());
  return builtinMetas_[index];
}


Type* Roots::getBuiltinType(BuiltinId id) const {
  auto index = builtinIdToIndex(id);
  ASSERT(index < builtinTypes_.size());
  return builtinTypes_[index];
}


String* Roots::getBuiltinName(BuiltinId id) const {
  auto index = builtinIdToIndex(id);
  ASSERT(index < builtinNames_.size());
  return builtinNames_[index];
}


Function* Roots::getBuiltinFunction(BuiltinId id) const {
  auto index = builtinIdToIndex(id);
  ASSERT(index < builtinFunctions_.size());
  return builtinFunctions_[index];
}

}
}

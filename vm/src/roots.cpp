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
#include "type-inl.h"

namespace codeswitch {
namespace internal {

void Roots::initialize(Heap* heap) {
  auto metaMeta = new(heap, 0, sizeof(Meta), Meta::kElementSize) Meta(META_BLOCK_TYPE);
  metaMeta->hasPointers_ = true;
  metaMeta->hasCustomSize_ = true;
  metaMeta->hasElementPointers_ = true;
  metaMeta->objectPointerMap().setWord(0, Meta::kPointerMap);
  metaMeta->elementPointerMap().setWord(0, Meta::kElementPointerMap);
  basicRoots_[META_META_ROOT_INDEX] = metaMeta;

  auto freeMeta = new(heap, 0, sizeof(Free), 0) Meta(FREE_BLOCK_TYPE);
  freeMeta->hasCustomSize_ = true;
  basicRoots_[FREE_META_ROOT_INDEX] = freeMeta;

  auto packageMeta = new(heap, 0, sizeof(Package), 0) Meta(PACKAGE_BLOCK_TYPE);
  packageMeta->hasPointers_ = true;
  packageMeta->objectPointerMap().setWord(0, Package::kPointerMap);
  basicRoots_[PACKAGE_META_ROOT_INDEX] = packageMeta;

  auto stackMeta = new(heap, 0, sizeof(Stack), 1) Meta(STACK_BLOCK_TYPE);
  stackMeta->hasPointers_ = true;
  stackMeta->hasCustomPointers_ = true;
  stackMeta->needsRelocation_ = true;
  basicRoots_[STACK_META_ROOT_INDEX] = stackMeta;

  auto functionMeta = new(heap, 0, sizeof(Function), 0) Meta(FUNCTION_BLOCK_TYPE);
  functionMeta->hasPointers_ = true;
  functionMeta->hasCustomSize_ = true;
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
  basicRoots_[I8_ARRAY_META_ROOT_INDEX] = i8ArrayMeta;

  auto i32ArrayMeta =
      new(heap, 0, sizeof(I32Array), sizeof(i32)) Meta(I32_ARRAY_BLOCK_TYPE);
  basicRoots_[I32_ARRAY_META_ROOT_INDEX] = i32ArrayMeta;

  auto i64ArrayMeta =
      new(heap, 0, sizeof(I64Array), sizeof(i64)) Meta(I64_ARRAY_BLOCK_TYPE);
  basicRoots_[I64_ARRAY_META_ROOT_INDEX] = i64ArrayMeta;

  auto blockArrayMeta =
      new(heap, 0, sizeof(BlockArray<Block>), kWordSize) Meta(BLOCK_ARRAY_BLOCK_TYPE);
  blockArrayMeta->hasPointers_ = true;
  blockArrayMeta->hasElementPointers_ = true;
  blockArrayMeta->objectPointerMap().setWord(0, 0);
  blockArrayMeta->elementPointerMap().setWord(0, 1);
  basicRoots_[BLOCK_ARRAY_META_ROOT_INDEX] = blockArrayMeta;

  auto taggedArrayMeta =
      new(heap, 0, sizeof(TaggedArray<Block>), kWordSize) Meta(TAGGED_ARRAY_BLOCK_TYPE);
  taggedArrayMeta->hasPointers_ = true;
  taggedArrayMeta->hasCustomPointers_ = true;
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

  Type* nullType = Type::tryAllocate(heap, 1);
  nullType->initialize(getBuiltinClass(BUILTIN_NOTHING_CLASS_ID), Type::NULLABLE_FLAG);
  basicRoots_[NULL_TYPE_ROOT_INDEX] = nullType;

  Meta* typeMeta = getBuiltinClass(BUILTIN_TYPE_CLASS_ID)->instanceMeta();
  typeMeta->blockType_ = TYPE_BLOCK_TYPE;
  basicRoots_[TYPE_META_ROOT_INDEX] = typeMeta;

  Meta* stringMeta = getBuiltinClass(BUILTIN_STRING_CLASS_ID)->instanceMeta();
  stringMeta->blockType_ = STRING_BLOCK_TYPE;
  basicRoots_[STRING_META_ROOT_INDEX] = stringMeta;
}

}
}

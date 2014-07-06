// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "roots.h"

#include "block-inl.h"
#include "package-inl.h"
#include "stack-inl.h"
#include "type-inl.h"

namespace codeswitch {
namespace internal {

void Roots::initialize(Heap* heap) {
  Meta* metaMeta = Meta::tryAllocate(heap, 0, Meta::kHeaderSize, Meta::kElementSize);
  metaMeta->initialize(META_BLOCK_TYPE, nullptr, Meta::kHeaderSize, Meta::kElementSize);
  metaMeta->setHasPointers(true);
  metaMeta->setHasCustomSize(true);
  metaMeta->objectPointerMap().setWord(0, Meta::kPointerMap);
  metaMeta->elementPointerMap().setWord(0, Meta::kElementPointerMap);
  basicRoots_[META_META_ROOT_INDEX] = metaMeta;

  Meta* packageMeta = Meta::tryAllocate(heap, 0, Package::kSize, 0);
  packageMeta->initialize(PACKAGE_BLOCK_TYPE, nullptr, Package::kSize, 0);
  packageMeta->objectPointerMap().setWord(0, Package::kPointerMap);
  basicRoots_[PACKAGE_META_ROOT_INDEX] = packageMeta;

  Meta* stackMeta = Meta::tryAllocate(heap, 0, Stack::kHeaderSize, 1);
  stackMeta->initialize(STACK_BLOCK_TYPE, nullptr, Stack::kHeaderSize, 1);
  stackMeta->setHasPointers(true);
  stackMeta->setHasCustomPointers(true);
  stackMeta->setNeedsRelocation(true);
  basicRoots_[STACK_META_ROOT_INDEX] = stackMeta;

  Meta* functionMeta = Meta::tryAllocate(heap, 0, Function::kHeaderSize, 0);
  functionMeta->initialize(FUNCTION_BLOCK_TYPE, nullptr, Function::kHeaderSize, 0);
  functionMeta->setHasPointers(true);
  functionMeta->setHasCustomSize(true);
  functionMeta->objectPointerMap().setWord(0, Function::kPointerMap);
  basicRoots_[FUNCTION_META_ROOT_INDEX] = functionMeta;

  Meta* classMeta = Meta::tryAllocate(heap, 0, Class::kSize, 0);
  classMeta->initialize(CLASS_BLOCK_TYPE, nullptr, Class::kSize, 0);
  classMeta->setHasPointers(true);
  classMeta->objectPointerMap().setWord(0, Class::kPointerMap);
  basicRoots_[CLASS_META_ROOT_INDEX] = classMeta;

  Meta* i8ArrayMeta = Meta::tryAllocate(heap, 0, I8Array::kHeaderSize, sizeof(i8));
  i8ArrayMeta->initialize(I8_ARRAY_BLOCK_TYPE, nullptr, I8Array::kHeaderSize, sizeof(i8));
  basicRoots_[I8_ARRAY_META_ROOT_INDEX] = i8ArrayMeta;

  Meta* i32ArrayMeta = Meta::tryAllocate(heap, 0, I32Array::kHeaderSize, sizeof(i32));
  i32ArrayMeta->initialize(I32_ARRAY_BLOCK_TYPE, nullptr, I32Array::kHeaderSize, sizeof(i32));
  basicRoots_[I32_ARRAY_META_ROOT_INDEX] = i32ArrayMeta;

  Meta* i64ArrayMeta = Meta::tryAllocate(heap, 0, I64Array::kHeaderSize, sizeof(u64));
  i64ArrayMeta->initialize(I64_ARRAY_BLOCK_TYPE, nullptr,
                           I64Array::kHeaderSize, sizeof(u64));
  basicRoots_[I64_ARRAY_META_ROOT_INDEX] = i64ArrayMeta;

  Meta* blockArrayMeta = Meta::tryAllocate(heap, 0, BlockArray::kHeaderSize, kWordSize);
  blockArrayMeta->initialize(BLOCK_ARRAY_BLOCK_TYPE, nullptr,
                             BlockArray::kHeaderSize, kWordSize);
  blockArrayMeta->setHasPointers(true);
  blockArrayMeta->setHasElementPointers(true);
  blockArrayMeta->objectPointerMap().setWord(0, 0);
  blockArrayMeta->elementPointerMap().setWord(0, 1);
  basicRoots_[BLOCK_ARRAY_META_ROOT_INDEX] = blockArrayMeta;

  Meta* taggedArrayMeta = Meta::tryAllocate(heap, 0, TaggedArray::kHeaderSize, kWordSize);
  taggedArrayMeta->initialize(TAGGED_ARRAY_BLOCK_TYPE, nullptr,
                              TaggedArray::kHeaderSize, kWordSize);
  taggedArrayMeta->setHasPointers(true);
  taggedArrayMeta->setHasCustomPointers(true);
  basicRoots_[TAGGED_ARRAY_META_ROOT_INDEX] = taggedArrayMeta;

  I8Array* emptyI8Array = I8Array::tryAllocate(heap, 0);
  basicRoots_[EMPTY_I8_ARRAY_ROOT_INDEX] = emptyI8Array;

  I32Array* emptyI32Array = I32Array::tryAllocate(heap, 0);
  basicRoots_[EMPTY_I32_ARRAY_ROOT_INDEX] = emptyI32Array;

  I64Array* emptyI64Array = I64Array::tryAllocate(heap, 0);
  basicRoots_[EMPTY_I64_ARRAY_ROOT_INDEX] = emptyI64Array;

  TaggedArray* emptyTaggedArray = TaggedArray::tryAllocate(heap, 0);
  basicRoots_[EMPTY_TAGGED_ARRAY_ROOT_INDEX] = emptyTaggedArray;

  BlockArray* emptyBlockArray = BlockArray::tryAllocate(heap, 0);
  basicRoots_[EMPTY_BLOCK_ARRAY_ROOT_INDEX] = emptyBlockArray;

  initializeBuiltins(heap);

  Type* nullType = Type::tryAllocate(heap, 1);
  nullType->initialize(getBuiltinClass(BUILTIN_NOTHING_CLASS_ID), Type::NULLABLE_FLAG);
  basicRoots_[NULL_TYPE_ROOT_INDEX] = nullType;

  Meta* typeMeta = getBuiltinClass(BUILTIN_TYPE_CLASS_ID)->instanceMeta();
  typeMeta->setType(TYPE_BLOCK_TYPE);
  basicRoots_[TYPE_META_ROOT_INDEX] = typeMeta;

  Meta* stringMeta = getBuiltinClass(BUILTIN_STRING_CLASS_ID)->instanceMeta();
  stringMeta->setType(STRING_BLOCK_TYPE);
  basicRoots_[STRING_META_ROOT_INDEX] = stringMeta;
}

}
}

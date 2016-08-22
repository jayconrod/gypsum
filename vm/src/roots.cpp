// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "roots.h"

#include "array.h"
#include "block.h"
#include "field.h"
#include "function.h"
#include "global.h"
#include "handle.h"
#include "hash-table.h"
#include "name.h"
#include "package.h"
#include "stack.h"
#include "string.h"
#include "thread-bindle.h"
#include "trait.h"
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

  auto packageVersionMeta = new(heap, 0, sizeof(PackageVersion), 0)
      Meta(PACKAGE_VERSION_BLOCK_TYPE);
  packageVersionMeta->hasPointers_ = true;
  packageVersionMeta->objectPointerMap().setWord(0, PackageVersion::kPointerMap);
  basicRoots_[PACKAGE_VERSION_META_ROOT_INDEX] = packageVersionMeta;

  auto packageDependencyMeta = new(heap, 0, sizeof(PackageDependency), 0)
      Meta(PACKAGE_DEPENDENCY_BLOCK_TYPE);
  packageDependencyMeta->hasPointers_ = true;
  packageDependencyMeta->objectPointerMap().setWord(0, PackageDependency::kPointerMap);
  basicRoots_[PACKAGE_DEPENDENCY_META_ROOT_INDEX] = packageDependencyMeta;

  auto stackMeta = new(heap, 0, sizeof(Stack), 1) Meta(STACK_BLOCK_TYPE);
  stackMeta->hasPointers_ = true;
  stackMeta->hasCustomPointers_ = true;
  stackMeta->needsRelocation_ = true;
  stackMeta->hasWordSizeLength_ = true;
  stackMeta->lengthOffset_ = offsetof(Stack, stackSize_);
  basicRoots_[STACK_META_ROOT_INDEX] = stackMeta;

  auto nameMeta = new(heap, 0, sizeof(Name), 0) Meta(NAME_BLOCK_TYPE);
  nameMeta->hasPointers_ = true;
  nameMeta->objectPointerMap().setWord(0, Name::kPointerMap);
  basicRoots_[NAME_META_ROOT_INDEX] = nameMeta;

  auto globalMeta = new(heap, 0, sizeof(Global), 0) Meta(GLOBAL_BLOCK_TYPE);
  globalMeta->hasPointers_ = true;
  globalMeta->hasCustomPointers_ = true;
  basicRoots_[GLOBAL_META_ROOT_INDEX] = globalMeta;

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

  auto traitMeta = new(heap, 0, sizeof(Trait), 0) Meta(TRAIT_BLOCK_TYPE);
  traitMeta->hasPointers_ = true;
  traitMeta->objectPointerMap().setWord(0, Trait::kPointerMap);
  basicRoots_[TRAIT_META_ROOT_INDEX] = traitMeta;

  auto traitTableMeta = new(heap, 0, sizeof(TraitTable), sizeof(TraitTable::Element))
      Meta(TRAIT_TABLE_BLOCK_TYPE);
  traitTableMeta->hasElementPointers_ = true;
  traitTableMeta->lengthOffset_ = offsetof(TraitTable, capacity_);
  traitTableMeta->elementPointerMap().setWord(0, TraitTable::kElementPointerMap);
  basicRoots_[TRAIT_TABLE_META_ROOT_INDEX] = traitTableMeta;

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

  auto emptyTraitTable = new(heap, 1) TraitTable;
  basicRoots_[EMPTY_TRAIT_TABLE_ROOT_INDEX] = emptyTraitTable;

  initializeBuiltins(heap);

  auto labelType = new(heap, 0) Type(Type::LABEL_TYPE);
  basicRoots_[LABEL_TYPE_ROOT_INDEX] = labelType;

  auto anyType = new(heap, 0) Type(Type::ANY_TYPE);
  basicRoots_[ANY_TYPE_ROOT_INDEX] = anyType;

  auto noType = new(heap, 0) Type(Type::NO_TYPE);
  basicRoots_[NO_TYPE_ROOT_INDEX] = noType;

  auto nullType = new(heap, 1) Type(getBuiltinClass(BUILTIN_NOTHING_CLASS_ID),
                                    Type::NULLABLE_FLAG);
  basicRoots_[NULL_TYPE_ROOT_INDEX] = nullType;

  auto packageClass = getBuiltinClass(BUILTIN_PACKAGE_CLASS_ID);
  auto packageMethods = packageClass->methods();
  auto packageMeta = new(heap, packageMethods->length(), sizeof(Package), 0)
      Meta(PACKAGE_BLOCK_TYPE);
  packageMeta->hasPointers_ = true;
  packageMeta->objectPointerMap().setWord(0, Package::kPointerMap);
  packageMeta->setClass(packageClass);
  for (length_t i = 0; i < packageMethods->length(); i++) {
    auto method = packageMethods->get(i);
    packageMeta->setData(i, method);
  }
  packageClass->setInstanceMeta(packageMeta);
  basicRoots_[PACKAGE_META_ROOT_INDEX] = packageMeta;
  builtinMetas_[builtinIdToIndex(BUILTIN_PACKAGE_CLASS_ID)] = packageMeta;

  Meta* typeMeta = getBuiltinClass(BUILTIN_TYPE_CLASS_ID)->instanceMeta();
  typeMeta->blockType_ = TYPE_BLOCK_TYPE;
  basicRoots_[TYPE_META_ROOT_INDEX] = typeMeta;

  Meta* stringMeta = getBuiltinClass(BUILTIN_STRING_CLASS_ID)->instanceMeta();
  stringMeta->blockType_ = STRING_BLOCK_TYPE;
  basicRoots_[STRING_META_ROOT_INDEX] = stringMeta;

  auto emptyString = new(heap, 0) String(nullptr);
  basicRoots_[EMPTY_STRING_ROOT_INDEX] = emptyString;

  auto trueString = String::rawFromUtf8CString(heap, "true");
  basicRoots_[TRUE_STRING_ROOT_INDEX] = trueString;

  auto falseString = String::rawFromUtf8CString(heap, "false");
  basicRoots_[FALSE_STRING_ROOT_INDEX] = falseString;

  typedef BlockHashMapTable<String, Block> DefaultBlockHashMapTable;
  auto blockHashMapTableMeta = new(heap, 0, sizeof(DefaultBlockHashMapTable),
                                   sizeof(DefaultBlockHashMapTable::Element))
      Meta(DefaultBlockHashMapTable::kBlockType);
  blockHashMapTableMeta->hasElementPointers_ = true;
  blockHashMapTableMeta->lengthOffset_ = offsetof(DefaultBlockHashMapTable, capacity_);
  blockHashMapTableMeta->elementPointerMap().setWord(
      0, DefaultBlockHashMapTable::kElementPointerMap);
  basicRoots_[BLOCK_HASH_MAP_TABLE_META_ROOT_INDEX] = blockHashMapTableMeta;

  typedef BlockHashMap<String, Block> DefaultBlockHashMap;
  auto blockHashMapMeta = new(heap, 0, sizeof(DefaultBlockHashMap), 0)
      Meta(DefaultBlockHashMapTable::kBlockType);
  blockHashMapMeta->hasPointers_ = true;
  blockHashMapMeta->objectPointerMap().setWord(0, DefaultBlockHashMap::kPointerMap);
  basicRoots_[BLOCK_HASH_MAP_META_ROOT_INDEX] = blockHashMapMeta;

  auto threadBindleMeta = new(heap, 0, sizeof(ThreadBindle), 0) Meta(THREAD_BINDLE_BLOCK_TYPE);
  threadBindleMeta->hasPointers_ = true;
  threadBindleMeta->objectPointerMap().setWord(0, ThreadBindle::kPointerMap);
  basicRoots_[THREAD_BINDLE_META_ROOT_INDEX] = threadBindleMeta;
}


Meta* Roots::getMetaForBlockType(int type) {
  switch (type) {
    case META_BLOCK_TYPE: return metaMeta();
    case FREE_BLOCK_TYPE: return freeMeta();
    case PACKAGE_BLOCK_TYPE: return packageMeta();
    case NAME_BLOCK_TYPE: return nameMeta();
    case PACKAGE_VERSION_BLOCK_TYPE: return packageVersionMeta();
    case PACKAGE_DEPENDENCY_BLOCK_TYPE: return packageDependencyMeta();
    case STACK_BLOCK_TYPE: return stackMeta();
    case GLOBAL_BLOCK_TYPE: return globalMeta();
    case FUNCTION_BLOCK_TYPE: return functionMeta();
    case CLASS_BLOCK_TYPE: return classMeta();
    case FIELD_BLOCK_TYPE: return fieldMeta();
    case TRAIT_BLOCK_TYPE: return traitMeta();
    case TRAIT_TABLE_BLOCK_TYPE: return traitTableMeta();
    case TYPE_PARAMETER_BLOCK_TYPE: return typeParameterMeta();
    case I8_ARRAY_BLOCK_TYPE: return i8ArrayMeta();
    case I32_ARRAY_BLOCK_TYPE: return i32ArrayMeta();
    case I64_ARRAY_BLOCK_TYPE: return i64ArrayMeta();
    case BLOCK_ARRAY_BLOCK_TYPE: return blockArrayMeta();
    case TAGGED_ARRAY_BLOCK_TYPE: return taggedArrayMeta();
    case BLOCK_HASH_MAP_TABLE_BLOCK_TYPE: return blockHashMapTableMeta();
    case BLOCK_HASH_MAP_BLOCK_TYPE: return blockHashMapMeta();
    case TYPE_BLOCK_TYPE: return typeMeta();
    case STRING_BLOCK_TYPE: return stringMeta();
    case THREAD_BINDLE_BLOCK_TYPE: return threadBindleMeta();
    default:
      UNREACHABLE();
      return nullptr;
  }
}


bool Roots::isValidBuiltinClassId(BuiltinId id) const {
  auto index = builtinIdToIndex(id);
  return 0 <= index && index < builtinClasses_.size();
}


bool Roots::isValidBuiltinTraitId(BuiltinId id) const {
  return false;
}


bool Roots::isValidBuiltinFunctionId(BuiltinId id) const {
  auto index = builtinIdToIndex(id);
  return 0 <= index && index < builtinFunctions_.size();
}


Class* Roots::getBuiltinClass(BuiltinId id) const {
  ASSERT(isValidBuiltinClassId(id));
  auto index = builtinIdToIndex(id);
  ASSERT(index < builtinClasses_.size());
  return builtinClasses_[index];
}


Trait* Roots::getBuiltinTrait(BuiltinId id) const {
  ASSERT(isValidBuiltinTraitId(id));
  UNREACHABLE();
  return nullptr;
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


Name* Roots::getBuiltinName(BuiltinId id) const {
  auto index = builtinIdToIndex(id);
  ASSERT(index < builtinNames_.size());
  return builtinNames_[index];
}


Function* Roots::getBuiltinFunction(BuiltinId id) const {
  ASSERT(isValidBuiltinFunctionId(id));
  auto index = builtinIdToIndex(id);
  ASSERT(index < builtinFunctions_.size());
  return builtinFunctions_[index];
}

}
}

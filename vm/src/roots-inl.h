// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef roots_inl_h
#define roots_inl_h

#include "block.h"
#include "roots.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

Meta* Roots::getMetaForBlockType(int type) {
  switch (type) {
    case META_BLOCK_TYPE: return metaMeta();
    case PACKAGE_BLOCK_TYPE: return packageMeta();
    case STACK_BLOCK_TYPE: return stackMeta();
    case FUNCTION_BLOCK_TYPE: return functionMeta();
    case CLASS_BLOCK_TYPE: return classMeta();
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


Function* Roots::getBuiltinFunction(BuiltinId id) const {
  auto index = builtinIdToIndex(id);
  ASSERT(index < builtinFunctions_.size());
  return builtinFunctions_[index];
}


template <class Callback>
void Roots::visit(Callback callback) {
  for (int i = 0; i < BASIC_ROOT_COUNT; i++) {
    callback(reinterpret_cast<Block**>(&basicRoots_[i]));
  }
  for (size_t i = 0; i < builtinClasses_.size(); i++) {
    callback(reinterpret_cast<Block**>(&builtinClasses_[i]));
  }
  for (size_t i = 0; i < builtinMetas_.size(); i++) {
    callback(reinterpret_cast<Block**>(&builtinMetas_[i]));
  }
  for (size_t i = 0; i < builtinTypes_.size(); i++) {
    callback(reinterpret_cast<Block**>(&builtinTypes_[i]));
  }
  for (size_t i = 0; i < builtinFunctions_.size(); i++) {
    callback(reinterpret_cast<Block**>(&builtinFunctions_[i]));
  }
}

}
}

#endif

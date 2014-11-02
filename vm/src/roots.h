// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef roots_h
#define roots_h

#include <vector>
#include "builtins.h"

namespace codeswitch {
namespace internal {

class Block;
template <class T>
class BlockArray;
class Class;
class Function;
class Heap;
class I8Array;
class I32Array;
class I64Array;
class Meta;
template <class T>
class TaggedArray;
class Type;


class Roots {
 public:
  Roots() {}
  Roots(const Roots&) = delete;

  void initialize(Heap* heap);

  #define BASIC_ROOT_LIST(F)                                          \
    F(Meta, metaMeta, META_META)                                      \
    F(Meta, freeMeta, FREE_META)                                      \
    F(Meta, packageMeta, PACKAGE_META)                                \
    F(Meta, stackMeta, STACK_META)                                    \
    F(Meta, functionMeta, FUNCTION_META)                              \
    F(Meta, classMeta, CLASS_META)                                    \
    F(Meta, fieldMeta, FIELD_META)                                    \
    F(Meta, typeParameterMeta, TYPE_PARAMETER_META)                   \
    F(Meta, i8ArrayMeta, I8_ARRAY_META)                               \
    F(Meta, i32ArrayMeta, I32_ARRAY_META)                             \
    F(Meta, i64ArrayMeta, I64_ARRAY_META)                             \
    F(Meta, blockArrayMeta, BLOCK_ARRAY_META)                         \
    F(Meta, taggedArrayMeta, TAGGED_ARRAY_META)                       \
    F(I8Array, emptyi8Array, EMPTY_I8_ARRAY)                          \
    F(I32Array, emptyi32Array, EMPTY_I32_ARRAY)                       \
    F(I64Array, emptyi64Array, EMPTY_I64_ARRAY)                       \
    F(TaggedArray<Block>, emptyTaggedArray, EMPTY_TAGGED_ARRAY)       \
    F(BlockArray<Block>, emptyBlockArray, EMPTY_BLOCK_ARRAY)          \
    F(Type, nullType, NULL_TYPE)                                      \
    F(Meta, typeMeta, TYPE_META)                                      \
    F(Meta, stringMeta, STRING_META)                                  \

  #define DEFINE_BASIC_GETTER(type, name, NAME)                       \
  type* name() const {                                                \
    return reinterpret_cast<type*>(basicRoots_[NAME##_ROOT_INDEX]);   \
  }
  BASIC_ROOT_LIST(DEFINE_BASIC_GETTER)
  #undef DEFINE_BASIC_GETTER

  Meta* getMetaForBlockType(int type);

  Class* getBuiltinClass(BuiltinId id) const;
  Meta* getBuiltinMeta(BuiltinId id) const;
  Type* getBuiltinType(BuiltinId id) const;
  Function* getBuiltinFunction(BuiltinId id) const;

  template <class Callback>
  void visitPointers(Callback callback);

 private:
  enum BasicRootIndex {
    #define BASIC_ROOT_INDEX(type, name, NAME) NAME##_ROOT_INDEX,
    BASIC_ROOT_LIST(BASIC_ROOT_INDEX)
    #undef BASIC_ROOT_INDEX
    BASIC_ROOT_COUNT,
    LAST_BASIC_ROOT_INDEX = BASIC_ROOT_COUNT - 1
  };

  // Defined in generated roots-builtins.cpp.
  void initializeBuiltins(Heap* heap);

  Block* basicRoots_[BASIC_ROOT_COUNT];
  std::vector<Class*> builtinClasses_;
  std::vector<Meta*> builtinMetas_;
  std::vector<Type*> builtinTypes_;
  std::vector<Function*> builtinFunctions_;

  #undef BASIC_META_LIST
};


template <class Callback>
void Roots::visitPointers(Callback callback) {
  for (int i = 0; i < BASIC_ROOT_COUNT; i++) {
    callback(reinterpret_cast<Block**>(&basicRoots_[i]));
  }
  for (auto& p : builtinClasses_) {
    callback(reinterpret_cast<Block**>(&p));
  }
  for (auto& p : builtinMetas_) {
    callback(reinterpret_cast<Block**>(&p));
  }
  for (auto& p : builtinTypes_) {
    callback(reinterpret_cast<Block**>(&p));
  }
  for (auto& p : builtinFunctions_) {
    callback(reinterpret_cast<Block**>(&p));
  }
}

}
}

#endif

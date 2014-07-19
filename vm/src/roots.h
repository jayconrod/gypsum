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
class BlockArray;
class Class;
class Function;
class Heap;
class I8Array;
class I32Array;
class I64Array;
class Meta;
class TaggedArray;
class Type;


class Roots {
 public:
  Roots() {}
  Roots(const Roots&) = delete;

  void initialize(Heap* heap);

  #define BASIC_ROOT_LIST(F)                                          \
    F(Meta, metaMeta, META_META)                                      \
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
    F(TaggedArray, emptyTaggedArray, EMPTY_TAGGED_ARRAY)              \
    F(BlockArray, emptyBlockArray, EMPTY_BLOCK_ARRAY)                 \
    F(Type, nullType, NULL_TYPE)                                      \
    F(Meta, typeMeta, TYPE_META)                                      \
    F(Meta, stringMeta, STRING_META)                                  \

  #define DEFINE_BASIC_GETTER(type, name, NAME)                       \
  type* name() const {                                                \
    return reinterpret_cast<type*>(basicRoots_[NAME##_ROOT_INDEX]);   \
  }
  BASIC_ROOT_LIST(DEFINE_BASIC_GETTER)
  #undef DEFINE_BASIC_GETTER

  inline Meta* getMetaForBlockType(int type);

  inline Class* getBuiltinClass(BuiltinId id) const;
  inline Meta* getBuiltinMeta(BuiltinId id) const;
  inline Type* getBuiltinType(BuiltinId id) const;
  inline Function* getBuiltinFunction(BuiltinId id) const;

  template <class Callback>
  void visit(Callback callback);

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

}
}

#endif

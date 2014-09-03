// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef type_h
#define type_h

#include <vector>
#include "bytecode.h"
#include "class.h"
#include "handle.h"
#include "object.h"
#include "tagged.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Roots;
class TypeParameter;

class Type: public Object {
 public:
  enum Form {
    // Primitive forms
    UNIT_TYPE,
    BOOLEAN_TYPE,
    I8_TYPE,
    I16_TYPE,
    I32_TYPE,
    I64_TYPE,
    F32_TYPE,
    F64_TYPE,

    // Object forms
    CLASS_TYPE,
    VARIABLE_TYPE,

    // Fake forms
    FIRST_PRIMITIVE_TYPE = UNIT_TYPE,
    LAST_PRIMITIVE_TYPE = F64_TYPE,
    FIRST_OBJECT_TYPE = CLASS_TYPE,
    LAST_OBJECT_TYPE = VARIABLE_TYPE,
    LAST_TYPE = VARIABLE_TYPE
  };

  enum Flags {
    NO_FLAGS = 0,
    NULLABLE_FLAG = 1 << 0,
    LAST_FLAG = NULLABLE_FLAG
  };

  static inline Type* tryAllocate(Heap* heap, word_t length);
  static inline Local<Type> allocate(Heap* heap, word_t length);
  inline void initialize(Form primitive, Flags flags = NO_FLAGS);
  inline void initialize(Class* clas, Flags flags = NO_FLAGS);
  inline void initialize(TypeParameter* param, Flags flags = NO_FLAGS);

  static inline Type* primitiveTypeFromForm(Roots* roots, Form form);
  static inline Type* unitType(Roots* roots);
  static inline Type* booleanType(Roots* roots);
  static inline Type* intTypeFromWidth(Roots* roots, Width width);
  static inline Type* i8Type(Roots* roots);
  static inline Type* i16Type(Roots* roots);
  static inline Type* i32Type(Roots* roots);
  static inline Type* i64Type(Roots* roots);
  static inline Type* floatTypeFromWidth(Roots* roots, Width width);
  static inline Type* f32Type(Roots* roots);
  static inline Type* f64Type(Roots* roots);
  static inline Type* wordType(Roots* roots);
  static inline Type* rootClassType(Roots* roots);
  static inline Type* nullType(Roots* roots);

  DEFINE_CAST(Type)

  inline word_t length();

  DEFINE_INL_ACCESSORS(word_t, bitField, setBitField, kBitFieldOffset)
  DEFINE_INL_BIT_ACCESSORS(Form, form, setForm, kBitFieldOffset, kFormWidth, kFormShift)
  DEFINE_INL_BIT_ACCESSORS(Flags, flags, setFlags, kBitFieldOffset, kFlagsWidth, kFlagsShift)
  inline bool isPrimitive();
  inline Form asPrimitive();
  inline bool isClass();
  inline Class* asClass();
  inline bool isVariable();
  inline TypeParameter* asVariable();
  inline bool isRootClass();
  inline bool isObject();
  inline bool isBoolean();
  inline bool isI8();
  inline bool isI16();
  inline bool isI32();
  inline bool isI64();
  inline bool isF32();
  inline bool isF64();
  inline bool isNullable();

  inline word_t typeSize();
  inline word_t alignment();

  bool isSubtypeOf(Type* other);
  bool equals(Type* other);
  Type* substitute(const std::vector<std::pair<TypeParameter*, Type*>>& bindings);

  static const int kLengthOffset = kBlockHeaderSize;
  static const int kBitFieldOffset = kLengthOffset + kWordSize;
  static const int kHeaderSize = kBitFieldOffset + kWordSize;
  static const int kClassOffset = kHeaderSize;
  static const int kPointerMap = 0;

  static const int kFormShift = 0;
  static const int kFormWidth = 4;
  static const word_t kFormMask = (1 << kFormWidth) - 1;
  static const int kFlagsShift = kFormShift + kFormWidth;
  static const int kFlagsWidth = 1;
  static const word_t kFlagsMask = (LAST_FLAG << 1) - 1;
};

}
}

#endif

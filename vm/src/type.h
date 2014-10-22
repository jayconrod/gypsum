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
  static const BlockType kBlockType = TYPE_BLOCK_TYPE;

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

  static word_t sizeForLength(length_t length);
  void* operator new (size_t, Heap* heap, length_t length);
  void* operator new (size_t, void* place, length_t length);
  explicit Type(Form primitive, Flags flags = NO_FLAGS);
  explicit Type(Class* clas, Flags flags = NO_FLAGS);
  explicit Type(TypeParameter* param, Flags flags = NO_FLAGS);
  static Local<Type> create(Heap* heap, Form primitive, Flags flags = NO_FLAGS);
  static Local<Type> create(Heap* heap, const Handle<Class>& clas, Flags fags = NO_FLAGS);
  static Local<Type> create(Heap* heap,
                            const Handle<TypeParameter>& param,
                            Flags flags = NO_FLAGS);

  static Type* primitiveTypeFromForm(Roots* roots, Form form);
  static Type* unitType(Roots* roots);
  static Type* booleanType(Roots* roots);
  static Type* intTypeFromWidth(Roots* roots, Width width);
  static Type* i8Type(Roots* roots);
  static Type* i16Type(Roots* roots);
  static Type* i32Type(Roots* roots);
  static Type* i64Type(Roots* roots);
  static Type* floatTypeFromWidth(Roots* roots, Width width);
  static Type* f32Type(Roots* roots);
  static Type* f64Type(Roots* roots);
  static Type* wordType(Roots* roots);
  static Type* rootClassType(Roots* roots);
  static Type* nullType(Roots* roots);

  length_t length() const { return elementsLength(); }

  Form form() const { return form_; }
  Flags flags() const { return flags_; }
  bool isPrimitive() const;
  Form asPrimitive() const;
  bool isClass() const;
  Class* asClass() const;
  bool isVariable() const;
  TypeParameter* asVariable() const;
  bool isRootClass() const;
  bool isObject() const;
  bool isBoolean() const;
  bool isI8() const;
  bool isI16() const;
  bool isI32() const;
  bool isI64() const;
  bool isF32() const;
  bool isF64() const;
  bool isNullable() const;

  word_t typeSize() const;
  word_t alignment() const;

  bool isSubtypeOf(Type* other) const;
  bool equals(Type* other) const;
  static Local<Type> substitute(const Local<Type>& type,
                                const std::vector<std::pair<Local<TypeParameter>,
                                                            Local<Type>>>& bindings);

 private:
  friend class Roots;
  static const word_t kPointerMap = 0;

  length_t length_;
  Form form_ : 4;
  Flags flags_ : 28;
  Block* elements_[0];
};

}
}

#endif

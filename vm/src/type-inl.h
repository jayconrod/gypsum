// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef type_inl_h
#define type_inl_h

#include "type.h"

#include "heap.h"
#include "roots-inl.h"
#include "type-parameter.h"

namespace codeswitch {
namespace internal {

Type* Type::tryAllocate(Heap* heap, word_t length) {
  word_t size = kHeaderSize + length * kWordSize;
  Type* type = reinterpret_cast<Type*>(heap->allocate(size));
  if (type == nullptr)
    return nullptr;

  type->setMeta(TYPE_BLOCK_TYPE);
  mem<word_t>(type, kLengthOffset) = length;
  return type;
}


Local<Type> Type::allocate(Heap* heap, word_t length) {
  DEFINE_ALLOCATION(heap, Type, tryAllocate(heap, length))
}


void Type::initialize(Form primitive, Flags flags) {
  ASSERT(length() == 0);
  ASSERT(FIRST_PRIMITIVE_TYPE <= primitive && primitive <= LAST_PRIMITIVE_TYPE);
  setForm(primitive);
  setFlags(flags);
}


void Type::initialize(Class* clas, Flags flags) {
  ASSERT(length() == 1);
  setForm(CLASS_TYPE);
  setFlags(flags);
  // TODO: there has got to be a cleaner way to do this this.
  Class** slot = &mem<Class*>(this, kClassOffset);
  *slot = clas;
  Heap::recordWrite(slot, clas);
}


void Type::initialize(TypeParameter* param, Flags flags) {
  ASSERT(length() == 1);
  setForm(VARIABLE_TYPE);
  setFlags(flags);
  // TODO: there has got to be a cleaner way to do this
  TypeParameter** slot = &mem<TypeParameter*>(this, kClassOffset);
  *slot = param;
  Heap::recordWrite(slot, param);
}


Type* Type::primitiveTypeFromForm(Roots* roots, Form form) {
  switch (form) {
    case UNIT_TYPE: return unitType(roots);
    case BOOLEAN_TYPE: return booleanType(roots);
    case I8_TYPE: return i8Type(roots);
    case I16_TYPE: return i16Type(roots);
    case I32_TYPE: return i32Type(roots);
    case I64_TYPE: return i64Type(roots);
    case F32_TYPE: return f32Type(roots);
    case F64_TYPE: return f64Type(roots);
    default:
      UNREACHABLE();
      return nullptr;
  }
}


Type* Type::unitType(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_UNIT_TYPE_ID);
}


Type* Type::booleanType(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_BOOLEAN_TYPE_ID);
}


Type* Type::intTypeFromWidth(Roots* roots, Width width) {
  switch (width) {
    case W8:  return roots->getBuiltinType(BUILTIN_I8_TYPE_ID);
    case W16: return roots->getBuiltinType(BUILTIN_I16_TYPE_ID);
    case W32: return roots->getBuiltinType(BUILTIN_I32_TYPE_ID);
    case W64: return roots->getBuiltinType(BUILTIN_I64_TYPE_ID);
    default:
      UNREACHABLE();
      return NULL;
  }
}


Type* Type::i8Type(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_I8_TYPE_ID);
}


Type* Type::i16Type(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_I16_TYPE_ID);
}


Type* Type::i32Type(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_I32_TYPE_ID);
}


Type* Type::i64Type(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_I64_TYPE_ID);
}


Type* Type::floatTypeFromWidth(Roots* roots, Width width) {
  switch (width) {
    case W32: return roots->getBuiltinType(BUILTIN_F32_TYPE_ID);
    case W64: return roots->getBuiltinType(BUILTIN_F64_TYPE_ID);
    default:
      UNREACHABLE();
  }
}


Type* Type::f32Type(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_F32_TYPE_ID);
}


Type* Type::f64Type(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_F64_TYPE_ID);
}


Type* Type::wordType(Roots* roots) {
  return intTypeFromWidth(roots, WORD);
}


Type* Type::rootClassType(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_ROOT_CLASS_ID);
}


Type* Type::nullType(Roots* roots) {
  return roots->nullType();
}


word_t Type::length() {
  return mem<word_t>(this, kLengthOffset);
}


bool Type::isPrimitive() {
  return FIRST_PRIMITIVE_TYPE <= form() && form() <= LAST_PRIMITIVE_TYPE;
}


Type::Form Type::asPrimitive() {
  ASSERT(isPrimitive());
  return form();
}


bool Type::isClass() {
  return form() == CLASS_TYPE;
}


Class* Type::asClass() {
  ASSERT(isClass() && length() > 0);
  return mem<Class*>(this, kClassOffset);
}


bool Type::isVariable() {
  return form() == VARIABLE_TYPE;
}


TypeParameter* Type::asVariable() {
  ASSERT(isVariable() && length() > 0);
  return mem<TypeParameter*>(this, kClassOffset);
}


bool Type::isRootClass() {
  return isClass() && asClass() == getVM()->roots()->getBuiltinClass(BUILTIN_ROOT_CLASS_ID);
}


bool Type::isObject() {
  return FIRST_OBJECT_TYPE <= form() && form() <= LAST_OBJECT_TYPE;
}


bool Type::isBoolean() {
  return form() == BOOLEAN_TYPE;
}


bool Type::isI8() {
  return form() == I8_TYPE;
}


bool Type::isI16() {
  return form() == I16_TYPE;
}


bool Type::isI32() {
  return form() == I32_TYPE;
}


bool Type::isI64() {
  return form() == I64_TYPE;
}


bool Type::isF32() {
  return form() == F32_TYPE;
}


bool Type::isF64() {
  return form() == F64_TYPE;
}


bool Type::isNullable() {
  return (flags() & NULLABLE_FLAG) != 0;
}


word_t Type::typeSize() {
  if (isPrimitive()) {
    switch (asPrimitive()) {
      case BOOLEAN_TYPE: return 1;
      case I8_TYPE: return 1;
      case I16_TYPE: return 2;
      case I32_TYPE: return 4;
      case I64_TYPE: return 8;
      case F32_TYPE: return 4;
      case F64_TYPE: return 8;
     default:
        UNREACHABLE();
        return 0;
    }
  } else {
    return kWordSize;
  }
}


word_t Type::alignment() {
  return typeSize();
}

}
}

#endif

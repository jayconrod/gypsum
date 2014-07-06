// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef type_inl_h
#define type_inl_h

#include "type.h"

#include "heap-inl.h"
#include "roots-inl.h"

namespace codeswitch {
namespace internal {

Type* Type::tryAllocate(Heap* heap, word_t length) {
  word_t size = kHeaderSize + length * kWordSize;
  Type* type = reinterpret_cast<Type*>(heap->allocateRaw(size));
  if (type == nullptr)
    return nullptr;

  type->setMeta(TYPE_BLOCK_TYPE);
  mem<word_t>(type, kLengthOffset) = length;
  return type;
}


Handle<Type> Type::allocate(Heap* heap, word_t length) {
  DEFINE_ALLOCATION(heap, Type, tryAllocate(heap, length))
}


void Type::initialize(PrimitiveType primitive, Flags flags) {
  ASSERT(length() == 0);
  word_t bits = 0;
  bits = bitInsert(bits, static_cast<word_t>(flags), kFlagsWidth, kFlagsShift);
  bits = bitInsert(bits, static_cast<word_t>(primitive),
                   kPrimitiveTypeWidth, kPrimitiveTypeShift);
  setBitField(bits);
}


void Type::initialize(Class* clas, Flags flags) {
  ASSERT(length() == 1);
  word_t bits = 0;
  bits = bitInsert(bits, static_cast<word_t>(flags), kFlagsWidth, kFlagsShift);
  bits = bitInsert(bits, static_cast<word_t>(NON_PRIMITIVE_TYPE),
                   kPrimitiveTypeWidth, kPrimitiveTypeShift);
  setBitField(bits);
  Class** slot = &mem<Class*>(this, kClassOffset);
  *slot = clas;
  Heap::recordWrite(slot, clas);
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
  auto primitive = static_cast<PrimitiveType>(
      bitExtract(bitField(), kPrimitiveTypeWidth, kPrimitiveTypeShift));
  return primitive != NON_PRIMITIVE_TYPE;
}


Type::PrimitiveType Type::asPrimitive() {
  auto primitive = static_cast<PrimitiveType>(
      bitExtract(bitField(), kPrimitiveTypeWidth, kPrimitiveTypeShift));
  ASSERT(primitive != NON_PRIMITIVE_TYPE);
  return primitive;
}


bool Type::isClass() {
  return !isPrimitive();
}


Class* Type::asClass() {
  ASSERT(isClass() && length() > 0);
  return mem<Class*>(this, kClassOffset);
}


bool Type::isRootClass() {
  return isClass() && asClass() == getVM()->roots()->getBuiltinClass(BUILTIN_ROOT_CLASS_ID);
}


bool Type::isObject() {
  return isClass();
}


bool Type::isBoolean() {
  return isPrimitive() && asPrimitive() == BOOLEAN_TYPE;
}


bool Type::isI8() {
  return isPrimitive() && asPrimitive() == I8_TYPE;
}


bool Type::isI16() {
  return isPrimitive() && asPrimitive() == I16_TYPE;
}


bool Type::isI32() {
  return isPrimitive() && asPrimitive() == I32_TYPE;
}


bool Type::isI64() {
  return isPrimitive() && asPrimitive() == I64_TYPE;
}


bool Type::isF32() {
  return isPrimitive() && asPrimitive() == F32_TYPE;
}


bool Type::isF64() {
  return isPrimitive() && asPrimitive() == F64_TYPE;
}


Type::Flags Type::flags() {
  return static_cast<Flags>(bitExtract(bitField(), kFlagsWidth, kFlagsShift));
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

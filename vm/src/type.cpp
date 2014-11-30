// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "type.h"

#include "block.h"
#include "gc.h"
#include "handle.h"
#include "roots.h"
#include "type-parameter.h"

using namespace std;

namespace codeswitch {
namespace internal {

word_t Type::sizeForLength(length_t length) {
  ASSERT(length <= kMaxLength);
  return align(sizeof(Type), kWordSize) + length * kWordSize;
}


void* Type::operator new (size_t, Heap* heap, length_t length) {
  ASSERT(length <= kMaxLength);
  auto size = sizeForLength(length);
  auto type = reinterpret_cast<Type*>(heap->allocate(size));
  type->length_ = length;
  return type;
}


void* Type::operator new (size_t, void* place, length_t length) {
  ASSERT(length <= kMaxLength);
  auto ty = reinterpret_cast<Type*>(place);
  ty->length_ = length;
  return ty;
}


Type::Type(Form primitive, Flags flags)
    : Object(TYPE_BLOCK_TYPE),
      form_(primitive),
      flags_(flags) {
  ASSERT(length_ == 0);
}


Type::Type(Class* clas, Flags flags)
    : Object(TYPE_BLOCK_TYPE),
      form_(CLASS_TYPE),
      flags_(flags) {
  ASSERT(length_ == 1);
  // The class may not be initialized yet, so we can't check its parameter count.
  elements_[0].set(this, clas);
}


Type::Type(Class* clas, const vector<Local<Type>>& typeArgs, Flags flags)
    : Object(TYPE_BLOCK_TYPE),
      form_(CLASS_TYPE),
      flags_(flags) {
  ASSERT(length_ == 1 + typeArgs.size());
  // The class may not be initialized yet, so we can't check its parameter count.
  elements_[0].set(this, clas);
  for (length_t i = 0; i < typeArgs.size(); i++) {
    elements_[i + 1].set(this, *typeArgs[i]);
  }
}


Type::Type(TypeParameter* param, Flags flags)
    : Object(TYPE_BLOCK_TYPE),
      form_(VARIABLE_TYPE),
      flags_(flags) {
  ASSERT(length_ == 1);
  elements_[0].set(this, param);
}


Local<Type> Type::create(Heap* heap, Form primitive, Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(new(heap, 0) Type(primitive, flags)));
}


Local<Type> Type::create(Heap* heap, const Handle<Class>& clas, Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(new(heap, 1) Type(*clas, flags)));
}


Local<Type> Type::create(Heap* heap,
                         const Handle<Class>& clas,
                         const vector<Local<Type>>& typeArgs,
                         Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(
      new(heap, 1 + typeArgs.size()) Type(*clas, typeArgs, flags)));
}


Local<Type> Type::create(Heap* heap, const Handle<TypeParameter>& param, Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(new(heap, 1) Type(*param, flags)));
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
      return nullptr;
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


Type* Type::nothingType(Roots* roots) {
  return roots->getBuiltinType(BUILTIN_NOTHING_CLASS_ID);
}


Type* Type::nullType(Roots* roots) {
  return roots->nullType();
}


length_t Type::typeArgumentCount() const {
  ASSERT(isClass());
  return length() - 1;
}


Type* Type::typeArgument(length_t index) const {
  ASSERT(isClass());
  auto typeArgIndex = index + 1;
  ASSERT(typeArgIndex < length());
  return block_cast<Type>(elements_[typeArgIndex].get());
}


Type::BindingList Type::getTypeArgumentBindings() const {
  ASSERT(isClass());
  Local<Class> clas(asClass());
  ASSERT(typeArgumentCount() == clas->typeParameterCount());
  BindingList bindings;
  auto count = typeArgumentCount();
  bindings.reserve(count);
  for (length_t i = 0; i < count; i++) {
    Binding binding(handle(clas->typeParameter(i)), handle(typeArgument(i)));
    bindings.push_back(binding);
  }
  return bindings;
}


bool Type::isPrimitive() const {
  return FIRST_PRIMITIVE_TYPE <= form() && form() <= LAST_PRIMITIVE_TYPE;
}


Type::Form Type::asPrimitive()const  {
  ASSERT(isPrimitive());
  return form();
}


bool Type::isClass() const {
  return form() == CLASS_TYPE;
}


Class* Type::asClass() const {
  ASSERT(isClass() && length() > 0);
  return block_cast<Class>(elements_[0].get());
}


bool Type::isVariable() const {
  return form() == VARIABLE_TYPE;
}


TypeParameter* Type::asVariable() const {
  ASSERT(isVariable() && length() > 0);
  return block_cast<TypeParameter>(elements_[0].get());
}


Class* Type::effectiveClass() const {
  ASSERT(isClass() || isVariable());
  auto ty = this;
  while (!ty->isClass()) {
    ty = ty->asVariable()->upperBound();
  }
  return ty->asClass();
}


bool Type::isRootClass() const {
  return isClass() && asClass() == getVM()->roots()->getBuiltinClass(BUILTIN_ROOT_CLASS_ID);
}


bool Type::isObject() const {
  return FIRST_OBJECT_TYPE <= form() && form() <= LAST_OBJECT_TYPE;
}


bool Type::isBoolean() const {
  return form() == BOOLEAN_TYPE;
}


bool Type::isI8() const {
  return form() == I8_TYPE;
}


bool Type::isI16() const {
  return form() == I16_TYPE;
}


bool Type::isI32() const {
  return form() == I32_TYPE;
}


bool Type::isI64() const {
  return form() == I64_TYPE;
}


bool Type::isF32() const {
  return form() == F32_TYPE;
}


bool Type::isF64() const {
  return form() == F64_TYPE;
}


bool Type::isNullable() const {
  return (flags() & NULLABLE_FLAG) != 0;
}


word_t Type::typeSize() const {
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


word_t Type::alignment() const {
  return typeSize();
}


bool Type::isSubtypeOf(Local<Type> left, Local<Type> right) {
  if (left->equals(*right))
    return true;
  if (left->isPrimitive() || right->isPrimitive())
    return false;
  if (left->isVariable() &&
      right->isVariable() &&
      left->asVariable()->hasCommonBound(right->asVariable())) {
    return true;
  }

  ASSERT(left->isObject() && right->isObject());
  while (left->isVariable()) {
    left = handle(left->asVariable()->upperBound());
  }
  while (right->isVariable()) {
    right = handle(right->asVariable()->lowerBound());
  }
  ASSERT(left->isClass() && right->isClass());
  if (left->isNullable() && !right->isNullable())
    return false;
  auto leftClass = handle(left->asClass());
  auto rightClass = handle(right->asClass());

  if (*leftClass == left->getVM()->roots()->getBuiltinClass(BUILTIN_NOTHING_CLASS_ID))
    return true;

  if (!leftClass->isSubclassOf(*rightClass))
    return false;

  auto leftEquiv = substituteForBaseClass(left, rightClass);
  ASSERT(leftEquiv->typeArgumentCount() == right->typeArgumentCount());
  for (length_t i = 0; i < leftEquiv->typeArgumentCount(); i++) {
    if (!leftEquiv->typeArgument(i)->equals(right->typeArgument(i)))
      return false;
  }
  return true;
}


bool Type::equals(Type* other) const {
  if (form() != other->form() || flags() != other->flags())
    return false;
  if (isPrimitive()) {
    return true;
  } else if (isClass()) {
    if (asClass() != other->asClass())
      return false;
    ASSERT(typeArgumentCount() == other->typeArgumentCount());
    for (length_t i = 0; i < typeArgumentCount(); i++) {
      if (!typeArgument(i)->equals(other->typeArgument(i)))
        return false;
    }
    return true;
  } else {
    ASSERT(isVariable());
    return asVariable() == other->asVariable();
  }
}


Local<Type> Type::substitute(const Handle<Type>& type, const BindingList& bindings) {
  if (type->isVariable()) {
    Local<TypeParameter> param(type->asVariable());
    for (auto binding : bindings) {
      if (*param == *binding.first) {
        return binding.second;
      }
    }
  } else if (type->isClass()) {
    vector<Local<Type>> newTypeArgs;
    newTypeArgs.reserve(type->typeArgumentCount());
    bool changed = false;
    for (length_t i = 0; i < type->typeArgumentCount(); i++) {
      auto oldArg = handle(type->typeArgument(i));
      auto newArg = substitute(oldArg, bindings);
      changed |= *oldArg != *newArg;
      newTypeArgs.push_back(newArg);
    }
    if (!changed)
      return type;
    return create(type->getHeap(), handle(type->asClass()), newTypeArgs);
  }
  return type;
}


Local<Type> Type::substituteForBaseClass(const Handle<Type>& type,
                                         const Handle<Class>& clas) {
  ASSERT(type->isObject());
  Local<Type> currentType(type);
  while (currentType->isVariable()) {
    currentType = handle(currentType->asVariable()->upperBound());
  }
  ASSERT(currentType->isClass());
  ASSERT(currentType->asClass()->isSubclassOf(*clas));

  while (currentType->asClass() != *clas) {
    auto bindings = currentType->getTypeArgumentBindings();
    Local<Class> currentClass(currentType->asClass());
    currentType = Type::substitute(handle(currentClass->supertype()), bindings);
  }
  return currentType;
}


Local<Type> Type::substituteForInheritance(const Handle<Type>& type,
                                           const Handle<Class>& receiverClass,
                                           const Handle<Class>& baseClass) {
  ASSERT(receiverClass->isSubclassOf(*baseClass));

  // Build a list of supertypes on the path from receiverClass to base. We will need to iterate over
  // this in reverse. Note that this does not include receiverClass.
  vector<Local<Type>> supertypes;
  Local<Class> clas(receiverClass);
  while (*clas != *baseClass) {
    supertypes.push_back(handle(clas->supertype()));
    clas = handle(clas->supertype()->asClass());
  }

  // Perform a substitution for each type in reverse.
  Local<Type> substituted(type);
  for (auto it = supertypes.rbegin(); it != supertypes.rend(); it++) {
    auto supertype = *it;
    BindingList bindings = supertype->getTypeArgumentBindings();
    substituted = substitute(substituted, bindings);
  }
  return substituted;
}


ostream& operator << (ostream& os, const Type* type) {
  os << brief(type)
     << "\n  length: " << type->length()
     << "\n  form: " << type->form();
  if (type->isClass()) {
    os << "\n  class: " << brief(type->asClass());
  } else if (type->isVariable()) {
    os << "\n  variable: " << brief(type->asVariable());
  }
  return os;
}

}
}

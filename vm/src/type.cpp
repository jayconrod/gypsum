// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "type.h"

#include "array.h"
#include "block.h"
#include "handle.h"
#include "package.h"
#include "roots.h"
#include "type-parameter.h"

using namespace std;

namespace codeswitch {
namespace internal {

word_t Type::sizeForLength(length_t length) {
  ASSERT(length <= kMaxLength);
  return elementsOffset(sizeof(Type), kWordSize) + length * kWordSize;
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


Type::Type(Trait* trait, Flags flags)
    : Object(TYPE_BLOCK_TYPE),
      form_(TRAIT_TYPE),
      flags_(flags) {
  ASSERT(length_ == 1);
  // This trait may not be initialized yet, so we can't check its parameter count.
  elements_[0].set(this, trait);
}


Type::Type(Trait* trait, const vector<Local<Type>>& typeArgs, Flags flags)
    : Object(TYPE_BLOCK_TYPE),
      form_(TRAIT_TYPE),
      flags_(flags) {
  ASSERT(length_ == 1 + typeArgs.size());
  // This trait may not be initialzied yet, so we can't check its parameter count.
  elements_[0].set(this, trait);
  for (length_t i = 0; i < typeArgs.size(); i++) {
    elements_[i + 1].set(this, *typeArgs[i]);
  }
}


Type::Type(ObjectTypeDefn* classOrTrait, const vector<Local<Type>>& typeArgs, Flags flags)
    : Object(TYPE_BLOCK_TYPE),
      form_(isa<Class>(classOrTrait) ? CLASS_TYPE : TRAIT_TYPE),
      flags_(flags) {
  ASSERT(length_ == 1 + typeArgs.size());
  // This definition may not be initialized yet, so we can't check its parameter count.
  elements_[0].set(this, classOrTrait);
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


Type::Type(const vector<Local<TypeParameter>>& variables, Type* type)
    : Object(TYPE_BLOCK_TYPE),
      form_(EXISTENTIAL_TYPE),
      flags_(NO_FLAGS) {
  ASSERT(length_ == 1 + variables.size());
  elements_[0].set(this, type);
  for (length_t i = 0; i < variables.size(); i++) {
    elements_[i + 1].set(this, *variables[i]);
  }
}


Type::Type(Type* type, Flags flags)
    : Object(TYPE_BLOCK_TYPE),
      form_(type->form_),
      flags_(flags) {
  ASSERT(length_ == type->length_);
  for (length_t i = 0; i < length_; i++) {
    elements_[i].set(this, type->elements_[i].get());
  }
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


Local<Type> Type::create(Heap* heap, const Handle<Trait>& trait, Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(new(heap, 1) Type(*trait, flags)));
}


Local<Type> Type::create(Heap* heap,
                         const Handle<Trait>& trait,
                         const vector<Local<Type>>& typeArgs,
                         Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(
      new(heap, 1 + typeArgs.size()) Type(*trait, typeArgs, flags)));
}


Local<Type> Type::create(Heap* heap,
                         const Handle<ObjectTypeDefn>& classOrTrait,
                         const vector<Local<Type>>& typeArgs,
                         Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(
      new(heap, 1 + typeArgs.size()) Type(*classOrTrait, typeArgs, flags)));
}


Local<Type> Type::create(Heap* heap, const Handle<TypeParameter>& param, Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(new(heap, 1) Type(*param, flags)));
}


Local<Type> Type::create(Heap* heap,
                         const vector<Local<TypeParameter>>& variables,
                         const Handle<Type>& type) {
  RETRY_WITH_GC(heap, return Local<Type>(
      new(heap, 1 + variables.size()) Type(variables, *type)));
}


Local<Type> Type::closeExistential(Heap* heap,
                                   vector<Local<TypeParameter>> declaredVariables,
                                   const Handle<Type>& type) {
  // TODO: this is very inefficient, but we don't have any way to sort or hash type parameters.
  // Replace this with something better after we do.
  auto usedVariables = type->findVariables();
  vector<Local<TypeParameter>> remainingVariables;
  for (auto& dv : declaredVariables) {
    for (auto& uv : usedVariables) {
      if (*dv == *uv) {
        remainingVariables.push_back(uv);
      }
    }
  }
  if (remainingVariables.empty()) {
    return type;
  } else {
    return create(heap, remainingVariables, type);
  }
}


Local<Type> Type::createExtern(Heap* heap,
                               const Handle<Class>& clas,
                               const vector<Local<Type>>& typeArgs,
                               Flags flags) {
  auto type = create(heap, clas, typeArgs, flags);
  type->form_ = EXTERN_CLASS_TYPE;
  return type;
}


Local<Type> Type::createExtern(Heap* heap,
                               const Handle<Trait>& trait,
                               const vector<Local<Type>>& typeArgs,
                               Flags flags) {
  auto type = create(heap, trait, typeArgs, flags);
  type->form_ = EXTERN_TRAIT_TYPE;
  return type;
}


Local<Type> Type::createExtern(Heap* heap, const Handle<TypeParameter>& param, Flags flags) {
  auto type = create(heap, param, flags);
  type->form_ = EXTERN_VARIABLE_TYPE;
  return type;
}


Local<Type> Type::createWithFlags(Heap* heap, const Handle<Type>& type, Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(new(heap, type->length()) Type(*type, flags)));
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


Type* Type::labelType(Roots* roots) {
  return roots->labelType();
}


Type* Type::anyType(Roots* roots) {
  return roots->anyType();
}


Type* Type::noType(Roots* roots) {
  return roots->noType();
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
  ASSERT(isClassOrTrait());
  return length() - 1;
}


Type* Type::typeArgument(length_t index) const {
  ASSERT(isClassOrTrait());
  auto typeArgIndex = index + 1;
  ASSERT(typeArgIndex < length());
  return block_cast<Type>(elements_[typeArgIndex].get());
}


Type::BindingList Type::getTypeArgumentBindings() const {
  ASSERT(isClass() || isTrait());
  auto typeParams = isClass() ? asClass()->typeParameters() : asTrait()->typeParameters();
  auto count = typeArgumentCount();
  ASSERT(count == typeParams->length());
  BindingList bindings;
  bindings.reserve(count);
  for (length_t i = 0; i < count; i++) {
    bindings.push_back(Binding(handle(typeParams->get(i)), handle(typeArgument(i))));
  }
  return bindings;
}


length_t Type::existentialVariableCount() const {
  ASSERT(isExistential());
  return length() - 1;
}


TypeParameter* Type::existentialVariable(length_t index) const {
  ASSERT(isExistential());
  auto varIndex = 1 + index;
  ASSERT(varIndex <= length());
  return block_cast<TypeParameter>(elements_[varIndex].get());
}


Type* Type::existentialInnerType() const {
  ASSERT(isExistential() && length() > 0);
  return block_cast<Type>(elements_[0].get());
}


vector<Local<TypeParameter>> Type::findVariables() const {
  vector<Local<TypeParameter>> variables;
  findVariables(&variables);
  return variables;
}


void Type::findVariables(vector<Local<TypeParameter>>* variables) const {
  // TODO: this may produce duplicates.
  if (isVariable()) {
    variables->push_back(handle(asVariable()));
  } if (isClassOrTrait()) {
    for (length_t i = 0; i < typeArgumentCount(); i++) {
      typeArgument(i)->findVariables(variables);
    }
  } else if (isExistential()) {
    existentialInnerType()->findVariables(variables);
  }
}


bool Type::isPrimitive() const {
  return FIRST_PRIMITIVE_TYPE <= form() && form() <= LAST_PRIMITIVE_TYPE;
}


Type::Form Type::asPrimitive()const  {
  ASSERT(isPrimitive());
  return form();
}


bool Type::isClass() const {
  return form() == CLASS_TYPE || form() == EXTERN_CLASS_TYPE;
}


Class* Type::asClass() const {
  ASSERT(isClass() && length() > 0);
  return block_cast<Class>(elements_[0].get());
}


bool Type::isTrait() const {
  return form() == TRAIT_TYPE || form() == EXTERN_TRAIT_TYPE;
}


Trait* Type::asTrait() const {
  ASSERT(isTrait() && length() > 0);
  return block_cast<Trait>(elements_[0].get());
}


bool Type::isClassOrTrait() const {
  return isClass() || isTrait();
}


ObjectTypeDefn* Type::asClassOrTrait() const {
  ASSERT(isClassOrTrait() && length() > 0);
  return reinterpret_cast<ObjectTypeDefn*>(elements_[0].get());
}


bool Type::isVariable() const {
  return form() == VARIABLE_TYPE || form() == EXTERN_VARIABLE_TYPE;
}


TypeParameter* Type::asVariable() const {
  ASSERT(isVariable() && length() > 0);
  return block_cast<TypeParameter>(elements_[0].get());
}


bool Type::isExistential() const {
  return form() == EXISTENTIAL_TYPE;
}


Class* Type::effectiveClass() const {
  ASSERT(isClass() || isTrait() || isVariable());
  auto ty = this;
  while (ty->isVariable()) {
    ty = ty->asVariable()->upperBound();
  }
  while (ty->isTrait()) {
    ty = ty->asTrait()->supertypes()->get(0);
  }
  return ty->asClass();
}


bool Type::isRootClass() const {
  return isClass() && asClass() == getVM()->roots()->getBuiltinClass(BUILTIN_ROOT_CLASS_ID);
}


bool Type::isObject() const {
  return FIRST_OBJECT_TYPE <= form() && form() <= LAST_OBJECT_TYPE;
}


bool Type::isUnit() const {
  return form() == UNIT_TYPE;
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


bool Type::isInteger() const {
  return I8_TYPE <= form() && form() <= I64_TYPE;
}


bool Type::isFloat() const {
  return F32_TYPE <= form() && form() <= F64_TYPE;
}


bool Type::isF32() const {
  return form() == F32_TYPE;
}


bool Type::isF64() const {
  return form() == F64_TYPE;
}


bool Type::isAnyType() const {
  return form() == ANY_TYPE;
}


bool Type::isNoType() const {
  return form() == NO_TYPE;
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
  } else if (isTrait()) {
    if (asTrait() != other->asTrait())
      return false;
    ASSERT(typeArgumentCount() == other->typeArgumentCount());
    for (length_t i = 0; i < typeArgumentCount(); i++) {
      if (!typeArgument(i)->equals(other->typeArgument(i)))
        return false;
    }
    return true;
  } else if (isVariable()) {
    return asVariable() == other->asVariable();
  } else {
    ASSERT(isExistential());
    auto count = existentialVariableCount();
    if (count != other->existentialVariableCount())
      return false;
    for (length_t i = 0; i < count; i++) {
      if (existentialVariable(i) != other->existentialVariable(i))
        return false;
    }
    return existentialInnerType()->equals(other->existentialInnerType());
  }
}


bool Type::isEquivalent(const Handle<Type>& left, const Handle<Type>& right) {
  // For everything except for existential types, "equivalent" means "equals".
  if (!left->isExistential() || !right->isExistential())
    return left->equals(*right);

  // Existential types must be the same length.
  if (left->existentialVariableCount() != right->existentialVariableCount())
    return false;

  auto n = left->existentialVariableCount();
  auto leftInnerType = handle(left->existentialInnerType());
  auto rightInnerType = handle(right->existentialInnerType());

  // If both existential types have the same variables and their inner types are equivalent,
  // then they are equivalent.
  bool sameVariables = true;
  for (length_t i = 0; i < n && sameVariables; i++) {
    sameVariables &= left->existentialVariable(i) == right->existentialVariable(i);
  }
  if (sameVariables) {
    return isEquivalent(leftInnerType, rightInnerType);
  }

  // If all variables have the same bounds, we can substitute one of the inner types, then
  // test for equivalence.
  bool sameBounds = true;
  for (length_t i = 0; i < n && sameBounds; i++) {
    auto leftVar = left->existentialVariable(i);
    auto rightVar = right->existentialVariable(i);
    sameBounds &= leftVar->upperBound()->equals(rightVar->upperBound()) &&
                  leftVar->lowerBound()->equals(rightVar->lowerBound());
  }
  if (!sameBounds)
    return false;
  BindingList bindings;
  for (length_t i = 0; i < n; i++) {
    bindings.push_back(Binding(handle(right->existentialVariable(i)),
                               create(left->getHeap(), handle(left->existentialVariable(i)))));
  }
  rightInnerType = substitute(rightInnerType, bindings);
  return isEquivalent(leftInnerType, rightInnerType);
}


bool Type::isSubtypeOf(const Handle<Type>& left, const Handle<Type>& right) {
  SubstitutionEnvironment subEnv;
  return isSubtypeOf(left, right, &subEnv);
}


bool Type::isSubtypeOf(
    const Handle<Type>& left,
    const Handle<Type>& right,
    SubstitutionEnvironment* subEnv) {
  subEnv->beginTransaction();
  auto result = isSubtypeOfRules(left, right, subEnv);
  if (result) {
    subEnv->commitTransaction();
  } else {
    subEnv->rollbackTransaction();
  }
  return result;
}


bool Type::isSubtypeOfRules(
    Local<Type> left,
    Local<Type> right,
    SubstitutionEnvironment* subEnv) {
  // Equivalence rule.
  if (isEquivalent(left, right))
    return true;

  // Top and bottom rules.
  if (left->isAnyType() || right->isNoType())
    return false;
  if (right->isAnyType() || left->isNoType())
    return true;

  // Primitive types.
  if (left->isPrimitive() || right->isPrimitive())
    return false;
  ASSERT(left->isObject() && right->isObject());

  // Existential types.
  if (right->isExistential()) {
    for (length_t i = 0; i < right->existentialVariableCount(); i++) {
      subEnv->addVariable(handle(right->existentialVariable(i)));
    }
    return isSubtypeOf(left, handle(right->existentialInnerType()), subEnv);
  }
  if (subEnv->isExistentialVar(right)) {
    return subEnv->trySubstitute(left, right);
  }
  if (left->isExistential()) {
    return isSubtypeOf(handle(left->existentialInnerType()), right, subEnv);
  }

  // A nullable type cannot be a subtype of a non-nullable type. Note that existential types
  // cannot be nullable (but their inner types can).
  if (left->isNullable() && !right->isNullable())
    return false;

  // Variable types.
  if (left->isVariable() &&
      right->isVariable() &&
      left->asVariable()->hasCommonBound(right->asVariable())) {
    return true;
  }
  while (left->isVariable()) {
    left = handle(left->asVariable()->upperBound());
  }
  while (right->isVariable()) {
    right = handle(right->asVariable()->lowerBound());
  }
  ASSERT(left->isClassOrTrait());
  ASSERT(right->isClassOrTrait());

  // Special cases for `Nothing`.
  auto nothing = left->getVM()->roots()->getBuiltinClass(BUILTIN_NOTHING_CLASS_ID);
  if (left->asClassOrTrait() == nothing)
    return true;
  if (right->asClassOrTrait() == nothing)
    return false;

  // Check that left is derived from right, and substitute for the same definition.
  if (left->asClassOrTrait() != right->asClassOrTrait()) {
    auto rightClassOrTrait = right->asClassOrTrait();
    Local<Type> leftBase;
    auto supertypes = handle(left->isClass()
        ? left->asClass()->supertypes()
        : left->asTrait()->supertypes());
    for (length_t i = 0; i < supertypes->length() && !leftBase; i++) {
      auto sty = supertypes->get(i);
      ASSERT(sty->isClass() || sty->isTrait());
      if (sty->asClassOrTrait() == rightClassOrTrait)
        leftBase = handle(sty);
    }
    if (!leftBase)
      return false;
    left = substitute(leftBase, left->getTypeArgumentBindings());
  }

  // Compare type arguments, based on variance.
  auto typeParams = handle(left->isClass()
      ? left->asClass()->typeParameters()
      : left->asTrait()->typeParameters());
  length_t n = left->typeArgumentCount();
  for (length_t i = 0; i < n; i++) {
    auto leftArg = handle(left->typeArgument(i));
    auto rightArg = handle(right->typeArgument(i));
    auto variance = typeParams->get(i)->variance();
    if (variance == COVARIANT) {
      if (!isSubtypeOf(leftArg, rightArg, subEnv))
        return false;
    } else if (variance == CONTRAVARIANT) {
      if (!isSubtypeOf(rightArg, leftArg, subEnv))
        return false;
    } else {
      if (!(subEnv->isExistentialVar(rightArg) && subEnv->trySubstitute(leftArg, rightArg)) &&
          !isEquivalent(leftArg, rightArg)) {
        return false;
      }
    }
  }
  return true;
}


Local<Type> Type::lub(const Handle<Type>& left, const Handle<Type>& right) {
  SubstitutionEnvironment subEnv;
  vector<pair<Local<Type>, Local<Type>>> stack;
  return lub(left, right, &subEnv, &stack);
}


Local<Type> Type::lub(
    const Handle<Type>& left,
    const Handle<Type>& right,
    SubstitutionEnvironment* subEnv,
    vector<pair<Local<Type>, Local<Type>>>* stack) {
  subEnv->beginTransaction();
  auto result = lubRules(left, right, subEnv, stack);
  if (!result->isAnyType()) {
    subEnv->commitTransaction();
  } else {
    subEnv->rollbackTransaction();
  }
  return result;
}


Local<Type> Type::lubRules(
    Local<Type> left,
    Local<Type> right,
    SubstitutionEnvironment* subEnv,
    vector<pair<Local<Type>, Local<Type>>>* stack) {
  auto origLeft = left;
  auto origRight = right;
  auto roots = left->getVM()->roots();
  auto heap = left->getHeap();

  // We need to be able to detect infinite recursion in order to ensure termination.
  // Consider the case below:
  //   class A[+T]
  //   class B <: A[B]
  //   class C <: A[C]
  // Suppose we want to find B lub C
  // The correct answer is A[B lub C] = A[A[B lub C]] = A[A[A[B lub C]]] ...
  // Since we have no way to correctly express the least upper bound in that case, we
  // settle for returning a close upper bound: A[Object].
  for (auto& entry : *stack) {
    if (entry.first->equals(*left) &&
        entry.second->equals(*right)) {
      if (left->isObject() && right->isObject()) {
        return handle(rootClassType(roots));
      } else {
        return handle(anyType(roots));
      }
    }
  }

  // If two types are equivalent, they are subtypes of each other. We just return the left one.
  if (isEquivalent(left, right)) {
    return left;
  }

  // Any type, no type.
  if (left->isAnyType() || right->isAnyType()) {
    return handle(Type::anyType(roots));
  }
  if (left->isNoType())
    return right;
  if (right->isNoType())
    return left;

  // Primitive types.
  if (left->isPrimitive() || right->isPrimitive()) {
    return handle(anyType(roots));
  }

  // Existential types (always recursive).
  if (left->isExistential() || right->isExistential()) {
    stack->push_back(make_pair(left, right));

    // Add the type parameters from both existential types to the substitution
    // environment. These will be treated a little differently than normal
    // type parameters.
    Local<Type> leftInnerType, rightInnerType;
    if (left->isExistential()) {
      leftInnerType = handle(left->existentialInnerType());
      for (length_t i = 0; i < left->existentialVariableCount(); i++) {
        subEnv->addVariable(handle(left->existentialVariable(i)));
      }
    } else {
      leftInnerType = left;
    }
    if (right->isExistential()) {
      rightInnerType = handle(right->existentialInnerType());
      for (length_t i = 0; i < right->existentialVariableCount(); i++) {
        subEnv->addVariable(handle(right->existentialVariable(i)));
      }
    } else {
      rightInnerType = right;
    }

    // Find the lub of the inner types.
    auto innerLubType = lub(leftInnerType, rightInnerType, subEnv, stack);
    stack->pop_back();

    // Return an existential type with any variables that are still used in the lub type.
    // If no variables are used, we will just return the bare type.
    return closeExistential(heap, subEnv->variables(), innerLubType);
  }

  // If either side is nullable, the result is nullable.
  auto leftWithFlags = left, rightWithFlags = right;
  auto combinedFlags = NO_FLAGS;
  if (left->isNullable() || right->isNullable()) {
    combinedFlags = NULLABLE_FLAG;
    if (!left->isNullable()) {
      leftWithFlags = createWithFlags(heap, left, combinedFlags);
    }
    if (!right->isNullable()) {
      rightWithFlags = createWithFlags(heap, right, combinedFlags);
    }
  }

  // Rules for subtypes.
  if (isSubtypeOf(leftWithFlags, rightWithFlags, subEnv)) {
    return rightWithFlags;
  }
  if (isSubtypeOf(rightWithFlags, leftWithFlags, subEnv)) {
    return leftWithFlags;
  }

  // Variable types.
  if (left->isVariable() && right->isVariable()) {
    auto sharedBound = handle(left->asVariable()->findCommonUpperBound(right->asVariable()));
    if (sharedBound) {
      return create(heap, sharedBound);
    }
  }

  // TODO: beyond this point, we can't find an accurate least upper bound because of the
  // presence of multiple shared traits. We will find a common base class instead, which
  // may be less specific. When union types are supported, we should just return
  // left | right.
  while (left->isVariable()) {
    left = handle(left->asVariable()->upperBound());
  }
  while (right->isVariable()) {
    right = handle(right->asVariable()->upperBound());
  }
  ASSERT(left->isClassOrTrait() && right->isClassOrTrait());

  // Find a common base class. We don't assume that there is a single root class
  // (even though there is), so this can fail.
  auto base = handle(left->asClassOrTrait()->findCommonBase(right->asClassOrTrait()));
  while (base) {
    left = substituteForBase(left, base);
    right = substituteForBase(right, base);

    // We need to combine the type arguments, according to the variance of the
    // corresponding type parameters. This is not necessarily possible. If we get
    // stuck, we'll try again with the superclass.
    vector<Local<Type>> combinedArgs;
    auto parameterCount = base->typeParameters()->length();
    combinedArgs.reserve(parameterCount);
    for (length_t i = 0; i < parameterCount; i++) {
      auto param = handle(base->typeParameters()->get(i));
      auto leftArg = handle(left->typeArgument(i));
      auto rightArg = handle(right->typeArgument(i));

      auto variance = param->variance();
      Local<Type> combined;
      if (variance == INVARIANT) {
        if (leftArg->equals(*rightArg)) {
          combined = leftArg;
        } else if (subEnv->isExistentialVar(leftArg) &&
                   subEnv->trySubstitute(rightArg, leftArg)) {
          combined = leftArg;
        } else if (subEnv->isExistentialVar(rightArg) &&
                   subEnv->trySubstitute(leftArg, rightArg)) {
          combined = rightArg;
        } else {
          break;
        }
      } else {
        stack->push_back(make_pair(origLeft, origRight));
        if (variance == COVARIANT) {
          combined = lub(leftArg, rightArg, subEnv, stack);
        } else {
          ASSERT(variance == CONTRAVARIANT);
          combined = glb(left, right);
        }
        stack->pop_back();
      }
      if (combined->isAnyType()) {
        break;
      }
      combinedArgs.push_back(combined);
    }

    if (combinedArgs.size() == parameterCount) {
      return create(heap, base, combinedArgs, combinedFlags);
    }

    if (!base->supertypes()->isEmpty()) {
      base = handle(base->supertypes()->get(0)->asClass());
    } else {
      base.clear();
    }
  }

  // If we reach this point, we ran out of superclasses.
  return handle(anyType(roots));
}


Local<Type> Type::glb(Local<Type> left, Local<Type> right) {
  if (left->equals(*right)) {
    return left;
  }
  if (Type::isSubtypeOf(left, right)) {
    return left;
  }
  if (Type::isSubtypeOf(right, left)) {
    return right;
  }
  if (left->isObject() && right->isObject()) {
    // TODO: this is a very crude approximation. When there are intersection types,
    // return one of those.
    auto roots = left->getVM()->roots();
    return handle(left->isNullable() && right->isNullable()
        ? Type::nullType(roots)
        : Type::nothingType(roots));
  }

  UNIMPLEMENTED();
  return Local<Type>();
}


Local<Type> Type::substitute(const Handle<Type>& type, const BindingList& bindings) {
  if (type->isVariable()) {
    Local<TypeParameter> param(type->asVariable());
    for (auto binding : bindings) {
      if (*param == *binding.first) {
        return binding.second;
      }
    }
  } else if (type->isClass() || type->isTrait()) {
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
    if (type->isClass()) {
      return create(type->getHeap(), handle(type->asClass()), newTypeArgs);
    } else {
      return create(type->getHeap(), handle(type->asTrait()), newTypeArgs);
    }
  }
  return type;
}


Local<Type> Type::substituteForInheritance(const Handle<Type>& type,
                                           Local<ObjectTypeDefn> receiverDefn,
                                           Local<ObjectTypeDefn> baseDefn) {
  if (*receiverDefn == *baseDefn)
    return type;

  auto supertypes = handle(isa<Class>(*receiverDefn)
      ? block_cast<Class>(*receiverDefn)->supertypes()
      : block_cast<Trait>(*receiverDefn)->supertypes());
  Local<Type> supertype;
  for (length_t i = 0; i < supertypes->length() && !supertype; i++) {
    if (supertypes->get(i)->asClassOrTrait() == *baseDefn)
      supertype = handle(supertypes->get(i));
  }
  ASSERT(!supertype.isEmpty());

  auto typeParams = handle(isa<Class>(*baseDefn)
      ? block_cast<Class>(*baseDefn)->typeParameters()
      : block_cast<Trait>(*baseDefn)->typeParameters());
  auto count = typeParams->length();
  ASSERT(count == supertype->typeArgumentCount());
  BindingList bindings;
  bindings.reserve(count);
  for (length_t i = 0; i < count; i++) {
    bindings.push_back(Binding(handle(typeParams->get(i)), handle(supertype->typeArgument(i))));
  }
  return substitute(type, bindings);
}


Local<Type> Type::substituteForBase(const Handle<Type>& type,
                                    const Handle<ObjectTypeDefn>& baseDefn) {
  auto heap = type->getHeap();

  ASSERT(type->isObject());
  if (type->isExistential()) {
    auto substitutedType = substituteForBase(handle(type->existentialInnerType()), baseDefn);
    if (substitutedType->equals(*type)) {
      return type;
    } else {
      vector<Local<TypeParameter>> variables;
      variables.reserve(type->existentialVariableCount());
      for (length_t i = 0; i < type->existentialVariableCount(); i++) {
        variables.push_back(handle(type->existentialVariable(i)));
      }
      return create(heap, variables, substitutedType);
    }
  } else if (type->isVariable()) {
    return substituteForBase(handle(type->asVariable()->upperBound()), baseDefn);
  } else {
    ASSERT(type->isClassOrTrait());
    auto nothingClass = type->getVM()->roots()->getBuiltinClass(BUILTIN_NOTHING_CLASS_ID);
    ASSERT(*baseDefn != nothingClass);
    auto typeDefn = handle(type->asClassOrTrait());
    ASSERT(*typeDefn != nothingClass);

    if (*typeDefn == *baseDefn) {
      return type;
    }
    Local<Type> baseType;
    for (length_t i = 0; i < typeDefn->supertypes()->length() && !baseType; i++) {
      auto supertype = handle(typeDefn->supertypes()->get(i));
      if (supertype->asClassOrTrait() == *baseDefn) {
        baseType = supertype;
      }
    }
    ASSERT(baseType);

    BindingList bindings;
    for (length_t i = 0; i < baseDefn->typeParameters()->length(); i++) {
      bindings.push_back(make_pair(handle(baseDefn->typeParameters()->get(i)),
                                   handle(type->typeArgument(i))));
    }
    return substitute(baseType, bindings);
  }
}


void Type::SubstitutionEnvironment::beginTransaction() {
  transactionStack_.push_back(substitutions_.size());
}


void Type::SubstitutionEnvironment::commitTransaction() {
  transactionStack_.pop_back();
}


void Type::SubstitutionEnvironment::rollbackTransaction() {
  auto index = transactionStack_.back();
  transactionStack_.pop_back();
  transactionStack_.erase(transactionStack_.begin() + index, transactionStack_.end());
}


void Type::SubstitutionEnvironment::addVariable(const Handle<TypeParameter>& var) {
  if (!haveVariable(*var)) {
    substitutions_.push_back(make_pair(Local<TypeParameter>(var), Local<Type>()));
  }
}


vector<Local<TypeParameter>> Type::SubstitutionEnvironment::variables() const {
  vector<Local<TypeParameter>> variables;
  for (auto& p : substitutions_) {
    auto haveVariable = false;
    for (auto& v : variables) {
      haveVariable |= *v == *p.first;
    }
    if (!haveVariable) {
      variables.push_back(p.first);
    }
  }
  return variables;
}


bool Type::SubstitutionEnvironment::isExistentialVar(const Handle<Type>& type) const {
  return type->isVariable() && haveVariable(type->asVariable());
}


bool Type::SubstitutionEnvironment::trySubstitute(
    const Handle<Type>& type,
    const Handle<Type>& varType) {
  ASSERT(varType->isVariable());
  auto tp = handle(varType->asVariable());
  auto index = indexOf(*tp);
  ASSERT(index >= 0);
  Local<Type> combinedType;
  if (substitutions_[index].second.isEmpty()) {
    combinedType = type;
  } else {
    vector<pair<Local<Type>, Local<Type>>> stack;
    combinedType = Type::lub(substitutions_[index].second, type, this, &stack);
  }
  if (Type::isSubtypeOf(combinedType, handle(tp->upperBound()), this) &&
      Type::isSubtypeOf(handle(tp->lowerBound()), combinedType, this)) {
    substitutions_[index].second = combinedType;
    return true;
  } else {
    return false;
  }
}


bool Type::SubstitutionEnvironment::haveVariable(TypeParameter* param) const {
  return indexOf(param) >= 0;
}


int Type::SubstitutionEnvironment::indexOf(TypeParameter* param) const {
  for (size_t i = 0; i < substitutions_.size(); i++) {
    if (*substitutions_[i].first == param)
      return static_cast<int>(i);
  }
  return -1;
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

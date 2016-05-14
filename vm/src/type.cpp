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


Local<Type> Type::create(Heap* heap, const Handle<TypeParameter>& param, Flags flags) {
  RETRY_WITH_GC(heap, return Local<Type>(new(heap, 1) Type(*param, flags)));
}


Local<Type> Type::create(Heap* heap,
                         const vector<Local<TypeParameter>>& variables,
                         const Handle<Type>& type) {
  RETRY_WITH_GC(heap, return Local<Type>(
      new(heap, 1 + variables.size()) Type(variables, *type)));
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


bool Type::isSubtypeOf(Local<Type> left, Local<Type> right) {
  // Equivalence rule.
  if (isEquivalent(left, right))
    return true;

  // Primitive types.
  if (left->isPrimitive() || right->isPrimitive())
    return false;
  ASSERT(left->isObject() && right->isObject());

  // Existential types.
  if (left->isExistential() || right->isExistential()) {
    auto leftInnerType =
        left->isExistential() ? handle(left->existentialInnerType()) : left;
    auto rightInnerType =
        right->isExistential() ? handle(right->existentialInnerType()) : right;
    return isSubtypeOf(leftInnerType, rightInnerType);
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
      if (!isSubtypeOf(leftArg, rightArg))
        return false;
    } else if (variance == CONTRAVARIANT) {
      if (!isSubtypeOf(rightArg, leftArg))
        return false;
    } else {
      if (!isEquivalent(leftArg, rightArg))
        return false;
    }
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


#define EXTERN_TYPE_INFO_POINTERS_LIST(F) \
  F(ExternTypeInfo, type_)                \
  F(ExternTypeInfo, package_)             \

DEFINE_POINTER_MAP(ExternTypeInfo, EXTERN_TYPE_INFO_POINTERS_LIST)

#undef EXTERN_TYPE_INFO_POINTERS_LIST


ExternTypeInfo::ExternTypeInfo(Type* type,
                               Package* package,
                               length_t dependencyIndex,
                               length_t externIndex)
    : Block(kBlockType),
      type_(this, type),
      package_(this, package),
      dependencyIndex_(dependencyIndex),
      externIndex_(externIndex) { }


Local<ExternTypeInfo> ExternTypeInfo::create(Heap* heap,
                                             const Handle<Type>& type,
                                             const Handle<Package>& package,
                                             length_t dependencyIndex,
                                             length_t externIndex) {
  RETRY_WITH_GC(heap, return Local<ExternTypeInfo>(
      new(heap) ExternTypeInfo(*type, *package, dependencyIndex, externIndex)));
}


void ExternTypeInfo::linkType() {
  auto type = type_.get();
  ASSERT(type->length() >= 1);
  auto dep = package_.get()->dependencies()->get(dependencyIndex_);
  if (type->form() == Type::EXTERN_CLASS_TYPE) {
    auto linkedClass = dep->linkedClasses()->get(externIndex_);
    type->elements_[0].set(type, linkedClass);
    type->form_ = Type::CLASS_TYPE;
  } else if (type->form() == Type::EXTERN_TRAIT_TYPE) {
    auto linkedTrait = dep->linkedTraits()->get(externIndex_);
    type->elements_[0].set(type, linkedTrait);
    type->form_ = Type::TRAIT_TYPE;
  } else {
    ASSERT(type->form() == Type::EXTERN_VARIABLE_TYPE);
    auto linkedTypeParameter = dep->linkedTypeParameters()->get(externIndex_);
    type->elements_[0].set(type, linkedTypeParameter);
    type->form_ = Type::VARIABLE_TYPE;
  }
}


ostream& operator << (ostream& os, const ExternTypeInfo* info) {
  os << brief(info)
     << "\n  type: " << brief(info->type())
     << "\n  package: " << brief(info->package())
     << "\n  dependencyIndex: " << info->dependencyIndex()
     << "\n  externIndex: " << info->externIndex();
  return os;
}

}
}

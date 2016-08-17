// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef type_h
#define type_h

#include <iostream>
#include <utility>
#include <vector>
#include "bytecode.h"
#include "class.h"
#include "flags.h"
#include "handle.h"
#include "object.h"
#include "tagged.h"
#include "trait.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class ObjectTypeDefn;
class Roots;
class TypeParameter;

typedef u32 Variance;
const Variance INVARIANT = 0;
const Variance COVARIANT = COVARIANT_FLAG;
const Variance CONTRAVARIANT = CONTRAVARIANT_FLAG;
const Variance BIVARIANT = COVARIANT | CONTRAVARIANT;

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
    TRAIT_TYPE,
    VARIABLE_TYPE,
    EXISTENTIAL_TYPE,

    // Pseudo forms (still object forms)
    EXTERN_CLASS_TYPE,
    EXTERN_TRAIT_TYPE,
    EXTERN_VARIABLE_TYPE,

    // Special forms
    LABEL_TYPE,

    // Other symbols
    FIRST_PRIMITIVE_TYPE = UNIT_TYPE,
    LAST_PRIMITIVE_TYPE = F64_TYPE,
    FIRST_OBJECT_TYPE = CLASS_TYPE,
    LAST_OBJECT_TYPE = EXTERN_VARIABLE_TYPE,
    LAST_TYPE = LABEL_TYPE
  };

  enum Flags {
    NO_FLAGS = 0,
    NULLABLE_FLAG = 1 << 0,
    LAST_FLAG = NULLABLE_FLAG,
    FLAGS_MASK = (LAST_FLAG << 1) - 1
  };

  typedef std::pair<Local<TypeParameter>, Local<Type>> Binding;
  typedef std::vector<Binding> BindingList;

  static word_t sizeForLength(length_t length);
  void* operator new (size_t, Heap* heap, length_t length);
  void* operator new (size_t, void* place, length_t length);
  explicit Type(Form primitive, Flags flags = NO_FLAGS);
  explicit Type(Class* clas, Flags flags = NO_FLAGS);
  Type(Class* clas, const std::vector<Local<Type>>& typeArgs, Flags flags = NO_FLAGS);
  explicit Type(Trait* trait, Flags flags = NO_FLAGS);
  Type(Trait* trait, const std::vector<Local<Type>>& typeArgs, Flags flags = NO_FLAGS);
  explicit Type(TypeParameter* param, Flags flags = NO_FLAGS);
  Type(const std::vector<Local<TypeParameter>>& variables, Type* type);
  Type(Type* type, Flags flags);
  static Local<Type> create(Heap* heap, Form primitive, Flags flags = NO_FLAGS);
  static Local<Type> create(Heap* heap, const Handle<Class>& clas, Flags fags = NO_FLAGS);
  static Local<Type> create(Heap* heap,
                            const Handle<Class>& clas,
                            const std::vector<Local<Type>>& typeArgs,
                            Flags flags = NO_FLAGS);
  static Local<Type> create(Heap* heap, const Handle<Trait>& trait, Flags flags = NO_FLAGS);
  static Local<Type> create(Heap* heap,
                            const Handle<Trait>& trait,
                            const std::vector<Local<Type>>& typeArgs,
                            Flags flags = NO_FLAGS);
  static Local<Type> create(Heap* heap,
                            const Handle<TypeParameter>& param,
                            Flags flags = NO_FLAGS);
  static Local<Type> create(Heap* heap,
                            const std::vector<Local<TypeParameter>>& variables,
                            const Handle<Type>& type);
  static Local<Type> createExtern(Heap* heap,
                                  const Handle<Class>& clas,
                                  const std::vector<Local<Type>>& typeArgs,
                                  Flags flags = NO_FLAGS);
  static Local<Type> createExtern(Heap* heap,
                                  const Handle<Trait>& trait,
                                  const std::vector<Local<Type>>& typeArgs,
                                  Flags flags = NO_FLAGS);
  static Local<Type> createExtern(Heap* heap,
                                  const Handle<TypeParameter>& param,
                                  Flags flags = NO_FLAGS);
  static Local<Type> createWithFlags(Heap* heap, const Handle<Type>& type, Flags flags);

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
  static Type* labelType(Roots* roots);
  static Type* rootClassType(Roots* roots);
  static Type* nothingType(Roots* roots);
  static Type* nullType(Roots* roots);

  length_t length() const { return elementsLength(); }
  length_t typeArgumentCount() const;
  Type* typeArgument(length_t index) const;
  BindingList getTypeArgumentBindings() const;
  length_t existentialVariableCount() const;
  TypeParameter* existentialVariable(length_t index) const;
  Type* existentialInnerType() const;

  Form form() const { return form_; }
  Flags flags() const { return flags_; }
  bool isPrimitive() const;
  Form asPrimitive() const;
  bool isLabel() const;
  bool isClass() const;
  Class* asClass() const;
  bool isTrait() const;
  Trait* asTrait() const;
  bool isClassOrTrait() const;
  ObjectTypeDefn* asClassOrTrait() const;
  bool isVariable() const;
  TypeParameter* asVariable() const;
  bool isExistential() const;
  Class* effectiveClass() const;
  bool isRootClass() const;
  bool isObject() const;
  bool isUnit() const;
  bool isBoolean() const;
  bool isI8() const;
  bool isI16() const;
  bool isI32() const;
  bool isI64() const;
  bool isInteger() const;
  bool isF32() const;
  bool isF64() const;
  bool isFloat() const;
  bool isNullable() const;

  word_t typeSize() const;
  word_t alignment() const;

  static bool isEquivalent(const Handle<Type>& left, const Handle<Type>& right);
  static bool isSubtypeOf(const Handle<Type>& left, const Handle<Type>& right);
  bool equals(Type* other) const;
  static Local<Type> lub(const Handle<Type>& left, const Handle<Type>& right);
  static Local<Type> substitute(const Handle<Type>& type, const BindingList& bindings);
  static Local<Type> substituteForInheritance(const Handle<Type>& type,
                                              Local<ObjectTypeDefn> receiverDefn,
                                              Local<ObjectTypeDefn> baseDefn);

 private:
  class SubstitutionEnvironment {
   public:
    void addVariable(const Handle<TypeParameter>& var);
    bool isExistentialVar(const Handle<Type>& type) const;
    bool trySubstitute(const Handle<Type>& type, const Handle<Type>& varType);

   private:
    // TODO: this is an inefficient representation, but we can't use type parameters as keys
    // in an unordered_map, since there's nothing to identify them, other than their address,
    // which can change.
    int indexOf(TypeParameter* param) const;

    std::vector<std::pair<Local<TypeParameter>, Local<Type>>> substitutionTypes_;
  };
  static bool isSubtypeOf(Local<Type> left, Local<Type> right, SubstitutionEnvironment subEnv);
  static Local<Type> lub(Local<Type> left, Local<Type> right, SubstitutionEnvironment subEnv);

  static const word_t kPointerMap = 0;

  length_t length_;
  Form form_ : 4;
  Flags flags_ : 28;
  Ptr<Block> elements_[0];

  friend class Roots;
};

std::ostream& operator << (std::ostream& os, const Type* type);

}
}

#endif

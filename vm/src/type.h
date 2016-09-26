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
    LABEL_TYPE,  // label produced by "label" and used by "branchl" instructions
    ANY_TYPE,  // top of the type lattice
    NO_TYPE,  // bottom of the type lattice

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
  Type(ObjectTypeDefn* classOrTrait,
       const std::vector<Local<Type>>& typeArgs,
       Flags flags = NO_FLAGS);
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
                            const Handle<ObjectTypeDefn>& classOrTrait,
                            const std::vector<Local<Type>>& typeArgs,
                            Flags flags = NO_FLAGS);
  static Local<Type> create(Heap* heap,
                            const Handle<TypeParameter>& param,
                            Flags flags = NO_FLAGS);
  static Local<Type> create(Heap* heap,
                            const std::vector<Local<TypeParameter>>& variables,
                            const Handle<Type>& type);
  static Local<Type> closeExistential(Heap* heap,
                                      std::vector<Local<TypeParameter>> typeParameters,
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
  static Type* anyType(Roots* roots);
  static Type* noType(Roots* roots);
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
  std::vector<Local<TypeParameter>> findVariables() const;

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
  bool isAnyType() const;
  bool isNoType() const;
  bool isNullable() const;

  word_t typeSize() const;
  word_t alignment() const;

  /** Returns whether two types are exactly the same. */
  bool equals(Type* other) const;

  /**
   * Returns whether two types contain the same values. Types may be equivalent but not
   * equal. For example, Object and forsome [X] X are equivalent, because every value that
   * is a member of one is also a member of the other.
   */
  static bool isEquivalent(const Handle<Type>& left, const Handle<Type>& right);

  /**
   * Returns whether `left` is a subtype of `right`. This is true if all the values in `left`
   * are contained in `right`.
   */
  static bool isSubtypeOf(const Handle<Type>& left, const Handle<Type>& right);

  /**
   * Returns the least upper bound of `left` and `right`: the smallest type that is a supertype
   * of both.
   *
   * This method is actually just an approximation of the least upper bound, since least upper
   * bound is not defined in all cases. There are situations (involving parameterized classes)
   * where the least upper bound may be infinitely complicated and cannot be expressed.
   */
  static Local<Type> lub(const Handle<Type>& left, const Handle<Type>& right);

  /**
   * Returns the greatest lower bound of `left` and `right`: the largest type that is a
   * subtype of both.
   *
   * This method is even more of an approximation than {@link #lub}. Because of the way class
   * inheritance fans out, it returns `Nothing` in most cases unless the two types are
   * equal or if there's some subtype relation between them.
   */
  static Local<Type> glb(Local<Type> left, Local<Type> right);

  /**
   * Replaces all variable types within `type` which refer to the type parameters in `bindings`
   * using the corresponding types in `bindings`. This is frequently used when applying
   * type arguments, for example, in a parameterized method call.
   */
  static Local<Type> substitute(const Handle<Type>& type, const BindingList& bindings);

  /**
   * Replaces parts of a type from an inherited definition using arguments applied to a
   * base definition.
   *
   * This method calls {@link #substitute} using a `bindings` list generated from `receiverDefn`
   * and `baseDefn`. The type parameters in the list are those of `baseDefn`. The type
   * arguments are from the type for `baseDefn` in `receiverDefn`'s supertypes list.
   *
   * For example, suppose we have a class `Box[X]` with a field of type `X` and a subclass
   * `IntBox <: Box[Integer]`. If we load the field from an `IntBox`, we will call this
   * method in order to substitute `Integer` for `X` so that the type of the field is `Integer`.
   *
   * @param type the type to perform substitution on.
   * @param receiverDefn the inherited class or trait.
   * @param baseDefn the base class or trait. Must be in the `supertypes` list
   *     of `receiverDefn`.
   */
  static Local<Type> substituteForInheritance(const Handle<Type>& type,
                                              Local<ObjectTypeDefn> receiverDefn,
                                              Local<ObjectTypeDefn> baseDefn);

  /**
   * Returns a class or trait type of the given class or trait.
   *
   * The type arguments are determined by substituting the type arguments from this type,
   * which must be an object type. For example, suppose we have a class `Foo[T]` with a
   * subclass `Bar <: Foo[String]` and there is some type parameter `X <: Bar`. If we
   * substitute the variable type `X` for the base `Foo`, the result will be `Foo[String]`.
   *
   * @param type the type to substitute (`X` in the example above). Must be an object type.
   * @param baseDefn the base class or trait. Must be a base of `type`.
   * @return a substituted class or trait type of `baseDefn`.
   */
  static Local<Type> substituteForBase(const Handle<Type>& type,
                                       const Handle<ObjectTypeDefn>& baseDefn);

 private:
  class SubstitutionEnvironment {
   public:
    void beginTransaction();
    void commitTransaction();
    void rollbackTransaction();

    void addVariable(const Handle<TypeParameter>& var);
    std::vector<Local<TypeParameter>> variables() const;
    bool isExistentialVar(const Handle<Type>& type) const;
    bool trySubstitute(const Handle<Type>& type, const Handle<Type>& varType);

   private:
    bool haveVariable(TypeParameter* param) const;
    int indexOf(TypeParameter* param) const;

    // TODO: this is an inefficient representation, but we can't use type parameters as keys
    // in an unordered_map, since there's nothing to identify them, other than their address,
    // which can change.
    std::vector<std::pair<Local<TypeParameter>, Local<Type>>> substitutions_;
    std::vector<size_t> transactionStack_;
  };
  void findVariables(std::vector<Local<TypeParameter>>* variables) const;
  static bool isSubtypeOf(
      const Handle<Type>& left,
      const Handle<Type>& right,
      SubstitutionEnvironment* subEnv);
  static bool isSubtypeOfRules(
      Local<Type> left,
      Local<Type> right,
      SubstitutionEnvironment* subEnv);
  static Local<Type> lub(
      const Handle<Type>& left,
      const Handle<Type>& right,
      SubstitutionEnvironment* subEnv,
      std::vector<std::pair<Local<Type>, Local<Type>>>* stack);
  static Local<Type> lubRules(
      Local<Type> left,
      Local<Type> right,
      SubstitutionEnvironment* subEnv,
      std::vector<std::pair<Local<Type>, Local<Type>>>* stack);

  static const word_t kPointerMap = 0;

  length_t length_;
  Form form_ : 8;
  Flags flags_ : 24;
  Ptr<Block> elements_[0];

  friend class Roots;
};

std::ostream& operator << (std::ostream& os, const Type* type);

}
}

#endif

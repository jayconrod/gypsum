 // Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <vector>
#include "array.h"
#include "builtins.h"
#include "class.h"
#include "name.h"
#include "object-type-defn.h"
#include "string.h"
#include "tagged.h"
#include "type.h"
#include "type-parameter.h"
#include "vm.h"

using namespace std;
using namespace codeswitch::internal;

static Local<BlockArray<Type>> buildSupertypes(const Handle<Type>& type) {
  ASSERT(type->isClassOrTrait());
  auto superDefn = handle(type->asClassOrTrait());
  auto typeParameters = handle(superDefn->typeParameters());
  auto ubertypes = handle(superDefn->supertypes());
  auto supertypes = BlockArray<Type>::create(type->getHeap(), ubertypes->length() + 1);
  supertypes->set(0, *type);

  Type::BindingList bindings;
  for (length_t i = 0; i < typeParameters->length(); i++) {
    bindings.push_back(Type::Binding(handle(typeParameters->get(i)),
                                     handle(type->typeArgument(i))));
  }
  for (length_t i = 0; i < ubertypes->length(); i++) {
    auto uty = handle(ubertypes->get(i));
    auto sty = Type::substitute(uty, bindings);
    supertypes->set(i + 1, *sty);
  }
  return supertypes;
}


static void setSupertype(Local<ObjectTypeDefn> defn, const Handle<Type>& type) {
  defn->setSupertypes(*buildSupertypes(type));
}


static void setSupertypeToRoot(Local<ObjectTypeDefn> defn) {
  auto rootType = handle(Type::rootClassType(defn->getVM()->roots()));
  defn->setSupertypes(*buildSupertypes(rootType));
}


TEST(CreateWithFlags) {
  TEST_PROLOGUE

  auto oldType = handle(Type::rootClassType(vm.roots()));
  auto newType = Type::createWithFlags(heap, oldType, Type::NULLABLE_FLAG);
  ASSERT_EQ(oldType->length(), newType->length());
  ASSERT_EQ(oldType->form(), newType->form());
  ASSERT_EQ(oldType->asClass(), newType->asClass());
  ASSERT_TRUE(newType->isNullable());
}


TEST(PrimitiveTypeEquals) {
  VM vm;

  auto unitType = Type::unitType(vm.roots());
  auto i8Type = Type::i8Type(vm.roots());
  auto rootClassType = Type::rootClassType(vm.roots());
  ASSERT_TRUE(unitType->equals(unitType));
  ASSERT_FALSE(unitType->equals(i8Type));
  ASSERT_FALSE(i8Type->equals(unitType));
  ASSERT_FALSE(unitType->equals(rootClassType));
}


TEST(ClassTypeEquals) {
  TEST_PROLOGUE

  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto C = Class::create(heap);
  auto typeParameters = BlockArray<TypeParameter>::create(heap, 1);
  typeParameters->set(0, *S);
  C->setTypeParameters(*typeParameters);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto SType = Type::create(heap, S);
  auto TType = Type::create(heap, T);
  auto CSType = Type::create(heap, C, vector<Local<Type>>{SType});
  auto CTType = Type::create(heap, C, vector<Local<Type>>{TType});

  ASSERT_TRUE(rootType->equals(*rootType));
  ASSERT_FALSE(rootType->equals(*CSType));
  ASSERT_FALSE(CSType->equals(*rootType));
  ASSERT_FALSE(CSType->equals(*CTType));
  ASSERT_TRUE(CSType->equals(*CSType));
  ASSERT_TRUE(CTType->equals(*CTType));
}


TEST(VariableTypeEquals) {
  TEST_PROLOGUE

  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto SType = Type::create(heap, S);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(heap, T);

  ASSERT_TRUE(SType->equals(*SType));
  ASSERT_TRUE(TType->equals(*TType));
  ASSERT_FALSE(SType->equals(*TType));
}


TEST(SubtypePrimitives) {
  TEST_PROLOGUE
  auto unitType = handle(Type::unitType(vm.roots()));
  auto i8Type = handle(Type::i8Type(vm.roots()));
  auto rootType = handle(Type::rootClassType(vm.roots()));
  ASSERT_TRUE(Type::isSubtypeOf(unitType, unitType));
  ASSERT_FALSE(Type::isSubtypeOf(unitType, i8Type));
  ASSERT_FALSE(Type::isSubtypeOf(unitType, rootType));
}


TEST(SubtypeClassParent) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto stringClass = handle(vm.roots()->getBuiltinClass(BUILTIN_STRING_CLASS_ID));
  auto stringType = Type::create(heap, stringClass);

  ASSERT_TRUE(Type::isSubtypeOf(rootType, rootType));
  ASSERT_TRUE(Type::isSubtypeOf(stringType, stringType));
  ASSERT_TRUE(Type::isSubtypeOf(stringType, rootType));
  ASSERT_FALSE(Type::isSubtypeOf(rootType, stringType));
}


TEST(SubtypeClassNothing) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto stringClass = handle(vm.roots()->getBuiltinClass(BUILTIN_STRING_CLASS_ID));
  auto stringType = Type::create(heap, stringClass);
  auto nothingType = handle(Type::nothingType(vm.roots()));

  ASSERT_TRUE(Type::isSubtypeOf(nothingType, rootType));
  ASSERT_TRUE(Type::isSubtypeOf(nothingType, stringType));
  ASSERT_TRUE(Type::isSubtypeOf(nothingType, nothingType));
}


TEST(SubtypeParameterSimple) {
  TEST_PROLOGUE

  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(heap, T);
  ASSERT_TRUE(Type::isSubtypeOf(TType, TType));
  ASSERT_TRUE(Type::isSubtypeOf(TType, handle(T->upperBound())));
  ASSERT_TRUE(Type::isSubtypeOf(handle(T->lowerBound()), TType));
}


TEST(SubtypeParametersOverlapping) {
  TEST_PROLOGUE

  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  auto A = Class::create(heap);
  setSupertypeToRoot(A);
  A->setTypeParameters(*emptyTypeParameters);
  auto AType = Type::create(heap, A);
  auto B = Class::create(heap);
  setSupertype(B, AType);
  B->setTypeParameters(*emptyTypeParameters);
  auto BType = Type::create(heap, B);
  auto C = Class::create(heap);
  setSupertype(C, BType);
  C->setTypeParameters(*emptyTypeParameters);
  auto CType = Type::create(heap, C);

  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0, AType, CType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0, BType, CType);
  auto SType = Type::create(heap, S);
  ASSERT_FALSE(Type::isSubtypeOf(SType, TType));
}


TEST(SubtypeParametersNonOverlapping) {
  TEST_PROLOGUE

  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  auto A = Class::create(heap);
  setSupertypeToRoot(A);
  A->setTypeParameters(*emptyTypeParameters);
  auto AType = Type::create(heap, A);
  auto B = Class::create(heap);
  setSupertype(B, AType);
  B->setTypeParameters(*emptyTypeParameters);
  auto BType = Type::create(heap, B);
  auto C = Class::create(heap);
  setSupertype(C, BType);
  C->setTypeParameters(*emptyTypeParameters);
  auto CType = Type::create(heap, C);

  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0, AType, BType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0, BType, CType);
  auto SType = Type::create(heap, S);
  ASSERT_TRUE(Type::isSubtypeOf(SType, TType));
}


TEST(SubtypeParametersTransitiveUpper) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto U = TypeParameter::create(heap, NAME("U"), STR("U"), 0, rootType, nothingType);
  auto UType = Type::create(heap, U);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0, UType, nothingType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0, TType, nothingType);
  auto SType = Type::create(heap, S);

  ASSERT_TRUE(Type::isSubtypeOf(SType, UType));
}


TEST(SubtypeParametersTransitiveLower) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto U = TypeParameter::create(heap, NAME("U"), STR("U"), 0, rootType, nothingType);
  auto UType = Type::create(heap, U);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0, rootType, UType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0, rootType, TType);
  auto SType = Type::create(heap, S);

  ASSERT_TRUE(Type::isSubtypeOf(UType, SType));
}


TEST(SubtypeParametersTransitiveMiddle) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto M = TypeParameter::create(heap, NAME("M"), STR("M"), 0, rootType, nothingType);
  auto MType = Type::create(heap, M);
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0, MType, nothingType);
  auto SType = Type::create(heap, S);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0, rootType, MType);
  auto TType = Type::create(heap, T);

  ASSERT_TRUE(Type::isSubtypeOf(SType, TType));
}


TEST(SubtypeClassWithParametersSelf) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0, rootType, nothingType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0, rootType, nothingType);
  auto SType = Type::create(heap, S);

  auto A = Class::create(heap);
  setSupertypeToRoot(A);
  auto ATypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  ATypeParameters->set(0, *T);
  A->setTypeParameters(*ATypeParameters);

  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  auto X = Class::create(heap);
  setSupertypeToRoot(X);
  X->setTypeParameters(*emptyTypeParameters);
  auto XType = Type::create(heap, X);
  auto Y = Class::create(heap);
  setSupertypeToRoot(Y);
  Y->setTypeParameters(*emptyTypeParameters);
  auto YType = Type::create(heap, Y);

  vector<Local<Type>> ATTypeArgs { Type::create(heap, T) };
  auto ATType = Type::create(heap, A, vector<Local<Type>>{TType});
  auto ASType = Type::create(heap, A, vector<Local<Type>>{SType});
  auto AXType = Type::create(heap, A, vector<Local<Type>>{XType});
  auto AYType = Type::create(heap, A, vector<Local<Type>>{YType});

  ASSERT_TRUE(Type::isSubtypeOf(ATType, ATType));
  ASSERT_FALSE(Type::isSubtypeOf(ATType, ASType));
  ASSERT_TRUE(Type::isSubtypeOf(AXType, AXType));
  ASSERT_FALSE(Type::isSubtypeOf(AXType, AYType));
}


TEST(SubtypeSubclassWithParameters) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0, rootType, nothingType);

  auto A = Class::create(heap);
  setSupertypeToRoot(A);
  auto ATypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  ATypeParameters->set(0, *T);
  A->setTypeParameters(*ATypeParameters);

  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  auto X = Class::create(heap);
  setSupertypeToRoot(X);
  X->setTypeParameters(*emptyTypeParameters);
  auto XType = Type::create(heap, X);

  auto Y = Class::create(heap);
  setSupertypeToRoot(Y);
  Y->setTypeParameters(*emptyTypeParameters);
  auto YType = Type::create(heap, Y);

  auto B = Class::create(heap);
  auto AXType = Type::create(heap, A, vector<Local<Type>>{XType});
  setSupertype(B, AXType);
  B->setTypeParameters(*emptyTypeParameters);
  auto BType = Type::create(heap, B);

  auto AYType = Type::create(heap, A, vector<Local<Type>>{YType});
  ASSERT_TRUE(Type::isSubtypeOf(BType, AXType));
  ASSERT_FALSE(Type::isSubtypeOf(BType, AYType));
}


TEST(SubtypeSuperclassWithParameters) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0, rootType, nothingType);

  auto A = Class::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  A->setTypeParameters(*emptyTypeParameters);
  setSupertypeToRoot(A);
  auto AType = Type::create(heap, A);

  auto B = Class::create(heap);
  auto BTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  BTypeParameters->set(0, *T);
  B->setTypeParameters(*BTypeParameters);
  setSupertype(B, AType);

  auto X = Class::create(heap);
  X->setTypeParameters(*emptyTypeParameters);
  setSupertypeToRoot(X);
  auto XType = Type::create(heap, X);

  auto BXType = Type::create(heap, B, vector<Local<Type>>{XType});
  ASSERT_TRUE(Type::isSubtypeOf(BXType, AType));
}


TEST(SubtypeClassWithCovariantParameter) {
  // Source[A] <: Source[B] with class Source[+T] and A <: B
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), COVARIANT_FLAG, rootType, nothingType);

  auto B = Class::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  B->setTypeParameters(*emptyTypeParameters);
  setSupertypeToRoot(B);
  auto BType = Type::create(heap, B);

  auto A = Class::create(heap);
  A->setTypeParameters(*emptyTypeParameters);
  setSupertype(A, BType);
  auto AType = Type::create(heap, A);

  auto Source = Class::create(heap);
  auto sourceTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  sourceTypeParameters->set(0, *T);
  Source->setTypeParameters(*sourceTypeParameters);
  setSupertypeToRoot(Source);

  auto SourceAType = Type::create(heap, Source, vector<Local<Type>>{AType});
  auto SourceBType = Type::create(heap, Source, vector<Local<Type>>{BType});
  ASSERT_TRUE(Type::isSubtypeOf(SourceAType, SourceBType));
  ASSERT_FALSE(Type::isSubtypeOf(SourceBType, SourceAType));
}


TEST(SubtypeClassWithContravariantParameter) {
  // Sink[A] <: Sink[B] with class Sink[-A] and A >: B
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), CONTRAVARIANT_FLAG, rootType, nothingType);

  auto A = Class::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  A->setTypeParameters(*emptyTypeParameters);
  setSupertypeToRoot(A);
  auto AType = Type::create(heap, A);

  auto B = Class::create(heap);
  B->setTypeParameters(*emptyTypeParameters);
  setSupertype(B, AType);
  auto BType = Type::create(heap, B);

  auto Sink = Class::create(heap);
  auto sinkTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  sinkTypeParameters->set(0, *T);
  Sink->setTypeParameters(*sinkTypeParameters);
  setSupertypeToRoot(Sink);

  auto SinkAType = Type::create(heap, Sink, vector<Local<Type>>{AType});
  auto SinkBType = Type::create(heap, Sink, vector<Local<Type>>{BType});
  ASSERT_TRUE(Type::isSubtypeOf(SinkAType, SinkBType));
  ASSERT_FALSE(Type::isSubtypeOf(SinkBType, SinkAType));
}


TEST(SubtypeClassTrait) {
  TEST_PROLOGUE

  auto T = Trait::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  T->setTypeParameters(*emptyTypeParameters);
  setSupertypeToRoot(T);
  auto TType = Type::create(heap, T);

  auto C = Class::create(heap);
  C->setTypeParameters(*emptyTypeParameters);
  auto supertypes = BlockArray<Type>::create(heap, 2);
  supertypes->set(0, T->supertypes()->get(0));
  supertypes->set(1, *TType);
  C->setSupertypes(*supertypes);
  auto CType = Type::create(heap, C);

  ASSERT_TRUE(Type::isSubtypeOf(CType, TType));
  ASSERT_FALSE(Type::isSubtypeOf(TType, CType));
}


TEST(SubtypeTraitClass) {
  TEST_PROLOGUE

  auto C = Class::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  C->setTypeParameters(*emptyTypeParameters);
  setSupertypeToRoot(C);
  auto CType = Type::create(heap, C);

  auto T = Trait::create(heap);
  T->setTypeParameters(*emptyTypeParameters);
  auto supertypes = BlockArray<Type>::create(heap, 2);
  supertypes->set(0, C->supertypes()->get(0));
  supertypes->set(1, *CType);
  T->setSupertypes(*supertypes);
  auto TType = Type::create(heap, T);

  ASSERT_TRUE(Type::isSubtypeOf(TType, CType));
  ASSERT_FALSE(Type::isSubtypeOf(CType, TType));
}


TEST(SubtypeTraitTrait) {
  TEST_PROLOGUE

  auto T1 = Trait::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  T1->setTypeParameters(*emptyTypeParameters);
  setSupertypeToRoot(T1);
  auto T1Type = Type::create(heap, T1);

  auto T2 = Trait::create(heap);
  T2->setTypeParameters(*emptyTypeParameters);
  auto supertypes = BlockArray<Type>::create(heap, 2);
  supertypes->set(0, T1->supertypes()->get(0));
  supertypes->set(1, *T1Type);
  T2->setSupertypes(*supertypes);
  auto T2Type = Type::create(heap, T2);

  ASSERT_TRUE(Type::isSubtypeOf(T2Type, T1Type));
  ASSERT_FALSE(Type::isSubtypeOf(T1Type, T2Type));
}


TEST(SubtypeTypeParameterTrait) {
  TEST_PROLOGUE

  auto T = Trait::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  T->setTypeParameters(*emptyTypeParameters);
  setSupertypeToRoot(T);
  auto TType = Type::create(heap, T);

  auto A = TypeParameter::create(heap, NAME("A"), STR("A"), 0,
                                 TType, handle(Type::nothingType(roots)));
  auto AType = Type::create(heap, A);
  ASSERT_TRUE(Type::isSubtypeOf(AType, TType));
  ASSERT_FALSE(Type::isSubtypeOf(TType, AType));

  auto B = TypeParameter::create(heap, NAME("B"), STR("B"), 0,
                                 handle(Type::rootClassType(roots)), TType);
  auto BType = Type::create(heap, B);
  ASSERT_TRUE(Type::isSubtypeOf(TType, BType));
  ASSERT_FALSE(Type::isSubtypeOf(BType, TType));
}


TEST(SubtypeEquivalentExistentials) {
  TEST_PROLOGUE

  auto X = TypeParameter::create(heap, NAME("X"), STR("X"), 0,
                                 handle(Type::rootClassType(roots)),
                                 handle(Type::nothingType(roots)));
  auto XType = Type::create(heap, X);
  auto eXType = Type::create(heap, vector<Local<TypeParameter>>{X}, XType);
  auto Y = TypeParameter::create(heap, NAME("Y"), STR("Y"), 0,
                                 handle(Type::rootClassType(roots)),
                                 handle(Type::nothingType(roots)));
  auto YType = Type::create(heap, Y);
  auto eYType = Type::create(heap, vector<Local<TypeParameter>>{Y}, YType);

  ASSERT_TRUE(Type::isSubtypeOf(eXType, eYType));
  ASSERT_TRUE(Type::isSubtypeOf(eYType, eXType));
}


TEST(SubtypeLeftExistential) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(roots));
  auto nothingType = handle(Type::nothingType(roots));

  auto Foo = Class::create(heap);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), COVARIANT_FLAG, rootType, nothingType);
  auto fooTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  fooTypeParameters->set(0, *T);
  Foo->setTypeParameters(*fooTypeParameters);
  setSupertypeToRoot(Foo);

  auto X = TypeParameter::create(heap, NAME("X"), STR("X"), 0, rootType, nothingType);
  auto XType = Type::create(heap, X);
  auto FooXType = Type::create(heap, Foo, vector<Local<Type>>{XType});
  auto eXFooType = Type::create(heap, vector<Local<TypeParameter>>{X}, FooXType);
  auto FooObjectType = Type::create(heap, Foo, vector<Local<Type>>{rootType});
  ASSERT_TRUE(Type::isSubtypeOf(eXFooType, FooObjectType));
}


TEST(SubtypeRightExistential) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(roots));
  auto stringType = handle(roots->getBuiltinType(BUILTIN_STRING_CLASS_ID));

  auto X = TypeParameter::create(heap, NAME("X"), STR("X"), NO_FLAGS, rootType, stringType);
  auto XType = Type::create(heap, X);
  auto eXType = Type::create(heap, vector<Local<TypeParameter>>{X}, XType);
  ASSERT_TRUE(Type::isSubtypeOf(stringType, eXType));
}


TEST(SubtypeRightParameterExistential) {
  // class C[T]
  // C[Object] <: forsome [X] C[X]

  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(roots));
  auto nothingType = handle(Type::nothingType(roots));

  auto Foo = Class::create(heap);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), NO_FLAGS, rootType, nothingType);
  auto fooTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  fooTypeParameters->set(0, *T);
  Foo->setTypeParameters(*fooTypeParameters);
  setSupertypeToRoot(Foo);

  auto FooObjectType = Type::create(heap, Foo, vector<Local<Type>>{rootType});

  auto X = TypeParameter::create(heap, NAME("X"), STR("X"), NO_FLAGS, rootType, nothingType);
  auto XType = Type::create(heap, X);
  auto FooXType = Type::create(heap, Foo, vector<Local<Type>>{XType});
  auto eFooXType = Type::create(heap, vector<Local<TypeParameter>>{X}, FooXType);

  ASSERT_TRUE(Type::isSubtypeOf(FooObjectType, eFooXType));
}


TEST(SubtypeRightExistentialFailUpperBound) {
  // String <: forsome [X <: String] X
  // Object </: forsome [X <: String] X

  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(roots));
  auto nothingType = handle(Type::nothingType(roots));
  auto stringType = handle(roots->getBuiltinType(BUILTIN_STRING_CLASS_ID));

  auto X = TypeParameter::create(heap, NAME("X"), STR("X"), NO_FLAGS, stringType, nothingType);
  auto XType = Type::create(heap, X);
  auto eXType = Type::create(heap, vector<Local<TypeParameter>>{X}, XType);

  ASSERT_TRUE(Type::isSubtypeOf(stringType, eXType));
  ASSERT_FALSE(Type::isSubtypeOf(rootType, eXType));
}


TEST(SubtypeRightExistentialFailLowerBound) {
  // String </: forsome [X >: Object] X
  // Object <: forsome [X >: Object] X

  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(roots));
  auto stringType = handle(roots->getBuiltinType(BUILTIN_STRING_CLASS_ID));

  auto X = TypeParameter::create(heap, NAME("X"), STR("X"), NO_FLAGS, rootType, rootType);
  auto XType = Type::create(heap, X);
  auto eXType = Type::create(heap, vector<Local<TypeParameter>>{X}, XType);

  ASSERT_FALSE(Type::isSubtypeOf(stringType, eXType));
  ASSERT_TRUE(Type::isSubtypeOf(rootType, eXType));
}


TEST(SubtypeRightExistentialSubstituteMultiple) {
  // class C[S, T]
  // C[String, Object] <: forsome [X] C[X, X]

  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(roots));
  auto nothingType = handle(Type::nothingType(roots));
  auto stringType = handle(roots->getBuiltinType(BUILTIN_STRING_CLASS_ID));

  auto C = Class::create(heap);
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0, rootType, nothingType);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0, rootType, nothingType);
  auto CTypeParameters = BlockArray<TypeParameter>::create(heap, 2);
  CTypeParameters->set(0, *S);
  CTypeParameters->set(1, *T);
  C->setTypeParameters(*CTypeParameters);
  setSupertypeToRoot(C);

  auto X = TypeParameter::create(heap, NAME("X"), STR("X"), 0, rootType, nothingType);
  auto XType = Type::create(heap, X);

  auto CType = Type::create(heap, C, vector<Local<Type>>{stringType, rootType});
  auto CXType = Type::create(heap, C, vector<Local<Type>>{XType, XType});
  auto eCXType = Type::create(heap, vector<Local<TypeParameter>>{X}, CXType);

  ASSERT_TRUE(Type::isSubtypeOf(CType, eCXType));
}


TEST(TypeGlbSame) {
  TEST_PROLOGUE

  auto i32Type = handle(Type::i32Type(roots));
  auto result = Type::glb(i32Type, i32Type);
  ASSERT_TRUE(i32Type->equals(*result));
}


TEST(TypeGlbSubtype) {
  TEST_PROLOGUE

  auto objectType = handle(Type::rootClassType(roots));
  auto stringType = handle(roots->getBuiltinType(BUILTIN_STRING_CLASS_ID));
  ASSERT_TRUE(stringType->equals(*Type::glb(objectType, stringType)));
  ASSERT_TRUE(stringType->equals(*Type::glb(stringType, objectType)));
}


TEST(TestGlbObjectTypes) {
  TEST_PROLOGUE

  auto stringType = handle(roots->getBuiltinType(BUILTIN_STRING_CLASS_ID));
  auto packageType = handle(roots->getBuiltinType(BUILTIN_PACKAGE_CLASS_ID));
  auto result = Type::glb(stringType, packageType);
  auto nothingType = handle(Type::nothingType(roots));
  ASSERT_TRUE(nothingType->equals(*result));

  auto nullableStringType = Type::createWithFlags(heap, stringType, Type::NULLABLE_FLAG);
  auto nullablePackageType = Type::createWithFlags(heap, packageType, Type::NULLABLE_FLAG);
  result = Type::glb(nullableStringType, nullablePackageType);
  auto nullType = handle(Type::nullType(roots));
  ASSERT_TRUE(nullType->equals(*result));
}


TEST(SubstituteTypeParameter) {
  TEST_PROLOGUE

  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto SType = Type::create(heap, S);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(heap, T);
  vector<pair<Local<TypeParameter>, Local<Type>>> bindings { { S, TType } };

  auto substituted = Type::substitute(SType, bindings);
  ASSERT_TRUE(TType->equals(*substituted));
}


TEST(SubstituteClassWithTypeParameter) {
  TEST_PROLOGUE

  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto C = Class::create(heap);
  auto typeParameters = BlockArray<TypeParameter>::create(heap, 1);
  typeParameters->set(0, *S);
  C->setTypeParameters(*typeParameters);
  vector<Local<Type>> CTypeArgs { Type::create(heap, S) };
  auto CType = Type::create(heap, C, CTypeArgs);

  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(heap, T);
  vector<pair<Local<TypeParameter>, Local<Type>>> bindings { { S, TType } };
  vector<Local<Type>> expectedTypeArgs { TType };
  auto expected = Type::create(heap, C, expectedTypeArgs);
  auto substituted = Type::substitute(CType, bindings);
  ASSERT_TRUE(expected->equals(*substituted));
}


TEST(SubstituteForInheritance) {
  TEST_PROLOGUE

  // Type parameters S, T, U
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto SType = Type::create(heap, S);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(heap, T);
  auto U = TypeParameter::create(heap, NAME("U"), STR("U"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto UType = Type::create(heap, U);

  // class A[S]
  //   var x: S
  auto A = Class::create(heap);
  auto ATypeParams = BlockArray<TypeParameter>::create(heap, 1);
  ATypeParams->set(0, *S);
  A->setTypeParameters(*ATypeParams);
  setSupertypeToRoot(A);

  // class B[T] <: A[T]
  auto B = Class::create(heap);
  auto BTypeParams = BlockArray<TypeParameter>::create(heap, 1);
  BTypeParams->set(0, *T);
  B->setTypeParameters(*BTypeParams);
  auto ATType = Type::create(heap, A, vector<Local<Type>>{TType});
  setSupertype(B, ATType);

  // class C[U] <: B[U]
  auto C = Class::create(heap);
  auto CTypeParams = BlockArray<TypeParameter>::create(heap, 1);
  CTypeParams->set(0, *U);
  C->setTypeParameters(*CTypeParams);
  auto BUType = Type::create(heap, B, vector<Local<Type>>{UType});
  setSupertype(C, BUType);

  // If we load x out of C, it should have type U.
  auto fieldType = Type::substituteForInheritance(SType, C, A);
  ASSERT_TRUE(UType->equals(*fieldType));
}

 // Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <vector>
#include "array.h"
#include "builtins.h"
#include "class.h"
#include "name.h"
#include "string.h"
#include "tagged.h"
#include "type.h"
#include "type-parameter.h"
#include "vm.h"

using namespace std;
using namespace codeswitch::internal;

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

  auto S = TypeParameter::create(heap, NAME("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto T = TypeParameter::create(heap, NAME("T"), 0,
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

  auto S = TypeParameter::create(heap, NAME("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto SType = Type::create(heap, S);
  auto T = TypeParameter::create(heap, NAME("T"), 0,
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

  auto T = TypeParameter::create(heap, NAME("T"), 0,
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
  A->setSupertype(Type::rootClassType(vm.roots()));
  A->setTypeParameters(*emptyTypeParameters);
  auto AType = Type::create(heap, A);
  auto B = Class::create(heap);
  B->setSupertype(*AType);
  B->setTypeParameters(*emptyTypeParameters);
  auto BType = Type::create(heap, B);
  auto C = Class::create(heap);
  C->setSupertype(*BType);
  C->setTypeParameters(*emptyTypeParameters);
  auto CType = Type::create(heap, C);

  auto T = TypeParameter::create(heap, NAME("T"), 0, AType, CType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), 0, BType, CType);
  auto SType = Type::create(heap, S);
  ASSERT_FALSE(Type::isSubtypeOf(SType, TType));
}


TEST(SubtypeParametersNonOverlapping) {
  TEST_PROLOGUE

  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  auto A = Class::create(heap);
  A->setSupertype(Type::rootClassType(vm.roots()));
  A->setTypeParameters(*emptyTypeParameters);
  auto AType = Type::create(heap, A);
  auto B = Class::create(heap);
  B->setSupertype(*AType);
  B->setTypeParameters(*emptyTypeParameters);
  auto BType = Type::create(heap, B);
  auto C = Class::create(heap);
  C->setSupertype(*BType);
  C->setTypeParameters(*emptyTypeParameters);
  auto CType = Type::create(heap, C);

  auto T = TypeParameter::create(heap, NAME("T"), 0, AType, BType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), 0, BType, CType);
  auto SType = Type::create(heap, S);
  ASSERT_TRUE(Type::isSubtypeOf(SType, TType));
}


TEST(SubtypeParametersTransitiveUpper) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto U = TypeParameter::create(heap, NAME("U"), 0, rootType, nothingType);
  auto UType = Type::create(heap, U);
  auto T = TypeParameter::create(heap, NAME("T"), 0, UType, nothingType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), 0, TType, nothingType);
  auto SType = Type::create(heap, S);

  ASSERT_TRUE(Type::isSubtypeOf(SType, UType));
}


TEST(SubtypeParametersTransitiveLower) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto U = TypeParameter::create(heap, NAME("U"), 0, rootType, nothingType);
  auto UType = Type::create(heap, U);
  auto T = TypeParameter::create(heap, NAME("T"), 0, rootType, UType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), 0, rootType, TType);
  auto SType = Type::create(heap, S);

  ASSERT_TRUE(Type::isSubtypeOf(UType, SType));
}


TEST(SubtypeParametersTransitiveMiddle) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto M = TypeParameter::create(heap, NAME("M"), 0, rootType, nothingType);
  auto MType = Type::create(heap, M);
  auto S = TypeParameter::create(heap, NAME("S"), 0, MType, nothingType);
  auto SType = Type::create(heap, S);
  auto T = TypeParameter::create(heap, NAME("T"), 0, rootType, MType);
  auto TType = Type::create(heap, T);

  ASSERT_TRUE(Type::isSubtypeOf(SType, TType));
}


TEST(SubtypeClassWithParametersSelf) {
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(heap, NAME("T"), 0, rootType, nothingType);
  auto TType = Type::create(heap, T);
  auto S = TypeParameter::create(heap, NAME("S"), 0, rootType, nothingType);
  auto SType = Type::create(heap, S);

  auto A = Class::create(heap);
  A->setSupertype(*rootType);
  auto ATypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  ATypeParameters->set(0, *T);
  A->setTypeParameters(*ATypeParameters);

  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  auto X = Class::create(heap);
  X->setSupertype(*rootType);
  X->setTypeParameters(*emptyTypeParameters);
  auto XType = Type::create(heap, X);
  auto Y = Class::create(heap);
  Y->setSupertype(*rootType);
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
  auto T = TypeParameter::create(heap, NAME("T"), 0, rootType, nothingType);

  auto A = Class::create(heap);
  A->setSupertype(*rootType);
  auto ATypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  ATypeParameters->set(0, *T);
  A->setTypeParameters(*ATypeParameters);

  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  auto X = Class::create(heap);
  X->setSupertype(*rootType);
  X->setTypeParameters(*emptyTypeParameters);
  auto XType = Type::create(heap, X);

  auto Y = Class::create(heap);
  Y->setSupertype(*rootType);
  Y->setTypeParameters(*emptyTypeParameters);
  auto YType = Type::create(heap, Y);

  auto B = Class::create(heap);
  auto AXType = Type::create(heap, A, vector<Local<Type>>{XType});
  B->setSupertype(*AXType);
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
  auto T = TypeParameter::create(heap, NAME("T"), 0, rootType, nothingType);

  auto A = Class::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  A->setTypeParameters(*emptyTypeParameters);
  A->setSupertype(*rootType);
  auto AType = Type::create(heap, A);

  auto B = Class::create(heap);
  auto BTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  BTypeParameters->set(0, *T);
  B->setTypeParameters(*BTypeParameters);
  B->setSupertype(*AType);

  auto X = Class::create(heap);
  X->setTypeParameters(*emptyTypeParameters);
  X->setSupertype(*rootType);
  auto XType = Type::create(heap, X);

  auto BXType = Type::create(heap, B, vector<Local<Type>>{XType});
  ASSERT_TRUE(Type::isSubtypeOf(BXType, AType));
}


TEST(SubtypeClassWithCovariantParameter) {
  // Source[A] <: Source[B] with class Source[+T] and A <: B
  TEST_PROLOGUE

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(heap, NAME("T"), COVARIANT_FLAG, rootType, nothingType);

  auto B = Class::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  B->setTypeParameters(*emptyTypeParameters);
  B->setSupertype(*rootType);
  auto BType = Type::create(heap, B);

  auto A = Class::create(heap);
  A->setTypeParameters(*emptyTypeParameters);
  A->setSupertype(*BType);
  auto AType = Type::create(heap, A);

  auto Source = Class::create(heap);
  auto sourceTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  sourceTypeParameters->set(0, *T);
  Source->setTypeParameters(*sourceTypeParameters);
  Source->setSupertype(*rootType);

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
  auto T = TypeParameter::create(heap, NAME("T"), CONTRAVARIANT_FLAG, rootType, nothingType);

  auto A = Class::create(heap);
  auto emptyTypeParameters = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  A->setTypeParameters(*emptyTypeParameters);
  A->setSupertype(*rootType);
  auto AType = Type::create(heap, A);

  auto B = Class::create(heap);
  B->setTypeParameters(*emptyTypeParameters);
  B->setSupertype(*AType);
  auto BType = Type::create(heap, B);

  auto Sink = Class::create(heap);
  auto sinkTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  sinkTypeParameters->set(0, *T);
  Sink->setTypeParameters(*sinkTypeParameters);
  Sink->setSupertype(*rootType);

  auto SinkAType = Type::create(heap, Sink, vector<Local<Type>>{AType});
  auto SinkBType = Type::create(heap, Sink, vector<Local<Type>>{BType});
  ASSERT_TRUE(Type::isSubtypeOf(SinkAType, SinkBType));
  ASSERT_FALSE(Type::isSubtypeOf(SinkBType, SinkAType));
}


TEST(SubtypeEquivalentExistentials) {
  TEST_PROLOGUE

  auto X = TypeParameter::create(heap, NAME("X"), 0,
                                 handle(Type::rootClassType(roots)),
                                 handle(Type::nothingType(roots)));
  auto XType = Type::create(heap, X);
  auto eXType = Type::create(heap, vector<Local<TypeParameter>>{X}, XType);
  auto Y = TypeParameter::create(heap, NAME("Y"), 0,
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
  auto T = TypeParameter::create(heap, NAME("T"), COVARIANT_FLAG, rootType, nothingType);
  auto fooTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  fooTypeParameters->set(0, *T);
  Foo->setTypeParameters(*fooTypeParameters);
  Foo->setSupertype(Type::rootClassType(roots));

  auto X = TypeParameter::create(heap, NAME("X"), 0, rootType, nothingType);
  auto XType = Type::create(heap, X);
  auto FooXType = Type::create(heap, Foo, vector<Local<Type>>{XType});
  auto eXFooType = Type::create(heap, vector<Local<TypeParameter>>{X}, FooXType);
  auto FooObjectType = Type::create(heap, Foo, vector<Local<Type>>{rootType});
  ASSERT_TRUE(Type::isSubtypeOf(eXFooType, FooObjectType));
}

TEST(SubstituteTypeParameter) {
  TEST_PROLOGUE

  auto S = TypeParameter::create(heap, NAME("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto SType = Type::create(heap, S);
  auto T = TypeParameter::create(heap, NAME("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(heap, T);
  vector<pair<Local<TypeParameter>, Local<Type>>> bindings { { S, TType } };

  auto substituted = Type::substitute(SType, bindings);
  ASSERT_TRUE(TType->equals(*substituted));
}


TEST(SubstituteClassWithTypeParameter) {
  TEST_PROLOGUE

  auto S = TypeParameter::create(heap, NAME("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto C = Class::create(heap);
  auto typeParameters = BlockArray<TypeParameter>::create(heap, 1);
  typeParameters->set(0, *S);
  C->setTypeParameters(*typeParameters);
  vector<Local<Type>> CTypeArgs { Type::create(heap, S) };
  auto CType = Type::create(heap, C, CTypeArgs);

  auto T = TypeParameter::create(heap, NAME("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(heap, T);
  vector<pair<Local<TypeParameter>, Local<Type>>> bindings { { S, TType } };
  vector<Local<Type>> expectedTypeArgs { TType };
  auto expected = Type::create(heap, C, expectedTypeArgs);
  auto substituted = Type::substitute(CType, bindings);
  ASSERT_TRUE(expected->equals(*substituted));
}


TEST(SubstituteForBaseClass) {
  TEST_PROLOGUE

  // class A[T]
  auto T = TypeParameter::create(heap, NAME("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto ATypeParams = BlockArray<TypeParameter>::create(heap, 1);
  ATypeParams->set(0, *T);
  auto A = Class::create(heap);
  A->setTypeParameters(*ATypeParams);
  A->setSupertype(Type::rootClassType(vm.roots()));

  // class B[U] <: A[U]
  auto U = TypeParameter::create(heap, NAME("U"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto BTypeParams = BlockArray<TypeParameter>::create(heap, 1);
  BTypeParams->set(0, *U);
  auto B = Class::create(heap);
  B->setTypeParameters(*BTypeParams);
  vector<Local<Type>> BSupertypeArgs { Type::create(heap, U) };
  auto BSupertype = Type::create(heap, A, BSupertypeArgs);
  B->setSupertype(*BSupertype);

  // class C
  auto C = Class::create(heap);
  auto emptyTypeParams = handle(reinterpret_cast<BlockArray<TypeParameter>*>(
      vm.roots()->emptyBlockArray()));
  C->setTypeParameters(*emptyTypeParams);

  // class D <: B[C]
  auto D = Class::create(heap);
  D->setTypeParameters(*emptyTypeParams);
  vector<Local<Type>> DSupertypeArgs { Type::create(heap, C) };
  auto DSupertype = Type::create(heap, B, DSupertypeArgs);
  D->setSupertype(*DSupertype);

  // V <: D
  auto V = TypeParameter::create(heap, NAME("V"), 0,
                                 Type::create(heap, D),
                                 handle(Type::nothingType(vm.roots())));
  auto VType = Type::create(heap, V);

  vector<Local<Type>> expectedTypeArgs { Type::create(heap, C) };
  auto expected = Type::create(heap, A, expectedTypeArgs);
  auto actual = Type::substituteForBaseClass(VType, A);
  ASSERT_TRUE(expected->equals(*actual));
}


TEST(SubstituteForInheritance) {
  TEST_PROLOGUE

  // Type parameters S, T, U
  auto S = TypeParameter::create(heap, NAME("S"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto SType = Type::create(heap, S);
  auto T = TypeParameter::create(heap, NAME("T"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(heap, T);
  auto U = TypeParameter::create(heap, NAME("U"), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto UType = Type::create(heap, U);

  // class A[S]
  //   var x: S
  auto A = Class::create(heap);
  auto ATypeParams = BlockArray<TypeParameter>::create(heap, 1);
  ATypeParams->set(0, *S);
  A->setTypeParameters(*ATypeParams);
  A->setSupertype(Type::rootClassType(vm.roots()));

  // class B[T] <: A[T]
  auto B = Class::create(heap);
  auto BTypeParams = BlockArray<TypeParameter>::create(heap, 1);
  BTypeParams->set(0, *T);
  B->setTypeParameters(*BTypeParams);
  auto ATType = Type::create(heap, A, vector<Local<Type>>{TType});
  B->setSupertype(*ATType);

  // class C[U] <: B[U]
  auto C = Class::create(heap);
  auto CTypeParams = BlockArray<TypeParameter>::create(heap, 1);
  CTypeParams->set(0, *U);
  C->setTypeParameters(*CTypeParams);
  auto BUType = Type::create(heap, B, vector<Local<Type>>{UType});
  C->setSupertype(*BUType);

  // If we load x out of C, it should have type U.
  auto fieldType = Type::substituteForInheritance(SType, C, A);
  ASSERT_TRUE(UType->equals(*fieldType));
}

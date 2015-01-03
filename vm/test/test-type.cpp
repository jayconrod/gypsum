// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <vector>
#include "array.h"
#include "builtins.h"
#include "class.h"
#include "tagged.h"
#include "type.h"
#include "type-parameter.h"
#include "vm.h"

using namespace std;
using namespace codeswitch::internal;

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
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto S = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto T = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto C = Class::create(vm.heap());
  auto typeParameters = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  typeParameters->set(0, tag(*S));
  C->setTypeParameters(*typeParameters);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto SType = Type::create(vm.heap(), S);
  auto TType = Type::create(vm.heap(), T);
  auto CSType = Type::create(vm.heap(), C, vector<Local<Type>>{SType});
  auto CTType = Type::create(vm.heap(), C, vector<Local<Type>>{TType});

  ASSERT_TRUE(rootType->equals(*rootType));
  ASSERT_FALSE(rootType->equals(*CSType));
  ASSERT_FALSE(CSType->equals(*rootType));
  ASSERT_FALSE(CSType->equals(*CTType));
  ASSERT_TRUE(CSType->equals(*CSType));
  ASSERT_TRUE(CTType->equals(*CTType));
}


TEST(VariableTypeEquals) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto S = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto SType = Type::create(vm.heap(), S);
  auto T = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(vm.heap(), T);

  ASSERT_TRUE(SType->equals(*SType));
  ASSERT_TRUE(TType->equals(*TType));
  ASSERT_FALSE(SType->equals(*TType));
}


TEST(SubtypePrimitives) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);
  auto unitType = handle(Type::unitType(vm.roots()));
  auto i8Type = handle(Type::i8Type(vm.roots()));
  auto rootType = handle(Type::rootClassType(vm.roots()));
  ASSERT_TRUE(Type::isSubtypeOf(unitType, unitType));
  ASSERT_FALSE(Type::isSubtypeOf(unitType, i8Type));
  ASSERT_FALSE(Type::isSubtypeOf(unitType, rootType));
}


TEST(SubtypeClassParent) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto stringClass = handle(vm.roots()->getBuiltinClass(BUILTIN_STRING_CLASS_ID));
  auto stringType = Type::create(vm.heap(), stringClass);

  ASSERT_TRUE(Type::isSubtypeOf(rootType, rootType));
  ASSERT_TRUE(Type::isSubtypeOf(stringType, stringType));
  ASSERT_TRUE(Type::isSubtypeOf(stringType, rootType));
  ASSERT_FALSE(Type::isSubtypeOf(rootType, stringType));
}


TEST(SubtypeClassNothing) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto stringClass = handle(vm.roots()->getBuiltinClass(BUILTIN_STRING_CLASS_ID));
  auto stringType = Type::create(vm.heap(), stringClass);
  auto nothingType = handle(Type::nothingType(vm.roots()));

  ASSERT_TRUE(Type::isSubtypeOf(nothingType, rootType));
  ASSERT_TRUE(Type::isSubtypeOf(nothingType, stringType));
  ASSERT_TRUE(Type::isSubtypeOf(nothingType, nothingType));
}


TEST(SubtypeParameterSimple) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto T = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(vm.heap(), T);
  ASSERT_TRUE(Type::isSubtypeOf(TType, TType));
  ASSERT_TRUE(Type::isSubtypeOf(TType, handle(T->upperBound())));
  ASSERT_TRUE(Type::isSubtypeOf(handle(T->lowerBound()), TType));
}


TEST(SubtypeParametersOverlapping) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto emptyTypeParameters = handle(reinterpret_cast<TaggedArray<TypeParameter>*>(
      vm.roots()->emptyTaggedArray()));
  auto A = Class::create(vm.heap());
  A->setSupertype(Type::rootClassType(vm.roots()));
  A->setTypeParameters(*emptyTypeParameters);
  auto AType = Type::create(vm.heap(), A);
  auto B = Class::create(vm.heap());
  B->setSupertype(*AType);
  B->setTypeParameters(*emptyTypeParameters);
  auto BType = Type::create(vm.heap(), B);
  auto C = Class::create(vm.heap());
  C->setSupertype(*BType);
  C->setTypeParameters(*emptyTypeParameters);
  auto CType = Type::create(vm.heap(), C);

  auto T = TypeParameter::create(vm.heap(), 0, AType, CType);
  auto TType = Type::create(vm.heap(), T);
  auto S = TypeParameter::create(vm.heap(), 0, BType, CType);
  auto SType = Type::create(vm.heap(), S);
  ASSERT_FALSE(Type::isSubtypeOf(SType, TType));
}


TEST(SubtypeParametersNonOverlapping) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto emptyTypeParameters = handle(reinterpret_cast<TaggedArray<TypeParameter>*>(
      vm.roots()->emptyTaggedArray()));
  auto A = Class::create(vm.heap());
  A->setSupertype(Type::rootClassType(vm.roots()));
  A->setTypeParameters(*emptyTypeParameters);
  auto AType = Type::create(vm.heap(), A);
  auto B = Class::create(vm.heap());
  B->setSupertype(*AType);
  B->setTypeParameters(*emptyTypeParameters);
  auto BType = Type::create(vm.heap(), B);
  auto C = Class::create(vm.heap());
  C->setSupertype(*BType);
  C->setTypeParameters(*emptyTypeParameters);
  auto CType = Type::create(vm.heap(), C);

  auto T = TypeParameter::create(vm.heap(), 0, AType, BType);
  auto TType = Type::create(vm.heap(), T);
  auto S = TypeParameter::create(vm.heap(), 0, BType, CType);
  auto SType = Type::create(vm.heap(), S);
  ASSERT_TRUE(Type::isSubtypeOf(SType, TType));
}


TEST(SubtypeParametersTransitiveUpper) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto U = TypeParameter::create(vm.heap(), 0, rootType, nothingType);
  auto UType = Type::create(vm.heap(), U);
  auto T = TypeParameter::create(vm.heap(), 0, UType, nothingType);
  auto TType = Type::create(vm.heap(), T);
  auto S = TypeParameter::create(vm.heap(), 0, TType, nothingType);
  auto SType = Type::create(vm.heap(), S);

  ASSERT_TRUE(Type::isSubtypeOf(SType, UType));
}


TEST(SubtypeParametersTransitiveLower) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto U = TypeParameter::create(vm.heap(), 0, rootType, nothingType);
  auto UType = Type::create(vm.heap(), U);
  auto T = TypeParameter::create(vm.heap(), 0, rootType, UType);
  auto TType = Type::create(vm.heap(), T);
  auto S = TypeParameter::create(vm.heap(), 0, rootType, TType);
  auto SType = Type::create(vm.heap(), S);

  ASSERT_TRUE(Type::isSubtypeOf(UType, SType));
}


TEST(SubtypeParametersTransitiveMiddle) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto M = TypeParameter::create(vm.heap(), 0, rootType, nothingType);
  auto MType = Type::create(vm.heap(), M);
  auto S = TypeParameter::create(vm.heap(), 0, MType, nothingType);
  auto SType = Type::create(vm.heap(), S);
  auto T = TypeParameter::create(vm.heap(), 0, rootType, MType);
  auto TType = Type::create(vm.heap(), T);

  ASSERT_TRUE(Type::isSubtypeOf(SType, TType));
}


TEST(SubtypeClassWithParametersSelf) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(vm.heap(), 0, rootType, nothingType);
  auto TType = Type::create(vm.heap(), T);
  auto S = TypeParameter::create(vm.heap(), 0, rootType, nothingType);
  auto SType = Type::create(vm.heap(), S);

  auto A = Class::create(vm.heap());
  A->setSupertype(*rootType);
  auto ATypeParameters = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  ATypeParameters->set(0, tag(*T));
  A->setTypeParameters(*ATypeParameters);

  auto emptyTypeParameters = handle(reinterpret_cast<TaggedArray<TypeParameter>*>(
      vm.roots()->emptyTaggedArray()));
  auto X = Class::create(vm.heap());
  X->setSupertype(*rootType);
  X->setTypeParameters(*emptyTypeParameters);
  auto XType = Type::create(vm.heap(), X);
  auto Y = Class::create(vm.heap());
  Y->setSupertype(*rootType);
  Y->setTypeParameters(*emptyTypeParameters);
  auto YType = Type::create(vm.heap(), Y);

  vector<Local<Type>> ATTypeArgs { Type::create(vm.heap(), T) };
  auto ATType = Type::create(vm.heap(), A, vector<Local<Type>>{TType});
  auto ASType = Type::create(vm.heap(), A, vector<Local<Type>>{SType});
  auto AXType = Type::create(vm.heap(), A, vector<Local<Type>>{XType});
  auto AYType = Type::create(vm.heap(), A, vector<Local<Type>>{YType});

  ASSERT_TRUE(Type::isSubtypeOf(ATType, ATType));
  ASSERT_FALSE(Type::isSubtypeOf(ATType, ASType));
  ASSERT_TRUE(Type::isSubtypeOf(AXType, AXType));
  ASSERT_FALSE(Type::isSubtypeOf(AXType, AYType));
}


TEST(SubtypeSubclassWithParameters) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(vm.heap(), 0, rootType, nothingType);

  auto A = Class::create(vm.heap());
  A->setSupertype(*rootType);
  auto ATypeParameters = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  ATypeParameters->set(0, tag(*T));
  A->setTypeParameters(*ATypeParameters);

  auto emptyTypeParameters = handle(reinterpret_cast<TaggedArray<TypeParameter>*>(
      vm.roots()->emptyTaggedArray()));
  auto X = Class::create(vm.heap());
  X->setSupertype(*rootType);
  X->setTypeParameters(*emptyTypeParameters);
  auto XType = Type::create(vm.heap(), X);

  auto Y = Class::create(vm.heap());
  Y->setSupertype(*rootType);
  Y->setTypeParameters(*emptyTypeParameters);
  auto YType = Type::create(vm.heap(), Y);

  auto B = Class::create(vm.heap());
  auto AXType = Type::create(vm.heap(), A, vector<Local<Type>>{XType});
  B->setSupertype(*AXType);
  B->setTypeParameters(*emptyTypeParameters);
  auto BType = Type::create(vm.heap(), B);

  auto AYType = Type::create(vm.heap(), A, vector<Local<Type>>{YType});
  ASSERT_TRUE(Type::isSubtypeOf(BType, AXType));
  ASSERT_FALSE(Type::isSubtypeOf(BType, AYType));
}


TEST(SubtypeSuperclassWithParameters) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(vm.heap(), 0, rootType, nothingType);

  auto A = Class::create(vm.heap());
  auto emptyTypeParameters = handle(reinterpret_cast<TaggedArray<TypeParameter>*>(
      vm.roots()->emptyTaggedArray()));
  A->setTypeParameters(*emptyTypeParameters);
  A->setSupertype(*rootType);
  auto AType = Type::create(vm.heap(), A);

  auto B = Class::create(vm.heap());
  auto BTypeParameters = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  BTypeParameters->set(0, tag(*T));
  B->setTypeParameters(*BTypeParameters);
  B->setSupertype(*AType);

  auto X = Class::create(vm.heap());
  X->setTypeParameters(*emptyTypeParameters);
  X->setSupertype(*rootType);
  auto XType = Type::create(vm.heap(), X);

  auto BXType = Type::create(vm.heap(), B, vector<Local<Type>>{XType});
  ASSERT_TRUE(Type::isSubtypeOf(BXType, AType));
}


TEST(SubtypeClassWithCovariantParameter) {
  // Source[A] <: Source[B] with class Source[+T] and A <: B
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(vm.heap(), COVARIANT_FLAG, rootType, nothingType);

  auto B = Class::create(vm.heap());
  auto emptyTypeParameters = handle(reinterpret_cast<TaggedArray<TypeParameter>*>(
      vm.roots()->emptyTaggedArray()));
  B->setTypeParameters(*emptyTypeParameters);
  B->setSupertype(*rootType);
  auto BType = Type::create(vm.heap(), B);

  auto A = Class::create(vm.heap());
  A->setTypeParameters(*emptyTypeParameters);
  A->setSupertype(*BType);
  auto AType = Type::create(vm.heap(), A);

  auto Source = Class::create(vm.heap());
  auto sourceTypeParameters = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  sourceTypeParameters->set(0, tag(*T));
  Source->setTypeParameters(*sourceTypeParameters);
  Source->setSupertype(*rootType);

  auto SourceAType = Type::create(vm.heap(), Source, vector<Local<Type>>{AType});
  auto SourceBType = Type::create(vm.heap(), Source, vector<Local<Type>>{BType});
  ASSERT_TRUE(Type::isSubtypeOf(SourceAType, SourceBType));
  ASSERT_FALSE(Type::isSubtypeOf(SourceBType, SourceAType));
}


TEST(SubtypeClassWithContravariantParameter) {
  // Sink[A] <: Sink[B] with class Sink[-A] and A >: B
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto rootType = handle(Type::rootClassType(vm.roots()));
  auto nothingType = handle(Type::nothingType(vm.roots()));
  auto T = TypeParameter::create(vm.heap(), CONTRAVARIANT_FLAG, rootType, nothingType);

  auto A = Class::create(vm.heap());
  auto emptyTypeParameters = handle(reinterpret_cast<TaggedArray<TypeParameter>*>(
      vm.roots()->emptyTaggedArray()));
  A->setTypeParameters(*emptyTypeParameters);
  A->setSupertype(*rootType);
  auto AType = Type::create(vm.heap(), A);

  auto B = Class::create(vm.heap());
  B->setTypeParameters(*emptyTypeParameters);
  B->setSupertype(*AType);
  auto BType = Type::create(vm.heap(), B);

  auto Sink = Class::create(vm.heap());
  auto sinkTypeParameters = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  sinkTypeParameters->set(0, tag(*T));
  Sink->setTypeParameters(*sinkTypeParameters);
  Sink->setSupertype(*rootType);

  auto SinkAType = Type::create(vm.heap(), Sink, vector<Local<Type>>{AType});
  auto SinkBType = Type::create(vm.heap(), Sink, vector<Local<Type>>{BType});
  ASSERT_TRUE(Type::isSubtypeOf(SinkAType, SinkBType));
  ASSERT_FALSE(Type::isSubtypeOf(SinkBType, SinkAType));
}


TEST(SubstituteTypeParameter) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto S = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto SType = Type::create(vm.heap(), S);
  auto T = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(vm.heap(), T);
  vector<pair<Local<TypeParameter>, Local<Type>>> bindings { { S, TType } };

  auto substituted = Type::substitute(SType, bindings);
  ASSERT_TRUE(TType->equals(*substituted));
}


TEST(SubstituteClassWithTypeParameter) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  auto S = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto C = Class::create(vm.heap());
  auto typeParameters = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  typeParameters->set(0, tag(*S));
  C->setTypeParameters(*typeParameters);
  vector<Local<Type>> CTypeArgs { Type::create(vm.heap(), S) };
  auto CType = Type::create(vm.heap(), C, CTypeArgs);

  auto T = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(vm.heap(), T);
  vector<pair<Local<TypeParameter>, Local<Type>>> bindings { { S, TType } };
  vector<Local<Type>> expectedTypeArgs { TType };
  auto expected = Type::create(vm.heap(), C, expectedTypeArgs);
  auto substituted = Type::substitute(CType, bindings);
  ASSERT_TRUE(expected->equals(*substituted));
}


TEST(SubstituteForBaseClass) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  // class A[T]
  auto T = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto ATypeParams = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  ATypeParams->set(0, tag(*T));
  auto A = Class::create(vm.heap());
  A->setTypeParameters(*ATypeParams);
  A->setSupertype(Type::rootClassType(vm.roots()));

  // class B[U] <: A[U]
  auto U = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto BTypeParams = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  BTypeParams->set(0, tag(*U));
  auto B = Class::create(vm.heap());
  B->setTypeParameters(*BTypeParams);
  vector<Local<Type>> BSupertypeArgs { Type::create(vm.heap(), U) };
  auto BSupertype = Type::create(vm.heap(), A, BSupertypeArgs);
  B->setSupertype(*BSupertype);

  // class C
  auto C = Class::create(vm.heap());
  auto emptyTypeParams = handle(reinterpret_cast<TaggedArray<TypeParameter>*>(
      vm.roots()->emptyTaggedArray()));
  C->setTypeParameters(*emptyTypeParams);

  // class D <: B[C]
  auto D = Class::create(vm.heap());
  D->setTypeParameters(*emptyTypeParams);
  vector<Local<Type>> DSupertypeArgs { Type::create(vm.heap(), C) };
  auto DSupertype = Type::create(vm.heap(), B, DSupertypeArgs);
  D->setSupertype(*DSupertype);

  // V <: D
  auto V = TypeParameter::create(vm.heap(), 0,
                                 Type::create(vm.heap(), D),
                                 handle(Type::nothingType(vm.roots())));
  auto VType = Type::create(vm.heap(), V);

  vector<Local<Type>> expectedTypeArgs { Type::create(vm.heap(), C) };
  auto expected = Type::create(vm.heap(), A, expectedTypeArgs);
  auto actual = Type::substituteForBaseClass(VType, A);
  ASSERT_TRUE(expected->equals(*actual));
}


TEST(SubstituteForInheritance) {
  VM vm;
  HandleScope handleScope(&vm);
  AllowAllocationScope allowAllocation(vm.heap(), true);

  // Type parameters S, T, U
  auto S = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto SType = Type::create(vm.heap(), S);
  auto T = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto TType = Type::create(vm.heap(), T);
  auto U = TypeParameter::create(vm.heap(), 0,
                                 handle(Type::rootClassType(vm.roots())),
                                 handle(Type::nothingType(vm.roots())));
  auto UType = Type::create(vm.heap(), U);

  // class A[S]
  //   var x: S
  auto A = Class::create(vm.heap());
  auto ATypeParams = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  ATypeParams->set(0, tag(*S));
  A->setTypeParameters(*ATypeParams);
  A->setSupertype(Type::rootClassType(vm.roots()));

  // class B[T] <: A[T]
  auto B = Class::create(vm.heap());
  auto BTypeParams = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  BTypeParams->set(0, tag(*T));
  B->setTypeParameters(*BTypeParams);
  auto ATType = Type::create(vm.heap(), A, vector<Local<Type>>{TType});
  B->setSupertype(*ATType);

  // class C[U] <: B[U]
  auto C = Class::create(vm.heap());
  auto CTypeParams = TaggedArray<TypeParameter>::create(vm.heap(), 1);
  CTypeParams->set(0, tag(*U));
  C->setTypeParameters(*CTypeParams);
  auto BUType = Type::create(vm.heap(), B, vector<Local<Type>>{UType});
  C->setSupertype(*BUType);

  // If we load x out of C, it should have type U.
  auto fieldType = Type::substituteForInheritance(SType, C, A);
  ASSERT_TRUE(UType->equals(*fieldType));
}

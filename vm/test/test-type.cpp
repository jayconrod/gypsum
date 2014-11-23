// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <vector>
#include "array.h"
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

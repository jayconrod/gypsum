// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <string>

#include "array.h"
#include "string.h"
#include "function.h"
#include "index.h"
#include "name.h"
#include "package.h"
#include "type.h"
#include "type-parameter.h"

using namespace std;
using namespace codeswitch::internal;

TEST(TestMangleFunctionNameSimple) {
  TEST_PROLOGUE

  auto package = Package::create(heap, 0);
  auto typeParameters = BlockArray<TypeParameter>::create(heap, 0);
  auto parameterTypes = BlockArray<Type>::create(heap, 8);
  parameterTypes->set(0, Type::unitType(roots));
  parameterTypes->set(1, Type::booleanType(roots));
  parameterTypes->set(2, Type::i8Type(roots));
  parameterTypes->set(3, Type::i16Type(roots));
  parameterTypes->set(4, Type::i32Type(roots));
  parameterTypes->set(5, Type::i64Type(roots));
  parameterTypes->set(6, Type::f32Type(roots));
  parameterTypes->set(7, Type::f64Type(roots));
  auto function = Function::create(heap, FID0);
  function->setName(*NAME("foo.bar.baz"));
  function->setTypeParameters(*typeParameters);
  function->setParameterTypes(*parameterTypes);

  auto mangledName = mangleFunctionName(function, package);
  auto actual = Name::toString(heap, mangledName)->toUtf8StlString();
  string expected("foo.bar.baz(U,Z,B,S,I,L,F,D)");
  ASSERT_EQ(expected, actual);
}


TEST(TestMangleFunctionNameClasses) {
  TEST_PROLOGUE

  auto upper = handle(Type::rootClassType(roots));
  auto lower = handle(Type::nothingType(roots));

  auto package = Package::create(heap, 0);
  auto P = TypeParameter::create(heap, NAME("P"), STR("P"), NO_FLAGS, upper, lower);
  auto Q = TypeParameter::create(heap, NAME("Q"), STR("Q"), NO_FLAGS, upper, lower);
  auto localClass = Class::create(heap, CID(0));
  localClass->setName(*NAME("local.Local"));
  localClass->setPackage(*package);
  auto localTypeParameters = BlockArray<TypeParameter>::create(heap, 2);
  localTypeParameters->set(0, *P);
  localTypeParameters->set(1, *Q);
  auto otherPackage = Package::create(heap, 1);
  otherPackage->setName(*NAME("foo.bar.baz"));
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), NO_FLAGS, upper, lower);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), NO_FLAGS, upper, lower);
  auto foreignClass = Class::create(heap, CID(1));
  foreignClass->setName(*NAME("foreign.Foreign"));
  auto foreignTypeParameters = BlockArray<TypeParameter>::create(heap, 2);
  foreignTypeParameters->set(0, *S);
  foreignTypeParameters->set(1, *T);
  foreignClass->setPackage(*otherPackage);

  auto X = TypeParameter::create(heap, NAME("X"), STR("X"), STATIC_FLAG, upper, lower);
  auto XType = Type::create(heap, X);
  auto Y = TypeParameter::create(heap, NAME("Y"), STR("Y"), NO_FLAGS, upper, lower);
  auto YType = Type::create(heap, Y, Type::NULLABLE_FLAG);
  auto localType = Type::create(heap, localClass, vector<Local<Type>>{XType, YType});
  auto foreignType = Type::create(heap, foreignClass, vector<Local<Type>>{YType, XType},
                                  Type::NULLABLE_FLAG);
  auto BuiltinType = handle(Type::rootClassType(roots));

  auto function = Function::create(heap, FID0);
  function->setName(*NAME("quux"));
  auto functionTypeParameters = BlockArray<TypeParameter>::create(heap, 2);
  functionTypeParameters->set(0, *X);
  functionTypeParameters->set(1, *Y);
  function->setTypeParameters(*functionTypeParameters);
  auto functionParameterTypes = BlockArray<Type>::create(heap, 3);
  functionParameterTypes->set(0, *localType);
  functionParameterTypes->set(1, *foreignType);
  functionParameterTypes->set(2, *BuiltinType);
  function->setParameterTypes(*functionParameterTypes);

  auto mangledName = mangleFunctionName(function, package);
  auto actual = Name::toString(heap, mangledName)->toUtf8StlString();
  string expected("quux[s<C::Object>C::Nothing,<C::Object>C::Nothing](C:local.Local[V0,V1?],Cfoo.bar.baz:foreign.Foreign[V1?,V0]?,C::Object)");
  ASSERT_EQ(expected, actual);
}


TEST(MangleFunctionNameExistential) {
  TEST_PROLOGUE

  auto upper = handle(Type::rootClassType(roots));
  auto lower = handle(Type::nothingType(roots));

  auto package = Package::create(heap, 0);
  auto S = TypeParameter::create(heap, NAME("S"), STR("S"), NO_FLAGS, upper, lower);
  auto T = TypeParameter::create(heap, NAME("T"), STR("T"), NO_FLAGS, upper, lower);
  auto C = Class::create(heap, CID0);
  C->setName(*NAME("C"));
  C->setPackage(*package);
  auto CTypeParameters = BlockArray<TypeParameter>::create(heap, 2);
  CTypeParameters->set(0, *S);
  CTypeParameters->set(1, *T);
  C->setTypeParameters(*CTypeParameters);
  auto P = TypeParameter::create(heap, NAME("P"), STR("P"), NO_FLAGS, upper, lower);
  auto PType = Type::create(heap, P);
  auto X = TypeParameter::create(heap, NAME("X"), STR("X"), NO_FLAGS, upper, lower);
  auto XType = Type::create(heap, X);
  auto eXType = Type::create(heap, vector<Local<TypeParameter>>{X},
                             Type::create(heap, C, vector<Local<Type>>{PType, XType}));
  auto function = Function::create(heap, FID0);
  function->setName(*NAME("foo"));
  auto functionTypeParameters = BlockArray<TypeParameter>::create(heap, 1);
  functionTypeParameters->set(0, *P);
  function->setTypeParameters(*functionTypeParameters);
  auto functionParameterTypes = BlockArray<Type>::create(heap, 1);
  functionParameterTypes->set(0, *eXType);
  function->setParameterTypes(*functionParameterTypes);


  auto mangledName = mangleFunctionName(function, package);
  auto actual = Name::toString(heap, mangledName)->toUtf8StlString();
  string expected("foo[<C::Object>C::Nothing](E[<C::Object>C::Nothing]C:C[V0,V1])");
  ASSERT_EQ(expected, actual);
}

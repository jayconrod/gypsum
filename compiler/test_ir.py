# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

import builtins
import bytecode
from flags import METHOD, STATIC
import ids
import ir
from ir_types import *
import utils_test


class TestIr(utils_test.TestCaseWithDefinitions):
    builtins.registerBuiltins(lambda name, ir: None)

    def setUp(self):
        super(TestIr, self).setUp()
        self.base = self.makeClass("Base", typeParameters=[], supertypes=[getRootClassType()])
        baseTy = ClassType(self.base)
        self.A = self.makeClass("A", supertypes=[baseTy] + self.base.supertypes)
        self.B = self.makeClass("B", supertypes=[baseTy] + self.base.supertypes)
        self.T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                        lowerBound=getNothingClassType(),
                                        flags=frozenset([STATIC]))

    def tearDown(self):
        super(TestIr, self).tearDown()
        self.package = None
        self.base = None
        self.A = None
        self.B = None
        self.T = None

    def testFindCommonBaseClass(self):
        commonClass = self.A.findCommonBaseClass(self.B)
        self.assertIs(self.base, commonClass)

    def testFindCommonBaseClassWithNothing(self):
        nothing = builtins.getNothingClass()
        self.assertIs(self.A, self.A.findCommonBaseClass(nothing))
        self.assertIs(self.A, nothing.findCommonBaseClass(self.A))

    def testFindCommonBaseClassWithNonUnifiedClasses(self):
        A = self.makeClass("A", supertypes=[])
        B = self.makeClass("B", supertypes=[])
        self.assertIsNone(A.findCommonBaseClass(B))

    def testFunctionCanCallWithWrongArgCount(self):
        f = self.makeFunction("f", returnType=UnitType,
                              parameterTypes=[UnitType], typeParameters=[])
        self.assertFalse(f.canCallWith([], []))

    def testFunctionCanCallWithWrongArgTypes(self):
        f = self.makeFunction("f", returnType=UnitType,
                              parameterTypes=[I64Type], typeParameters=[])
        self.assertFalse(f.canCallWith([], [UnitType]))

    def testFunctionCanCallWithWrongTypeArgCount(self):
        f = self.makeFunction("f", returnType=UnitType, typeParameters=[self.T])
        self.assertFalse(f.canCallWith([], []))

    def testFunctionCanCallWithTypeArgOutOfBounds(self):
        S = self.makeTypeParameter("S", upperBound=ClassType(self.A),
                                   lowerBound=getNothingClassType(), flags=frozenset([STATIC]))
        f = self.makeFunction("f", returnType=UnitType, typeParameters=[S])
        self.assertFalse(f.canCallWith([getRootClassType()], []))

    def testFunctionCanCallWithCorrectArgs(self):
        t = VariableType(self.T)
        f = self.makeFunction("f", returnType=t, typeParameters=[self.T], parameterTypes=[t])
        self.assertTrue(f.canCallWith([getRootClassType()], [getRootClassType()]))

    def testMayOverrideParamSub(self):
        rt = ClassType(self.base)
        f1 = self.makeFunction("f", returnType=UnitType,
                               typeParameters=[],
                               parameterTypes=[rt, ClassType(self.A)],
                               flags=frozenset([METHOD]),
                               definingClass=self.base)
        f2 = self.makeFunction("f", returnType=UnitType,
                               typeParameters=[],
                               parameterTypes=[rt, ClassType(self.base)],
                               flags=frozenset([METHOD]),
                               definingClass=self.base)
        self.assertTrue(f2.mayOverride(f1))
        self.assertFalse(f1.mayOverride(f2))

    def testMayOverrideTypeParamsDiff(self):
        rt = ClassType(self.base)
        f1 = self.makeFunction("f", returnType=UnitType,
                               typeParameters=[self.T], parameterTypes=[rt],
                               flags=frozenset([METHOD]))
        S = self.makeTypeParameter("S", upperBound=ClassType(self.base),
                                   lowerBound=ClassType(self.A))
        f2 = self.makeFunction("f", returnType=UnitType,
                               typeParameters=[S], parameterTypes=[rt],
                               flags=frozenset([METHOD]))
        self.assertFalse(f2.mayOverride(f1))
        self.assertFalse(f1.mayOverride(f2))

    def testMangleFunctionNameSimple(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        f = self.makeFunction(ir.Name(["foo", "bar", "baz"]), returnType=UnitType,
                              typeParameters=[],
                              parameterTypes=[UnitType, BooleanType, I8Type, I16Type,
                                              I32Type, I64Type, F32Type, F64Type])
        self.assertEquals("foo.bar.3baz(U,Z,B,S,I,L,F,D)", ir.mangleFunctionName(f, package))

    def testMangleFunctionNameClasses(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        P = package.addTypeParameter(ir.Name(["P"]))
        Q = package.addTypeParameter(ir.Name(["Q"]))
        Local = package.addClass(ir.Name(["local", "Local"]), typeParameters=[P, Q])
        otherPackage = ir.Package(name=ir.Name(["foo", "bar", "baz"]))
        otherPackage.id.index = 0
        S = otherPackage.addTypeParameter(ir.Name(["S"]))
        T = otherPackage.addTypeParameter(ir.Name(["T"]))
        Foreign = otherPackage.addClass(ir.Name(["foreign", "Foreign"]), typeParameters=[S, T])
        Foreign.id.packageId = otherPackage.id
        package.dependencies = [ir.PackageDependency(otherPackage.name, None, None)]

        X = package.addTypeParameter(ir.Name(["X"]), upperBound=getRootClassType(),
                                     lowerBound=getNothingClassType(),
                                     flags=frozenset([STATIC]))
        XType = VariableType(X)
        Y = package.addTypeParameter(ir.Name(["Y"]), upperBound=getRootClassType(),
                                     lowerBound=getNothingClassType())
        YType = VariableType(Y, frozenset([NULLABLE_TYPE_FLAG]))
        LocalType = ClassType(Local, (XType, YType))
        ForeignType = ClassType(Foreign, (YType, XType), frozenset([NULLABLE_TYPE_FLAG]))
        BuiltinType = getRootClassType()

        f = package.addFunction(ir.Name(["quux"]), typeParameters=[X, Y],
                                parameterTypes=[LocalType, ForeignType, BuiltinType])
        expected = "4quux[s<C::6Object>C::7Nothing,<C::6Object>C::7Nothing](C:11local.Local[V0,V1?],C11foo.bar.baz:15foreign.Foreign[V1?,V0]?,C::6Object)"
        self.assertEquals(expected, ir.mangleFunctionName(f, package))

    def testMangleFunctionNameExistential(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        S = package.addTypeParameter(ir.Name(["S"]), upperBound=getRootClassType(),
                                     lowerBound=getNothingClassType())
        T = package.addTypeParameter(ir.Name(["T"]), upperBound=getRootClassType(),
                                     lowerBound=getNothingClassType())
        C = package.addClass(ir.Name(["C"]), typeParameters=[S, T])
        P = package.addTypeParameter(ir.Name(["P"]), upperBound=getRootClassType(),
                                     lowerBound=getNothingClassType())
        PType = VariableType(P)
        X = package.addTypeParameter(ir.Name(["X"]), upperBound=getRootClassType(),
                                     lowerBound=getNothingClassType())
        XType = VariableType(X)
        eXType = ExistentialType((X,), ClassType(C, (PType, XType)))
        f = package.addFunction(ir.Name(["foo"]), typeParameters=[P],
                                parameterTypes=[eXType])
        expected = "3foo[<C::6Object>C::7Nothing](E[<C::6Object>C::7Nothing]C:1C[V0,V1])"
        self.assertEquals(expected, ir.mangleFunctionName(f, package))


class TestName(unittest.TestCase):
    def testFromStringBasic(self):
        name = ir.Name.fromString("foo")
        self.assertEquals(ir.Name(["foo"]), name)
        name = ir.Name.fromString("foo.bar.baz")
        self.assertEquals(ir.Name(["foo", "bar", "baz"]), name)

    def testFromStringChars(self):
        name = ir.Name.fromString("||")
        self.assertEquals(ir.Name(["||"]), name)
        self.assertRaises(ValueError, ir.Name.fromString, "||", isPackageName=True)

    def testUnicodeShortReturnsStr(self):
        name = ir.Name([unicode("foo")])
        self.assertEquals(str, type(name.short()))


class TestPackageVersion(unittest.TestCase):
    def testFromStringBasic(self):
        version = ir.PackageVersion.fromString("1")
        self.assertEquals(ir.PackageVersion([1]), version)
        version = ir.PackageVersion.fromString("1.23")
        self.assertEquals(ir.PackageVersion([1, 23]), version)


class TestPackageDependency(unittest.TestCase):
    def assertNameAndVersionEquals(self, expected, actual):
        self.assertEquals(expected.name, actual.name)
        self.assertEquals(expected.minVersion, actual.minVersion)
        self.assertEquals(expected.maxVersion, actual.maxVersion)

    def testFromStringBasic(self):
        dep = ir.PackageDependency.fromString("foo.bar")
        expected = ir.PackageDependency(ir.Name(["foo", "bar"]), None, None)
        self.assertNameAndVersionEquals(expected, dep)

    def testFromStringWithMin(self):
        dep = ir.PackageDependency.fromString("foo.bar:1.2")
        expected = ir.PackageDependency(ir.Name(["foo", "bar"]),
                                        ir.PackageVersion([1, 2]), None)
        self.assertNameAndVersionEquals(expected, dep)

    def testFromStringWithMax(self):
        dep = ir.PackageDependency.fromString("foo.bar:-3.4")
        expected = ir.PackageDependency(ir.Name(["foo", "bar"]),
                                        None, ir.PackageVersion([3, 4]))
        self.assertNameAndVersionEquals(expected, dep)

    def testFromStringWithBoth(self):
        dep = ir.PackageDependency.fromString("foo.bar:1.2-3.4")
        expected = ir.PackageDependency(ir.Name(["foo", "bar"]),
                                        ir.PackageVersion([1, 2]),
                                        ir.PackageVersion([3, 4]))
        self.assertNameAndVersionEquals(expected, dep)

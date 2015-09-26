# Copyright 2014-2015, Jay Conrod. All rights reserved.
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
        self.base = self.makeClass("Base", supertypes=[getRootClassType()])
        baseTy = ClassType(self.base)
        self.A = self.makeClass("A", supertypes=[baseTy])
        self.B = self.makeClass("B", supertypes=[baseTy])
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
        f = self.makeFunction("f", returnType=UnitType, parameterTypes=[UnitType])
        self.assertFalse(f.canCallWith([], []))

    def testFunctionCanCallWithWrongArgTypes(self):
        f = self.makeFunction("f", returnType=UnitType, parameterTypes=[I64Type])
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
        f1 = self.makeFunction("f", returnType=UnitType, parameterTypes=[rt, ClassType(self.A)],
                               flags=frozenset([METHOD]))
        f2 = self.makeFunction("f", returnType=UnitType,
                               parameterTypes=[rt, ClassType(self.base)],
                               flags=frozenset([METHOD]))
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

    def testFindPathToBaseClassMissing(self):
        A = self.makeClass("A", supertypes=[getRootClassType()])
        B = self.makeClass("B", supertypes=[getRootClassType()])
        self.assertEquals(None, A.findClassPathToBaseClass(B))

    def testFindPathToBaseClassSelf(self):
        A = self.makeClass("A", supertypes=[getRootClassType()])
        self.assertEquals([], A.findClassPathToBaseClass(A))

    def testFindPathToBaseClassShort(self):
        A = self.makeClass("A", supertypes=[getRootClassType()])
        B = self.makeClass("B", supertypes=[ClassType(A)])
        self.assertEquals([A], B.findClassPathToBaseClass(A))

    def testFindPathToBaseClassLong(self):
        A = self.makeClass("A", supertypes=[getRootClassType()])
        B = self.makeClass("B", supertypes=[ClassType(A)])
        C = self.makeClass("C", supertypes=[ClassType(B)])
        self.assertEquals([B, A], C.findClassPathToBaseClass(A))


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

# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

execfile("ir.py")

from builtins import registerBuiltins, getNothingClass
from ir_types import *
from utils_test import TestCaseWithDefinitions


class TestIr(TestCaseWithDefinitions):
    builtins.registerBuiltins(lambda name, ir: None)

    def setUp(self):
        super(TestIr, self).setUp()
        self.base = self.makeClass("Base", supertypes=[getRootClassType()])
        baseTy = ClassType(self.base)
        self.A = self.makeClass("A", supertypes=[baseTy])
        self.B = self.makeClass("B", supertypes=[baseTy])
        self.T = self.makeTypeParameter("T")

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
        nothing = getNothingClass()
        self.assertIs(self.A, self.A.findCommonBaseClass(nothing))
        self.assertIs(self.A, nothing.findCommonBaseClass(self.A))

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
        S = self.makeTypeParameter("S", upperBound=ClassType(self.A))
        f = self.makeFunction("f", returnType=UnitType, typeParameters=[S])
        self.assertFalse(f.canCallWith([getRootClassType()], []))

    def testFunctionCanCallWithCorrectArgs(self):
        t = VariableType(self.T)
        f = self.makeFunction("f", returnType=t, typeParameters=[self.T], parameterTypes=[t])
        self.assertTrue(f.canCallWith([getRootClassType()], [getRootClassType()]))

    def testMayOverrideParamSub(self):
        rt = ClassType(self.base)
        f1 = self.makeFunction("f", returnType=UnitType, parameterTypes=[rt, ClassType(self.A)])
        f1.clas = self.base
        f2 = self.makeFunction("f", returnType=UnitType,
                               parameterTypes=[rt, ClassType(self.base)])
        f2.clas = self.base
        self.assertTrue(f2.mayOverride(f1))
        self.assertFalse(f1.mayOverride(f2))

    def testMayOverrideTypeParamsDiff(self):
        rt = ClassType(self.base)
        f1 = self.makeFunction("f", returnType=UnitType,
                               typeParameters=[self.T], parameterTypes=[rt])
        f1.clas = self.base
        S = self.makeTypeParameter("S", upperBound=ClassType(self.base),
                                   lowerBound=ClassType(self.A))
        f2 = self.makeFunction("f", returnType=UnitType,
                               typeParameters=[S], parameterTypes=[rt])
        f2.clas = self.base
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

# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from builtins import *
from ir import *
from ir_types import *


class TestIntermediateRepresentation(unittest.TestCase):
    registerBuiltins(lambda name, ir: None)

    def setUp(self):
        self.package = Package()
        self.base = Class("Base", [], [getRootClassType()], None, [], [], [], frozenset())
        self.package.addClass(self.base)
        baseTy = ClassType(self.base)
        self.A = Class("A", [], [baseTy], None, [], [], [], frozenset())
        self.package.addClass(self.A)
        self.B = Class("B", [], [baseTy], None, [], [], [], frozenset())
        self.package.addClass(self.B)
        self.T = TypeParameter("T", getRootClassType(), getNothingClassType(), frozenset())
        self.package.addTypeParameter(self.T)

    def testFindCommonBaseClass(self):
        commonClass = self.A.findCommonBaseClass(self.B)
        self.assertIs(self.base, commonClass)

    def testFunctionCanCallWithWrongArgCount(self):
        f = Function("f", UnitType, [], [UnitType], None, None, frozenset())
        self.assertFalse(f.canCallWith([], []))

    def testFunctionCanCallWithWrongArgTypes(self):
        f = Function("f", UnitType, [], [I64Type], None, None, frozenset())
        self.assertFalse(f.canCallWith([], [UnitType]))

    def testFunctionCanCallWithWrongTypeArgCount(self):
        f = Function("f", UnitType, [self.T], [], None, None, frozenset())
        self.assertFalse(f.canCallWith([], []))

    def testFunctionCanCallWithTypeArgOutOfBounds(self):
        S = TypeParameter("S", ClassType(self.A), getNothingClassType(), frozenset())
        self.package.addTypeParameter(S)
        f = Function("f", UnitType, [S], [], None, None, frozenset())
        self.assertFalse(f.canCallWith([getRootClassType()], []))

    def testFunctionCanCallWithCorrectArgs(self):
        t = VariableType(self.T)
        f = Function("f", t, [self.T], [t], None, None, frozenset())
        self.assertTrue(f.canCallWith([getRootClassType()], [getRootClassType()]))

    def testMayOverrideParamSub(self):
        rt = ClassType(self.base)
        f1 = Function("f", UnitType, [], [rt, ClassType(self.A)], None, None, frozenset())
        f1.clas = self.base
        f2 = Function("f", UnitType, [], [rt, ClassType(self.base)], None, None, frozenset())
        f2.clas = self.base
        self.assertTrue(f2.mayOverride(f1))
        self.assertFalse(f1.mayOverride(f2))

    def testMayOverrideTypeParamsDiff(self):
        rt = ClassType(self.base)
        f1 = Function("f", UnitType, [self.T], [rt], None, None, frozenset())
        f1.clas = self.base
        S = TypeParameter("S", ClassType(self.base), ClassType(self.A), frozenset())
        f2 = Function("f", UnitType, [S], [rt], None, None, frozenset())
        f2.clas = self.base
        self.assertFalse(f2.mayOverride(f1))
        self.assertFalse(f1.mayOverride(f2))

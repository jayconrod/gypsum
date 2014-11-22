# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from builtins import *
from ir_types import *
from utils import *


class TestIrTypes(unittest.TestCase):
    registerBuiltins(lambda name, ir: None)

    def makeClass(self, name, superclass):
        clas = Class(name, [], [ClassType(superclass)], None, None, None, None, frozenset())
        clas.id = self.nextId()
        return clas

    def setUp(self):
        self.nextId = Counter()
        self.A = self.makeClass("A", getRootClass())
        self.B = self.makeClass("B", self.A)
        self.C = self.makeClass("C", self.B)
        X = TypeParameter("X", getRootClassType(), getNothingClassType(), frozenset())
        Y = TypeParameter("Y", getRootClassType(), getNothingClassType(), frozenset())
        self.P = Class("P", [X, Y], [getRootClassType()], None, None, None, None, frozenset())
        self.P.id = self.nextId()

    def testSubtypeSelf(self):
        self.assertTrue(ClassType(self.A).isSubtypeOf(ClassType(self.A)))

    def testSubtypeParent(self):
        self.assertTrue(ClassType(self.B).isSubtypeOf(ClassType(self.A)))
        self.assertFalse(ClassType(self.A).isSubtypeOf(ClassType(self.B)))

    def testSubtypeParameterSelf(self):
        T = TypeParameter("T", ClassType(self.A), ClassType(self.B), frozenset())
        ty = VariableType(T)
        self.assertTrue(ty.isSubtypeOf(ty))

    def testSubtypeParametersOverlapping(self):
        T = TypeParameter("T", ClassType(self.A), ClassType(self.C), frozenset())
        S = TypeParameter("S", ClassType(self.B), ClassType(self.C), frozenset())
        self.assertFalse(VariableType(S).isSubtypeOf(VariableType(T)))

    def testSubtypeParametersNonOverlapping(self):
        T = TypeParameter("T", ClassType(self.A), ClassType(self.B), frozenset())
        S = TypeParameter("S", ClassType(self.B), ClassType(self.C), frozenset())
        self.assertTrue(VariableType(S).isSubtypeOf(VariableType(T)))

    def testSubtypeClassWithParametersSelf(self):
        T = TypeParameter("T", getRootClassType(), getNothingClassType(), frozenset())
        T.id = 0
        S = TypeParameter("S", getRootClassType(), getNothingClassType(), frozenset())
        S.id = 1
        A = Class("A", [T], [getRootClassType()], None, [], [], [], frozenset())
        A.id = 0
        X = Class("X", [], [getRootClassType()], None, [], [], [], frozenset())
        X.id = 1
        Y = Class("Y", [], [getRootClassType()], None, [], [], [], frozenset())
        Y.id = 2
        ATty = ClassType(A, (VariableType(T),))
        ASty = ClassType(A, (VariableType(S),))
        AXty = ClassType(A, (ClassType(X),))
        AYty = ClassType(A, (ClassType(Y),))
        self.assertTrue(ATty.isSubtypeOf(ATty))
        self.assertFalse(ATty.isSubtypeOf(ASty))
        self.assertTrue(AXty.isSubtypeOf(AXty))
        self.assertFalse(AXty.isSubtypeOf(AYty))

    def testSubtypeClassWithParametersSubclass(self):
        T = TypeParameter("T", getRootClassType(), getNothingClassType(), frozenset())
        T.id = 0
        A = Class("A", [T], [getRootClassType()], None, [], [], [], frozenset())
        A.id = 0
        X = Class("X", [], [getRootClassType()], None, [], [], [], frozenset())
        X.id = 1
        Y = Class("Y", [], [getRootClassType()], None, [], [], [], frozenset())
        Y.id = 2
        AXty = ClassType(A, (ClassType(X),))
        B = Class("B", [], [AXty], None, [], [], [], frozenset())
        B.id = 3
        Bty = ClassType(B)
        AYty = ClassType(A, (ClassType(Y),))
        self.assertTrue(Bty.isSubtypeOf(AXty))
        self.assertFalse(Bty.isSubtypeOf(AYty))

    def testSubtypeClassWithParametersSuperclass(self):
        T = TypeParameter("T", getRootClassType(), getNothingClassType(), frozenset())
        T.id = 0
        A = Class("A", [], [getRootClassType()], None, [], [], [], frozenset())
        A.id = 0
        Aty = ClassType(A)
        B = Class("B", [T], [Aty], None, [], [], [], frozenset())
        B.id = 1
        X = Class("X", [], [getRootClassType()], None, [], [], [], frozenset())
        X.id = 2
        BXty = ClassType(B, (ClassType(X),))
        self.assertTrue(BXty.isSubtypeOf(Aty))

    def testSubstitute(self):
        T = TypeParameter("T", ClassType(self.A), ClassType(self.B), frozenset())
        T.id = self.nextId()
        a = ClassType(self.A)
        b = ClassType(self.B)
        p = ClassType(self.P, tuple(VariableType(pt) for pt in self.P.typeParameters))
        self.assertEquals(UnitType, UnitType.substitute([T], [a]))
        self.assertEquals(a, a.substitute([T], [a]))
        self.assertEquals(a, VariableType(T).substitute([T], [a]))
        self.assertEquals(ClassType(self.P, (a, b)),
                          p.substitute(self.P.typeParameters, [a, b]))

    def testSubstituteForBaseClass(self):
        T = TypeParameter("T", getRootClassType(), getNothingClassType(), frozenset())
        T.id = 0
        A = Class("A", [T], [getRootClassType()], None, [], [], [], frozenset())
        A.id = 0
        U = TypeParameter("U", getRootClassType(), getNothingClassType(), frozenset())
        U.id = 1
        B = Class("B", [U], [ClassType(A, (VariableType(U),))], None, [], [], [], frozenset())
        B.id = 1
        C = Class("C", [], [getRootClassType()], None, [], [], [], frozenset())
        C.id = 2
        D = Class("D", [], [ClassType(B, (ClassType(C),))], None, [], [], [], frozenset())
        D.id = 3
        V = TypeParameter("V", ClassType(D), getNothingClassType(), frozenset())
        V.id = 2
        self.assertEquals(ClassType(A, (ClassType(C),)),
                          VariableType(V).substituteForBaseClass(A))

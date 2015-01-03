# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from builtins import *
from ir import *
from ir_types import *
from location import *
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
        self.X = TypeParameter("X", getRootClassType(), getNothingClassType(), frozenset())
        self.X.id = 0
        self.Y = TypeParameter("Y", getRootClassType(), getNothingClassType(), frozenset())
        self.Y.id = 1
        self.P = Class("P", [self.X, self.Y], [getRootClassType()],
                       None, None, None, None, frozenset())
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

    def testSubtypeParametersTransitiveUpper(self):
        U = TypeParameter("U", getRootClassType(), getNothingClassType(), frozenset())
        T = TypeParameter("T", VariableType(U), getNothingClassType(), frozenset())
        S = TypeParameter("S", VariableType(T), getNothingClassType(), frozenset())
        self.assertTrue(VariableType(S).isSubtypeOf(VariableType(U)))

    def testSubtypeParametersTransitivieLower(self):
        U = TypeParameter("U", getRootClassType(), getNothingClassType(), frozenset())
        T = TypeParameter("T", getRootClassType(), VariableType(U), frozenset())
        S = TypeParameter("S", getRootClassType(), VariableType(T), frozenset())
        self.assertTrue(VariableType(U).isSubtypeOf(VariableType(T)))

    def testSubtypeParametersTransitiveMiddle(self):
        M = TypeParameter("M", getRootClassType(), getNothingClassType(), frozenset())
        S = TypeParameter("S", VariableType(M), getNothingClassType(), frozenset())
        T = TypeParameter("T", getRootClassType(), VariableType(M), frozenset())
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

    def testSubtypeWithCovariantParameter(self):
        # Source[A] <: Source[B] with class Source[+T] and A <: B
        T = TypeParameter("T", getRootClassType(), getNothingClassType(),
                          frozenset([COVARIANT]))
        T.id = 0
        B = Class("B", [], [getRootClassType()], None, [], [], [], frozenset())
        B.id = 0
        Bty = ClassType(B)
        A = Class("A", [], [Bty], None, [], [], [], frozenset())
        A.id = 1
        Aty = ClassType(A)
        Source = Class("Source", [T], [getRootClassType()], None, [], [], [], frozenset())
        Source.id = 2
        SourceAty = ClassType(Source, (Aty,))
        SourceBty = ClassType(Source, (Bty,))
        self.assertTrue(SourceAty.isSubtypeOf(SourceBty))
        self.assertFalse(SourceBty.isSubtypeOf(SourceAty))

    def testSubtypeWithContravariantParameter(self):
        # Sink[A] <: Sink[B] with class Sink[-T] and B <: A
        T = TypeParameter("T", getRootClassType(), getNothingClassType(),
                          frozenset([CONTRAVARIANT]))
        T.id = 0
        A = Class("A", [], [getRootClassType()], None, [], [], [], frozenset())
        A.id = 0
        Aty = ClassType(A)
        B = Class("B", [], [Aty], None, [], [], [], frozenset())
        B.id = 1
        Bty = ClassType(B)
        Sink = Class("Sink", [T], [getRootClassType()], None, [], [], [], frozenset())
        Sink.id = 2
        SinkAty = ClassType(Sink, (Aty,))
        SinkBty = ClassType(Sink, (Bty,))
        self.assertTrue(SinkAty.isSubtypeOf(SinkBty))
        self.assertFalse(SinkBty.isSubtypeOf(SinkAty))

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

    def testCombineNothing(self):
        aTy = ClassType(self.A)
        nothingTy = getNothingClassType()
        self.assertEquals(aTy, aTy.combine(nothingTy, NoLoc))
        self.assertEquals(aTy, nothingTy.combine(aTy, NoLoc))

    def testCombineNull(self):
        aTy = ClassType(self.A)
        nullTy = getNullType()
        aNullTy = ClassType(self.A, (), frozenset([NULLABLE_TYPE_FLAG]))
        self.assertEquals(aNullTy, aTy.combine(nullTy, NoLoc))
        self.assertEquals(aNullTy, nullTy.combine(aTy, NoLoc))

    def testCombineWithTypeArgs(self):
        pxy = ClassType(self.P, (VariableType(self.X), VariableType(self.Y)))
        pyx = ClassType(self.P, (VariableType(self.Y), VariableType(self.X)))
        self.assertEquals(pxy, pxy.combine(pxy, NoLoc))
        self.assertRaises(TypeException, pxy.combine, pyx, NoLoc)

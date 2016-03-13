# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from builtins import *
from ir import *
from ir_types import *
from ir import * # Needed for Class()
from location import *
from errors import * #For TypeException
from utils import *
from flags import * # For CONTRAVARIANT and COVARIANT
from utils_test import TestCaseWithDefinitions


class TestIrTypes(TestCaseWithDefinitions):
    registerBuiltins(lambda name, ir: None)

    def setUp(self):
        super(TestIrTypes, self).setUp()
        self.A = self.makeClass("A", typeParameters=[], supertypes=[getRootClassType()])
        self.B = self.makeClass("B", typeParameters=[], supertypes=[ClassType(self.A)])
        self.C = self.makeClass("C", typeParameters=[], supertypes=[ClassType(self.B)])
        self.X = self.makeTypeParameter("X")
        self.Y = self.makeTypeParameter("Y")
        self.P = self.makeClass("P", typeParameters=[self.X, self.Y],
                                supertypes=[getRootClassType()])

    def tearDown(self):
        super(TestIrTypes, self).tearDown()
        self.A = None
        self.B = None
        self.C = None
        self.X = None
        self.Y = None
        self.P = None

    def testSubtypeSelf(self):
        self.assertTrue(ClassType(self.A).isSubtypeOf(ClassType(self.A)))

    def testSubtypeParent(self):
        self.assertTrue(ClassType(self.B).isSubtypeOf(ClassType(self.A)))
        self.assertFalse(ClassType(self.A).isSubtypeOf(ClassType(self.B)))

    def testSubtypeParameterSelf(self):
        T = self.makeTypeParameter("T", upperBound=ClassType(self.A),
                                   lowerBound=ClassType(self.B))
        ty = VariableType(T)
        self.assertTrue(ty.isSubtypeOf(ty))

    def testSubtypeParametersOverlapping(self):
        T = self.makeTypeParameter("T", upperBound=ClassType(self.A),
                                   lowerBound=ClassType(self.C))
        S = self.makeTypeParameter("S", upperBound=ClassType(self.B),
                                   lowerBound=ClassType(self.C))
        self.assertFalse(VariableType(S).isSubtypeOf(VariableType(T)))

    @unittest.skip("need a general way to do lub on subtype graph")
    def testSubtypeParametersNonOverlapping(self):
        T = self.makeTypeParameter("T", upperBound=ClassType(self.A),
                                   lowerBound=ClassType(self.B))
        S = self.makeTypeParameter("S", upperBound=ClassType(self.B),
                                   lowerBound=ClassType(self.C))
        self.assertTrue(VariableType(S).isSubtypeOf(VariableType(T)))

    def testSubtypeParametersTransitiveUpper(self):
        U = self.makeTypeParameter("U")
        T = self.makeTypeParameter("T", upperBound=VariableType(U))
        S = self.makeTypeParameter("S", upperBound=VariableType(T))
        self.assertTrue(VariableType(S).isSubtypeOf(VariableType(U)))

    @unittest.skip("TypeParameter.findCommonUpperBound needs to return U for this to work")
    def testSubtypeParametersTransitivieLower(self):
        # U, T <: U, S <: T
        # So S <: U
        U = self.makeTypeParameter("U")
        T = self.makeTypeParameter("T", lowerBound=VariableType(U))
        S = self.makeTypeParameter("S", lowerBound=VariableType(T))
        self.assertTrue(VariableType(U).isSubtypeOf(VariableType(S)))

    @unittest.skip("TypeParameter.findCommonUpperBound needs to return T for this to work")
    def testSubtypeParametersTransitiveMiddle(self):
        # M, S <: M, M <: T, so S <: T
        M = self.makeTypeParameter("M")
        S = self.makeTypeParameter("S", upperBound=VariableType(M))
        T = self.makeTypeParameter("T", lowerBound=VariableType(M))
        self.assertTrue(VariableType(S).isSubtypeOf(VariableType(T)))

    def testSubtypeClassWithParametersSelf(self):
        T = self.makeTypeParameter("T")
        S = self.makeTypeParameter("S")
        A = self.makeClass("A", typeParameters=[T], supertypes=[getRootClassType()])
        X = self.makeClass("X", typeParameters=[], supertypes=[getRootClassType()])
        Y = self.makeClass("Y", typeParameters=[], supertypes=[getRootClassType()])
        ATty = ClassType(A, (VariableType(T),))
        ASty = ClassType(A, (VariableType(S),))
        AXty = ClassType(A, (ClassType(X),))
        AYty = ClassType(A, (ClassType(Y),))
        self.assertTrue(ATty.isSubtypeOf(ATty))
        self.assertFalse(ATty.isSubtypeOf(ASty))
        self.assertTrue(AXty.isSubtypeOf(AXty))
        self.assertFalse(AXty.isSubtypeOf(AYty))

    def testSubtypeClassWithParametersSubclass(self):
        T = self.makeTypeParameter("T")
        A = self.makeClass("A", typeParameters=[T], supertypes=[getRootClassType()])
        X = self.makeClass("X", supertypes=[getRootClassType()])
        Y = self.makeClass("Y", supertypes=[getRootClassType()])
        AXty = ClassType(A, (ClassType(X),))
        B = self.makeClass("B", supertypes=[AXty])
        Bty = ClassType(B)
        AYty = ClassType(A, (ClassType(Y),))
        self.assertTrue(Bty.isSubtypeOf(AXty))
        self.assertFalse(Bty.isSubtypeOf(AYty))

    def testSubtypeClassWithParametersSuperclass(self):
        T = self.makeTypeParameter("T")
        A = self.makeClass("A", typeParameters=[], supertypes=[getRootClassType()])
        Aty = ClassType(A)
        B = self.makeClass("B", typeParameters=[T], supertypes=[Aty])
        X = self.makeClass("X", typeParameters=[], supertypes=[getRootClassType()])
        BXty = ClassType(B, (ClassType(X),))
        self.assertTrue(BXty.isSubtypeOf(Aty))

    def testSubtypeWithCovariantParameter(self):
        # Source[A] <: Source[B] with class Source[+T] and A <: B
        T = self.makeTypeParameter("T", flags=frozenset([COVARIANT]))
        B = self.makeClass("B", typeParameters=[], supertypes=[getRootClassType()])
        Bty = ClassType(B)
        A = self.makeClass("A", typeParameters=[], supertypes=[Bty])
        Aty = ClassType(A)
        Source = self.makeClass("Source", typeParameters=[T], supertypes=[getRootClassType()])
        SourceAty = ClassType(Source, (Aty,))
        SourceBty = ClassType(Source, (Bty,))
        self.assertTrue(SourceAty.isSubtypeOf(SourceBty))
        self.assertFalse(SourceBty.isSubtypeOf(SourceAty))

    def testSubtypeWithContravariantParameter(self):
        # Sink[A] <: Sink[B] with class Sink[-T] and B <: A
        T = self.makeTypeParameter("T", flags=frozenset([CONTRAVARIANT]))
        A = self.makeClass("A", typeParameters=[], supertypes=[getRootClassType()])
        Aty = ClassType(A)
        B = self.makeClass("B", typeParameters=[], supertypes=[Aty])
        Bty = ClassType(B)
        Sink = self.makeClass("Sink", typeParameters=[T], supertypes=[getRootClassType()])
        SinkAty = ClassType(Sink, (Aty,))
        SinkBty = ClassType(Sink, (Bty,))
        self.assertTrue(SinkAty.isSubtypeOf(SinkBty))
        self.assertFalse(SinkBty.isSubtypeOf(SinkAty))

    def testSubtypeNothingAndVariable(self):
        T = self.makeTypeParameter("T")
        Tty = VariableType(T)
        self.assertTrue(getNothingClassType().isSubtypeOf(Tty))

    def testEquivalentExistentials(self):
        X = self.makeTypeParameter("X", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        Y = self.makeTypeParameter("Y", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        eX = ExistentialType((X,), VariableType(X))
        eY = ExistentialType((Y,), VariableType(Y))
        self.assertTrue(eX.isSubtypeOf(eY))
        self.assertTrue(eY.isSubtypeOf(eX))

    def testJointExistentials(self):
        S = self.makeTypeParameter("S", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType(),
                                   flags=frozenset([STATIC, COVARIANT]))
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType(),
                                   flags=frozenset([STATIC, COVARIANT]))
        Foo = self.makeClass("Foo", typeParameters=[S, T],
                             supertypes=[getRootClassType()])
        X = self.makeTypeParameter("X", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        Y = self.makeTypeParameter("Y", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        eX = ExistentialType((X,), ClassType(Foo, (VariableType(X), getNothingClassType())))
        eY = ExistentialType((Y,), ClassType(Foo, (getNothingClassType(), VariableType(Y))))
        expected = ExistentialType((X, Y), ClassType(Foo, (VariableType(X), VariableType(Y))))
        self.assertTrue(expected.isEquivalent(eX.lub(eY)))

    def testExistentialOpen(self):
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType(),
                                   flags=frozenset([COVARIANT]))
        Foo = self.makeClass("Foo", typeParameters=[T], supertypes=[getRootClassType()])
        FooStringType = ClassType(Foo, (getStringType(),))
        X = self.makeTypeParameter("X", upperBound=getStringType(),
                                   lowerBound=getNothingClassType())
        FooExType = ExistentialType((X,), ClassType(Foo, (VariableType(X),)))
        self.assertTrue(FooExType.isSubtypeOf(FooStringType))

    def testExistentialDifferentBoundsNotEquivalent(self):
        X = self.makeTypeParameter("X", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        Y = self.makeTypeParameter("Y", upperBound=getStringType(),
                                   lowerBound=getNothingClassType())
        eXType = ExistentialType((X,), VariableType(X))
        eYType = ExistentialType((Y,), VariableType(Y))
        self.assertFalse(eXType.isEquivalent(eYType))

    def testExistentialDifferentBoundsLub(self):
        A = self.makeClass("A", typeParameters=[], supertypes=[getRootClassType()])
        AType = ClassType(A)
        S = self.makeTypeParameter("S", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        SType = VariableType(S)
        B = self.makeClass("B", typeParameters=[S], supertypes=[AType])
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        C = self.makeClass("C", typeParameters=[T], supertypes=[AType])

        X = self.makeTypeParameter("X", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        Y = self.makeTypeParameter("Y", upperBound=getStringType(),
                                   lowerBound=getNothingClassType())
        eBXType = ExistentialType((X,), ClassType(B, (VariableType(X),)))
        eCYType = ExistentialType((Y,), ClassType(C, (VariableType(Y),)))
        self.assertEquals(AType, eBXType.lub(eCYType))

    def testSubstitute(self):
        T = self.makeTypeParameter("T", upperBound=ClassType(self.A),
                                   lowerBound=ClassType(self.B))
        a = ClassType(self.A)
        b = ClassType(self.B)
        p = ClassType(self.P, tuple(VariableType(pt) for pt in self.P.typeParameters))
        self.assertEquals(UnitType, UnitType.substitute([T], [a]))
        self.assertEquals(a, a.substitute([T], [a]))
        self.assertEquals(a, VariableType(T).substitute([T], [a]))
        self.assertEquals(ClassType(self.P, (a, b)),
                          p.substitute(self.P.typeParameters, [a, b]))

    def testSubstituteForBaseClass(self):
        T = self.makeTypeParameter("T")
        A = self.makeClass("A", typeParameters=[T], supertypes=[getRootClassType()])
        U = self.makeTypeParameter("U")
        B = self.makeClass("B", typeParameters=[U],
                           supertypes=[ClassType(A, (VariableType(U),))])
        C = self.makeClass("C", supertypes=[getRootClassType()])
        D = self.makeClass("D", supertypes=[ClassType(B, (ClassType(C),))])
        V = self.makeTypeParameter("V", upperBound=ClassType(D))
        self.assertEquals(ClassType(A, (ClassType(C),)),
                          VariableType(V).substituteForBase(A))

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
        self.assertEquals(self.P.supertypes[0], pxy.combine(pyx, NoLoc))

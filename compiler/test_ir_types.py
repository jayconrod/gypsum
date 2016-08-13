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
        self.B = self.makeClass("B", typeParameters=[],
                                supertypes=[ClassType(self.A)] + self.A.supertypes)
        self.C = self.makeClass("C", typeParameters=[],
                                supertypes=[ClassType(self.B)] + self.B.supertypes)
        self.X = self.makeTypeParameter("X", upperBound=getRootClassType(),
                                        lowerBound=getNothingClassType())
        self.Y = self.makeTypeParameter("Y", upperBound=getRootClassType(),
                                        lowerBound=getNothingClassType())
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

    def testSubtypeNull(self):
        ATy = ClassType(self.A, (), frozenset([NULLABLE_TYPE_FLAG]))
        self.assertTrue(getNullType().isSubtypeOf(ATy))

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

    def testSubtypeParametersNonOverlapping(self):
        T = self.makeTypeParameter("T", upperBound=ClassType(self.A),
                                   lowerBound=ClassType(self.B))
        S = self.makeTypeParameter("S", upperBound=ClassType(self.B),
                                   lowerBound=ClassType(self.C))
        self.assertTrue(VariableType(S).isSubtypeOf(VariableType(T)))

    def testSubtypeParametersTransitiveUpper(self):
        U = self.makeTypeParameter("U", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        T = self.makeTypeParameter("T", upperBound=VariableType(U),
                                   lowerBound=getNothingClassType())
        S = self.makeTypeParameter("S", upperBound=VariableType(T),
                                   lowerBound=getNothingClassType())
        self.assertTrue(VariableType(S).isSubtypeOf(VariableType(U)))

    def testSubtypeParametersTransitiveLower(self):
        # U, T >: U, S >: T
        # So S <: U
        U = self.makeTypeParameter("U", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=VariableType(U))
        S = self.makeTypeParameter("S", upperBound=getRootClassType(),
                                   lowerBound=VariableType(T))
        self.assertTrue(VariableType(U).isSubtypeOf(VariableType(S)))

    def testSubtypeParametersTransitiveMiddle(self):
        # M, S <: M, M <: T, so S <: T
        M = self.makeTypeParameter("M", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        S = self.makeTypeParameter("S", upperBound=VariableType(M),
                                   lowerBound=getNothingClassType())
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=VariableType(M))
        self.assertTrue(VariableType(S).isSubtypeOf(VariableType(T)))

    def testSubtypeClassWithParametersSelf(self):
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        S = self.makeTypeParameter("S", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
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
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
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
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        A = self.makeClass("A", typeParameters=[], supertypes=[getRootClassType()])
        Aty = ClassType(A)
        B = self.makeClass("B", typeParameters=[T], supertypes=[Aty])
        X = self.makeClass("X", typeParameters=[], supertypes=[getRootClassType()])
        BXty = ClassType(B, (ClassType(X),))
        self.assertTrue(BXty.isSubtypeOf(Aty))

    def testSubtypeWithCovariantParameter(self):
        # Source[A] <: Source[B] with class Source[+T] and A <: B
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType(),
                                   flags=frozenset([COVARIANT]))
        B = self.makeClass("B", typeParameters=[], supertypes=[getRootClassType()])
        Bty = ClassType(B)
        A = self.makeClass("A", typeParameters=[], supertypes=[Bty] + B.supertypes)
        Aty = ClassType(A)
        Source = self.makeClass("Source", typeParameters=[T], supertypes=[getRootClassType()])
        SourceAty = ClassType(Source, (Aty,))
        SourceBty = ClassType(Source, (Bty,))
        self.assertTrue(SourceAty.isSubtypeOf(SourceBty))
        self.assertFalse(SourceBty.isSubtypeOf(SourceAty))

    def testSubtypeWithContravariantParameter(self):
        # Sink[A] <: Sink[B] with class Sink[-T] and B <: A
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType(),
                                   flags=frozenset([CONTRAVARIANT]))
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
        T = self.makeTypeParameter("T",
                                   upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        Tty = VariableType(T)
        self.assertTrue(getNothingClassType().isSubtypeOf(Tty))

    def testSubtypeRightExistential(self):
        # class C[T]
        # C[Object] <: forsome [X] C[X]
        T = self.makeTypeParameter("T",
                                   upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        C = self.makeClass("C", typeParameters=[T], supertypes=[getRootClassType()])
        Cty = ClassType(C, (getRootClassType(),))
        X = self.makeTypeParameter("X",
                                   upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        eXty = ExistentialType([X], ClassType(C, (VariableType(X),)))
        self.assertTrue(Cty.isSubtypeOf(eXty))

    def testSubtypeRightExistentialContradiction(self):
        # class A[T]
        # class B[U, V]
        # B[A[String], A[Object]] </: forsome [X <: forsome [Y] A[Y]] B[X, X]
        # At the time this was written, existentials could not be used as bounds. That
        # restriction might be relaxed in the future, and the implementation does not
        # completely rely on it. So it's nice to test that implementation.
        T = self.makeTypeParameter("T", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        A = self.makeClass("A", typeParameters=[T], supertypes=[getRootClassType()])
        U = self.makeTypeParameter("U", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        V = self.makeTypeParameter("V", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        B = self.makeClass("B", typeParameters=[U, V], supertypes=[getRootClassType()])
        Y = self.makeTypeParameter("Y", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        X = self.makeTypeParameter("X", upperBound=ExistentialType([Y], ClassType(A, (VariableType(Y),))),
                                   lowerBound=getNothingClassType())
        leftType = ClassType(B, (ClassType(A, (getStringType(),)), ClassType(A, (getRootClassType(),))))
        rightType = ExistentialType([X], ClassType(B, (VariableType(X), VariableType(X))))
        self.assertFalse(leftType.isSubtypeOf(rightType))

    def testSubtypeRightExistentialFailUpperBound(self):
        X = self.makeTypeParameter("X", upperBound=getStringType(),
                                   lowerBound=getNothingClassType())
        eXType = ExistentialType([X], VariableType(X))
        self.assertTrue(getStringType().isSubtypeOf(eXType))
        self.assertFalse(getRootClassType().isSubtypeOf(eXType))

    def testSubtypeRightExistentialFailLowerBound(self):
        X = self.makeTypeParameter("X", upperBound=getRootClassType(),
                                   lowerBound=getRootClassType())
        eXType = ExistentialType([X], VariableType(X))
        self.assertFalse(getStringType().isSubtypeOf(eXType))
        self.assertTrue(getRootClassType().isSubtypeOf(eXType))

    def testEquivalentExistentials(self):
        X = self.makeTypeParameter("X", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        Y = self.makeTypeParameter("Y", upperBound=getRootClassType(),
                                   lowerBound=getNothingClassType())
        eX = ExistentialType((X,), VariableType(X))
        eY = ExistentialType((Y,), VariableType(Y))
        self.assertTrue(eX.isSubtypeOf(eY))
        self.assertTrue(eY.isSubtypeOf(eX))
        self.assertTrue(eX.isEquivalent(eY))
        self.assertTrue(eY.isEquivalent(eX))

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

    def testLubSubTrait(self):
        A = self.makeTrait("A", typeParameters=[], supertypes=[getRootClassType()])
        ATy = ClassType(A)
        B = self.makeTrait("B", typeParameters=[], supertypes=[ATy, getRootClassType()])
        BTy = ClassType(B)
        self.assertEquals(ATy, ATy.lub(BTy))
        self.assertEquals(ATy, BTy.lub(ATy))

    def testLubClassesSharedTraits(self):
        # TODO: when union types are supported the correct result here is C1 | C2.
        Tr1 = self.makeTrait("Tr1", typeParameters=[], supertypes=[getRootClassType()])
        Tr1Type = ClassType(Tr1)
        Tr2 = self.makeTrait("Tr2", typeParameters=[], supertypes=[getRootClassType()])
        Tr2Type = ClassType(Tr2)
        C1 = self.makeClass("C1", typeParameters=[],
                            supertypes=[getRootClassType(), Tr1Type, Tr2Type])
        C1Type = ClassType(C1)
        C2 = self.makeClass("C2", typeParameters=[],
                            supertypes=[getRootClassType(), Tr1Type, Tr2Type])
        C2Type = ClassType(C2)
        self.assertEquals(getRootClassType(), C1Type.lub(C2Type))

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

    def testSubstituteForBase(self):
        T = self.makeTypeParameter("T")
        A = self.makeClass("A", typeParameters=[T], supertypes=[getRootClassType()])
        U = self.makeTypeParameter("U")
        B = self.makeClass("B", typeParameters=[U],
                           supertypes=[ClassType(A, (VariableType(U),)), getRootClassType()])
        C = self.makeClass("C", supertypes=[getRootClassType()])
        D = self.makeClass("D",
                           supertypes=[
                               ClassType(B, (ClassType(C),)),
                               ClassType(A, (ClassType(C),)),
                               getRootClassType()
                           ])
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

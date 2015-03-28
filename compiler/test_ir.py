# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

import builtins
import bytecode
import flags
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


class TestExternalize(unittest.TestCase):
    def setUp(self):
        self.package = ir.Package(ids.TARGET_PACKAGE_ID)
        self.otherPackage = ir.Package()
        self.loader = utils_test.MockPackageLoader([self.otherPackage])
        field = self.otherPackage.newField("x", None, I64Type, frozenset([flags.PUBLIC]))
        self.clas = self.otherPackage.addClass("Foo", None, [], [getRootClassType()], None,
                                               None, [field], None, frozenset([flags.PUBLIC]))
        self.classTy = ClassType(self.clas)
        self.clas.constructors = [self.otherPackage.addFunction("$constructor", None, UnitType,
                                                                [], [], None, None,
                                                                frozenset([flags.PUBLIC,
                                                                           flags.METHOD]))]
        self.clas.methods = [self.otherPackage.addFunction("m", None, UnitType,
                                                           [], [], None, None,
                                                           frozenset([flags.PUBLIC,
                                                                      flags.METHOD]))]
        self.externClassTy = self.externalizeType(self.classTy)
        self.param = self.otherPackage.addTypeParameter("T", None, getRootClassType(),
                                                        getNothingClassType(),
                                                        frozenset([flags.STATIC]))
        self.varTy = VariableType(self.param)
        self.externVarTy = self.externalizeType(self.varTy)
        self.externParam = self.getExtern(self.param)

    def externalize(self, defn):
        return self.package.externalize(defn, self.loader)

    def externalizeType(self, ty):
        return self.package.externalizeType(ty, self.loader)

    def getExtern(self, defn):
        dep = self.package.dependencies[defn.id.packageId.index]
        if isinstance(defn, ir.Global):
            externs = dep.externGlobals
        elif isinstance(defn, ir.Function):
            if flags.METHOD in defn.flags:
                externs = dep.externMethods
            else:
                externs = dep.externFunctions
        elif isinstance(defn, ir.Class):
            externs = dep.externClasses
        else:
            assert isinstance(defn, ir.TypeParameter)
            externs = dep.externTypeParameters
        externDefn = externs[defn.id.externIndex]
        return externDefn

    def checkExternPosition(self, defn):
        self.assertIn(flags.EXTERN, defn.flags)
        expected = self.getExtern(defn)
        self.assertIs(expected, defn)

    def testExternalizeGlobal(self):
        globl = self.otherPackage.addGlobal("g", None, self.classTy,
                                            frozenset([flags.PUBLIC]))
        externGlobal = self.externalize(globl)
        expected = ir.Global("g", None, globl.id, self.externClassTy,
                             frozenset([flags.PUBLIC, flags.EXTERN]))
        self.assertEquals(expected, externGlobal)
        self.checkExternPosition(externGlobal)

    def testExternalizeFunction(self):
        function = self.otherPackage.addFunction("f", None, self.classTy, [self.param],
                                                 [self.varTy], None, None,
                                                 frozenset([flags.PUBLIC]))
        externFunction = self.externalize(function)
        expected = ir.Function("f", None, function.id, self.externClassTy, [self.externParam],
                               [self.externVarTy], None, None,
                               frozenset([flags.PUBLIC, flags.EXTERN]))
        self.assertEquals(expected, externFunction)

    def testExternalizeClass(self):
        clas = self.otherPackage.addClass("C", None, [self.param], [getRootClassType()],
                                          None, None, None, None, frozenset([flags.PUBLIC]))
        clasTy = ClassType(clas, (self.varTy,))
        ctor = self.otherPackage.addFunction("$constructor", None, UnitType, [self.param],
                                             [clasTy], None, None,
                                             frozenset([flags.PUBLIC, flags.METHOD]))
        clas.constructors = [ctor]
        field = self.otherPackage.newField("x", None, clasTy, frozenset([flags.PUBLIC]))
        clas.fields = [field]
        method = self.otherPackage.addFunction("f", None, UnitType, [self.param], [clasTy],
                                               None, None,
                                               frozenset([flags.PUBLIC, flags.METHOD]))
        builtinMethod = \
            builtins.getBuiltinFunctionById(bytecode.BUILTIN_ROOT_CLASS_TO_STRING_ID)
        clas.methods = [method, builtinMethod]
        externClass = self.externalize(clas)
        externClassTy = ClassType(externClass, (self.externVarTy,))
        expected = ir.Class("C", None, clas.id, [self.externParam], [getRootClassType()],
                            None, None, None, None, frozenset([flags.PUBLIC, flags.EXTERN]))
        expectedCtor = ir.Function("$constructor", None, ctor.id, UnitType, [self.externParam],
                                   [externClassTy], None, None,
                                   frozenset([flags.PUBLIC, flags.METHOD, flags.EXTERN]))
        expected.constructors = [expectedCtor]
        expectedField = ir.Field("x", None, externClassTy, frozenset([flags.PUBLIC]))
        expected.fields = [expectedField]
        expectedMethod = ir.Function("f", None, method.id, UnitType, [self.externParam],
                                     [externClassTy], None, None,
                                     frozenset([flags.PUBLIC, flags.METHOD, flags.EXTERN]))
        expected.methods = [expectedMethod, builtinMethod]
        self.assertEquals(expected, externClass)

    def testExternalizeTypeParameter(self):
        param = self.otherPackage.addTypeParameter("S", None, self.classTy, self.classTy,
                                                   frozenset([flags.STATIC]))
        externParam = self.package.externalize(param, self.loader)
        expected = ir.TypeParameter("S", None, param.id, self.externClassTy, self.externClassTy,
                                    frozenset([flags.STATIC, flags.EXTERN]))
        self.assertEquals(expected, externParam)

    def testExternalizePrimitiveType(self):
        externType = self.externalizeType(UnitType)
        self.assertEquals(UnitType, externType)

    def testExternalizeBuiltinType(self):
        builtinType = getRootClassType()
        externType = self.externalizeType(builtinType)
        self.assertEquals(builtinType, externType)

    def testExternalizeLocalType(self):
        clas = self.package.addClass("Foo", None, None, None, None, None, None, None, None)
        localType = ClassType(clas)
        externType = self.externalizeType(localType)
        self.assertEquals(localType, externType)

    def testExternalizeClassType(self):
        externClassTy = self.externalizeType(self.classTy)
        externClass = self.getExtern(self.clas)
        expected = ClassType(externClass)
        self.assertEquals(expected, externClassTy)

    def testExternalizeClassTypeWithArg(self):
        clas = self.otherPackage.addClass("Bar", None, [self.param], [getRootClassType()],
                                          None, [], [], [], frozenset([flags.PUBLIC]))
        classType = ClassType(clas, (ClassType(clas, (getRootClassType(),)),))
        externClassType = self.externalizeType(classType)
        externClass = self.getExtern(clas)
        expected = ClassType(externClass, (ClassType(externClass, (getRootClassType(),)),))
        self.assertEquals(expected, externClassType)

    def testExternalizeVariableType(self):
        externVarTy = self.externalizeType(self.varTy)
        externParam = self.getExtern(self.param)
        expected = VariableType(externParam)
        self.assertEquals(expected, externVarTy)

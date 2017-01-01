# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

import builtins
import bytecode
import externalization
from flags import (
    ARRAY,
    CONSTRUCTOR,
    EXTERN,
    FINAL,
    LET,
    METHOD,
    PRIVATE,
    PUBLIC,
    STATIC,
)
import ids
import ir
import ir_types
from name import ARRAY_LENGTH_SUFFIX, CONSTRUCTOR_SUFFIX, Name
import utils_test


class TestExternalization(utils_test.TestCaseWithDefinitions):
    def setUp(self):
        super(TestExternalization, self).setUp()
        self.package = ir.Package(ids.TARGET_PACKAGE_ID)
        self.package.buildNameIndex()
        self.rootClassType = ir_types.getRootClassType()
        self.nothingClassType = ir_types.getNothingClassType()
        self.otherPackage = ir.Package()
        self.packageLoader = utils_test.FakePackageLoader([self.otherPackage])
        self.externalizer = externalization.Externalizer(self.package, self.packageLoader)

        field = self.otherPackage.newField(Name(["x"]), type=ir_types.I64Type,
                                           flags=frozenset([PUBLIC]))
        self.clas = self.otherPackage.addClass(Name(["Foo"]), typeParameters=[],
                                               supertypes=[self.rootClassType],
                                               constructors=[], fields=[field],
                                               methods=[], flags=frozenset([PUBLIC]))
        self.classTy = ir_types.ClassType(self.clas)
        ctor = self.otherPackage.addFunction(Name([CONSTRUCTOR_SUFFIX]),
                                             returnType=ir_types.UnitType,
                                             typeParameters=[], parameterTypes=[],
                                             flags=frozenset([PUBLIC, METHOD]))
        self.clas.constructors = [ctor]
        method = self.otherPackage.addFunction(Name(["m"]), returnType=ir_types.UnitType,
                                               typeParameters=[], parameterTypes=[],
                                               flags=frozenset([PUBLIC, METHOD]))
        self.clas.methods = [method]
        self.param = self.otherPackage.addTypeParameter(Name(["T"]),
                                                        upperBound=self.rootClassType,
                                                        lowerBound=self.nothingClassType,
                                                        flags=frozenset([PUBLIC, STATIC]))
        self.dep = self.package.ensureDependency(self.otherPackage)
        self.externParam = self.externalizer.externalizeDefn(self.param)
        self.varTy = ir_types.VariableType(self.param)

    def getExtern(self, defn):
        dep = self.package.dependencies[defn.id.packageId.index]
        if isinstance(defn, ir.Global):
            externs = dep.externGlobals
        elif isinstance(defn, ir.Function):
            if METHOD in defn.flags:
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
        self.assertIn(EXTERN, defn.flags)
        expected = self.getExtern(defn)
        self.assertIs(expected, defn)

    def testExternalizeGlobal(self):
        globl = self.otherPackage.addGlobal(Name(["g"]), type=self.classTy,
                                            flags=frozenset([PUBLIC]))
        externGlobal = self.externalizer.externalizeDefn(globl)
        expected = self.makeGlobal(Name(["g"]), id=globl.id, type=self.classTy,
                                   flags=frozenset([PUBLIC, EXTERN]))
        self.assertEquals(expected, externGlobal)
        self.checkExternPosition(externGlobal)

    def testExternalizeFunction(self):
        function = self.otherPackage.addFunction(Name(["f"]), returnType=self.classTy,
                                                 typeParameters=[self.param],
                                                 parameterTypes=[self.varTy],
                                                 flags=frozenset([PUBLIC]))
        externFunction = self.externalizer.externalizeDefn(function)
        expected = ir.Function(Name(["f"]), function.id, returnType=self.classTy,
                               typeParameters=[self.externParam],
                               parameterTypes=[self.varTy],
                               flags=frozenset([PUBLIC, EXTERN]))
        self.assertEquals(expected, externFunction)

    def testExternalizeClass(self):
        clas = self.otherPackage.addClass(Name(["C"]),
                                          typeParameters=[self.param],
                                          supertypes=[self.rootClassType],
                                          constructors=[], fields=[],
                                          methods=[], elementType=self.varTy,
                                          flags=frozenset([ARRAY, FINAL, PUBLIC]))
        clasTy = ir_types.ClassType(clas, (self.varTy,))
        ctor = self.otherPackage.addFunction(Name(["C", CONSTRUCTOR_SUFFIX]),
                                             returnType=ir_types.UnitType,
                                             typeParameters=[self.param],
                                             parameterTypes=[clasTy],
                                             flags=frozenset([PUBLIC, METHOD, CONSTRUCTOR]))
        privateCtor = self.otherPackage.addFunction(Name(["C", CONSTRUCTOR_SUFFIX]),
                                                    returnType=ir_types.UnitType,
                                                    typeParameters=[self.param],
                                                    parameterTypes=[clasTy],
                                                    flags=frozenset([PRIVATE, METHOD, CONSTRUCTOR]))
        clas.constructors = [ctor, privateCtor]
        field = self.otherPackage.newField(Name(["C", "x"]),
                                           type=clasTy, flags=frozenset([PUBLIC]))
        privateField = self.otherPackage.newField(Name(["C", "y"]),
                                                  type=clasTy, flags=frozenset([PRIVATE]))
        clas.fields = [field, privateField]
        method = self.otherPackage.addFunction(Name(["C", "f"]),
                                               returnType=ir_types.UnitType,
                                               typeParameters=[self.param],
                                               parameterTypes=[clasTy],
                                               flags=frozenset([PUBLIC, METHOD]))
        privateMethod = self.otherPackage.addFunction(Name(["C", "g"]),
                                                      returnType=ir_types.UnitType,
                                                      typeParameters=[self.param],
                                                      parameterTypes=[clasTy],
                                                      flags=frozenset([PRIVATE, METHOD]))
        builtinMethod = \
            builtins.getBuiltinFunctionById(bytecode.BUILTIN_ROOT_CLASS_TO_STRING_ID)
        clas.methods = [method, privateMethod, builtinMethod]
        externClass = self.externalizer.externalizeDefn(clas)
        expected = ir.Class(Name(["C"]), clas.id, typeParameters=[self.externParam],
                            supertypes=[self.rootClassType],
                            elementType=self.varTy,
                            flags=frozenset([ARRAY, FINAL,
                                             PUBLIC, EXTERN]))
        expectedCtor = ir.Function(Name(["C", CONSTRUCTOR_SUFFIX]), ctor.id,
                                   returnType=ir_types.UnitType,
                                   typeParameters=[self.externParam],
                                   parameterTypes=[clasTy],
                                   flags=frozenset([PUBLIC, METHOD, CONSTRUCTOR, EXTERN]))
        expected.constructors = [expectedCtor]
        expectedField = ir.Field(Name(["C", "x"]), type=clasTy,
                                 flags=frozenset([PUBLIC]))
        expected.fields = [expectedField]
        expectedMethod = ir.Function(Name(["C", "f"]), method.id,
                                     returnType=ir_types.UnitType,
                                     typeParameters=[self.externParam],
                                     parameterTypes=[clasTy],
                                     flags=frozenset([PUBLIC, METHOD, EXTERN]))
        externBuiltinMethod = ir.Function(Name(["Object", "to-string"]), builtinMethod.id,
                                          returnType=ir_types.getStringType(),
                                          typeParameters=[],
                                          parameterTypes=[ir_types.getRootClassType()],
                                          flags=frozenset([EXTERN, PUBLIC, METHOD]))
        expected.methods = [expectedMethod, externBuiltinMethod]
        self.assertEquals(expected, externClass)

    def testExternalizeArrayClassWithPrivateLength(self):
        clas = self.otherPackage.addClass(Name(["C"]),
                                          typeParameters=[],
                                          supertypes=[self.rootClassType],
                                          constructors=[],
                                          methods=[], elementType=ir_types.I8Type,
                                          flags=frozenset([ARRAY, FINAL, PUBLIC]))
        lengthField = self.otherPackage.newField(Name(["C", ARRAY_LENGTH_SUFFIX]),
                                                 type=ir_types.I32Type,
                                                 flags=frozenset([ARRAY, LET, PRIVATE]))
        clas.fields = [lengthField]
        externClass = self.externalizer.externalizeDefn(clas)
        self.assertEquals([lengthField], externClass.fields)

    def testExternalizeTrait(self):
        trait = self.otherPackage.addTrait(Name(["Tr"]),
                                           typeParameters=[self.param],
                                           supertypes=[self.rootClassType],
                                           methods=[],
                                           flags=frozenset([PUBLIC]))
        traitType = ir_types.ClassType(trait, (self.varTy,))
        method = self.otherPackage.addFunction(Name(["Tr", "f"]),
                                               returnType=ir_types.UnitType,
                                               typeParameters=[self.param],
                                               parameterTypes=[traitType],
                                               flags=frozenset([PUBLIC, METHOD]))
        privateMethod = self.otherPackage.addFunction(Name(["Tr", "g"]),
                                                      returnType=ir_types.UnitType,
                                                      typeParameters=[self.param],
                                                      parameterTypes=[traitType],
                                                      flags=frozenset([PRIVATE, METHOD]))
        trait.methods = [method, privateMethod]
        externTrait = self.externalizer.externalizeDefn(trait)
        expected = ir.Trait(Name(["Tr"]), trait.id, typeParameters=[self.externParam],
                            supertypes=[self.rootClassType],
                            flags=frozenset([PUBLIC, EXTERN]))
        expectedMethod = ir.Function(Name(["Tr", "f"]), method.id,
                                     returnType=ir_types.UnitType,
                                     typeParameters=[self.externParam],
                                     parameterTypes=[traitType],
                                     flags=frozenset([EXTERN, PUBLIC, METHOD]))
        expected.methods = [expectedMethod]
        self.assertEquals(expected, externTrait)

    def testExternalizeBuiltinMethodName(self):
        method = builtins.getBuiltinFunctionById(bytecode.BUILTIN_ROOT_CLASS_EQ_OP_ID)
        externMethod = self.externalizer.externalizeMethod(method, self.dep)
        self.assertIn(method.name.short(), self.package.strings)

    def testExternalizeTypeParameter(self):
        param = self.otherPackage.addTypeParameter(Name(["S"]),
                                                   upperBound=self.classTy,
                                                   lowerBound=self.classTy,
                                                   flags=frozenset([PUBLIC, STATIC]))
        externParam = self.externalizer.externalizeDefn(param)
        expected = ir.TypeParameter(Name(["S"]), param.id,
                                    upperBound=self.classTy, lowerBound=self.classTy,
                                    flags=frozenset([PUBLIC, STATIC, EXTERN]))
        self.assertEquals(expected, externParam)

    def testExternalizeBuiltinDefn(self):
        rootClass = builtins.getRootClass()
        externClass = self.externalizer.externalizeDefn(rootClass)
        self.assertIs(rootClass, externClass)

    def testExternalizeLocalDefn(self):
        localGlobal = self.package.addGlobal(Name(["g"]), type=ir_types.UnitType)
        externGlobal = self.externalizer.externalizeDefn(localGlobal)
        self.assertIs(localGlobal, externGlobal)

    def testExternalizeVariableType(self):
        T = self.otherPackage.addTypeParameter(Name(["T"]),
                                               upperBound=ir_types.getRootClassType(),
                                               lowerBound=ir_types.getNothingClassType())
        self.externalizer.externalizeType(ir_types.VariableType(T))
        self.assertIsNotNone(T.id.externIndex)

    def testExternalizeClassType(self):
        C = self.otherPackage.addClass(Name(["C"]),
                                       typeParameters=[],
                                       supertypes=[ir_types.getRootClassType()],
                                       constructors=[], fields=[], methods=[],
                                       flags=frozenset([PUBLIC]))
        self.externalizer.externalizeType(ir_types.ClassType(C))
        self.assertIsNotNone(C.id.externIndex)

    def testExternalizeLocalClassTypeWithForeignArg(self):
        T = self.otherPackage.addTypeParameter(Name(["T"]),
                                               upperBound=ir_types.getRootClassType(),
                                               lowerBound=ir_types.getNothingClassType())
        S = self.package.addTypeParameter(Name(["S"]),
                                          upperBound=ir_types.getRootClassType(),
                                          lowerBound=ir_types.getNothingClassType())
        C = self.package.addClass(Name(["C"]),
                                  typeParameters=[S],
                                  supertypes=[ir_types.getRootClassType()],
                                  constructors=[], fields=[], methods=[])
        self.externalizer.externalizeType(ir_types.ClassType(C, (ir_types.VariableType(T),)))
        self.assertIsNotNone(T.id.externIndex)

    def testExternalizeExistentialType(self):
        X = self.otherPackage.addTypeParameter(Name(["X"]),
                                               upperBound=ir_types.getRootClassType(),
                                               lowerBound=ir_types.getNothingClassType())
        ty = ir_types.ExistentialType((X,), ir_types.VariableType(X))
        self.externalizer.externalizeType(ty)
        self.assertIsNotNone(X.id.externIndex)

# Copyright 2015-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

import builtins
import bytecode
import externalization
import flags
import ids
import ir
import ir_types
import utils_test


class TestExternalization(utils_test.TestCaseWithDefinitions):
    def setUp(self):
        super(TestExternalization, self).setUp()
        self.package = ir.Package(ids.TARGET_PACKAGE_ID)
        self.rootClassType = ir_types.getRootClassType()
        self.nothingClassType = ir_types.getNothingClassType()
        self.otherPackage = ir.Package()
        self.packageLoader = utils_test.FakePackageLoader([self.otherPackage])
        self.externalizer = externalization.Externalizer(self.package, self.packageLoader)

        field = self.otherPackage.newField(ir.Name(["x"]), type=ir_types.I64Type,
                                           flags=frozenset([flags.PUBLIC]))
        self.clas = self.otherPackage.addClass(ir.Name(["Foo"]), typeParameters=[],
                                               supertypes=[self.rootClassType],
                                               constructors=[], fields=[field],
                                               methods=[], flags=frozenset([flags.PUBLIC]))
        self.classTy = ir_types.ClassType(self.clas)
        ctor = self.otherPackage.addFunction(ir.Name([ir.CONSTRUCTOR_SUFFIX]),
                                             returnType=ir_types.UnitType,
                                             typeParameters=[], parameterTypes=[],
                                             flags=frozenset([flags.PUBLIC, flags.METHOD]))
        self.clas.constructors = [ctor]
        method = self.otherPackage.addFunction(ir.Name(["m"]), returnType=ir_types.UnitType,
                                               typeParameters=[], parameterTypes=[],
                                               flags=frozenset([flags.PUBLIC, flags.METHOD]))
        self.clas.methods = [method]
        self.param = self.otherPackage.addTypeParameter(ir.Name(["T"]),
                                                        upperBound=self.rootClassType,
                                                        lowerBound=self.nothingClassType,
                                                        flags=frozenset([flags.STATIC]))
        self.dep = self.package.ensureDependency(self.otherPackage)
        self.externParam = self.externalizer.externalizeDefn(self.param)
        self.varTy = ir_types.VariableType(self.param)

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
        globl = self.otherPackage.addGlobal(ir.Name(["g"]), type=self.classTy,
                                            flags=frozenset([flags.PUBLIC]))
        externGlobal = self.externalizer.externalizeDefn(globl)
        expected = self.makeGlobal(ir.Name(["g"]), id=globl.id, type=self.classTy,
                                   flags=frozenset([flags.PUBLIC, flags.EXTERN]))
        self.assertEquals(expected, externGlobal)
        self.checkExternPosition(externGlobal)

    def testExternalizeFunction(self):
        function = self.otherPackage.addFunction(ir.Name(["f"]), returnType=self.classTy,
                                                 typeParameters=[self.param],
                                                 parameterTypes=[self.varTy],
                                                 flags=frozenset([flags.PUBLIC]))
        externFunction = self.externalizer.externalizeDefn(function)
        expected = ir.Function(ir.Name(["f"]), function.id, returnType=self.classTy,
                               typeParameters=[self.externParam],
                               parameterTypes=[self.varTy],
                               flags=frozenset([flags.PUBLIC, flags.EXTERN]))
        self.assertEquals(expected, externFunction)

    def testExternalizeClass(self):
        clas = self.otherPackage.addClass(ir.Name(["C"]),
                                          typeParameters=[self.param],
                                          supertypes=[self.rootClassType],
                                          constructors=[], fields=[],
                                          methods=[], elementType=self.varTy,
                                          flags=frozenset([flags.ARRAY, flags.FINAL, flags.PUBLIC]))
        clasTy = ir_types.ClassType(clas, (self.varTy,))
        ctor = self.otherPackage.addFunction(ir.Name(["C", ir.CONSTRUCTOR_SUFFIX]),
                                             returnType=ir_types.UnitType,
                                             typeParameters=[self.param],
                                             parameterTypes=[clasTy],
                                             flags=frozenset([flags.PUBLIC, flags.METHOD, flags.CONSTRUCTOR]))
        privateCtor = self.otherPackage.addFunction(ir.Name(["C", ir.CONSTRUCTOR_SUFFIX]),
                                                    returnType=ir_types.UnitType,
                                                    typeParameters=[self.param],
                                                    parameterTypes=[clasTy],
                                                    flags=frozenset([flags.PRIVATE, flags.METHOD, flags.CONSTRUCTOR]))
        clas.constructors = [ctor, privateCtor]
        field = self.otherPackage.newField(ir.Name(["C", "x"]),
                                           type=clasTy, flags=frozenset([flags.PUBLIC]))
        privateField = self.otherPackage.newField(ir.Name(["C", "y"]),
                                                  type=clasTy, flags=frozenset([flags.PRIVATE]))
        clas.fields = [field, privateField]
        method = self.otherPackage.addFunction(ir.Name(["C", "f"]),
                                               returnType=ir_types.UnitType,
                                               typeParameters=[self.param],
                                               parameterTypes=[clasTy],
                                               flags=frozenset([flags.PUBLIC, flags.METHOD]))
        privateMethod = self.otherPackage.addFunction(ir.Name(["C", "g"]),
                                                      returnType=ir_types.UnitType,
                                                      typeParameters=[self.param],
                                                      parameterTypes=[clasTy],
                                                      flags=frozenset([flags.PRIVATE, flags.METHOD]))
        builtinMethod = \
            builtins.getBuiltinFunctionById(bytecode.BUILTIN_ROOT_CLASS_TO_STRING_ID)
        clas.methods = [method, privateMethod, builtinMethod]
        externClass = self.externalizer.externalizeDefn(clas)
        expected = ir.Class(ir.Name(["C"]), clas.id, typeParameters=[self.externParam],
                            supertypes=[self.rootClassType],
                            elementType=self.varTy,
                            flags=frozenset([flags.ARRAY, flags.FINAL,
                                             flags.PUBLIC, flags.EXTERN]))
        expectedCtor = ir.Function(ir.Name(["C", ir.CONSTRUCTOR_SUFFIX]), ctor.id,
                                   returnType=ir_types.UnitType,
                                   typeParameters=[self.externParam],
                                   parameterTypes=[clasTy],
                                   flags=frozenset([flags.PUBLIC, flags.METHOD, flags.CONSTRUCTOR, flags.EXTERN]))
        expected.constructors = [expectedCtor]
        expectedField = ir.Field(ir.Name(["C", "x"]), type=clasTy,
                                 flags=frozenset([flags.PUBLIC]))
        expected.fields = [expectedField]
        expectedMethod = ir.Function(ir.Name(["C", "f"]), method.id,
                                     returnType=ir_types.UnitType,
                                     typeParameters=[self.externParam],
                                     parameterTypes=[clasTy],
                                     flags=frozenset([flags.PUBLIC, flags.METHOD, flags.EXTERN]))
        externBuiltinMethod = ir.Function(ir.Name(["Object", "to-string"]), builtinMethod.id,
                                          returnType=ir_types.getStringType(),
                                          typeParameters=[],
                                          parameterTypes=[ir_types.getRootClassType()],
                                          flags=frozenset([flags.EXTERN, flags.PUBLIC, flags.METHOD]))
        expected.methods = [expectedMethod, externBuiltinMethod]
        self.assertEquals(expected, externClass)

    def testExternalizeArrayClassWithPrivateLength(self):
        clas = self.otherPackage.addClass(ir.Name(["C"]),
                                          typeParameters=[],
                                          supertypes=[self.rootClassType],
                                          constructors=[],
                                          methods=[], elementType=ir_types.I8Type,
                                          flags=frozenset([flags.ARRAY, flags.FINAL, flags.PUBLIC]))
        lengthField = self.otherPackage.newField(ir.Name(["C", ir.ARRAY_LENGTH_SUFFIX]),
                                                 type=ir_types.I32Type,
                                                 flags=frozenset([flags.ARRAY, flags.LET, flags.PRIVATE]))
        clas.fields = [lengthField]
        externClass = self.externalizer.externalizeDefn(clas)
        self.assertEquals([lengthField], externClass.fields)

    def testExternalizeTrait(self):
        trait = self.otherPackage.addTrait(ir.Name(["Tr"]),
                                           typeParameters=[self.param],
                                           supertypes=[self.rootClassType],
                                           methods=[],
                                           flags=frozenset([flags.PUBLIC]))
        traitType = ir_types.ClassType(trait, (self.varTy,))
        method = self.otherPackage.addFunction(ir.Name(["Tr", "f"]),
                                               returnType=ir_types.UnitType,
                                               typeParameters=[self.param],
                                               parameterTypes=[traitType],
                                               flags=frozenset([flags.PUBLIC, flags.METHOD]))
        privateMethod = self.otherPackage.addFunction(ir.Name(["Tr", "g"]),
                                                      returnType=ir_types.UnitType,
                                                      typeParameters=[self.param],
                                                      parameterTypes=[traitType],
                                                      flags=frozenset([flags.PRIVATE, flags.METHOD]))
        trait.methods = [method, privateMethod]
        externTrait = self.externalizer.externalizeDefn(trait)
        expected = ir.Trait(ir.Name(["Tr"]), trait.id, typeParameters=[self.externParam],
                            supertypes=[self.rootClassType],
                            flags=frozenset([flags.PUBLIC, flags.EXTERN]))
        expectedMethod = ir.Function(ir.Name(["Tr", "f"]), method.id,
                                     returnType=ir_types.UnitType,
                                     typeParameters=[self.externParam],
                                     parameterTypes=[traitType],
                                     flags=frozenset([flags.EXTERN, flags.PUBLIC, flags.METHOD]))
        expected.methods = [expectedMethod]
        self.assertEquals(expected, externTrait)

    def testExternalizeBuiltinMethodName(self):
        method = builtins.getBuiltinFunctionById(bytecode.BUILTIN_ROOT_CLASS_EQ_OP_ID)
        externMethod = self.externalizer.externalizeMethod(method, self.dep)
        self.assertIn(method.name.short(), self.package.strings)

    def testExternalizeTypeParameter(self):
        param = self.otherPackage.addTypeParameter(ir.Name(["S"]),
                                                   upperBound=self.classTy,
                                                   lowerBound=self.classTy,
                                                   flags=frozenset([flags.STATIC]))
        externParam = self.externalizer.externalizeDefn(param)
        expected = ir.TypeParameter(ir.Name(["S"]), param.id,
                                    upperBound=self.classTy, lowerBound=self.classTy,
                                    flags=frozenset([flags.STATIC, flags.EXTERN]))
        self.assertEquals(expected, externParam)

    def testExternalizeBuiltinDefn(self):
        rootClass = builtins.getRootClass()
        externClass = self.externalizer.externalizeDefn(rootClass)
        self.assertIs(rootClass, externClass)

    def testExternalizeLocalDefn(self):
        localGlobal = self.package.addGlobal(ir.Name(["g"]), type=ir_types.UnitType)
        externGlobal = self.externalizer.externalizeDefn(localGlobal)
        self.assertIs(localGlobal, externGlobal)

    def testExternalizeVariableType(self):
        T = self.otherPackage.addTypeParameter(ir.Name(["T"]),
                                               upperBound=ir_types.getRootClassType(),
                                               lowerBound=ir_types.getNothingClassType())
        self.externalizer.externalizeType(ir_types.VariableType(T))
        self.assertIsNotNone(T.id.externIndex)

    def testExternalizeClassType(self):
        C = self.otherPackage.addClass(ir.Name(["C"]),
                                       typeParameters=[],
                                       supertypes=[ir_types.getRootClassType()],
                                       constructors=[], fields=[], methods=[],
                                       flags=frozenset([flags.PUBLIC]))
        self.externalizer.externalizeType(ir_types.ClassType(C))
        self.assertIsNotNone(C.id.externIndex)

    def testExternalizeLocalClassTypeWithForeignArg(self):
        T = self.otherPackage.addTypeParameter(ir.Name(["T"]),
                                               upperBound=ir_types.getRootClassType(),
                                               lowerBound=ir_types.getNothingClassType())
        S = self.package.addTypeParameter(ir.Name(["S"]),
                                          upperBound=ir_types.getRootClassType(),
                                          lowerBound=ir_types.getNothingClassType())
        C = self.package.addClass(ir.Name(["C"]),
                                  typeParameters=[S],
                                  supertypes=[ir_types.getRootClassType()],
                                  constructors=[], fields=[], methods=[])
        self.externalizer.externalizeType(ir_types.ClassType(C, (ir_types.VariableType(T),)))
        self.assertIsNotNone(T.id.externIndex)

    def testExternalizeExistentialType(self):
        X = self.otherPackage.addTypeParameter(ir.Name(["X"]),
                                               upperBound=ir_types.getRootClassType(),
                                               lowerBound=ir_types.getNothingClassType())
        ty = ir_types.ExistentialType((X,), ir_types.VariableType(X))
        self.externalizer.externalizeType(ty)
        self.assertIsNotNone(X.id.externIndex)

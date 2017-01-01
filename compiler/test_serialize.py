# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

import builtins
import bytecode
import externalization
from flags import ABSTRACT, ARRAY, CONSTRUCTOR, EXTERN, FINAL, LET, METHOD, NATIVE, OVERRIDE, PRIVATE, PUBLIC, STATIC
import ids
import ir
import ir_types
from name import CLASS_INIT_SUFFIX, CONSTRUCTOR_SUFFIX, Name
import serialize
import utils_test


class MockFile(object):
    def __init__(self, bytes=None):
        self.bytes = bytearray(bytes) if bytes is not None else bytearray()
        self.readPtr = 0

    def read(self, n):
        if self.readPtr + n > len(self.bytes):
            raise IOError("read off the end of the file")
        result = self.bytes[self.readPtr : self.readPtr + n]
        self.readPtr += n
        return str(result)

    def write(self, s):
        self.bytes.extend(s)


class TestSerialize(utils_test.TestCaseWithDefinitions):
    def setUp(self):
        self.file = MockFile()
        self.ser = serialize.Serializer(None, self.file)
        self.des = serialize.Deserializer(self.file, utils_test.FakePackageLoader([]))

    def copyPackage(self, package):
        file = MockFile()
        ser = serialize.Serializer(package, file)
        ser.serialize()
        loader = utils_test.FakePackageLoader([])
        des = serialize.Deserializer(file, loader)
        des.deserialize()
        return des.package

    def testWriteVbn(self):
        def checkVbn(n, bytes):
            self.file.bytes = bytearray()
            self.ser.writeVbn(n)
            expected = bytearray(''.join(map(chr, bytes)))
            self.assertEquals(expected, self.file.bytes)

        checkVbn(0, [0])
        checkVbn(1, [1])
        checkVbn(-1, [0x7F])
        checkVbn(1000, [0xE8, 0x07])
        checkVbn(-1000, [0x98, 0x78])
        checkVbn(0x7FFFFFFFFFFFFFFF, [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00])
        checkVbn(-0x8000000000000000, [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01])

    def testReadVbn(self):
        def checkVbn(n, bytes):
            self.file.readPtr = 0
            self.file.bytes = bytearray(bytes)
            value = self.des.readVbn()
            self.assertEquals(n, value)
            self.assertEquals(self.file.readPtr, len(bytes))

        checkVbn(0, [0])
        checkVbn(1, [1])
        checkVbn(-1, [0x7F])
        checkVbn(1000, [0xE8, 0x07])
        checkVbn(-1000, [0x98, 0x78])
        checkVbn(0x7FFFFFFFFFFFFFFF, [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00])
        checkVbn(-0x8000000000000000, [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01])

    def testRewriteList(self):
        def write(s):
            self.file.write(s)
        def read():
            return self.file.read(1)
        expected = ['a', 'b', 'c']
        self.ser.writeList(write, expected)
        lst = self.des.readList(read)
        self.assertEquals(expected, lst)

    def testRewriteBuiltinFunctionId(self):
        function = builtins.getBuiltinFunctionById(bytecode.BUILTIN_ROOT_CLASS_TO_STRING_ID)
        self.ser.writeMethodId(function)
        outFunction = self.des.readMethodId()
        self.assertIs(function, outFunction)

    def testRewriteLocalMethodId(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        method = package.addFunction(Name(["foo"]), returnType=ir_types.UnitType,
                                     typeParameters=[], parameterTypes=[],
                                     flags=frozenset([METHOD]))
        self.ser.package = package
        self.ser.writeMethodId(method)
        self.des.package = package
        outMethod = self.des.readMethodId()
        self.assertIs(method, outMethod)

    def testRewriteForeignMethodId(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        package.buildNameIndex()
        otherPackage = ir.Package()
        method = otherPackage.addFunction(Name(["foo"]), returnType=ir_types.UnitType,
                                          typeParameters=[], parameterTypes=[],
                                          flags=frozenset([PUBLIC, METHOD]))
        loader = utils_test.FakePackageLoader([otherPackage])
        externalizer = externalization.Externalizer(package, loader)
        externMethod = externalizer.externalizeDefn(method)
        self.ser.package = package
        self.ser.writeMethodId(externMethod)
        self.des.package = package
        outMethod = self.des.readMethodId()
        self.assertIs(externMethod, outMethod)

    def testRewriteFlags(self):
        flagSet = frozenset([ABSTRACT, PUBLIC, STATIC])
        self.ser.writeFlags(flagSet)
        flagSetOut = self.des.readFlags()
        self.assertEquals(flagSet, flagSetOut)

    def testRewriteType(self):
        def checkType(ty):
            self.file.bytes = bytearray()
            self.file.readPtr = 0
            self.ser.writeType(ty)
            tyOut = self.des.readType()
            self.assertEquals(ty, tyOut)

        package = ir.Package(id=ids.TARGET_PACKAGE_ID)
        package.classes = self.des.createEmptyClassList(4, package.id)
        package.typeParameters = self.des.createEmptyTypeParameterList(3, package.id)
        self.ser.package = package
        self.des.package = package

        checkType(ir_types.UnitType)
        checkType(ir_types.BooleanType)
        checkType(ir_types.I8Type)
        checkType(ir_types.I16Type)
        checkType(ir_types.I16Type)
        checkType(ir_types.I32Type)
        checkType(ir_types.I64Type)
        checkType(ir_types.F32Type)
        checkType(ir_types.F64Type)

        nullFlags = frozenset([ir_types.NULLABLE_TYPE_FLAG])
        checkType(ir_types.ClassType(package.classes[0]))
        checkType(ir_types.ClassType(package.classes[1], (), nullFlags))
        checkType(ir_types.ClassType(package.classes[2],
                                     (ir_types.ClassType(package.classes[3]),
                                      ir_types.VariableType(package.typeParameters[1]))))
        checkType(ir_types.VariableType(package.typeParameters[1], nullFlags))
        checkType(ir_types.ExistentialType((package.typeParameters[2],),
                                           (ir_types.VariableType(package.typeParameters[2],
                                                                  nullFlags))))

    def testRewriteExternClassType(self):
        package = ir.Package(id=ids.TARGET_PACKAGE_ID)
        package.buildNameIndex()
        depPackage = ir.Package()
        loader = utils_test.FakePackageLoader([depPackage])
        depClass = depPackage.addClass(Name(["C"]), typeParameters=[],
                                       supertypes=[ir_types.getRootClassType()],
                                       constructors=[], fields=[],
                                       methods=[], flags=frozenset([PUBLIC]))
        externalizer = externalization.Externalizer(package, loader)
        externClass = externalizer.externalizeDefn(depClass)
        self.assertIn(EXTERN, externClass.flags)
        self.assertIs(depPackage, package.dependencies[0].package)
        self.assertIs(externClass, package.dependencies[0].externClasses[0])

        nullFlags = frozenset([ir_types.NULLABLE_TYPE_FLAG])
        externClassType = ir_types.ClassType(externClass, (), nullFlags)
        self.ser.writeType(externClassType)

        self.des.package = package
        outType = self.des.readType()
        self.assertEquals(externClassType, outType)

    def testRewriteName(self):
        package = ir.Package()
        package.buildNameIndex()
        foobar = Name(["foo", "bar"])
        bazquux = Name(["baz", "quux"])
        package.findOrAddName(foobar)
        package.findOrAddName(bazquux)

        self.ser.package = package
        self.ser.writeName(foobar)
        self.ser.writeName(bazquux)

        self.des.package =  package
        self.assertEquals(foobar, self.des.readName())
        self.assertEquals(bazquux, self.des.readName())

    def testRewriteGlobal(self):
        package = ir.Package()
        package.buildNameIndex()
        globl = package.addGlobal(Name(["g"]), sourceName="g", type=ir_types.I64Type,
                                  flags=frozenset([PUBLIC, LET]))
        self.ser.package = package
        self.ser.writeGlobal(globl)

        self.des.package = package
        outGlobal = ir.Global(None, globl.id)
        self.des.readGlobal(outGlobal)
        self.assertEquals(globl, outGlobal)

    def testRewriteField(self):
        package = ir.Package()
        package.buildNameIndex()
        field = package.newField(Name(["C", "foo"]), type=ir_types.I64Type,
                                 flags=frozenset([PUBLIC, LET]))
        self.ser.package = package
        self.ser.writeField(field)

        self.des.package = package
        clas = package.addClass(Name(["C"]))
        outField = self.des.readField(clas, 2)
        self.assertEquals(field, outField)
        self.assertIs(clas, outField.definingClass)
        self.assertEquals(2, outField.index)

    def testRewriteTypeParameter(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        package.buildNameIndex()
        typeParam = package.addTypeParameter(Name(["T"]),
                                             upperBound=ir_types.getRootClassType(),
                                             lowerBound=ir_types.getNothingClassType(),
                                             flags=frozenset([STATIC]))
        self.ser.package = package
        self.ser.writeTypeParameter(typeParam)
        outTypeParam = ir.TypeParameter(None, typeParam.id)
        self.des.package = package
        self.des.readTypeParameter(outTypeParam)
        self.assertEquals(typeParam, outTypeParam)

    def testRewriteForeignTypeParameter(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        package.buildNameIndex()
        otherPackage = ir.Package()
        typeParam = otherPackage.addTypeParameter(Name(["T"]),
                                                  upperBound=ir_types.getRootClassType(),
                                                  lowerBound=ir_types.getNothingClassType(),
                                                  flags=frozenset([STATIC]))
        loader = utils_test.FakePackageLoader([otherPackage])
        externalizer = externalization.Externalizer(package, loader)
        foreignTypeParam = externalizer.externalizeDefn(typeParam)
        self.ser.package = package
        self.ser.writeTypeParameter(foreignTypeParam)
        outTypeParam = ir.TypeParameter(None, foreignTypeParam.id)
        self.des.package = package
        self.des.readTypeParameter(outTypeParam)
        self.assertEquals(foreignTypeParam, outTypeParam)

    def testRewriteClass(self):
        package = ir.Package(id=ids.TARGET_PACKAGE_ID)
        package.buildNameIndex()
        typeParam = package.addTypeParameter(Name(["Foo", "T"]),
                                             upperBound=ir_types.getRootClassType(),
                                             lowerBound=ir_types.getNothingClassType(),
                                             flags=frozenset([STATIC]))
        rootType = ir_types.getRootClassType()
        trait = package.addTrait(Name(["Tr"]),
                                 typeParameters=[], supertypes=[rootType], flags=frozenset([]))
        traitType = ir_types.ClassType(trait)
        traitMethod = package.addFunction(Name(["Tr", "m"]),
                                          returnType=ir_types.UnitType, typeParameters=[],
                                          parameterTypes=[traitType],
                                          flags=frozenset([PUBLIC, ABSTRACT,
                                                           METHOD]))
        trait.methods = [traitMethod]
        field = package.newField(Name(["Foo", "x"]),
                                 type=ir_types.I64Type, flags=frozenset([PRIVATE]))
        clas = package.addClass(Name(["Foo"]), typeParameters=[typeParam],
                                supertypes=[rootType, traitType],
                                constructors=[], fields=[field],
                                methods=[], elementType=ir_types.VariableType(typeParam),
                                flags=frozenset([PUBLIC, FINAL, ARRAY]))
        ty = ir_types.ClassType(clas)
        constructor = package.addFunction(Name(["Foo", CONSTRUCTOR_SUFFIX]),
                                          returnType=ir_types.UnitType, typeParameters=[],
                                          parameterTypes=[ty],
                                          flags=frozenset([PUBLIC, METHOD]))
        clas.constructors = [constructor]
        localMethod = package.addFunction(Name(["Foo", "m"]), returnType=ir_types.I64Type,
                                          typeParameters=[], parameterTypes=[ty],
                                          flags=frozenset([PUBLIC, OVERRIDE,
                                                           METHOD]))
        otherPackage = ir.Package()
        loader = utils_test.FakePackageLoader([otherPackage])
        otherMethod = otherPackage.addFunction(Name(["Foo", "o"]),
                                               returnType=ir_types.I64Type,
                                               typeParameters=[],
                                               parameterTypes=[ir_types.getRootClassType()],
                                               flags=frozenset([PUBLIC, METHOD]))
        externalizer = externalization.Externalizer(package, loader)
        externMethod = externalizer.externalizeDefn(otherMethod)
        builtinMethod = builtins.getBuiltinFunctionById(bytecode.BUILTIN_ROOT_CLASS_TO_STRING_ID)
        clas.methods = [localMethod, externMethod, builtinMethod]

        self.ser.package = package
        self.ser.writeClass(clas)
        self.des.package = package
        outClass = ir.Class(None, clas.id)
        self.des.readClass(outClass)
        self.assertEquals(clas, outClass)

    def testRewriteForeignClass(self):
        # "Compile" a foreign package with a class we'll depend on.
        otherPackage = ir.Package(id=ids.TARGET_PACKAGE_ID, name=Name(["foo", "bar"]))
        otherPackage.buildNameIndex()
        rootType = ir_types.getRootClassType()
        trait = otherPackage.addTrait(Name(["Tr"]), typeParameters=[],
                                      supertypes=[rootType], flags=frozenset([PUBLIC]))
        traitType = ir_types.ClassType(trait)
        traitMethod = otherPackage.addFunction(Name(["Tr", "m"]),
                                               returnType=rootType,
                                               typeParameters=[],
                                               parameterTypes=[traitType],
                                               flags=frozenset([PUBLIC, METHOD, ABSTRACT]))
        trait.methods = [traitMethod]

        clas = otherPackage.addClass(Name(["C"]), supertypes=[],
                                     elementType=ir_types.I8Type,
                                     flags=frozenset([PUBLIC, ARRAY, FINAL]))
        T = otherPackage.addTypeParameter(Name(["C", "T"]),
                                          upperBound=ir_types.getRootClassType(),
                                          lowerBound=ir_types.getNothingClassType(),
                                          flags=frozenset([PUBLIC, STATIC]))
        clas.typeParameters = [T]
        classType = ir_types.ClassType(clas, (ir_types.VariableType(T),))
        init = otherPackage.addFunction(Name(["C", CLASS_INIT_SUFFIX]),
                                        returnType=ir_types.UnitType,
                                        typeParameters=[T], parameterTypes=[classType],
                                        flags=frozenset([PRIVATE, METHOD, NATIVE]))
        clas.initializer = init
        ctor = otherPackage.addFunction(Name(["C", CONSTRUCTOR_SUFFIX]),
                                        returnType=ir_types.UnitType,
                                        typeParameters=[T], parameterTypes=[classType],
                                        flags=frozenset([PUBLIC, CONSTRUCTOR, METHOD, NATIVE]))
        clas.constructors = [ctor]
        method1 = otherPackage.addFunction(Name(["C", "m"]),
                                           returnType=ir_types.VariableType(T),
                                           typeParameters=[T], parameterTypes=[classType],
                                           flags=frozenset([PUBLIC, METHOD, ABSTRACT]))

        method2 = otherPackage.addFunction(Name(["C", "n"]),
                                           returnType=ir_types.I32Type,
                                           typeParameters=[T], parameterTypes=[classType],
                                           flags=frozenset([PUBLIC, STATIC, METHOD, NATIVE]))
        clas.methods = [method1, method2]
        field = otherPackage.newField(Name(["C", "x"]), type=ir_types.VariableType(T),
                                      flags=frozenset([PUBLIC, LET]))
        clas.fields = [field]

        # "Compile" a package that depends on the foreign class.
        externPackage = self.copyPackage(otherPackage)
        foreignClass = externPackage.findClass(name=clas.name)
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        package.buildNameIndex()
        externLoader = utils_test.FakePackageLoader([externPackage])
        externalizer = externalization.Externalizer(package, externLoader)
        externClass = externalizer.externalizeDefn(foreignClass)

        # Serialize and deserialize the package.
        self.ser.package = package
        self.ser.serialize()
        loadedPackage = self.copyPackage(otherPackage)
        desLoader = utils_test.FakePackageLoader([loadedPackage])
        self.des = serialize.Deserializer(self.file, desLoader)
        self.des.deserialize()
        rewrittenPackage = self.des.package
        rewrittenClass = rewrittenPackage.dependencies[0].externClasses[0]

        # Compare properties which are safe to compare. Anything that refers back to the class
        # (e.g., receiver types of constructors and methods) is not safe to compare.
        self.assertEquals(externClass.name, rewrittenClass.name)
        self.assertEquals(externClass.typeParameters, rewrittenClass.typeParameters)
        self.assertEquals(externClass.supertypes, rewrittenClass.supertypes)
        self.assertIsNone(rewrittenClass.initializer)
        self.assertEquals(len(externClass.constructors), len(rewrittenClass.constructors))
        self.assertEquals(len(externClass.fields), len(rewrittenClass.fields))
        self.assertEquals(len(externClass.methods), len(rewrittenClass.methods))
        self.assertEquals(externClass.elementType, rewrittenClass.elementType)
        self.assertEquals(externClass.flags, rewrittenClass.flags)

    def testRewriteTrait(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        package.buildNameIndex()
        rootType = ir_types.getRootClassType()
        typeParam = package.addTypeParameter(Name(["Tr", "T"]),
                                             upperBound=rootType,
                                             lowerBound=ir_types.getNothingClassType(),
                                             flags=frozenset([STATIC]))
        trait = package.addTrait(Name(["Tr"]), typeParameters=[typeParam],
                                 supertypes=[rootType], flags=frozenset([PUBLIC]))
        traitType = ir_types.ClassType.forReceiver(trait)
        traitMethod = package.addFunction(Name(["Tr", "m"]), returnType=ir_types.UnitType,
                                          typeParameters=[typeParam],
                                          parameterTypes=[traitType],
                                          flags=frozenset([PUBLIC, METHOD,
                                                           ABSTRACT]))
        trait.methods = [traitMethod]

        self.ser.package = package
        self.ser.writeTrait(trait)
        self.des.package = package
        outTrait = ir.Trait(None, trait.id)
        self.des.readTrait(outTrait)
        self.assertEquals(trait, outTrait)

    def testRewriteForeignTrait(self):
        # "Compile" a foreign package with a trait we'll depend on.
        otherPackage = ir.Package(id=ids.TARGET_PACKAGE_ID, name=Name(["foo", "bar"]))
        otherPackage.buildNameIndex()

        rootType = ir_types.getRootClassType()
        T = otherPackage.addTypeParameter(Name(["Tr", "T"]),
                                          upperBound=rootType,
                                          lowerBound=ir_types.getNothingClassType(),
                                          flags=frozenset([PUBLIC, STATIC]))
        trait = otherPackage.addTrait(Name(["Tr"]), typeParameters=[T],
                                      supertypes=[rootType], flags=frozenset([PUBLIC]))
        traitType = ir_types.ClassType.forReceiver(trait)
        traitMethod = otherPackage.addFunction(Name(["Tr", "m"]),
                                               returnType=ir_types.UnitType,
                                               typeParameters=[T],
                                               parameterTypes=[traitType],
                                               flags=frozenset([PUBLIC, METHOD,
                                                                ABSTRACT]))
        trait.methods = [traitMethod]

        # "Compile" a package that depends on the foreign trait.
        externPackage = self.copyPackage(otherPackage)
        foreignTrait = externPackage.findTrait(name=trait.name)
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        package.buildNameIndex()
        externLoader = utils_test.FakePackageLoader([externPackage])
        externalizer = externalization.Externalizer(package, externLoader)
        externTrait = externalizer.externalizeDefn(foreignTrait)

        # Serialize and deserialize the package.
        self.ser.package = package
        self.ser.serialize()
        loadedPackage = self.copyPackage(otherPackage)
        desLoader = utils_test.FakePackageLoader([loadedPackage])
        self.des = serialize.Deserializer(self.file, desLoader)
        self.des.deserialize()
        rewrittenPackage = self.des.package
        rewrittenTrait = rewrittenPackage.dependencies[0].externTraits[0]

        # Compare properties which are safe to compare. Anything that refers back to the trait
        # (e.g., receiver types of constructors and methods) is not safe to compare.
        self.assertEquals(externTrait.name, rewrittenTrait.name)
        self.assertEquals(externTrait.typeParameters, rewrittenTrait.typeParameters)
        self.assertEquals(externTrait.supertypes, rewrittenTrait.supertypes)
        self.assertEquals(len(externTrait.methods), len(rewrittenTrait.methods))
        self.assertEquals(externTrait.flags, rewrittenTrait.flags)

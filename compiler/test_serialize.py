# Copyright 2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

import builtins
import bytecode
import flags
import ids
import ir
import ir_types
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


class TestSerialization(unittest.TestCase):
    def setUp(self):
        self.file = MockFile()
        self.ser = serialize.Serializer(None, self.file)
        self.des = serialize.Deserializer(self.file)

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
        method = package.addFunction("foo", None, ir_types.UnitType, [], [],
                                      None, None, frozenset([flags.METHOD]))
        self.ser.package = package
        self.ser.writeMethodId(method)
        self.des.package = package
        outMethod = self.des.readMethodId()
        self.assertIs(method, outMethod)

    def testRewriteForeignMethodId(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        otherPackage = ir.Package()
        method = otherPackage.addFunction("foo", None, ir_types.UnitType, [], [],
                                           None, None, frozenset([flags.METHOD]))
        method.clas = builtins.getRootClass()
        loader = utils_test.MockPackageLoader([otherPackage])
        externMethod = package.externalize(method, loader)
        self.ser.package = package
        self.ser.writeMethodId(externMethod)
        self.des.package = package
        outMethod = self.des.readMethodId()
        self.assertIs(externMethod, outMethod)

    def testRewriteFlags(self):
        flagSet = frozenset([flags.ABSTRACT, flags.PUBLIC, flags.STATIC])
        self.ser.writeFlags(flagSet)
        flagSetOut = self.des.readFlags()
        self.assertEquals(flagSet, flagSetOut)

    def testRewriteName(self):
        package = ir.Package()
        package.strings = ["foo", "bar", "baz"]
        self.ser.package = package
        self.des.package = package

        self.ser.writeName("foo")
        nameOut = self.des.readName()
        self.assertEquals("foo", nameOut)

    def testRewriteType(self):
        def checkType(ty):
            self.file.bytes = bytearray()
            self.file.readPtr = 0
            self.ser.writeType(ty)
            tyOut = self.des.readType()
            self.assertEquals(ty, tyOut)

        package = ir.Package(id=ids.TARGET_PACKAGE_ID)
        package.classes = self.des.createEmptyClassList(4, package.id)
        package.typeParameters = self.des.createEmptyTypeParameterList(2, package.id)
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

    def testRewriteExternClassType(self):
        package = ir.Package(id=ids.TARGET_PACKAGE_ID)
        depPackage = ir.Package()
        loader = utils_test.MockPackageLoader([depPackage])
        depClass = depPackage.addClass("C", None, [], [ir_types.getRootClassType()], None,
                                       [], [], [], frozenset([flags.PUBLIC]))
        externClass = package.externalize(depClass, loader)
        self.assertIn(flags.EXTERN, externClass.flags)
        self.assertIs(depPackage, package.dependencies[0].package)
        self.assertIs(externClass, package.dependencies[0].externClasses[0])

        nullFlags = frozenset([ir_types.NULLABLE_TYPE_FLAG])
        externClassType = ir_types.ClassType(externClass, (), nullFlags)
        self.ser.writeType(externClassType)

        self.des.package = package
        package.externTypes = []
        outType = self.des.readType()
        self.assertEquals(externClassType, outType)
        self.assertEquals([externClassType], package.externTypes)

    def testRewriteName(self):
        package = ir.Package()
        package.findOrAddString("foo")
        package.findOrAddString("bar")

        self.ser.package = package
        self.ser.writeName(u"foo")
        self.ser.writeName(u"bar")

        self.des.package =  package
        self.assertEquals(u"foo", self.des.readName())
        self.assertEquals(u"bar", self.des.readName())

    def testRewriteField(self):
        package = ir.Package()
        field = package.newField("foo", None, ir_types.I64Type,
                                 frozenset([flags.PUBLIC, flags.LET]))
        self.ser.package = package
        self.ser.writeField(field)

        self.des.package = package
        outField = self.des.readField()

    def testRewriteTypeParameter(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        typeParam = package.addTypeParameter("T", None,
                                             ir_types.getRootClassType(),
                                             ir_types.getNothingClassType(),
                                             frozenset([flags.STATIC]))
        self.ser.package = package
        self.ser.writeTypeParameter(typeParam)
        outTypeParam = ir.TypeParameter(None, None, typeParam.id, None, None, None)
        self.des.package = package
        self.des.readTypeParameter(outTypeParam)
        self.assertEquals(typeParam, outTypeParam)

    def testRewriteClass(self):
        package = ir.Package(ids.TARGET_PACKAGE_ID)
        typeParam = package.addTypeParameter("T", None,
                                             ir_types.getRootClassType(),
                                             ir_types.getNothingClassType(),
                                             frozenset([flags.STATIC]))
        supertype = ir_types.getRootClassType()
        field = package.newField("x", None, ir_types.I64Type, frozenset([flags.PRIVATE]))
        clas = package.addClass("Foo", None, [typeParam], [supertype], None, None,
                                [field], None, frozenset([flags.PUBLIC]))
        ty = ir_types.ClassType(clas)
        constructor = package.addFunction("$constructor", None, ir_types.UnitType, [],
                                          [ty], None, None,
                                          frozenset([flags.PUBLIC, flags.METHOD]))
        constructor.clas = clas
        clas.constructors = [constructor]
        localMethod = package.addFunction("m", None, ir_types.I64Type, [],
                                          [ty], None, None,
                                          frozenset([flags.PUBLIC, flags.METHOD]))
        localMethod.clas = clas
        otherPackage = ir.Package()
        loader = utils_test.MockPackageLoader([otherPackage])
        otherMethod = otherPackage.addFunction("o", None, ir_types.I64Type, [],
                                               [ir_types.getRootClassType()], None, None,
                                               frozenset([flags.PUBLIC, flags.METHOD]))
        otherMethod.clas = builtins.getRootClass()
        externMethod = package.externalize(otherMethod, loader)
        builtinMethod = builtins.getBuiltinFunctionById(bytecode.BUILTIN_ROOT_CLASS_TO_STRING_ID)
        clas.methods = [localMethod, externMethod, builtinMethod]

        self.ser.package = package
        self.ser.writeClass(clas)
        self.des.package = package
        outClass = ir.Class(None, None, clas.id, None, None, None, None, None, None, None)
        self.des.readClass(outClass)
        self.assertEquals(clas, outClass)
        for method in outClass.constructors + outClass.methods:
            self.assertIs(method.parameterTypes[0].clas, method.clas)

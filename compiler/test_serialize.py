# Copyright 2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

import flags
import ids
import ir
import ir_types
import serialize

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

    def testRewriteIdList(self):
        globals = self.des.createEmptyGlobalList(3, ids.TARGET_PACKAGE_ID)
        self.ser.writeIdList(globals)
        globalsOut = self.des.readList(self.des.readId, globals)
        self.assertEquals(globals, globalsOut)

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

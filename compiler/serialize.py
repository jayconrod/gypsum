# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import struct
import sys
import os

from builtins import *
from flags import *
from ir import *
from ir_instructions import *
from ir_types import *
from utils import *

def serialize(package, fileName):
    if fileName == "-":
        outFile = sys.stdout
        shouldClose = False
    else:
        outFile = open(fileName, "wb")
        shouldClose = True
    try:
        serializer = Serializer(package, outFile)
        serializer.serialize()
    finally:
        if shouldClose:
            outFile.close()


class Serializer(object):
    def __init__(self, package, outFile):
        self.package = package
        self.outFile = outFile

    def serialize(self):
        self.writeHeader()
        for s in self.package.strings:
            self.writeString(s)
        for f in self.package.functions:
            self.writeFunction(f)
        for c in self.package.classes:
            self.writeClass(c)
        for p in self.package.typeParameters:
            self.writeTypeParameter(p)

    def writeHeader(self):
        self.outFile.write(struct.pack("<Ihhqiiiii",
                                       0x676b7073,   # magic number
                                       0,            # major version
                                       7,            # minor version
                                       0,            # flags
                                       len(self.package.strings),
                                       len(self.package.functions),
                                       len(self.package.classes),
                                       len(self.package.typeParameters),
                                       self.package.entryFunction))

    def rewrite(self, format, value, offset, whence=os.SEEK_SET):
        self.outFile.seek(offset, whence)
        self.outFile.write(struct.pack("<" + format, value))
        self.outFile.seek(0, os.SEEK_END)

    def writeString(self, s):
        length = len(s)
        encoded = s.encode("utf-8")
        size = len(encoded)
        self.writeVbn(length)
        self.writeVbn(size)
        self.outFile.write(encoded)

    def writeFunction(self, function):
        self.writeFlags(function.flags)
        self.writeList(lambda p: self.writeVbn(p.id), function.typeParameters)
        self.writeType(function.returnType)
        self.writeVbn(len(function.parameterTypes))
        for ty in function.parameterTypes:
            self.writeType(ty)
        localsSize = 8 * len(filter(lambda v: v.kind is LOCAL, function.variables))
        self.writeVbn(localsSize)
        instructions, blockOffsetTable = self.encodeInstructions(function)
        self.writeVbn(len(instructions))
        self.outFile.write(instructions)
        self.writeVbn(len(blockOffsetTable))
        for offset in blockOffsetTable:
            self.writeVbn(offset)

    def encodeInstructions(self, function):
        buf = bytearray()
        blockOffsetTable = []

        # Translate the instructions in the blocks into raw bytecode.
        for block in function.blocks:
            blockOffsetTable.append(len(buf))
            for inst in block.instructions:
                buf.append(inst.opcode())
                if isinstance(inst, f32):
                    self.encodeFloat(32, inst.op(0), buf)
                elif isinstance(inst, f64):
                    self.encodeFloat(64, inst.op(0), buf)
                else:
                    for i in xrange(inst.operandCount()):
                        self.encodeVbn(inst.op(i), buf)
        return buf, blockOffsetTable

    def writeClass(self, clas):
        self.writeFlags(clas.flags)
        self.writeType(clas.supertypes[0])
        self.writeVbn(len(clas.fields))
        for field in clas.fields:
            self.writeField(field)
        self.writeVbn(len(clas.constructors))
        for ctor in clas.constructors:
            self.writeVbn(ctor.id)
        self.writeVbn(len(clas.methods))
        for method in clas.methods:
            self.writeVbn(method.id)

    def writeField(self, field):
        self.writeFlags(field.flags)
        self.writeType(field.type)

    def writeTypeParameter(self, typeParameter):
        self.writeFlags(typeParameter.flags)
        self.writeType(typeParameter.upperBound)
        self.writeType(typeParameter.lowerBound)

    def writeType(self, type):
        # TODO: serialize this in a way that doesn't couple us so closely to Type::Form
        id = None
        if type is UnitType:
            form = 0
        elif type is BooleanType:
            form = 1
        elif type is I8Type:
            form = 2
        elif type is I16Type:
            form = 3
        elif type is I32Type:
            form = 4
        elif type is I64Type:
            form = 5
        elif type is F32Type:
            form = 6
        elif type is F64Type:
            form = 7
        elif isinstance(type, ClassType):
            form = 8
            id = type.clas.id
        else:
            assert isinstance(type, VariableType)
            form = 9
            id = type.typeParameter.id
        flags = 0
        if NULLABLE_TYPE_FLAG in type.flags:
            flags = flags | 1
        bits = form | flags << 4
        self.writeVbn(bits)
        if id is not None:
            self.writeVbn(id)

    def writeFlags(self, flags):
        bits = flagSetToFlagBits(flags)
        self.outFile.write(struct.pack("<I", bits))

    def writeList(self, writer, list):
        self.writeVbn(len(list))
        for elem in list:
            writer(elem)

    def writeVbn(self, value):
        buf = bytearray()
        self.encodeVbn(value, buf)
        self.outFile.write(buf)

    def encodeVbn(self, value, buf):
        offset = 0
        done = False
        while not done:
            bits = value & 0x7F
            value >>= 7
            done = (value == 0 or ~value == 0) and \
                   bit(bits, 6) == bit(value, 0)
            if not done:
                bits |= 0x80
            buf.append(bits)

    def encodeFloat(self, width, value, buf):
        format = "<f" if width == 32 else "<d"
        fbuf = struct.pack(format, value)
        buf += fbuf

# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import struct
import sys
import os

import builtins
import flags
import ir
import ir_instructions
import ir_types
import utils

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
        for g in self.package.globals:
            self.writeGlobal(g)
        for f in self.package.functions:
            self.writeFunction(f)
        for c in self.package.classes:
            self.writeClass(c)
        for p in self.package.typeParameters:
            self.writeTypeParameter(p)

    def writeHeader(self):
        self.outFile.write(struct.pack("<Ihhqiiiiiii",
                                       0x676b7073,   # magic number
                                       0,            # major version
                                       12,           # minor version
                                       0,            # flags
                                       len(self.package.strings),
                                       len(self.package.globals),
                                       len(self.package.functions),
                                       len(self.package.classes),
                                       len(self.package.typeParameters),
                                       self.package.entryFunction,
                                       self.package.initFunction))
        self.writeString(self.package.name)
        self.writeString(self.package.version)

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

    def writeGlobal(self, globl):
        self.writeFlags(globl.flags)
        self.writeType(globl.type)

    def writeFunction(self, function):
        self.writeFlags(function.flags)
        self.writeList(lambda p: self.writeVbn(p.id), function.typeParameters)
        self.writeType(function.returnType)
        self.writeVbn(len(function.parameterTypes))
        for ty in function.parameterTypes:
            self.writeType(ty)
        assert function.blocks is not None or flags.ABSTRACT in function.flags
        if function.blocks is not None:
            localsSize = 8 * len(filter(lambda v: v.kind is ir.LOCAL, function.variables))
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
                if isinstance(inst, ir_instructions.f32):
                    self.encodeFloat(32, inst.op(0), buf)
                elif isinstance(inst, ir_instructions.f64):
                    self.encodeFloat(64, inst.op(0), buf)
                else:
                    for i in xrange(inst.operandCount()):
                        self.encodeVbn(inst.op(i), buf)
        return buf, blockOffsetTable

    def writeClass(self, clas):
        self.writeFlags(clas.flags)
        self.writeIdList(clas.typeParameters)
        self.writeType(clas.supertypes[0])
        self.writeList(self.writeField, clas.fields)
        self.writeIdList(clas.constructors)
        self.writeIdList(clas.methods)

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
        if type is ir_types.UnitType:
            form = 0
        elif type is ir_types.BooleanType:
            form = 1
        elif type is ir_types.I8Type:
            form = 2
        elif type is ir_types.I16Type:
            form = 3
        elif type is ir_types.I32Type:
            form = 4
        elif type is ir_types.I64Type:
            form = 5
        elif type is ir_types.F32Type:
            form = 6
        elif type is ir_types.F64Type:
            form = 7
        elif isinstance(type, ir_types.ClassType):
            form = 8
            id = type.clas.id
        else:
            assert isinstance(type, ir_types.VariableType)
            form = 9
            id = type.typeParameter.id
        flags = 0
        if ir_types.NULLABLE_TYPE_FLAG in type.flags:
            flags = flags | 1
        bits = form | flags << 4
        self.writeVbn(bits)
        if id is not None:
            self.writeVbn(id)
        if isinstance(type, ir_types.ClassType):
            self.writeList(self.writeType, type.typeArguments)

    def writeFlags(self, flags_var):
        bits = flags.flagSetToFlagBits(flags_var)
        self.outFile.write(struct.pack("<I", bits))

    def writeIdList(self, list):
        self.writeList(lambda elem: self.writeVbn(elem.id), list)

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
                   utils.bit(bits, 6) == utils.bit(value, 0)
            if not done:
                bits |= 0x80
            buf.append(bits)

    def encodeFloat(self, width, value, buf):
        format = "<f" if width == 32 else "<d"
        fbuf = struct.pack(format, value)
        buf += fbuf

__all__ = ["serialize"]

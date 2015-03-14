# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import struct
import sys
import os

import builtins
import flags
import ids
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


def deserialize(fileName):
    try:
        with open(fileName, "rb") as inFile:
            deserializer = Deserializer(inFile)
            deserializer.deserialize()
            return deserializer.package
    except ValueError as exn:
        raise IOError(exn)


HEADER_FORMAT = "<Ihhqiiiiiiii"
MAGIC = 0x676b7073
MAJOR_VERSION = 0
MINOR_VERSION = 17

FLAG_FORMAT = "<i"


class Serializer(object):
    def __init__(self, package, outFile):
        self.package = package
        self.outFile = outFile

    def serialize(self):
        self.writeHeader()
        for d in self.package.dependencies:
            self.writeDependencyHeader(d)
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
        for d in self.package.dependencies:
            self.writeDependency(d)

    def writeHeader(self):
        entryFunctionIndex = self.package.entryFunction.index \
                             if self.package.entryFunction is not None \
                             else -1
        initFunctionIndex = self.package.initFunction.index \
                            if self.package.initFunction is not None \
                            else -1
        self.outFile.write(struct.pack(HEADER_FORMAT,
                                       MAGIC,
                                       MAJOR_VERSION,
                                       MINOR_VERSION,
                                       0,
                                       len(self.package.strings),
                                       len(self.package.globals),
                                       len(self.package.functions),
                                       len(self.package.classes),
                                       len(self.package.typeParameters),
                                       len(self.package.dependencies),
                                       entryFunctionIndex,
                                       initFunctionIndex))
        self.writeString(str(self.package.name))
        self.writeString(str(self.package.version))

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
        self.writeName(globl.name)
        self.writeFlags(globl.flags)
        self.writeType(globl.type)

    def writeFunction(self, function):
        self.writeName(function.name)
        self.writeFlags(function.flags)
        self.writeList(lambda p: self.writeVbn(p.id.index), function.typeParameters)
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
        self.writeName(clas.name)
        self.writeFlags(clas.flags)
        self.writeIdList(clas.typeParameters)
        self.writeType(clas.supertypes[0])
        self.writeList(self.writeField, clas.fields)
        self.writeIdList(clas.constructors)
        self.writeIdList(clas.methods)

    def writeField(self, field):
        self.writeName(field.name)
        self.writeFlags(field.flags)
        self.writeType(field.type)

    def writeTypeParameter(self, typeParameter):
        self.writeName(typeParameter.name)
        self.writeFlags(typeParameter.flags)
        self.writeType(typeParameter.upperBound)
        self.writeType(typeParameter.lowerBound)

    def writeDependencyHeader(self, dependency):
        self.writeString(dependency.dependencyString())
        self.writeVbn(len(dependency.globals))
        self.writeVbn(len(dependency.functions))
        self.writeVbn(len(dependency.classes))

    def writeDependency(self, dependency):
        for g in dependency.globals:
            self.writeGlobal(g)
        for f in dependency.functions:
            self.writeFunction(f)
        for c in dependency.classes:
            self.writeClass(c)

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
            self.writeVbn(id.index)
        if isinstance(type, ir_types.ClassType):
            self.writeList(self.writeType, type.typeArguments)

    def writeName(self, name):
        index = self.package.findString(name)
        self.writeVbn(index)

    def writeFlags(self, flags_var):
        bits = flags.flagSetToFlagBits(flags_var)
        self.outFile.write(struct.pack(FLAG_FORMAT, bits))

    def writeIdList(self, list):
        self.writeList(lambda elem: self.writeVbn(elem.id.index), list)

    def writeList(self, writer, list):
        self.writeVbn(len(list))
        for elem in list:
            writer(elem)

    def writeVbn(self, value):
        buf = bytearray()
        self.encodeVbn(value, buf)
        self.outFile.write(buf)

    def encodeVbn(self, value, buf):
        sign = 1 if value < 0 else 0
        doneValue = -1 if value < 0 else 0

        shift = 0
        more = True
        while more and shift < 63:
            bits = (value >> shift) & 0x7F
            shift += 7
            lastSign = bits >> 6
            more = (value >> shift) != doneValue or lastSign != sign
            if more:
                bits |= 0x80
            buf.append(bits)
        if more:
            bits = (value >> shift) & 1
            buf.append(bits)

    def encodeFloat(self, width, value, buf):
        format = "<f" if width == 32 else "<d"
        fbuf = struct.pack(format, value)
        buf += fbuf


class Deserializer(object):
    def __init__(self, inFile):
        self.inFile = inFile
        self.package = ir.Package()
        self.entryFunctionIndex = None
        self.initFunctionIndex = None

    def deserialize(self):
        self.readHeader()
        for i in xrange(len(self.package.dependencies)):
            self.package.dependencies[i] = self.readDependencyHeader()
        for i in xrange(len(self.package.strings)):
            self.package.strings[i] = self.readString()
        for gbl in self.package.globals:
            self.readGlobal(gbl)
        for func in self.package.functions:
            self.readFunction(func)
        for clas in self.package.classes:
            self.readClass(clas)
        for param in self.package.typeParameters:
            self.readTypeParameter(param)
        for dep in self.package.dependencies:
            self.readDependency(dep)

    def readHeader(self):
        headerSize = struct.calcsize(HEADER_FORMAT)
        headerBytes = self.inFile.read(headerSize)
        headers = struct.unpack(HEADER_FORMAT, headerBytes)
        magic, major, minor = headers[0:3]
        if magic != MAGIC or major != MAJOR_VERSION or minor != MINOR_VERSION:
            raise IOError("package headers don't look valid")

        flags = headers[3]

        stringCount = headers[4]
        self.package.strings = [None] * stringCount

        # We pre-allocate top-level definitions so they can point to each other if we read
        # them out of order. This is particularly important for type definitions.
        globalCount = headers[5]
        self.package.globals = self.createEmptyGlobalList(globalCount, self.package.id)

        functionCount = headers[6]
        self.package.functions = self.createEmptyFunctionList(functionCount, self.package.id)

        classCount = headers[7]
        self.package.classes = self.createEmptyClassList(classCount, self.package.id)

        typeParamCount = headers[8]
        self.package.typeParameters = self.createEmptyTypeParameterList(typeParamCount,
                                                                        self.package.id)

        depCount = headers[9]
        self.package.dependencies = [None] * depCount

        entryFunctionIndex = headers[10]
        if entryFunctionIndex != -1:
            if entryFunctionIndex < 0 or entryFunctionIndex >= len(self.package.functions):
                raise IOError("invalid entry function index")
            self.package.entryFunction = self.package.functions[entryFunctionIndex]
        initFunctionIndex = headers[11]
        if initFunctionIndex != -1:
            if initFunctionIndex < 0 or initFunctionIndex >= len(self.package.functions):
                raise IOError("invalid init function index")
            self.package.initFunction = self.package.functions[initFunctionIndex]

        nameStr = self.readString()
        self.package.name = ir.PackageName.fromString(nameStr)
        versionStr = self.readString()
        self.package.version = ir.PackageVersion.fromString(versionStr)

    def createEmptyGlobalList(self, count, packageId):
        gids = (ids.DefnId(packageId, ids.DefnId.GLOBAL, i) for i in xrange(count))
        globals = list(ir.Global(None, None, id, None, None) for id in gids)
        return globals

    def createEmptyFunctionList(self, count, packageId):
        fids = (ids.DefnId(packageId, ids.DefnId.FUNCTION, i) for i in xrange(count))
        functions = list(ir.Function(None, None, id, None, None, None, None, None, None)
                         for id in fids)
        return functions

    def createEmptyClassList(self, count, packageId):
        cids = (ids.DefnId(self.package.id, ids.DefnId.CLASS, i) for i in xrange(count))
        classes = list(ir.Class(None, None, id, None, None, None, None, None, None, None)
                       for id in cids)
        return classes

    def createEmptyTypeParameterList(self, count, packageId):
        tids = (ids.DefnId(packageId, ids.DefnId.TYPE_PARAMETER, i) for i in xrange(count))
        params = list(ir.TypeParameter(None, None, id, None, None, None) for id in tids)
        return params

    def readString(self):
        length = self.readVbn()
        size = self.readVbn()
        encoded = self.inFile.read(size)
        return encoded.decode("utf-8")

    def readGlobal(self, globl):
        globl.name = self.readName()
        globl.flags = self.readFlags()
        globl.type = self.readType()

    def readFunction(self, function):
        function.name = self.readName()
        function.flags = self.readFlags()
        function.typeParameters = self.readList(self.readId, self.package.typeParameters)
        function.returnType = self.readType()
        function.parameterTypes = self.readList(self.readType)
        if flags.ABSTRACT not in function.flags:
            localsSize = self.readVbn()
            instructionsSize = self.readVbn()
            instructionsBuffer = self.inFile.read(instructionsSize)
            blockOffsets = self.readList(self.readVbn)
            function.blocks = self.decodeInstructions(instructionsBuffer, blockOffsets)

    def decodeInstructions(self, instructionsBuffer, blockOffsets):
        # TODO: implement if we ever actually need this.
        return None

    def readClass(self, clas):
        clas.name = self.readName()
        clas.flags = self.readFlags()
        clas.typeParameters = self.readList(self.readId, self.package.typeParameters)
        clas.supertype = self.readType()
        clas.fields = self.readList(self.readField)
        clas.constructors = self.readList(self.readId, self.package.functions)
        clas.methods = self.readList(self.readId, self.package.methods)

    def readField(self):
        name = self.readName()
        ty = self.readType()
        flags = self.readFlags()
        field = ir.Field(name, None, ty, flags)
        return field

    def readTypeParameter(self, param):
        param.name = self.readName()
        param.flags = self.readFlags()
        param.upperBound = self.readType()
        param.lowerBound = self.readType()

    def readDependencyHeader(self, index):
        depStr = self.readString()
        dep = ir.PackageDependency.fromString(depStr)
        packageId = ids.PackageId(index)
        globalCount = self.readVbn()
        dep.globals = self.createEmptyGlobalList(globalCount, packageId)
        functionCount = self.readVbn()
        dep.functions = self.createEmptyFunctionList(functionCount, packageId)
        classCount = self.readVbn()
        dep.classes = self.createEmptyClassList(classCount, packageId)

    def readDependency(self, dep):
        for g in dep.globals:
            self.readGlobal(g)
        for f in dep.functions:
            self.readFunction(f)
        for c in dep.classes:
            self.readClass(c)

    def readType(self):
        bits = self.readVbn()
        form = bits & 0xF
        flagBits = bits >> 4

        flags = []
        if (flagBits & 1) != 0:
            flags.append(ir_types.NULLABLE_TYPE_FLAG)
        if (flagBits & ~1) != 0:
            raise IOError("invalid type flags")
        flags = frozenset(flags)

        ty = None
        if form == 0:
            ty = ir_types.UnitType
        elif form == 1:
            ty = ir_types.BooleanType
        elif form == 2:
            ty = ir_types.I8Type
        elif form == 3:
            ty = ir_types.I16Type
        elif form == 4:
            ty = ir_types.I32Type
        elif form == 5:
            ty = ir_types.I64Type
        elif form == 6:
            ty = ir_types.F32Type
        elif form == 7:
            ty = ir_types.F64Type
        elif form == 8:
            clas = self.readId(self.package.classes)
            typeArgs = tuple(self.readList(self.readType))
            ty = ir_types.ClassType(clas, typeArgs, flags)
        elif form == 9:
            param = self.readId(self.package.typeParameters)
            ty = ir_types.VariableType(param, flags)
        else:
            raise IOError("invalid type flags")

        if ty.isPrimitive() and flagBits != 0:
            raise IOError("invalid type flags")

        return ty

    def readName(self):
        return self.readId(self.package.strings)

    def readFlags(self):
        size = struct.calcsize(FLAG_FORMAT)
        bits = struct.unpack(FLAG_FORMAT, self.inFile.read(size))[0]
        flagSet = flags.flagBitsToFlagSet(bits)
        return flagSet

    def readList(self, reader, *args):
        n = self.readVbn()
        elems = []
        for i in xrange(n):
            elem = reader(*args)
            elems.append(elem)
        return elems

    def readId(self, collection):
        index = self.readVbn()
        if index < 0 or index >= len(collection):
            raise IOError("invalid index")
        return collection[index]

    def readVbn(self):
        n = 0
        shift = 0
        more = True
        while shift < 63 and more:
            b = ord(self.inFile.read(1))
            more = (b & 0x80) != 0
            n |= (b & 0x7F) << shift
            shift += 7
        if more:
            b = ord(self.inFile.read(1))
            if (b & 1) > 1:
                raise IOError("VBN overflow")
            n |= b << shift
            shift += 1
        negate = (n & (1 << (shift - 1))) != 0
        if negate:
            n = -(1 << shift) + n
        return n

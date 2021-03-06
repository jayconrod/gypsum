# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import struct
import sys
import os

import builtins
from flags import (
    ABSTRACT,
    CONSTRUCTOR,
    EXTERN,
    METHOD,
    NATIVE,
    OVERRIDE,
    PUBLIC,
    STATIC,
    flagBitsToFlagSet,
    flagSetToFlagBits,
)
import ids
import ir
import ir_instructions
import ir_types
from name import Name
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
    except:
        if fileName != "-":
            os.remove(fileName)
        raise
    finally:
        if shouldClose:
            outFile.close()


def deserialize(fileName, packageLoader):
    try:
        with open(fileName, "rb") as inFile:
            deserializer = Deserializer(inFile, packageLoader)
            deserializer.deserialize()
            return deserializer.package
    except (ValueError, IndexError) as exn:
        raise IOError(exn)


def deserializeNameAndVersion(fileName):
    with open(fileName, "rb") as inFile:
        deserializer = Deserializer(inFile, None)
        return deserializer.deserializeNameAndVersion()


HEADER_FORMAT = "<Ihhqiiiiiiiii"
MAGIC = 0x676b7073
MAJOR_VERSION = 0
MINOR_VERSION = 23

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
        for n in self.package.names:
            self.writeName(n)
        for d in self.package.dependencies:
            self.writeDependency(d)
        for g in self.package.globals:
            self.writeGlobal(g)
        for f in self.package.functions:
            self.writeFunction(f)
        for c in self.package.classes:
            self.writeClass(c)
        for t in self.package.traits:
            self.writeTrait(t)

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
                                       len(self.package.names),
                                       len(self.package.globals),
                                       len(self.package.functions),
                                       len(self.package.classes),
                                       len(self.package.traits),
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
        encoded = s.encode("utf-8")
        length = len(encoded)
        self.writeVbn(length)
        self.outFile.write(encoded)

    def writeStringIndex(self, s):
        index = self.package.findString(s)
        self.writeVbn(index)

    def writeGlobal(self, globl):
        self.writeNameIndex(globl.name)
        self.writeOption(self.writeStringIndex, globl.sourceName)
        self.writeFlags(globl.flags)
        self.writeType(globl.type)

    def writeFunction(self, function):
        self.writeNameIndex(function.name)
        self.writeOption(self.writeStringIndex, function.sourceName)
        self.writeFlags(function.flags)
        self.writeList(self.writeTypeParameter, function.typeParameters)
        self.writeType(function.returnType)
        self.writeList(self.writeType, function.parameterTypes)
        if EXTERN not in function.flags:
            self.writeDefiningClassId(function.definingClass)
        assert function.overrides is None or \
            (EXTERN not in function.flags and OVERRIDE in function.flags)
        if function.overrides is not None:
            self.writeList(self.writeMethodId, function.overrides)
        assert (function.instTypes is None) == (function.blocks is None)
        if function.instTypes is not None:
            self.writeList(self.writeType, function.instTypes)

        assert function.blocks is not None or \
               0 < len(frozenset([ABSTRACT, EXTERN, NATIVE]) & function.flags)

        if function.blocks is not None:
            localsSize = 8 * len(filter(lambda v: v.kind is ir.LOCAL, function.variables))
            self.writeVbn(localsSize)
            instructions, blockOffsetTable = self.encodeInstructions(function)
            self.writeVbn(len(instructions))
            self.outFile.write(instructions)
            self.writeList(self.writeVbn, blockOffsetTable)

    def encodeInstructions(self, function):
        buf = bytearray()
        blockOffsetTable = []

        # Translate the instructions in the blocks into raw bytecode.
        for block in function.blocks:
            blockOffsetTable.append(len(buf))
            for inst in block.instructions:
                buf.append(inst.opcode())
                if isinstance(inst, ir_instructions.branchl):
                    self.encodeVbn(inst.operandCount(), buf)
                    for i in xrange(inst.operandCount()):
                        self.encodeVbn(inst.op(i), buf)
                elif isinstance(inst, ir_instructions.f32):
                    self.encodeFloat(32, inst.op(0), buf)
                elif isinstance(inst, ir_instructions.f64):
                    self.encodeFloat(64, inst.op(0), buf)
                else:
                    for i in xrange(inst.operandCount()):
                        self.encodeVbn(inst.op(i), buf)
        return buf, blockOffsetTable

    def writeClass(self, clas):
        self.writeNameIndex(clas.name)
        self.writeOption(self.writeStringIndex, clas.sourceName)
        self.writeFlags(clas.flags)
        self.writeList(self.writeTypeParameter, clas.typeParameters)
        self.writeList(self.writeType, clas.supertypes)
        self.writeList(self.writeField, clas.fields)
        if clas.isForeign():
            self.writeForeignMethodList(clas.constructors)
            self.writeForeignMethodList(clas.methods)
        else:
            self.writeMethodList(clas.constructors)
            self.writeMethodList(clas.methods)
        self.writeOption(self.writeType, clas.elementType)

    def writeField(self, field):
        self.writeNameIndex(field.name)
        self.writeOption(self.writeStringIndex, field.sourceName)
        self.writeFlags(field.flags)
        self.writeType(field.type)

    def writeTrait(self, trait):
        self.writeNameIndex(trait.name)
        self.writeOption(self.writeStringIndex, trait.sourceName)
        self.writeFlags(trait.flags)
        self.writeList(self.writeTypeParameter, trait.typeParameters)
        self.writeList(self.writeType, trait.supertypes)
        if trait.isForeign():
            self.writeForeignMethodList(trait.methods)
        else:
            self.writeMethodList(trait.methods)

    def writeTypeParameter(self, typeParameter):
        self.writeNameIndex(typeParameter.name)
        self.writeOption(self.writeStringIndex, typeParameter.sourceName)
        self.writeFlags(typeParameter.flags)
        self.writeType(typeParameter.upperBound)
        self.writeType(typeParameter.lowerBound)

    def writeDependencyHeader(self, dependency):
        self.writeString(dependency.dependencyString())
        self.writeVbn(len(dependency.externGlobals))
        self.writeVbn(len(dependency.externFunctions))
        self.writeVbn(len(dependency.externClasses))
        self.writeVbn(len(dependency.externTraits))
        self.writeVbn(len(dependency.externMethods))

    def writeDependency(self, dependency):
        for g in dependency.externGlobals:
            self.writeGlobal(g)
        for f in dependency.externFunctions:
            self.writeFunction(f)
        for c in dependency.externClasses:
            self.writeClass(c)
        for t in dependency.externTraits:
            self.writeTrait(t)
        for m in dependency.externMethods:
            self.writeFunction(m)

    def writeType(self, type):
        # TODO: serialize this in a way that doesn't couple us so closely to Type::Form
        packageIndex = None
        defnIndex = None
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
            clas = type.clas
            packageIndex = clas.id.getPackageIndex()
            defnIndex = clas.id.getDefnIndex()
            if isinstance(clas, ir.Class):
                form = 8
            else:
                assert isinstance(clas, ir.Trait)
                form = 9
        elif isinstance(type, ir_types.VariableType):
            form = 10
            assert type.typeParameter.index is not None
            defnIndex = type.typeParameter.index
        else:
            assert isinstance(type, ir_types.ExistentialType)
            form = 11
            self.writeVbn(form)
            self.writeList(self.writeTypeParameter, type.variables)
            self.writeType(type.ty)
            return
        flags = 0
        if ir_types.NULLABLE_TYPE_FLAG in type.flags:
            flags = flags | 1
        bits = form | flags << 4
        self.writeVbn(bits)
        if packageIndex is not None:
            self.writeVbn(packageIndex)
        if defnIndex is not None:
            self.writeVbn(defnIndex)
        if isinstance(type, ir_types.ClassType):
            self.writeList(self.writeType, type.typeArguments)

    def writeName(self, name):
        def writeComponent(comp):
            index = self.package.findString(comp)
            self.writeVbn(index)
        self.writeList(writeComponent, name.components)

    def writeNameIndex(self, name):
        index = self.package.findName(name)
        self.writeVbn(index)

    def writeFlags(self, flags_var):
        bits = flagSetToFlagBits(flags_var)
        self.outFile.write(struct.pack(FLAG_FORMAT, bits))

    def writeTypeParameterList(self, list):
        assert all(not p.isBuiltin() for p in list)
        assert all(p.isForeign() for p in list) or all(not p.isForeign() for p in list)
        def writeId(typeParam):
            self.writeVbn(typeParam.id.externIndex
                          if EXTERN in typeParam.flags \
                          else typeParam.id.index)
        self.writeList(writeId, list)

    def writeMethodList(self, list):
        self.writeList(self.writeMethodId, list)

    def writeMethodId(self, method):
        self.writeVbn(method.id.getPackageIndex())
        index = method.id.getDefnIndex()
        if method.id.getPackageIndex() == ids.BUILTIN_PACKAGE_INDEX:
            index = ~index
        self.writeVbn(index)

    def writeForeignMethodList(self, list):
        self.writeList(self.writeVbn, [m.id.index for m in list])

    def writeDefiningClassId(self, classOrTrait):
        if classOrTrait is None:
            self.writeVbn(0)
        elif isinstance(classOrTrait, ir.Class):
            self.writeVbn(1)
            self.writeVbn(classOrTrait.id.getDefnIndex())
        else:
            assert isinstance(classOrTrait, ir.Trait)
            self.writeVbn(2)
            self.writeVbn(classOrTrait.id.getDefnIndex())

    def writeList(self, writer, list):
        self.writeVbn(len(list))
        for elem in list:
            writer(elem)

    def writeOption(self, writer, opt):
        if opt is None:
            self.writeVbn(0)
        else:
            self.writeVbn(1)
            writer(opt)

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
    def __init__(self, inFile, packageLoader):
        self.inFile = inFile
        self.packageLoader = packageLoader
        self.package = ir.Package()
        self.isLinked = False
        self.entryFunctionIndex = None
        self.initFunctionIndex = None
        self.typeParameters = []

    def deserializeNameAndVersion(self):
        self.readHeader()
        return self.package.name, self.package.version

    def deserialize(self):
        self.readHeader()
        for i in xrange(len(self.package.dependencies)):
            self.package.dependencies[i] = self.readDependencyHeader(i)
        for i in xrange(len(self.package.strings)):
            self.package.strings[i] = self.readString()
        for i in xrange(len(self.package.names)):
            self.package.names[i] = self.readName()
        for dep in self.package.dependencies:
            self.readDependency(dep)
        self.package.link()
        self.isLinked = True
        for gbl in self.package.globals:
            self.readGlobal(gbl)
        for func in self.package.functions:
            self.readFunction(func)
        for clas in self.package.classes:
            self.readClass(clas)
        for trait in self.package.traits:
            self.readTrait(trait)

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

        nameCount = headers[5]
        self.package.names = [None] * nameCount

        # We pre-allocate top-level definitions so they can point to each other if we read
        # them out of order. This is particularly important for type definitions.
        globalCount = headers[6]
        self.package.globals = self.createEmptyGlobalList(globalCount, self.package.id)

        functionCount = headers[7]
        self.package.functions = self.createEmptyFunctionList(functionCount, self.package.id)

        classCount = headers[8]
        self.package.classes = self.createEmptyClassList(classCount, self.package.id)

        traitCount = headers[9]
        self.package.traits = self.createEmptyTraitList(traitCount, self.package.id)

        depCount = headers[10]
        self.package.dependencies = [None] * depCount

        entryFunctionIndex = headers[11]
        if entryFunctionIndex != -1:
            if entryFunctionIndex < 0 or entryFunctionIndex >= len(self.package.functions):
                raise IOError("invalid entry function index")
            self.package.entryFunction = self.package.functions[entryFunctionIndex]
        initFunctionIndex = headers[12]
        if initFunctionIndex != -1:
            if initFunctionIndex < 0 or initFunctionIndex >= len(self.package.functions):
                raise IOError("invalid init function index")
            self.package.initFunction = self.package.functions[initFunctionIndex]

        nameStr = self.readString()
        self.package.name = Name.fromString(nameStr, isPackageName=True)
        versionStr = self.readString()
        self.package.version = ir.PackageVersion.fromString(versionStr)
        self.package.id.name = self.package.name

    def createEmptyGlobalList(self, count, packageId):
        gids = (ids.DefnId(packageId, ids.DefnId.GLOBAL, i) for i in xrange(count))
        globals = list(ir.Global(None, id) for id in gids)
        return globals

    def createEmptyFunctionList(self, count, packageId):
        fids = (ids.DefnId(packageId, ids.DefnId.FUNCTION, i) for i in xrange(count))
        functions = list(ir.Function(None, id) for id in fids)
        return functions

    def createEmptyClassList(self, count, packageId):
        cids = (ids.DefnId(packageId, ids.DefnId.CLASS, i) for i in xrange(count))
        classes = list(ir.Class(None, id) for id in cids)
        return classes

    def createEmptyTraitList(self, count, packageId):
        tids = (ids.DefnId(packageId, ids.DefnId.TRAIT, i) for i in xrange(count))
        traits = list(ir.Trait(None, id) for id in tids)
        return traits

    def readString(self):
        length = self.readVbn()
        encoded = self.inFile.read(length)
        return encoded.decode("utf-8")

    def readStringIndex(self):
        index = self.readVbn()
        return self.package.strings[index]

    def readGlobal(self, globl):
        globl.name = self.readNameIndex()
        globl.sourceName = self.readOption(self.readStringIndex)
        globl.flags = self.readFlags()
        globl.type = self.readType()

    def readFunction(self, function, dep=None):
        assert len(self.typeParameters) == 0
        function.name = self.readNameIndex()
        function.sourceName = self.readOption(self.readStringIndex)
        function.flags = self.readFlags()
        assert (EXTERN in function.flags) == (dep is not None)
        function.typeParameters = self.readList(self.readTypeParameter)
        function.returnType = self.readType()
        function.parameterTypes = self.readList(self.readType)
        if EXTERN not in function.flags:
            function.definingClass = self.readDefiningClassId()
            if OVERRIDE in function.flags:
                function.overrides = self.readList(self.readMethodId)
            if frozenset([ABSTRACT, NATIVE]).isdisjoint(function.flags):
                function.instTypes = self.readList(self.readType)
                localsSize = self.readVbn()
                instructionsSize = self.readVbn()
                instructionsBuffer = self.inFile.read(instructionsSize)
                blockOffsets = self.readList(self.readVbn)
                function.blocks = self.decodeInstructions(instructionsBuffer, blockOffsets)
        if METHOD in function.flags and \
           CONSTRUCTOR not in function.flags and \
           STATIC not in function.flags:
            function.overriddenBy = {}
        del self.typeParameters[:]

    def decodeInstructions(self, instructionsBuffer, blockOffsets):
        # TODO: implement if we ever actually need this.
        return None

    def readClass(self, clas, dep=None):
        assert len(self.typeParameters) == 0
        clas.name = self.readNameIndex()
        clas.sourceName = self.readOption(self.readStringIndex)
        clas.flags = self.readFlags()
        assert (EXTERN in clas.flags) == (dep is not None)
        clas.typeParameters = self.readList(self.readTypeParameter)
        clas.supertypes = self.readList(self.readType)
        clas.fields = self.readFields(clas)
        clas.constructors = self.readList(self.readMethodId, dep)
        clas.methods = self.readList(self.readMethodId, dep)
        clas.elementType = self.readOption(self.readType)
        del self.typeParameters[:]

    def readFields(self, definingClass):
        n = self.readVbn()
        fields = []
        for i in xrange(n):
            field = self.readField(definingClass, i)
            fields.append(field)
        return fields

    def readField(self, definingClass, index):
        name = self.readNameIndex()
        sourceName = self.readOption(self.readStringIndex)
        flags = self.readFlags()
        ty = self.readType()
        field = ir.Field(name, sourceName=sourceName, type=ty, flags=flags,
                         definingClass=definingClass, index=index)
        return field

    def readTrait(self, trait, dep=None):
        assert len(self.typeParameters) == 0
        trait.name = self.readNameIndex()
        trait.sourceName = self.readOption(self.readStringIndex)
        trait.flags = self.readFlags()
        assert (EXTERN in trait.flags) == (dep is not None)
        trait.typeParameters = self.readList(self.readTypeParameter)
        trait.supertypes = self.readList(self.readType)
        trait.methods = self.readList(self.readMethodId, dep)
        del self.typeParameters[:]

    def readTypeParameter(self):
        index = len(self.typeParameters)
        name = self.readNameIndex()
        sourceName = self.readOption(self.readStringIndex)
        flags = self.readFlags()
        p = ir.TypeParameter(name=name, id=None,
                             sourceName=sourceName, flags=flags, index=index)
        self.typeParameters.append(p)
        p.upperBound = self.readType()
        p.lowerBound = self.readType()
        return p

    def readDependencyHeader(self, index):
        depStr = self.readString()
        dep = ir.PackageDependency.fromString(depStr)
        # TODO: pass in version when that's supported
        dep.package = self.packageLoader.loadPackage(dep.name)
        packageId = ids.PackageId(index, dep.name)
        globalCount = self.readVbn()
        dep.externGlobals = self.createEmptyGlobalList(globalCount, packageId)
        functionCount = self.readVbn()
        dep.externFunctions = self.createEmptyFunctionList(functionCount, packageId)
        classCount = self.readVbn()
        dep.externClasses = self.createEmptyClassList(classCount, packageId)
        traitCount = self.readVbn()
        dep.externTraits = self.createEmptyTraitList(traitCount, packageId)
        methodCount = self.readVbn()
        dep.externMethods = self.createEmptyFunctionList(methodCount, packageId)
        return dep

    def readDependency(self, dep):
        for g in dep.externGlobals:
            self.readGlobal(g)
            assert EXTERN in g.flags
        for f in dep.externFunctions:
            self.readFunction(f, dep)
        for c in dep.externClasses:
            self.readClass(c, dep)
        for t in dep.externTraits:
            self.readTrait(t, dep)
        for m in dep.externMethods:
            self.readFunction(m, dep)

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
            # CLASS_TYPE
            packageIndex = self.readVbn()
            defnIndex = self.readVbn()
            if packageIndex == ids.BUILTIN_PACKAGE_INDEX:
                clas = builtins.getBuiltinClassById(defnIndex)
            elif packageIndex == ids.LOCAL_PACKAGE_INDEX:
                clas = self.package.classes[defnIndex]
            elif self.isLinked:
                clas = self.package.dependencies[packageIndex].linkedClasses[defnIndex]
            else:
                clas = self.package.dependencies[packageIndex].externClasses[defnIndex]
            typeArgs = tuple(self.readList(self.readType))
            ty = ir_types.ClassType(clas, typeArgs, flags)
        elif form == 9:
            # TRAIT_TYPE
            packageIndex = self.readVbn()
            defnIndex = self.readVbn()
            if packageIndex == ids.BUILTIN_PACKAGE_INDEX:
                trait = builtins.getBuiltinTraitById(defnIndex)
            elif packageIndex == ids.LOCAL_PACKAGE_INDEX:
                trait = self.package.traits[defnIndex]
            elif self.isLinked:
                trait = self.package.dependencies[packageIndex].linkedTraits[defnIndex]
            else:
                trait = self.package.dependencies[packageIndex].externTraits[defnIndex]
            typeArgs = tuple(self.readList(self.readType))
            ty = ir_types.ClassType(trait, typeArgs, flags)
        elif form == 10:
            # VARIABLE_TYPE
            index = self.readVbn()
            assert 0 <= index and index < len(self.typeParameters)
            param = self.typeParameters[index]
            ty = ir_types.VariableType(param, flags)
        elif form == 11:
            # EXISTENTIAL_TYPE
            if flagBits != 0:
                raise IOError("flags must not be set for existential type")
            typeParameterCount = len(self.typeParameters)
            variables = self.readList(self.readTypeParameter)
            innerType = self.readType()
            del self.typeParameters[typeParameterCount:]
            ty = ir_types.ExistentialType(variables, innerType)
        else:
            raise IOError("invalid type flags")

        if ty.isPrimitive() and flagBits != 0:
            raise IOError("invalid type flags")

        return ty

    def readName(self):
        readComponent = lambda: self.readId(self.package.strings)
        components = self.readList(readComponent)
        return Name(components)

    def readNameIndex(self):
        index = self.readVbn()
        return self.package.names[index]

    def readDefiningClassId(self):
        n = self.readVbn()
        if n == 0:
            return None
        elif n == 1:
            return self.readId(self.package.classes)
        elif n == 2:
            return self.readId(self.package.traits)
        else:
            raise IOError("invalid defining class id")

    def readFlags(self):
        size = struct.calcsize(FLAG_FORMAT)
        bits = struct.unpack(FLAG_FORMAT, self.inFile.read(size))[0]
        flagSet = flagBitsToFlagSet(bits)
        return flagSet

    def readList(self, reader, *args):
        n = self.readVbn()
        elems = []
        for i in xrange(n):
            elem = reader(*args)
            elems.append(elem)
        return elems

    def readOption(self, reader, *args):
        n = self.readVbn()
        if n == 0:
            return None
        elif n == 1:
            opt = reader(*args)
            return opt
        else:
            raise IOError("invalid option")

    def readMethodId(self, dep=None):
        if dep is not None:
            methods = dep.externMethods
        else:
            packageIndex = self.readVbn()
            if packageIndex == ids.BUILTIN_PACKAGE_INDEX:
                methodIndex = self.readVbn()
                return builtins.getBuiltinFunctionById(methodIndex)
            elif packageIndex == ids.LOCAL_PACKAGE_INDEX:
                methods = self.package.functions
            else:
                methods = self.package.dependencies[packageIndex].externFunctions
        methodIndex = self.readVbn()
        return methods[methodIndex]

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

# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.

import re
import yaml

import flags
import ids
import ir
import ir_types
from name import CONSTRUCTOR_SUFFIX, Name
import utils

import bytecode

def registerBuiltins(bind):
    _initialize()
    for clas in _builtinClasses:
        bind(clas.name, clas)
    for function in _builtinFunctions:
        bind(function.name, function)


def getRootClass():
    _initialize()
    return _builtinClassNameMap["Object"]


def getNothingClass():
    _initialize()
    return _builtinClassNameMap["Nothing"]


def getExceptionClass():
    _initialize()
    return _builtinClassNameMap["Exception"]


def getTypeClass():
    _initialize()
    return _builtinClassNameMap["Type"]


def getStringClass():
    _initialize()
    return _builtinClassNameMap["String"]


def getPackageClass():
    _initialize()
    return _builtinClassNameMap["Package"]


def getBuiltinClasses(includePrimitives):
    _initialize()
    return [clas for clas in _builtinClassNameMap.itervalues()
            if includePrimitives or not hasattr(clas, "isPrimitive")]


def getBuiltinClassById(idOrIndex):
    _initialize()
    if isinstance(idOrIndex, int):
        id = bytecode.getBuiltinClassId(idOrIndex)
    else:
        assert isinstance(idOrIndex, ids.DefnId)
        id = idOrIndex
    return _builtinClassIdMap[id]


def getBuiltinFunctionById(idOrIndex):
    _initialize()
    if isinstance(idOrIndex, int):
        id = bytecode.getBuiltinFunctionId(idOrIndex)
    else:
        assert isinstance(idOrIndex, ids.DefnId)
        id = idOrIndex
    return _builtinFunctionIdMap[id]


def getBuiltinClassFromType(ty):
    _initialize()
    return _builtinClassTypeMap.get(str(ty))


def getBuiltinFunctions():
    _initialize()
    return _builtinFunctionNameMap.values()


_builtinClasses = []
_builtinFunctions = []

_builtinClassNameMap = {}
_builtinClassTypeMap = {}
_builtinClassIdMap = {}
_builtinFunctionNameMap = {}
_builtinFunctionIdMap = {}

_initialized = False

def _initialize():
    global _initialized
    if _initialized:
        return
    _initialized = True

    def buildType(typeName):
        if typeName == "unit":
            return ir_types.UnitType
        elif typeName == "boolean":
            return ir_types.BooleanType
        elif typeName == "i8":
            return ir_types.I8Type
        elif typeName == "i16":
            return ir_types.I16Type
        elif typeName == "i32":
            return ir_types.I32Type
        elif typeName == "i64":
            return ir_types.I64Type
        elif typeName == "f32":
            return ir_types.F32Type
        elif typeName == "f64":
            return ir_types.F64Type
        else:
            m = re.match(r"([A-Za-z0-9_-]+)(\??)", typeName)
            clas = _builtinClassNameMap[m.group(1)]
            flags = frozenset([ir_types.NULLABLE_TYPE_FLAG] if m.group(2) == "?" else [])
            return ir_types.ClassType(clas, (), flags)

    def buildFunction(functionData, namePrefix=None):
        nameComponents = []
        if namePrefix is not None:
            nameComponents.append(namePrefix)
        sourceName = functionData.get("name", CONSTRUCTOR_SUFFIX)
        nameComponents.append(sourceName)
        name = Name(nameComponents)
        id = getattr(bytecode, functionData["id"])
        flags = buildFlags(functionData["flags"])
        function = ir.Function(name, id,
                               sourceName=sourceName,
                               returnType=buildType(functionData["returnType"]),
                               typeParameters=[],
                               parameterTypes=map(buildType, functionData["parameterTypes"]),
                               flags=flags, insts=functionData.get("insts"))
        _builtinFunctionIdMap[id] = function
        return function

    def buildMethod(functionData, clas):
        function = buildFunction(functionData, clas.name.short())
        assert flags.METHOD in function.flags
        function.definingClass = clas
        function.overriddenBy = {}
        if "overrides" in functionData:
            function.overrides = []
            for overrideName in functionData["overrides"]:
                overrideId = getattr(bytecode, overrideName)
                override = _builtinFunctionIdMap[overrideId]
                function.overrides.append(override)
                override.overriddenBy[clas.id] = function
        return function

    def buildConstructor(functionData, clas):
        function = buildMethod(functionData, clas)
        assert flags.CONSTRUCTOR in function.flags
        return function

    def buildField(fieldData, index, clas):
        name = Name([clas.name.short(), fieldData["name"]])
        ty = buildType(fieldData["type"])
        flags = buildFlags(fieldData["flags"])
        return ir.Field(name, sourceName=fieldData["name"], type=ty, flags=flags,
                        index=index, definingClass=clas)

    def declareClass(classData):
        name = Name([classData["name"]])
        flags = buildFlags(classData["flags"])
        clas = ir.Class(name, None, sourceName=classData["name"],
                        typeParameters=[], methods=[], flags=flags)
        _builtinClasses.append(clas)
        _builtinClassNameMap[classData["name"]] = clas

    def defineClass(classData):
        clas = _builtinClassNameMap[classData["name"]]
        clas.id = getattr(bytecode,classData["id"])
        if not classData["isPrimitive"]:
            if classData["supertype"] is not None:
                superclass = _builtinClassNameMap[classData["supertype"]]
                clas.supertypes = [ir_types.ClassType(superclass)]
            else:
                clas.supertypes = []
            clas.constructors = [buildConstructor(ctorData, clas)
                                 for ctorData in classData["constructors"]]
            clas.fields = [buildField(fieldData, index, clas)
                           for index, fieldData in enumerate(classData["fields"])]
        else:
            clas.supertypes = []
            clas.fields = []
            clas.isPrimitive = True
        inheritedMethodCount = len(clas.methods)
        for m in classData["methods"]:
            method = buildMethod(m, clas)
            clas.methods.append(method)

        _builtinClassTypeMap[classData["name"]] = clas
        _builtinClassIdMap[clas.id] = clas

    def defineFunction(functionData):
        function = buildFunction(functionData)
        _builtinFunctions.append(function)
        _builtinFunctionNameMap[function.name] = function

    def buildFlags(flagsData):
        return frozenset(map(flags.canonicalizeFlagName, flagsData))

    with utils.openCommonFile("builtins.yaml") as builtinsFile:
        classes, functions = yaml.load_all(builtinsFile.read())
    for ty in classes:
        declareClass(ty)
    for ty in classes:
        defineClass(ty)
    for fn in functions:
        defineFunction(fn)

__all__ = [
    "registerBuiltins",
    "getRootClass",
    "getNothingClass",
    "getExceptionClass",
    "getTypeClass",
    "getStringClass",
    "getBuiltinClasses",
    "getBuiltinClassById",
    "getBuiltinFunctionById",
    "getBuiltinClassFromType",
    "getBuiltinFunctions"
]

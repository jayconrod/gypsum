# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import os.path
import re
import yaml

import ir
import ir_types

import bytecode

def registerBuiltins(bind):
    _initialize()
    for key, value in _builtinClassNameMap.iteritems():
        bind(key, value)
    for key, value in _builtinFunctionNameMap.iteritems():
        bind(key, value)


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


def getBuiltinClasses(includePrimitives):
    return [clas for clas in _builtinClassNameMap.itervalues()
            if includePrimitives or not hasattr(clas, "isPrimitive")]


def getBuiltinClassFromType(ty):
    _initialize()
    return _builtinClassTypeMap.get(ty)


def getBuiltinFunctions():
    return _builtinFunctionNameMap.values()


def isBuiltinId(id):
    return id < 0


_builtinClassNameMap = {}
_builtinClassTypeMap = {}
_builtinFunctionNameMap = {}

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

    def buildFunction(functionData):
        name = functionData.get("name", "$constructor")
        function = ir.Function(name,
                               buildType(functionData["returnType"]),
                               [],
                               map(buildType, functionData["parameterTypes"]),
                               [], [], frozenset())
        function.id = getattr(bytecode,functionData["id"])
        if "insts" in functionData:
            function.insts = functionData["insts"]
        return function

    def buildMethod(functionData, clas):
        function = buildFunction(functionData)
        function.clas = clas
        return function

    def buildField(fieldData):
        name = fieldData["name"]
        ty = buildType(fieldData["type"])
        return ir.Field(name, ty, frozenset())

    def declareClass(classData):
        clas = ir.Class(classData["name"], [], None, None, None, None, None, frozenset())
        _builtinClassNameMap[classData["name"]] = clas

    def defineClass(classData):
        clas = _builtinClassNameMap[classData["name"]]
        clas.id = getattr(bytecode,classData["id"])
        if not classData["isPrimitive"]:
            if classData["supertype"] is not None:
                superclass = _builtinClassNameMap[classData["supertype"]]
                clas.supertypes = [ir_types.ClassType(superclass)]
                clas.fields = list(superclass.fields)
                clas.methods = list(superclass.methods)
            else:
                clas.supertypes = []
                clas.fields = []
                clas.methods = []
            clas.constructors = [buildMethod(c, clas) for c in classData["constructors"]]
            clas.fields += map(buildField, classData["fields"])
        else:
            clas.supertypes = []
            clas.fields = []
            clas.methods = []
            clas.isPrimitive = True
        inheritedMethodCount = len(clas.methods)
        for m in classData["methods"]:
            addMethod(clas.methods, inheritedMethodCount, buildMethod(m, clas))

        _builtinClassTypeMap[buildType(clas.name)] = clas

    def addMethod(methods, inheritedCount, method):
        for i, m in enumerate(methods[:inheritedCount]):
            if method.name == m.name and method.mayOverride(m):
                method.override = m
                methods[i] = method
                return
        methods.append(method)

    def defineFunction(functionData):
        function = buildFunction(functionData)
        _builtinFunctionNameMap[function.name] = function

    builtinsPath = os.path.join(os.path.dirname(__file__), "..", "common", "builtins.yaml")
    with open(builtinsPath) as builtinsFile:
        classes, functions = yaml.load_all(builtinsFile.read())
    for ty in classes:
        declareClass(ty)
    for ty in classes:
        defineClass(ty)
    for fn in functions:
        defineFunction(fn)

__all__ = ["registerBuiltins", "getRootClass", "getNothingClass",
           "getExceptionClass", "getTypeClass", "getStringClass",
           "getBuiltinClasses", "getBuiltinClassFromType",
           "getBuiltinFunctions", "isBuiltinId"]

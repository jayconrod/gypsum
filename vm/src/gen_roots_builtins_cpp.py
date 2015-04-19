#!/usr/bin/env python

# Copyright 2014-2015 Jay Conrod. All rights reserved.

# This file is part of CodeSwitch. Use of this source code is governed by
# the 3-clause BSD license that can be found in the LICENSE.txt file.


import os.path
import sys
import yaml


if len(sys.argv) != 3:
    sys.stderr.write("usage: %s builtins.yaml roots-builtins.cpp\n" % sys.argv[0])
    sys.exit(1)


builtinsYamlName = sys.argv[1]
rootsBuiltinsName = sys.argv[2]

def declareClass(out, classData):
    out.write("\n  { // %s" % classData["id"])
    if not classData["isPrimitive"]:
        out.write("""
    auto clas = reinterpret_cast<Class*>(heap->allocate(sizeof(Class)));
    builtinClasses_.push_back(clas);""")
    out.write("""
    auto ty = reinterpret_cast<Type*>(heap->allocate(Type::sizeForLength(%d)));
    builtinTypes_.push_back(ty);
  }""" % (0 if classData["isPrimitive"] else 1))


def declareFunction(out, funcData):
    out.write("""
  { // %s
    auto function = reinterpret_cast<Function*>(heap->allocate(Function::sizeForFunction(0)));
    builtinFunctions_.push_back(function);
  }""" % funcData["id"])


def initType(out, classData):
    out.write("\n  { // %s\n" % classData["id"])
    out.write("    auto ty = getBuiltinType(%s);\n" % classData["id"])
    if classData["isPrimitive"]:
        primitiveType = classData["id"][8:-3]
        out.write("    new(ty, 0) Type(Type::%s);\n" % primitiveType)
    else:
        out.write("    auto clas = getBuiltinClass(%s);\n" % classData["id"])
        out.write("    new(ty, 1) Type(clas);\n")
    out.write("  }")


def initName(out, classData):
    out.write("\n  { // %s\n" % classData["id"])
    out.write("    auto name = String::rawFromUtf8CString(heap, \"%s\");\n" % classData["name"])
    out.write("    builtinNames_.push_back(name);\n  }")


def initClass(out, classData):
    assert not classData["isPrimitive"]
    out.write("\n  { // %s\n" % classData["id"])
    out.write("    auto clas = getBuiltinClass(%s);\n" % classData["id"])
    out.write("    auto name = getBuiltinName(%s);\n" % classData["id"])
    out.write("    auto typeParameters = reinterpret_cast<BlockArray<TypeParameter>*>(" +
              "emptyBlockArray());\n")
    if classData["supertype"] is None:
        out.write("    Type* supertype = nullptr;\n")
    else:
        out.write("    auto supertype = %s;\n" % getTypeFromName(classData["supertype"]))
    if len(classData["fields"]) == 0:
        out.write("    auto fields = reinterpret_cast<BlockArray<Field>*>(emptyBlockArray());\n")
    else:
        out.write("    auto fields = new(heap, %d) BlockArray<Field>;\n" %
                  len(classData["fields"]))
        for i, fieldData in enumerate(classData["fields"]):
            out.write("    auto field%dName = String::rawFromUtf8CString(heap, \"%s\");\n" %
                      (i, fieldData["name"]))
            typeName = fieldData["type"]
            out.write("    auto field%d = new(heap) Field(field%dName, 0, %s);\n" %
                      (i, i, getTypeFromName(typeName)))
            out.write("    fields->set(%d, field%d);\n" % (i, i))
    if "elements" not in classData:
        out.write("    Type* elementType = nullptr;\n")
        out.write("    length_t lengthFieldIndex = kIndexNotSet;\n")
    else:
        out.write("    auto elementType = %s;\n" % getTypeFromName(classData["elements"]))
        lengthFieldIndex = next(i for i, field in
                                enumerate(classData["fields"])
                                if field["name"] == "length")
        out.write("    auto lengthFieldIndex = %d;\n" % lengthFieldIndex)
    if len(classData["constructors"]) == 0:
        out.write("    auto constructors = reinterpret_cast<BlockArray<Function>*>(" +
                  "emptyBlockArray());\n")
    else:
        out.write("    auto constructors = new(heap, %d) BlockArray<Function>;\n" %
                  len(classData["constructors"]))
        for i, ctorData in enumerate(classData["constructors"]):
            out.write("    constructors->set(%d, getBuiltinFunction(%s));\n" %
                      (i, ctorData["id"]))
    allMethodIds = [method["id"] for method in findInheritedMethods(classData)]
    if len(allMethodIds) == 0:
        out.write("    auto methods = reinterpret_cast<BlockArray<Function>*>(" +
                  "emptyBlockArray());\n")
    else:
        out.write("    auto methods = new(heap, %d) BlockArray<Function>;\n" %
                  len(allMethodIds))
        for i, id in enumerate(allMethodIds):
            out.write("    methods->set(%d, getBuiltinFunction(%s));\n" % (i, id))
    out.write("    ::new(clas) Class(name, 0, typeParameters, supertype, fields, " +
              "constructors, methods, nullptr, nullptr, elementType, lengthFieldIndex);\n")
    if classData.get("isOpaque"):
        out.write("    builtinMetas_.push_back(nullptr);\n  }")
    else:
        out.write("    auto meta = clas->buildInstanceMeta();\n")
        out.write("    clas->setInstanceMeta(meta);\n")
        out.write("    builtinMetas_.push_back(meta);\n  }")


def initFunction(out, functionData):
    out.write("\n  { // %s\n" % functionData["id"])
    out.write("    auto function = getBuiltinFunction(%s);\n" % functionData["id"])
    if "name" not in functionData:
        out.write("    auto name = emptyString();\n")
    else:
        out.write("    auto name = String::rawFromUtf8CString(heap, \"%s\");\n" %
                  functionData["name"])
    out.write("    auto returnType = %s;\n" % getTypeFromName(functionData["returnType"]))
    out.write("    auto parameterTypes = new(heap, %d) BlockArray<Type>;\n" %
              len(functionData["parameterTypes"]))
    for i, name in enumerate(functionData["parameterTypes"]):
        out.write("    parameterTypes->set(%d, %s);\n" % (i, getTypeFromName(name)))
    out.write("    ::new(function) Function(name, 0, emptyTypeParameters, " +
              "returnType, parameterTypes, 0, emptyInstructions, nullptr, nullptr, nullptr);\n")
    out.write("    function->setBuiltinId(%s);\n" % functionData["id"])
    out.write("  }")


def findClass(name):
    return next(classData for classData in classesData if classData["name"] == name)


def findInheritedMethods(classData):
    ownMethods = classData["methods"]
    if classData["supertype"] is None:
        return ownMethods
    else:
        superclassData = findClass(classData["supertype"])
        inheritedMethods = findInheritedMethods(superclassData)
        methods = list(inheritedMethods)
        indexMap = {method["name"]: index for index, method in enumerate(methods)}
        for method in ownMethods:
            if method["name"] in indexMap:
                methods[indexMap[method["name"]]] = method
            else:
                methods.append(method)
        return methods


def getTypeFromName(name):
    if name == "Object?":
        return "nullableRootClassType"
    else:
        id = next(classData["id"] for classData in classesData if classData["name"] == name)
        return "getBuiltinType(%s)" % id


with open(builtinsYamlName) as builtinsYamlFile:
    classesData, functionsData = yaml.load_all(builtinsYamlFile.read())

with open(rootsBuiltinsName, "w") as rootsBuiltinsFile:
    rootsBuiltinsFile.write("""// DO NOT MODIFY
// This file was automatically generated by gen_roots_builtins_cpp.py

#include "roots.h"

#include <new>
#include <vector>
#include "array.h"
#include "builtins.h"
#include "block.h"
#include "class.h"
#include "field.h"
#include "function.h"
#include "string.h"
#include "type.h"

using namespace std;

namespace codeswitch {
namespace internal {

void Roots::initializeBuiltins(Heap* heap) {
  //
  // Allocate classes, types, and functions.
  //""")

    for classData in classesData:
        declareClass(rootsBuiltinsFile, classData)
        if not classData["isPrimitive"]:
            for ctorData in classData["constructors"]:
                declareFunction(rootsBuiltinsFile, ctorData)
        for methodData in classData["methods"]:
            declareFunction(rootsBuiltinsFile, methodData)
    for functionData in functionsData:
        declareFunction(rootsBuiltinsFile, functionData)

    rootsBuiltinsFile.write("""

  //
  // Initialize types
  //""")
    for classData in classesData:
        initType(rootsBuiltinsFile, classData)

    rootsBuiltinsFile.write("""

  auto nullableRootClassType = new(heap, 1) Type(getBuiltinClass(BUILTIN_ROOT_CLASS_ID),
                                                 Type::NULLABLE_FLAG);""")

    rootsBuiltinsFile.write("""

  //
  // Initialize names
  //""")
    for classData in classesData:
        initName(rootsBuiltinsFile, classData)

    rootsBuiltinsFile.write("""

  //
  // Initialize classes
  //""")
    for classData in classesData:
        if not classData["isPrimitive"]:
            initClass(rootsBuiltinsFile, classData)

    rootsBuiltinsFile.write("""

  //
  // Initialize functions
  //
  vector<u8> emptyInstructions;
  auto emptyTypeParameters = reinterpret_cast<BlockArray<TypeParameter>*>(emptyBlockArray());
""")
    for classData in classesData:
        if not classData["isPrimitive"]:
            for ctorData in classData["constructors"]:
                initFunction(rootsBuiltinsFile, ctorData)
        for methodData in classData["methods"]:
            initFunction(rootsBuiltinsFile, methodData)
    for functionData in functionsData:
        initFunction(rootsBuiltinsFile, functionData)

    rootsBuiltinsFile.write("\n}\n\n}\n}\n")

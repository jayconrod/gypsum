#!/usr/bin/env python

# Copyright 2014 Jay Conrod. All rights reserved.

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
    auto clas = Class::tryAllocate(heap);
    builtinClasses_.push_back(clas);""")
    out.write("""
    auto ty = Type::tryAllocate(heap, %d);
    builtinTypes_.push_back(ty);
  }""" % (0 if classData["isPrimitive"] else 1))


def declareFunction(out, funcData):
    out.write("""
  { // %s
    auto function = Function::tryAllocate(heap, 0);
    builtinFunctions_.push_back(function);
  }""" % funcData["id"])


def initType(out, classData):
    out.write("\n  { // %s\n" % classData["id"])
    out.write("    auto ty = getBuiltinType(%s);\n" % classData["id"])
    if classData["isPrimitive"]:
        primitiveType = classData["id"][8:-3]
        out.write("    ty->initialize(Type::%s);\n" % primitiveType)
    else:
        out.write("    auto clas = getBuiltinClass(%s);\n" % classData["id"])
        out.write("    ty->initialize(clas);\n")
    out.write("  }")


def initClass(out, classData):
    assert not classData["isPrimitive"]
    out.write("\n  { // %s\n" % classData["id"])
    out.write("    auto clas = getBuiltinClass(%s);\n" % classData["id"])
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
            typeName = fieldData["type"]
            out.write("    auto field%d = Field::tryAllocate(heap);\n" % i)
            out.write("    field%d->initialize(0, %s);\n" % (i, getTypeFromName(typeName)))
            out.write("    fields->set(%d, field%d);\n" % (i, i))
    if "elements" not in classData:
        out.write("    Type* elementType = nullptr;\n")
    else:
        out.write("    auto elementType = %s;\n" % getTypeFromName(classData["elements"]))
    if len(classData["constructors"]) == 0:
        out.write("    auto constructors = emptyi64Array();\n")
    else:
        out.write("    auto constructors = new(heap, %d) I64Array;\n" %
                  len(classData["constructors"]))
        for i, ctorData in enumerate(classData["constructors"]):
            out.write("    constructors->set(%d, %s);\n" %
                      (i, ctorData["id"]))
    allMethodIds = findInheritedMethodIds(classData)
    if len(allMethodIds) == 0:
        out.write("    auto methods = emptyi64Array();\n")
    else:
        out.write("    auto methods = new(heap, %d) I64Array;\n" % len(allMethodIds))
        for i, id in enumerate(allMethodIds):
            out.write("    methods->set(%d, %s);\n" % (i, id))
    out.write("    clas->initialize(0, supertype, fields, elementType, constructors, " +
              "methods, nullptr, nullptr);\n")
    out.write("    auto meta = clas->tryBuildInstanceMeta(heap);\n")
    out.write("    builtinMetas_.push_back(meta);\n  }")


def initFunction(out, functionData):
    out.write("\n  { // %s\n" % functionData["id"])
    out.write("    auto function = getBuiltinFunction(%s);\n" % functionData["id"])
    typeNames = [functionData["returnType"]] + functionData["parameterTypes"]
    out.write("    auto types = new(heap, %d) BlockArray<Type>;\n" % len(typeNames))
    for i, name in enumerate(typeNames):
        out.write("    types->set(%d, %s);\n" % (i, getTypeFromName(name)))
    out.write("    function->initialize(0, emptyTypeParameters, types, 0, " +
              "emptyInstructions, nullptr, nullptr, nullptr);\n")
    out.write("    function->setBuiltinId(%s);\n" % functionData["id"])
    out.write("  }")



def findClass(name):
    return next(classData for classData in classesData if classData["name"] == name)


def findInheritedMethodIds(classData):
    ownMethods = [methodData["id"] for methodData in classData["methods"]]
    if classData["supertype"] is None:
        return ownMethods
    else:
        superclassData = findClass(classData["supertype"])
        methods = findInheritedMethodIds(superclassData)
        methods += ownMethods
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

#include "roots-inl.h"

#include <vector>
#include "array.h"
#include "builtins.h"
#include "block.h"
#include "class-inl.h"
#include "field.h"
#include "function-inl.h"
#include "type-inl.h"

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

  auto nullableRootClassType = Type::tryAllocate(heap, 1);
  nullableRootClassType->initialize(getBuiltinClass(BUILTIN_ROOT_CLASS_ID),
                                    Type::NULLABLE_FLAG);""")

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
  auto emptyTypeParameters = reinterpret_cast<TaggedArray<TypeParameter>*>(emptyTaggedArray());
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

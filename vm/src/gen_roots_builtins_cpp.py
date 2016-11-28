#!/usr/bin/env python

# Copyright 2014-2016 Jay Conrod. All rights reserved.

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
    out.write("    auto name = nameFromUtf8CString(heap, \"%s\");\n" % classData["name"])
    out.write("    builtinNames_.push_back(name);\n  }")


def initClass(out, classData):
    assert not classData["isPrimitive"]
    out.write("\n  { // %s\n" % classData["id"])
    out.write("    auto clas = getBuiltinClass(%s);\n" % classData["id"])
    out.write("    DefnId id(DefnId::CLASS, kBuiltinPackageId, static_cast<length_t>(%s), false /* isLocal */);\n" %
              classData["id"])
    out.write("    auto name = getBuiltinName(%s);\n" % classData["id"])
    out.write("    auto typeParameters = reinterpret_cast<BlockArray<TypeParameter>*>(" +
              "emptyBlockArray());\n")
    if classData["supertype"] is None:
        out.write("    auto supertypes = reinterpret_cast<BlockArray<Type>*>(emptyBlockArray());\n")
    else:
        out.write("    auto supertypes = new(heap, 1) BlockArray<Type>;\n")
        out.write("    supertypes->set(0, %s);\n" % getTypeFromName(classData["supertype"]))
    out.write("    u32 flags = " + buildFlags(classData["flags"]) + ";\n")
    if len(classData["fields"]) == 0:
        out.write("    auto fields = reinterpret_cast<BlockArray<Field>*>(emptyBlockArray());\n")
    else:
        out.write("    auto fields = new(heap, %d) BlockArray<Field>;\n" %
                  len(classData["fields"]))
        for i, fieldData in enumerate(classData["fields"]):
            out.write("    auto field%dName = nameFromUtf8CString(heap, \"%s\");\n" %
                      (i, fieldData["name"]))
            out.write("    auto field%dType = %s;\n" % (i, getTypeFromName(fieldData["type"])))
            flags = buildFlags(fieldData["flags"])
            out.write(("    auto field%d = new(heap) Field(field%dName, nullptr, " +
                       "%s, field%dType, static_cast<u32>(kNotSet));\n") %
                      (i, i, flags, i))
            out.write("    fields->set(%d, field%d);\n" % (i, i))
    if "elements" not in classData:
        out.write("    Type* elementType = nullptr;\n")
    else:
        out.write("    auto elementType = %s;\n" % getTypeFromName(classData["elements"]))
    if len(classData["constructors"]) == 0:
        out.write("    auto constructors = reinterpret_cast<BlockArray<Function>*>(" +
                  "emptyBlockArray());\n")
    else:
        out.write("    auto constructors = new(heap, %d) BlockArray<Function>;\n" %
                  len(classData["constructors"]))
        for i, ctorData in enumerate(classData["constructors"]):
            out.write("    constructors->set(%d, getBuiltinFunction(%s));\n" %
                      (i, ctorData["id"]))
    methodIds = [m["id"] for m in classData["methods"]]
    if len(methodIds) == 0:
        out.write("    auto methods = reinterpret_cast<BlockArray<Function>*>(" +
                  "emptyBlockArray());\n")
    else:
        out.write("    auto methods = new(heap, %d) BlockArray<Function>;\n" % len(methodIds))
        for i, id in enumerate(methodIds):
            out.write("    methods->set(%d, getBuiltinFunction(%s));\n" % (i, id))
    out.write("    ::new(clas) Class(id, name, nullptr, flags, typeParameters, supertypes, " +
              "fields, constructors, methods, nullptr, nullptr, elementType);\n")
    if classData.get("isOpaque"):
        out.write("    builtinMetas_.push_back(nullptr);\n  }")
    else:
        out.write("    auto meta = *Class::ensureInstanceMeta(handle(clas));\n")
        out.write("    clas->setInstanceMeta(meta);\n")
        out.write("    builtinMetas_.push_back(meta);\n  }")


def initFunction(out, functionData):
    out.write("\n  { // %s\n" % functionData["id"])
    out.write("    auto function = getBuiltinFunction(%s);\n" % functionData["id"])
    out.write("    DefnId id(DefnId::FUNCTION, kBuiltinPackageId, static_cast<length_t>(%s), false /* isLocal */);\n" %
              functionData["id"])
    if "name" not in functionData:
        out.write("    auto name = nameFromUtf8CString(heap, \"$constructor\");\n")
    else:
        out.write("    auto name = nameFromUtf8CString(heap, \"%s\");\n" %
                  functionData["name"])
    out.write("    auto returnType = %s;\n" % getTypeFromName(functionData["returnType"]))
    out.write("    auto parameterTypes = new(heap, %d) BlockArray<Type>;\n" %
              len(functionData["parameterTypes"]))
    for i, name in enumerate(functionData["parameterTypes"]):
        out.write("    parameterTypes->set(%d, %s);\n" % (i, getTypeFromName(name)))
    if "OVERRIDE" in functionData["flags"]:
        out.write("    auto overrides = new(heap, %d) BlockArray<Function>;\n" %
                  len(functionData["overrides"]))
        for i, id in enumerate(functionData["overrides"]):
            out.write("    overrides->set(%d, getBuiltinFunction(%s));\n" % (i, id))
    else:
        out.write("    BlockArray<Function>* overrides = nullptr;\n")
    out.write("    u32 flags = " + buildFlags(functionData["flags"]) + ";\n")
    out.write("    ::new(function) Function(id, name, nullptr, flags, emptyTypeParameters, " +
              "returnType, parameterTypes, nullptr, 0, emptyInstructions, " +
              "nullptr, nullptr, overrides, nullptr, nullptr);\n")
    out.write("    function->setBuiltinId(%s);\n" % functionData["id"])
    out.write("  }")


def buildFlags(flagsData):
    if len(flagsData) == 0:
        return "NO_FLAGS"
    else:
        return " | ".join([f + "_FLAG" for f in flagsData])


def findClass(name):
    return next(classData for classData in classesData if classData["name"] == name)


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
#include "defnid.h"
#include "field.h"
#include "flags.h"
#include "function.h"
#include "name.h"
#include "string.h"
#include "type.h"

using namespace std;

namespace codeswitch {
namespace internal {

static Name* nameFromUtf8CString(Heap* heap, const char* cstr) {
  auto vmstr = String::rawFromUtf8CString(heap, cstr);
  auto components = new(heap, 1) BlockArray<String>;
  components->set(0, vmstr);
  auto name = new(heap) Name(components);
  return name;
}


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
  // Allocate and initialize names
  //""")
    for classData in classesData:
        initName(rootsBuiltinsFile, classData)

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

    rootsBuiltinsFile.write("""

  //
  // Initialize classes
  //""")
    for classData in classesData:
        if not classData["isPrimitive"]:
            initClass(rootsBuiltinsFile, classData)

    rootsBuiltinsFile.write("\n}\n\n}\n}\n")

# Copyright 2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import flags
import ids
import ir
import ir_types
from utils import each


def externalize(info):
    externalizer = Externalizer(info.package, info.packageLoader)

    for useInfo in info.useInfo.itervalues():
        defn = useInfo.defnInfo.irDefn
        if isinstance(defn, ir.Package):
            info.package.ensureDependency(defn)
        elif isinstance(defn, ir.IrTopDefn):
            externalizer.externalizeDefn(defn)

    for ty in info.typeInfo.itervalues():
        externalizer.externalizeType(ty)



class Externalizer(object):
    def __init__(self, package, packageLoader):
        assert package.id is ids.TARGET_PACKAGE_ID
        self.package = package
        self.packageLoader = packageLoader

    def externalizeDefn(self, defn):
        if not defn.isForeign():
            return defn
        id = defn.id

        # Make sure we have a dependency on the package that contains this definition.
        self.package.ensureDependency(self.packageLoader.getPackageById(id.packageId))

        # Find the list we're going to put our external description in.
        self.package.findOrAddString(defn.name)
        externFlags = defn.flags | frozenset([flags.EXTERN])
        dep = self.package.dependencies[id.packageId.index]
        if isinstance(defn, ir.Global):
            externDefns = dep.externGlobals
        elif isinstance(defn, ir.Function):
            if flags.METHOD in defn.flags:
                externDefns = dep.externMethods
            else:
                externDefns = dep.externFunctions
        elif isinstance(defn, ir.Class):
            externDefns = dep.externClasses
        elif isinstance(defn, ir.TypeParameter):
            externDefns = dep.externTypeParameters
        else:
            raise NotImplementedError

        # Base case: we already externalized this definition, so re-use that.
        if id.externIndex is not None:
            return externDefns[id.externIndex]

        # Recursive case: add a placeholder definition so recursive calls can find it.
        # Externalize anything else the definition depends on.
        id.externIndex = len(externDefns)

        if isinstance(defn, ir.Global):
            externDefn = ir.Global(defn.name, defn.astDefn, id, defn.type, externFlags)
            externDefns.append(externDefn)
            self.externalizeType(defn.type)
        elif isinstance(defn, ir.Function):
            externDefn = ir.Function(defn.name, defn.astDefn, id,
                                     defn.returnType, None, defn.parameterTypes,
                                     None, None, externFlags)
            externDefns.append(externDefn)
            self.externalizeType(defn.returnType)
            externDefn.typeParameters = map(self.externalizeDefn, defn.typeParameters)
            each(self.externalizeType, defn.parameterTypes)
        elif isinstance(defn, ir.Class):
            externDefn = ir.Class(defn.name, defn.astDefn, id,
                                  None, defn.supertypes, None,
                                  None, defn.fields, None, externFlags)
            externDefns.append(externDefn)
            externDefn.typeParameters = map(self.externalizeDefn, defn.typeParameters)
            each(self.externalizeType, defn.supertypes)
            externDefn.constructors = map(self.externalizeDefn, defn.constructors)
            each(self.package.findOrAddString, (f.name for f in defn.fields))
            each(self.externalizeType, (f.type for f in defn.fields))
            externDefn.methods = map(self.externalizeDefn, defn.methods)
        elif isinstance(defn, ir.TypeParameter):
            externDefn = ir.TypeParameter(defn.name, defn.astDefn, id,
                                          defn.upperBound, defn.lowerBound, externFlags)
            externDefns.append(externDefn)
            self.externalizeType(defn.upperBound)
            self.externalizeType(defn.lowerBound)
        else:
            raise NotImplementedError

        return externDefn

    def externalizeType(self, ty):
        if isinstance(ty, ir_types.ClassType):
            self.externalizeDefn(ty.clas)
        elif isinstance(ty, ir_types.VariableType):
            self.externalizeDefn(ty.typeParameter)

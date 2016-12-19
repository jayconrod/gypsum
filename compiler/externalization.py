# Copyright 2015-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import flags
import ids
import ir
import ir_types
from utils import each


def externalize(info):
    """Records dependencies on foreign definitions.

    Each CodeSwitch package has a list of other packages it depends on. This list goes beyond
    recording the name and version of each required package: it records the definitions that
    are actually used. These are "extern" definitions (marked with the `EXTERN` flag), and
    they can be referred to directly by the package like normal definitions.

    During compilation, we may refer to "foreign" definitions which are actually in other
    packages (not in the dependency list). This externalization step detects these foreign
    definitions, creates "extern" stubs, and records them.
    """
    externalizer = Externalizer(info.package, info.packageLoader)

    for useInfo in info.useInfo.itervalues():
        defn = useInfo.defnInfo.irDefn
        if isinstance(defn, ir.Package):
            info.package.ensureDependency(defn)
        elif isinstance(defn, ir.IrTopDefn):
            externalizer.externalizeDefn(defn)
        elif isinstance(defn, ir.Field):
            externalizer.externalizeDefn(defn.definingClass)
            info.package.findOrAddName(defn.name)

    def externalizeInheritedMethods(objDefn):
        each(externalizer.externalizeDefn, objDefn.methods)
    each(externalizeInheritedMethods, info.package.classes)
    each(externalizeInheritedMethods, info.package.traits)

    each(externalizer.externalizeType, info.typeInfo.itervalues())
    each(externalizer.externalizeDefn, info.stdExternInfo.itervalues())


class Externalizer(object):
    def __init__(self, package, packageLoader):
        assert package.id is ids.TARGET_PACKAGE_ID
        self.package = package
        self.packageLoader = packageLoader

    def externalizeDefn(self, defn):
        if not defn.isForeign():
            return defn
        assert isinstance(defn, ir.TypeParameter) or flags.PUBLIC in defn.flags
        id = defn.id

        # Make sure we have a dependency on the package that contains this definition.
        self.package.ensureDependency(self.packageLoader.getPackageById(id.packageId))

        # Find the list we're going to put our external description in.
        self.package.findOrAddName(defn.name)
        externFlags = defn.flags | frozenset([flags.EXTERN])
        dep = self.package.dependencies[id.packageId.index]
        if isinstance(defn, ir.Global):
            externDefns = dep.externGlobals
        elif isinstance(defn, ir.Function):
            externDefns = dep.externFunctions
        elif isinstance(defn, ir.Class):
            externDefns = dep.externClasses
        elif isinstance(defn, ir.Trait):
            externDefns = dep.externTraits
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
            externDefn = ir.Global(defn.name, id, astDefn=defn.astDefn,
                                   type=defn.type, flags=externFlags)
            externDefns.append(externDefn)
            self.externalizeType(defn.type)
        elif isinstance(defn, ir.Function):
            externDefn = ir.Function(defn.name, id, astDefn=defn.astDefn,
                                     returnType=defn.returnType,
                                     parameterTypes=defn.parameterTypes,
                                     flags=externFlags)
            externDefns.append(externDefn)
            self.externalizeType(defn.returnType)
            externDefn.typeParameters = [self.externalizeDefn(param)
                                         for param in defn.typeParameters]
            each(self.externalizeType, defn.parameterTypes)
        elif isinstance(defn, ir.Class):
            externDefn = ir.Class(defn.name, id, astDefn=defn.astDefn,
                                  supertypes=defn.supertypes,
                                  elementType=defn.elementType,
                                  flags=externFlags)
            externDefns.append(externDefn)
            externDefn.typeParameters = [self.externalizeDefn(param)
                                         for param in defn.typeParameters]
            each(self.externalizeType, defn.supertypes)
            externDefn.constructors = [self.externalizeMethod(ctor, dep)
                                       for ctor in defn.constructors
                                       if flags.PUBLIC in ctor.flags]
            externDefn.fields = [f for f in defn.fields
                                 if flags.PUBLIC in f.flags or flags.ARRAY in f.flags]
            each(self.package.findOrAddName, (f.name for f in externDefn.fields))
            each(self.externalizeType, (f.type for f in externDefn.fields))
            externDefn.methods = [self.externalizeMethod(m, dep)
                                  for m in defn.methods
                                  if flags.PUBLIC in m.flags]
            if externDefn.elementType is not None:
                self.externalizeType(externDefn.elementType)
        elif isinstance(defn, ir.Trait):
            externDefn = ir.Trait(defn.name, id, astDefn=defn.astDefn,
                                  supertypes=defn.supertypes,
                                  flags=externFlags)
            externDefns.append(externDefn)
            externDefn.typeParameters = [self.externalizeDefn(param)
                                         for param in defn.typeParameters]
            each(self.externalizeType, defn.supertypes)
            externDefn.methods = [self.externalizeMethod(m, dep)
                                  for m in defn.methods
                                  if flags.PUBLIC in m.flags]
        elif isinstance(defn, ir.TypeParameter):
            externDefn = ir.TypeParameter(defn.name, id, astDefn=defn.astDefn,
                                          upperBound=defn.upperBound,
                                          lowerBound=defn.lowerBound,
                                          flags=externFlags)
            externDefns.append(externDefn)
            self.externalizeType(externDefn.upperBound)
            self.externalizeType(externDefn.lowerBound)
        else:
            raise NotImplementedError

        return externDefn

    def externalizeType(self, ty):
        if isinstance(ty, ir_types.ClassType):
            self.externalizeDefn(ty.clas)
            each(self.externalizeType, ty.typeArguments)
        elif isinstance(ty, ir_types.VariableType) and \
             ty.typeParameter.isForeign():
            self.externalizeDefn(ty.typeParameter)
        elif isinstance(ty, ir_types.ExistentialType):
            each(self.externalizeDefn, ty.variables)
            self.externalizeType(ty.ty)

    def externalizeMethod(self, method, dep):
        self.package.findOrAddName(method.name)
        id = ids.DefnId(ids.TARGET_PACKAGE_ID, ids.DefnId.FUNCTION, len(dep.externMethods))
        externFlags = method.flags | frozenset([flags.EXTERN])
        externMethod = ir.Function(method.name, id, astDefn=method.astDefn,
                                   returnType=method.returnType,
                                   parameterTypes=method.parameterTypes,
                                   flags=externFlags)
        dep.externMethods.append(externMethod)
        self.externalizeType(method.returnType)
        externMethod.typeParameters = map(self.externalizeDefn, method.typeParameters)
        each(self.externalizeType, method.parameterTypes)
        return externMethod

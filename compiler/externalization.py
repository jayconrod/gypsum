# Copyright Jay Conrod. All rights reserved.
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
    for useInfo in info.useInfo.itervalues():
        defn = useInfo.defnInfo.irDefn
        if isinstance(defn, ir.Package):
            info.package.ensureDependency(defn)
        elif isinstance(defn, ir.IrTopDefn):
            externalizeDefn(info, defn)
        elif isinstance(defn, ir.Field):
            externalizeDefn(info, defn.definingClass)
            info.package.findOrAddName(defn.name)

    def externalizeInheritedMethods(objDefn):
        for m in objDefn.methods:
            externalizeDefn(info, m)
    each(externalizeInheritedMethods, info.package.classes)
    each(externalizeInheritedMethods, info.package.traits)

    for t in info.typeInfo.itervalues():
        externalizeType(info, t)
    for d in info.stdExternInfo.itervalues():
        externalizeDefn(info, d)


def externalizeDefn(info, defn):
    if not defn.isForeign():
        return defn
    assert flags.PUBLIC in defn.flags
    id = defn.id

    # Make sure we have a dependency on the package that contains this definition.
    info.package.ensureDependency(info.packageLoader.getPackageById(id.packageId))

    # Find the list we're going to put our external description in.
    info.package.findOrAddName(defn.name)
    externFlags = defn.flags | frozenset([flags.EXTERN])
    dep = info.package.dependencies[id.packageId.index]
    if isinstance(defn, ir.Global):
        externDefns = dep.externGlobals
    elif isinstance(defn, ir.Function):
        externDefns = dep.externFunctions
    elif isinstance(defn, ir.Class):
        externDefns = dep.externClasses
    elif isinstance(defn, ir.Trait):
        externDefns = dep.externTraits
    else:
        raise NotImplementedError()

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
        externalizeType(info, defn.type)
    elif isinstance(defn, ir.Function):
        externDefn = ir.Function(defn.name, id, astDefn=defn.astDefn,
                                 typeParameters=defn.typeParameters,
                                 returnType=defn.returnType,
                                 parameterTypes=defn.parameterTypes,
                                 flags=externFlags)
        externDefns.append(externDefn)
        externalizeType(info, defn.returnType)
        for p in defn.typeParameters:
            externalizeTypeParameter(info, p)
        for t in defn.parameterTypes:
            externalizeType(info, t)
        if defn.instTypes is not None:
            for t in defn.instTypes:
                externalizeType(info, t)
    elif isinstance(defn, ir.Class):
        externDefn = ir.Class(defn.name, id, astDefn=defn.astDefn,
                              typeParameters=defn.typeParameters,
                              supertypes=defn.supertypes,
                              elementType=defn.elementType,
                              flags=externFlags)
        externDefns.append(externDefn)
        for p in defn.typeParameters:
            externalizeTypeParameter(info, p)
        for t in defn.supertypes:
            externalizeType(info, t)
        externDefn.constructors = [externalizeMethod(info, ctor, dep)
                                   for ctor in defn.constructors
                                   if flags.PUBLIC in ctor.flags]
        externDefn.fields = [f for f in defn.fields
                             if flags.PUBLIC in f.flags or flags.ARRAY in f.flags]
        each(info.package.findOrAddName, (f.name for f in externDefn.fields))
        for f in externDefn.fields:
            externalizeType(info, f.type)
        externDefn.methods = [externalizeMethod(info, m, dep)
                              for m in defn.methods
                              if flags.PUBLIC in m.flags]
        if externDefn.elementType is not None:
            externalizeType(info, externDefn.elementType)
    elif isinstance(defn, ir.Trait):
        externDefn = ir.Trait(defn.name, id, astDefn=defn.astDefn,
                              typeParameters=defn.typeParameters,
                              supertypes=defn.supertypes,
                              flags=externFlags)
        externDefns.append(externDefn)
        for p in defn.typeParameters:
            externalizeTypeParameter(info, p)
        for t in defn.supertypes:
            externalizeType(info, t)
        externDefn.methods = [externalizeMethod(info, m, dep)
                              for m in defn.methods
                              if flags.PUBLIC in m.flags]
    else:
        raise NotImplementedError

    return externDefn


def externalizeTypeParameter(info, typeParam):
    if typeParam.isExternalized:
        return
    typeParam.isExternalized = True
    info.package.findOrAddName(typeParam.name)
    externalizeType(info, typeParam.upperBound)
    externalizeType(info, typeParam.lowerBound)


def externalizeType(info, ty):
    if isinstance(ty, ir_types.ClassType):
        externalizeDefn(info, ty.clas)
        for ta in ty.typeArguments:
            externalizeType(info, ta)
    elif isinstance(ty, ir_types.VariableType):
        externalizeTypeParameter(info, ty.typeParameter)
    elif isinstance(ty, ir_types.ExistentialType):
        for v in ty.variables:
            externalizeTypeParameter(info, v)
        externalizeType(info, ty.ty)


def externalizeMethod(info, method, dep):
    info.package.findOrAddName(method.name)
    id = ids.DefnId(ids.TARGET_PACKAGE_ID, ids.DefnId.FUNCTION, len(dep.externMethods))
    externFlags = method.flags | frozenset([flags.EXTERN])
    externMethod = ir.Function(method.name, id, astDefn=method.astDefn,
                               typeParameters=method.typeParameters,
                               returnType=method.returnType,
                               parameterTypes=method.parameterTypes,
                               flags=externFlags)
    dep.externMethods.append(externMethod)
    externalizeType(info, method.returnType)
    for p in method.typeParameters:
        externalizeTypeParameter(info, p)
    for t in method.parameterTypes:
        externalizeType(info, t)
    return externMethod

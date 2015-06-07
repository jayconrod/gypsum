# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import ast
import builtins
import data
import flags
import ids
import ir_types
import bytecode
from utils import each, hashList

import re
import StringIO

class Package(object):
    def __init__(self, id=None, name=None, version=None):
        if id is None:
            id = ids.PackageId()
        if name is None:
            name = Name(["default"])
        if version is None:
            version = PackageVersion([0])
        assert isinstance(id, ids.PackageId) and \
               isinstance(name, Name) and \
               isinstance(version, PackageVersion)
        self.id = id
        self.name = name
        self.version = version
        self.dependencies = []
        self.globals = []
        self.functions = []
        self.classes = []
        self.typeParameters = []
        self.strings = []
        self.entryFunction = None
        self.initFunction = None
        self.exports = None
        self.externTypes = None

    def __str__(self):
        buf = StringIO.StringIO()
        if len(self.dependencies) > 0:
            buf.write("dependencies:\n")
            for dep in self.dependencies:
                buf.write("  %s\n" % dep)
            buf.write("\n")
        for g in self.globals:
            buf.write("%s\n\n" % g)
        for f in self.functions:
            buf.write("%s\n\n" % f)
        for c in self.classes:
            buf.write("%s\n\n" % c)
        for p in self.typeParameters:
            buf.write("%s\n\n" % p)
        buf.write("entry function: %s\n" % self.entryFunction)
        buf.write("init function: %s\n" % self.initFunction)
        return buf.getvalue()

    def addGlobal(self, name, astDefn, *args):
        id = ids.DefnId(self.id, ids.DefnId.GLOBAL, len(self.globals))
        self.addName(name)
        g = Global(name, astDefn, id, *args)
        self.globals.append(g)
        return g

    def addFunction(self, name, astDefn, *args):
        id = ids.DefnId(self.id, ids.DefnId.FUNCTION, len(self.functions))
        self.addName(name)
        f = Function(name, astDefn, id, *args)
        self.functions.append(f)
        return f

    def addClass(self, name, astDefn, *args):
        id = ids.DefnId(self.id, ids.DefnId.CLASS, len(self.classes))
        self.addName(name)
        c = Class(name, astDefn, id, *args)
        self.classes.append(c)
        return c

    def addTypeParameter(self, name, astDefn, *args):
        id = ids.DefnId(self.id, ids.DefnId.TYPE_PARAMETER, len(self.typeParameters))
        self.addName(name)
        p = TypeParameter(name, astDefn, id, *args)
        self.typeParameters.append(p)
        return p

    def newField(self, name, *args):
        self.addName(name)
        return Field(name, *args)

    def ensureDependency(self, package):
        if package.id.index is not None:
            assert self.dependencies[package.id.index].package is package
            return self.dependencies[package.id.index]
        dep = PackageDependency.fromPackage(package)
        package.id.index = len(self.dependencies)
        self.dependencies.append(dep)
        return self.dependencies[-1]

    def ensureExports(self):
        if self.exports is not None:
            return self.exports

        self.exports = {}

        def addExport(defn):
            assert defn.name not in self.exports
            self.exports[defn.name] = defn

        for g in self.globals:
            if flags.PUBLIC in g.flags:
                addExport(g)
        for f in self.functions:
            if flags.PUBLIC in f.flags and flags.METHOD not in f.flags:
                addExport(f)
        for c in self.classes:
            if flags.PUBLIC in c.flags:
                addExport(c)

        return self.exports

    def link(self):
        # We assume package validation is done elsewhere (either when the linked packages were
        # installed or after linking), so we don't do it here.
        # TODO: once validation is implemented, re-check this method and make sure we aren't
        # making any bad assumptions here.
        for dep in self.dependencies:
            depExports = dep.package.ensureExports()
            dep.linkedGlobals = [depExports[g.name] for g in dep.externGlobals]
            assert all(isinstance(g, Global) for g in dep.linkedGlobals)
            dep.linkedFunctions = [depExports[f.name] for f in dep.externFunctions]
            assert all(isinstance(f, Function) for f in dep.linkedFunctions)
            dep.linkedClasses = [depExports[c.name] for c in dep.externClasses]
            assert all(isinstance(c, Class) for c in dep.linkedClasses)

        for ty in self.externTypes:
            assert flags.EXTERN in ty.clas.flags
            depIndex = ty.clas.id.packageId.index
            externIndex = ty.clas.id.externIndex
            ty.clas = self.dependencies[depIndex].linkedClasses[externIndex]
        self.externTypes = None

    def addName(self, name):
        assert isinstance(name, Name)
        each(self.findOrAddString, name.components)

    def findString(self, s):
        if isinstance(s, str):
            s = unicode(s)
        assert isinstance(s, unicode)
        return self.strings.index(s)

    def findOrAddString(self, s):
        if isinstance(s, str):
            s = unicode(s)
        assert isinstance(s, unicode)
        for i in xrange(0, len(self.strings)):
            if self.strings[i] == s:
                return i
        self.strings.append(s)
        return len(self.strings) - 1

    def findFunction(self, **kwargs):
        return next(self.find(self.functions, kwargs))

    def findClass(self, **kwargs):
        return next(self.find(self.classes, kwargs))

    def findGlobal(self, **kwargs):
        return next(self.find(self.globals, kwargs))

    def findTypeParameter(self, **kwargs):
        return next(self.find(self.typeParameters, kwargs))

    def findDependency(self, **kwargs):
        return next(self.find(self.dependencies, kwargs))

    def find(self, defns, kwargs):
        def matchItem(defn, key, value):
            if key == "name" and isinstance(value, str):
                return Name.fromString(value) == defn.name
            elif key == "flag":
                return value in defn.flags
            elif key == "pred":
                return value(defn)
            elif key == "clas":
                 if isinstance(defn, Function):
                     return defn.getReceiverClass() is value
                 else:
                     assert isinstance(defn, TypeParameter)
                     return defn.clas is value
            else:
                return getattr(defn, key) == value
        def matchAll(defn):
            return all(matchItem(defn, k, v) for k, v in kwargs.iteritems())
        return (d for d in defns if matchAll(d))


class Name(object):
    """The name of a package or a definition within a package.

    Names consist of a list between 1 and 100 (inclusive) components. Each component is a
    string of between 1 and 1000 unicode characters. The only invalid character is '.'; this
    acts as a separator when names are printed.

    Package name components are restricted to upper and lower case Roman letters, numbers,
    and '_' (although '_' cannot be the first character).

    Names are not guaranteed to be unique among all definitions in the same package. They
    should be unique among public and protected definitions though."""

    nameComponentSrc = "[^.]+"
    nameSrc = r"%s(?:\.%s)*" % (nameComponentSrc, nameComponentSrc)
    nameRex = re.compile(r"\A%s\Z" % nameSrc)

    packageComponentSrc = "[A-Za-z][A-Za-z0-9_]*"
    packageSrc = r"%s(?:\.%s)*" % (packageComponentSrc, packageComponentSrc)
    packageRex = re.compile(r"\A%s\Z" % packageSrc)

    def __init__(self, components):
        self.components = list(components)

    @staticmethod
    def fromString(s, isPackageName=False):
        m = Name.packageRex.match(s) if isPackageName else Name.nameRex.match(s)
        if not m or m.end() != len(s):
            raise ValueError("invalid package name: " + s)
        return Name(s.split("."))

    def __cmp__(self, other):
        return cmp(self.components, other.components)

    def __hash__(self):
        return hashList(self.components)

    def __repr__(self):
        return "Name(%s)" % ".".join(self.components)

    def __str__(self):
        return ".".join(self.components)

    def __add__(self, other):
        return Name(self.components + other.components)

    def withSuffix(self, suffix):
        return Name(self.components + [suffix])

    def hasPrefix(self, components):
        return len(components) < len(self.components) and \
               all(a == b for a, b in zip(self.components, components))

    def short(self):
        s = self.components[-1]
        if isinstance(s, unicode):
            s = str(s)
        return s


CLOSURE_SUFFIX = "$closure"
CONSTRUCTOR_SUFFIX = "$constructor"
CONTEXT_SUFFIX = "$context"
PACKAGE_INIT_NAME = Name(["$pkginit"])
CLASS_INIT_SUFFIX = "$init"
ANON_PARAMETER_SUFFIX = "$parameter"
RECEIVER_SUFFIX = "$this"


class PackagePrefix(object):
    def __init__(self, name):
        assert isinstance(name, Name)
        self.name = name


class PackageVersion(object):
    versionComponentSrc = "[0-9]+"
    versionSrc = r"%s(?:\.%s)*" % (versionComponentSrc, versionComponentSrc)
    versionRex = re.compile(r"\A%s\Z" % versionSrc)

    def __init__(self, components):
        self.components = components

    @staticmethod
    def fromString(s):
        if not PackageVersion.versionRex.match(s):
            raise ValueError("invalid package version: " + s)
        return PackageVersion([int(c) for c in s.split(".")])

    def __cmp__(self, other):
        return cmp(self.components, other.components)

    def __repr__(self):
        return "PackageVersion(%s)" % ".".join(self.components)

    def __str__(self):
        return ".".join(str(c) for c in self.components)


class PackageDependency(object):
    dependencySrc = "(%s)(?::(%s)?(?:-(%s))?)?" % \
                    (Name.packageSrc, PackageVersion.versionSrc, PackageVersion.versionSrc)
    dependencyRex = re.compile(r"\A%s\Z" % dependencySrc)

    def __init__(self, name, minVersion, maxVersion):
        assert isinstance(name, Name) and \
               (minVersion is None or isinstance(minVersion, PackageVersion)) and \
               (maxVersion is None or isinstance(maxVersion, PackageVersion))
        self.name = name
        self.minVersion = minVersion
        self.maxVersion = maxVersion
        self.package = None
        self.externGlobals = []
        self.linkedGlobals = None
        self.externFunctions = []
        self.linkedFunctions = None
        self.externClasses = []
        self.linkedClasses = None
        self.externMethods = []
        self.externTypeParameters = []

    @staticmethod
    def fromString(s):
        m = PackageDependency.dependencyRex.match(s)
        if not m:
            raise ValueError("invalid package dependency: " + s)
        name = Name.fromString(m.group(1))
        minVersion = PackageVersion.fromString(m.group(2)) if m.group(2) else None
        maxVersion = PackageVersion.fromString(m.group(3)) if m.group(3) else None
        return PackageDependency(name, minVersion, maxVersion)

    @staticmethod
    def fromPackage(package):
        dep = PackageDependency(package.name, package.version, None)
        dep.package = package
        return dep

    def dependencyString(self):
        minStr = str(self.minVersion) if self.minVersion is not None else ""
        maxStr = "-" + str(self.maxVersion) if self.maxVersion is not None else ""
        versionStr = ":" + minStr + maxStr if minStr or maxStr else ""
        return str(self.name) + versionStr

    def __str__(self):
        buf = StringIO.StringIO()
        buf.write(str(self.name))
        if self.minVersion:
            buf.write(":" + str(self.minVersion))
        if self.maxVersion:
            buf.write("-" + str(self.maxVersion))
        buf.write("\n\n")
        for g in self.externGlobals:
            buf.write("%s\n\n" % g)
        return buf.getvalue()

    def __repr__(self):
        return "PackageDependency(%s, %s, %s)" % \
            (repr(self.name), repr(self.minVersion), repr(self.maxVersion))


class IrDefinition(data.Data):
    propertyNames = ("name", "astDefn")
    skipCompareNames = ("astDefn",)

    def __init__(self, *args, **extra):
        name = args[0] if len(args) > 0 else extra["name"]
        assert name is None or isinstance(name, Name)
        astDefn = args[1] if len(args) > 1 else extra["astDefn"]
        assert astDefn is None or isinstance(astDefn, ast.AstNode)
        super(IrDefinition, self).__init__(*args, **extra)

    def isTypeDefn(self):
        return False

    def getLocation(self):
        return self.astDefn.location if self.astDefn is not None else None


class IrTopDefn(IrDefinition):
    propertyNames = IrDefinition.propertyNames + ("id",)
    skipCompareNames = IrDefinition.skipCompareNames + ("id",)

    def isBuiltin(self):
        return self.id.isBuiltin()

    def isForeign(self):
        return not self.isBuiltin() and self.id.packageId is not ids.TARGET_PACKAGE_ID

    def isLocal(self):
        return not self.isBuiltin() and not self.isForeign()


class Global(IrTopDefn):
    propertyNames = IrTopDefn.propertyNames + ("type", "flags")

    def __str__(self):
        buf = StringIO.StringIO()
        if len(self.flags) > 0:
            buf.write(" ".join(self.flags) + " ")
        buf.write("var %s%s: %s" % (self.name, self.id, str(self.type)))
        return buf.getvalue()


class Function(IrTopDefn):
    propertyNames = IrTopDefn.propertyNames + \
                    ("returnType", "typeParameters", "parameterTypes",
                     "variables", "blocks", "flags")

    def getReceiverClass(self):
        assert self.isMethod()
        ty = self.parameterTypes[0]
        return builtins.getBuiltinClassFromType(ty) if ty.isPrimitive() else ty.clas

    def canCallWith(self, typeArgs, argTypes):
        if len(self.typeParameters) != len(typeArgs):
            return False
        upperBounds = [param.upperBound.substitute(self.typeParameters, typeArgs)
                       for param in self.typeParameters]
        lowerBounds = [param.lowerBound.substitute(self.typeParameters, typeArgs)
                       for param in self.typeParameters]
        if not all(typeArg.isSubtypeOf(upperBound) and
                   lowerBound.isSubtypeOf(typeArg)
                   for typeArg, lowerBound, upperBound
                   in zip(typeArgs, lowerBounds, upperBounds)):
            return False

        if len(self.parameterTypes) != len(argTypes):
            return False
        if self.isMethod():
            # Nullable receivers are fine, since they are checked when a method is called.
            argTypes = [argTypes[0].withoutFlag(ir_types.NULLABLE_TYPE_FLAG)] + argTypes[1:]
        paramTypes = [pt.substitute(self.typeParameters, typeArgs)
                      for pt in self.parameterTypes]
        return all(at.isSubtypeOf(pt) for at, pt in zip(argTypes, paramTypes))

    def isMethod(self):
        return flags.METHOD in self.flags

    def isConstructor(self):
        # TODO: come up with a better way to indicate this, maybe a flag.
        return self.name.short() == CONSTRUCTOR_SUFFIX

    def isFinal(self):
        if not self.isMethod() or self.isConstructor():
            return True
        receiverClass = self.getReceiverClass()
        return hasattr(receiverClass, "isPrimitive") and receiverClass.isPrimitive

    def mayOverride(self, other):
        assert self.isMethod() and other.isMethod()
        selfExplicitTypeParameters = getExplicitTypeParameters(self)
        otherExplicitTypeParameters = getExplicitTypeParameters(other)
        typeParametersAreCompatible = \
            len(selfExplicitTypeParameters) == len(otherExplicitTypeParameters) and \
            all(atp.isEquivalent(btp) for atp, btp in
                zip(selfExplicitTypeParameters, otherExplicitTypeParameters))
        selfParameterTypes = self.parameterTypes[1:]
        selfClass = self.getReceiverClass()
        otherClass = other.getReceiverClass()
        otherParameterTypes = [pty.substituteForInheritance(selfClass, otherClass) \
                               for pty in other.parameterTypes[1:]]
        parameterTypesAreCompatible = \
            len(selfParameterTypes) == len(otherParameterTypes) and \
            all(bt.isSubtypeOf(at) for at, bt in zip(selfParameterTypes, otherParameterTypes))
        return typeParametersAreCompatible and \
               parameterTypesAreCompatible

    def __repr__(self):
        return "Function(%s, %s, %s, %s, %s, %s, %s)" % \
            (self.name, repr(self.returnType), repr(self.typeParameters),
             repr(self.parameterTypes), repr(self.variables), repr(self.blocks),
             repr(self.flags))

    def __str__(self):
        buf = StringIO.StringIO()
        if len(self.flags) > 0:
            buf.write(" ".join(self.flags) + " ")
        buf.write("def %s%s" % (self.name, str(self.id)))
        if self.typeParameters is not None and len(self.typeParameters) > 0:
            buf.write("[%s]" % ", ".join([str(tp) for tp in self.typeParameters]))
        if self.parameterTypes is not None and len(self.parameterTypes) > 0:
            buf.write("(%s)" % ", ".join([str(pt) for pt in self.parameterTypes]))
        if self.returnType is not None:
            buf.write(": " + str(self.returnType))
        if self.variables is not None and len(self.variables) > 0 or \
           self.blocks is not None and len(self.blocks) > 0:
            buf.write(" =\n")
        if self.variables is not None:
            for v in self.variables:
                buf.write("  var %s: %s (%s)\n" % (v.name, v.type, v.kind))
        if self.blocks is not None:
            for block in self.blocks:
                buf.write("%d:\n" % block.id)
                for inst in block.instructions:
                    buf.write("  %s\n" % inst)
        return buf.getvalue()


class Class(IrTopDefn):
    propertyNames = IrTopDefn.propertyNames + ("typeParameters", "supertypes",
                     "initializer", "constructors", "fields", "methods", "flags")

    def superclass(self):
        assert self is not builtins.getNothingClass()
        if len(self.supertypes) == 0:
            return None
        else:
            return self.supertypes[0].clas

    def superclasses(self):
        """Returns a generator of superclasses in depth-first order, including this class."""
        assert self is not builtins.getNothingClass()
        yield self
        clas = self
        while len(clas.supertypes) > 0:
            clas = clas.supertypes[0].clas
            yield clas

    def findTypePathToBaseClass(self, base):
        """Returns a list of supertypes (ClassTypes), which represent a path through the class
        DAG from this class to the given base class. The path does not include a type for this
        class, but it does include the supertype for the base. If the given class is not a
        base, returns None. This class must not be Nothing, since there is no well-defined
        path in that case."""
        assert self is not builtins.getNothingClass()
        path = []
        indexStack = [0]
        assert self.id is not None
        visited = set([self.id])

        while len(indexStack) > 0:
            index = indexStack[-1]
            indexStack[-1] += 1
            clas = path[-1].clas if len(path) > 0 else self
            if clas is base:
                return path
            elif index == len(clas.supertypes):
                if len(path) > 0:
                    path.pop()
                indexStack.pop()
            elif clas.supertypes[index].clas.id not in visited:
                supertype = clas.supertypes[index]
                assert supertype.clas.id is not None
                visited.add(supertype.clas.id)
                path.append(supertype)
                indexStack.append(0)
        return None

    def findClassPathToBaseClass(self, base):
        path = self.findTypePathToBaseClass(base)
        if path is None:
            return path
        else:
            return [sty.clas for sty in path]

    def findDistanceToBaseClass(self, base):
        return len(self.findClassPathToBaseClass(base))

    def isSubclassOf(self, other):
        if self is other or self is builtins.getNothingClass():
            return True
        elif other is builtins.getNothingClass():
            return False
        else:
            return other in self.superclasses()

    def findCommonBaseClass(self, other):
        """Returns a class which (a) is a superclass of both classes, and (b) has no subclasses
        which are superclasses of both classes."""
        if self is other:
            return self
        if self is builtins.getNothingClass():
            return other
        if other is builtins.getNothingClass():
            return self

        selfBases = list(self.superclasses())
        otherBases = list(other.superclasses())
        if selfBases[-1] is not otherBases[-1]:
            return None

        selfLast = -len(selfBases) - 1
        otherLast = -len(otherBases) - 1
        i = -1
        while i > selfLast and i > otherLast and selfBases[i] is otherBases[i]:
            i -= 1
        return selfBases[i + 1]

    def getConstructor(self, argTypes):
        # TODO: support constructor overloading
        assert len(self.constructors) <= 1
        if len(self.constructors) > 0:
            return self.constructors[0]
        else:
            return None

    def findMethodByShortName(self, name):
        assert isinstance(name, str)
        for m in self.methods:
            if m.name.short() == name:
                return m
        return None

    def getMethodDict(self):
        methodDict = {}
        for i, m in enumerate(self.methods):
            if m.name not in methodDict:
                methodDict[m.name] = []
            methodDict[m.name].append((i, m))
        return methodDict

    def getField(self, name):
        for f in self.fields:
            if f.name.short() == name:
                return f
        return None

    def getMethod(self, name):
        for m in self.methods:
            if m.name.short() == name:
                return m
        return None

    def getMember(self, name):
        method = self.getMethod(name)
        if method is not None:
            return method
        return self.getField(name)

    def getMethodIndex(self, method):
        for i, m in enumerate(self.methods):
            if m is method:
                return i
        raise KeyError("method does not belong to this class")

    def getFieldIndex(self, field):
        for i, f in enumerate(self.fields):
            if f is field:
                return i
        raise KeyError("field does not belong to this class")

    def isTypeDefn(self):
        return True

    def __repr__(self):
        return "Class(%s, %s, %s, %s, %s, %s, %s, %s)" % \
            (repr(self.name), repr(self.typeParameters), repr(self.supertypes),
             repr(self.initializer), repr(self.constructors),
             repr(self.fields), repr(self.methods), repr(self.flags))

    def __str__(self):
        buf = StringIO.StringIO()
        buf.write("%s class %s%s" % (" ".join(self.flags), self.name, self.id))
        buf.write("\n")
        for field in self.fields:
            buf.write("  %s\n" % str(field))
        if self.initializer is not None:
            buf.write("  %s\n" % str(self.initializer))
        for ctor in self.constructors:
            buf.write("  constructor %s\n" % ctor.id)
        for method in self.methods:
            buf.write("  method %s\n" % method.id)
        return buf.getvalue()


class TypeParameter(IrTopDefn):
    propertyNames = IrTopDefn.propertyNames + ("upperBound", "lowerBound", "flags")

    def __init__(self, name, astDefn, id, upperBound, lowerBound, flags):
        super(TypeParameter, self).__init__(name, astDefn, id, upperBound, lowerBound, flags)
        self.clas = None

    def isEquivalent(self, other):
        return self.upperBound == other.upperBound and \
               self.lowerBound == other.lowerBound

    def isTypeDefn(self):
        return True

    def contains(self, ty):
        return ty.isSubtypeOf(self.upperBound) and self.lowerBound.isSubtypeOf(ty)

    def __repr__(self):
        return "TypeParameter(%s, %s, %s, %s)" % \
            (self.name, self.upperBound, self.lowerBound, self.flags)

    def __str__(self):
        return "%s type %s%s <: %s >: %s" % \
            (" ".join(self.flags), self.name, self.id, self.upperBound, self.lowerBound)

    def variance(self):
        if flags.COVARIANT in self.flags:
            return flags.COVARIANT
        elif flags.CONTRAVARIANT in self.flags:
            return flags.CONTRAVARIANT
        else:
            return ir_types.INVARIANT

    def hasCommonBound(self, other):
        """Returns true if there is some type parameter reachable by following upper bounds
        of this parameter which is also reachable by following lower bounds of `other`. This
        is used by Type.isSubtypeOf."""
        otherLowerBounds = [other]
        while isinstance(otherLowerBounds[-1].lowerBound, ir_types.VariableType):
            otherLowerBounds.append(otherLowerBounds[-1].lowerBound.typeParameter)
        current = self
        while True:
            if any(bound is current for bound in otherLowerBounds):
                return True
            if not isinstance(current.upperBound, ir_types.VariableType):
                return False
            current = current.upperBound.typeParameter

    def findCommonUpperBound(self, other):
        """Returns a type parameter which is an upper bound (directly or indirectly) of this
        type parameter and another one. If no such parameter exists, returns None. This is
        used by Type.lub."""
        selfBounds = [self]
        bound = self.upperBound
        while isinstance(bound, ir_types.VariableType):
            selfBounds.append(bound.typeParameter)
            bound = bound.typeParameter.upperBound

        if other in selfBounds:
            return other
        bound = other.upperBound
        while isinstance(bound, ir_types.VariableType):
            if bound.typeParameter in selfBounds:
                return bound.typeParameter
            bound = bound.typeParameter.upperBound
        return None


# List of variable kinds
LOCAL = "local"
PARAMETER = "parameter"

class Variable(IrDefinition):
    propertyNames = IrDefinition.propertyNames + ("type", "kind", "flags")


class Field(IrDefinition):
    propertyNames = IrDefinition.propertyNames + ("type", "flags")


# Miscellaneous functions for dealing with arguments and parameters.
def getExplicitTypeParameterCount(irDefn):
    if hasattr(irDefn, "astDefn") and \
       hasattr(irDefn.astDefn, "typeParameters"):
        return len(irDefn.astDefn.typeParameters)
    else:
        return len(irDefn.typeParameters)


def getExplicitTypeParameters(irDefn):
    firstExplicit = len(irDefn.typeParameters) - getExplicitTypeParameterCount(irDefn)
    return irDefn.typeParameters[firstExplicit:]


def getImplicitTypeParameters(irDefn):
    firstExplicit = len(irDefn.typeParameters) - getExplicitTypeParameterCount(irDefn)
    return irDefn.typeParameters[:firstExplicit]


def getAllArgumentTypes(irFunction, receiverType, typeArgs, argTypes):
    """Checks compatibility of arguments with the given function.

    In Gypsum, some type arguments and argument types may be implied. Currently, this is
    limited to arguments for parameters that were implied by the enclosing scope of the
    function. This function checks compatibility with the given (explicit) type arguments and
    argument types, including the receiver type (which may be None for regular function calls).
    If the function is compatible, this function returns a (list(Type), list(Type)) tuple
    containing the full list of type arguments and argument types (including the receiver).
    If the function is not compatible, returns None."""
    if receiverType is not None:
        if isinstance(receiverType, ir_types.ObjectType):
            receiverType = receiverType.substituteForBaseClass(irFunction.getReceiverClass())
        implicitTypeArgs = list(receiverType.getTypeArguments())
        allArgTypes = [receiverType] + argTypes
    else:
        implicitTypeParams = getImplicitTypeParameters(irFunction)
        implicitTypeArgs = [ir_types.VariableType(t) for t in implicitTypeParams]
        allArgTypes = argTypes
    allTypeArgs = implicitTypeArgs + typeArgs

    if irFunction.canCallWith(allTypeArgs, allArgTypes):
        return (allTypeArgs, allArgTypes)
    else:
        return None


__all__ = [
    "Package",
    "Name",
    "CLOSURE_SUFFIX",
    "CONSTRUCTOR_SUFFIX",
    "CONTEXT_SUFFIX",
    "PACKAGE_INIT_NAME",
    "CLASS_INIT_SUFFIX",
    "ANON_PARAMETER_SUFFIX",
    "RECEIVER_SUFFIX",
    "PackagePrefix",
    "PackageDependency",
    "Global",
    "Function",
    "Class",
    "TypeParameter",
    "Variable",
    "Field",
    "LOCAL",
    "PARAMETER",
]

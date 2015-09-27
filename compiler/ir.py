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
from utils import each, hashList, reprFormat

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

    def addGlobal(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.GLOBAL, len(self.globals))
        self.addName(name)
        g = Global(name, id, *args, **kwargs)
        self.globals.append(g)
        return g

    def addFunction(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.FUNCTION, len(self.functions))
        self.addName(name)
        f = Function(name, id, *args, **kwargs)
        self.functions.append(f)
        return f

    def addClass(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.CLASS, len(self.classes))
        self.addName(name)
        c = Class(name, id, *args, **kwargs)
        self.classes.append(c)
        return c

    def addTypeParameter(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.TYPE_PARAMETER, len(self.typeParameters))
        self.addName(name)
        p = TypeParameter(name, id, *args, **kwargs)
        self.typeParameters.append(p)
        return p

    def newField(self, name, **kwargs):
        self.addName(name)
        return Field(name, **kwargs)

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
            if flags.PUBLIC in f.flags and \
               (flags.METHOD not in f.flags or \
                len(frozenset([flags.STATIC, flags.CONSTRUCTOR]) & f.flags) > 0):
                addExport(f)
        for c in self.classes:
            if flags.PUBLIC in c.flags:
                addExport(c)
        for p in self.typeParameters:
            if flags.PUBLIC in c.flags:
                addExport(p)

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
            dep.linkedTypeParameters = [depExports[p.name] for p in dep.externTypeParameters]
            assert all(isinstance(p, TypeParameter) for p in dep.linkedTypeParameters)

        for ty in self.externTypes:
            if isinstance(ty, ir_types.ClassType):
                assert flags.EXTERN in ty.clas.flags
                depIndex = ty.clas.id.packageId.index
                externIndex = ty.clas.id.externIndex
                ty.clas = self.dependencies[depIndex].linkedClasses[externIndex]
            else:
                assert isinstance(ty, ir_types.VariableType)
                depIndex = ty.typeParameter.id.packageId.index
                externIndex = ty.typeParameter.id.externIndex
                ty.typeParameter = self.dependencies[depIndex].linkedTypeParameters[externIndex]
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
        return "PackageVersion(%s)" % ".".join(map(repr, self.components))

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
        self.externTypeParameters = []
        self.linkedTypeParameters = None
        self.externMethods = []

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
        for f in self.externFunctions:
            buf.write("%s\n\n" % f)
        for c in self.externClasses:
            buf.write("%s\n\n" % c)
        for p in self.externTypeParameters:
            buf.write("%s\n\n" % p)
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


class ParameterizedDefn(IrTopDefn):
    def canApplyTypeArgs(self, typeArgs):
        if len(self.typeParameters) != len(typeArgs):
            return False
        upperBounds = [param.upperBound.substitute(self.typeParameters, typeArgs)
                       for param in self.typeParameters]
        lowerBounds = [param.lowerBound.substitute(self.typeParameters, typeArgs)
                       for param in self.typeParameters]
        return all(typeArg.isSubtypeOf(upperBound) and
                   lowerBound.isSubtypeOf(typeArg)
                   for typeArg, lowerBound, upperBound
                   in zip(typeArgs, lowerBounds, upperBounds))


class Global(IrTopDefn):
    """Represents a global variable or constant.

    Attributes:
        name (Name): the name of the global
        id (DefnId): unique identifier for the global.
        astDefn (AstNode?): the location in source code where the global is defined.
        type (Type?): the type of the global. May be `None` before type analysis.
        flags (frozenset[flag]): flags indicating how this global may be used. Valid flags are
            `EXTERN`, `LET`, `PUBLIC`.
    """

    def __init__(self, name, id, astDefn=None, type=None, flags=frozenset()):
        # TODO: pass name and astDefn to super when we no longer subclass data.Data
        self.name = name
        self.id = id
        self.astDefn = astDefn
        self.type = type
        self.flags = flags

    def __repr__(self):
        return reprFormat(self, "name", "type", "flags")

    def __str__(self):
        buf = StringIO.StringIO()
        if len(self.flags) > 0:
            buf.write(" ".join(self.flags) + " ")
        buf.write("var %s%s: %s" % (self.name, self.id, str(self.type)))
        return buf.getvalue()

    # TODO: remove below when we no longer subclass data.Data
    propertyNames = IrTopDefn.propertyNames + ("type", "flags")


class Function(ParameterizedDefn):
    """Represents a function or method.

    Attributes:
        name (Name): the name of the function.
        id (DefnId): unique identifier for the function.
        astDefn (AstNode?): the location in source code where the function is defined.
        returnType (Type?): the return type of the function. May be `None` before type analysis.
        typeParameters (list[TypeParameter]?): a list of type parameters used in this
            definition. When this function is called, type arguments need to be passed which
            match these parameters. Parameters defined in an outer scope should appear here,
            too, at the beginning of the list. These are "implicit" parameters. This may be
            `None` before declaration analysis is complete.
        parameterTypes (list[Type]?): a list of types of parameters. When this function is
            called, values of these types are passed on the stack. This may be `None` before
            type analysis.
        variables (list[Variable]?): a list of local variables and parameters used in the
            function. The compiler uses indices in the list when emitting ldlocal / stlocal
            instruction. This may be `None` before semantic analysis and for `EXTERN` or
            `ABSTRACT` or builtin functions.
        blocks (list[BasicBlock]?): a list of basic blocks containing instructions for the
            function. The first block is the only entry point. This may be `None` before
            semantic analysis and for `EXTERN` or `ABSTRACT` or builtin functions.
        flags (frozenset[flag]): a flags indicating how this function is used. Valid flags are
            `ABSTRACT`, `EXTERN`, `PUBLIC`, `PROTECTED`, `PRIVATE`, `STATIC`, `CONSTRUCTOR`,
            `METHOD`.
        insts (list[Instruction]?): a list of instructions to insert instead of calling this
            function. This is set for some (not all) builtin functions. For instance, the `+`
            method of `i64` has a list containing an `addi64` instruction.
    """

    def __init__(self, name, id, astDefn=None, returnType=None, typeParameters=None,
                 parameterTypes=None, variables=None, blocks=None, flags=frozenset(),
                 insts=None):
        # TODO: pass name, id, and astDefn to super when we no longer subclass data.Data.
        self.name = name
        self.id = id
        self.astDefn = astDefn
        self.returnType = returnType
        self.typeParameters = typeParameters
        self.parameterTypes = parameterTypes
        self.variables = variables
        self.blocks = blocks
        self.flags = flags
        self.insts = insts

    def __repr__(self):
        return reprFormat(self, "name", "returnType", "typeParameters", "parameterTypes",
                          "variables", "blocks", "flags")

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

    def getReceiverClass(self):
        """Returns the class of the receiver.

        This is obtained from the type of the first parameter, so it can only be called on
        non-static methods."""
        assert flags.METHOD in self.flags and flags.STATIC not in self.flags
        ty = self.parameterTypes[0]
        return builtins.getBuiltinClassFromType(ty) if ty.isPrimitive() else ty.clas

    def getDefiningClass(self, receiverClass):
        """Returns the class that defined this function.

        This is used for static methods since `getReceiverClass` can't be used. `receiverType`
        is the type of the receiver used to access this method."""

        assert flags.METHOD in self.flags
        if flags.STATIC not in self.flags:
            return self.getReceiverClass()

        # TODO: for now, we walk the class tree to find the higher class that defines this
        # method. This is expensive, and it will become a lot more expensive when traits are
        # introduced. When annotations are introduced, we can define an internal annotation
        # to point to the defining class.

        # This method may be called before class flattening, which means the method might only
        # be found in the defining class and not the derived classes. So we walk up the tree
        # and find the first class that has the method, then find the first class that doesn't.
        clas = receiverClass
        while not any(m is self for m in clas.methods):
            clas = clas.supertypes[0].clas
        nextClass = clas.supertypes[0].clas
        while any(m is self for m in nextClass.methods):
            clas = nextClass
            nextClass = nextClass.supertypes[0].clas
        return clas

    def canCallWith(self, typeArgs, argTypes):
        if not self.canApplyTypeArgs(typeArgs):
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
        return flags.METHOD in self.flags and flags.STATIC not in self.flags

    def isConstructor(self):
        return flags.CONSTRUCTOR in self.flags

    def isFinal(self):
        if not self.isMethod() or self.isConstructor():
            return True
        receiverClass = self.getReceiverClass()
        return hasattr(receiverClass, "isPrimitive") and receiverClass.isPrimitive

    def mayOverride(self, other):
        if not self.isMethod() or not other.isMethod():
            return False
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

    # TODO: remove below when we no longer subclass data.Data
    propertyNames = IrTopDefn.propertyNames + \
                    ("returnType", "typeParameters", "parameterTypes",
                     "variables", "blocks", "flags")


class Class(ParameterizedDefn):
    """Represents a class definition.

    Attributes:
        name (Name): the name of the class.
        id (DefnId): unique identifier for the class.
        astDefn (AstNode?): the location in source code where the class is defined.
        typeParameters (list[TypeParameter]?): a list of type parameters used in this
            definition. Values with a `ClassType` for this class must have type arguments that
            correspond to these parameters. Note that this list should include not only the
            parameters from the class definition, but also any parameters defined in outer
            scopes (which are "implicit" and should come first). This may be `None` before
            declaration analysis is complete.
        supertypes (list[ClassType]?): a list of types from which this class is derived,
            including type arguments. The subtype relation uses this. This may be `None`
            before type declaration analysis is complete.
        initializer (Function?): a function which initializes new instances of this class.
            Constructors call this after calling a superconstructor if they don't call another
            constructor. May be `None` before declaration analysis is complete or if there
            is no initializer.
        constructors (list[Function]?): a list of functions which initialize new instances of
            this class. These functions are called directly after creating a new instance. This
            may be `None` before declaration analysis is complete.
        fields (list[Field]?): a list of fields found in instances of this class. This may be
            `None` before declaration analysis is complete. Inherited fields are added to
            this list during class flattening.
        methods (list[Function]?): a list of functions that operate on instances of this class.
            This may be `None` before declaration analysis is complete. Inherited methods may
            be added to this list during class flattening
        flags (frozenset[flag]): flags indicating how this class is used. Valid flags are
            `ABSTRACT`, `EXTERN`, `PUBLIC`, `PROTECTED`, `PRIVATE`.
    """

    def __init__(self, name, id, astDefn=None, typeParameters=None, supertypes=None,
                 initializer=None, constructors=None, fields=None, methods=None,
                 flags=frozenset()):
        # TODO: pass name, id, and astDefn to super when we no longer subclass data.Data
        self.name = name
        self.id = id
        self.astDefn = astDefn
        self.typeParameters = typeParameters
        self.supertypes = supertypes
        self.initializer = initializer
        self.constructors = constructors
        self.fields = fields
        self.methods = methods
        self.flags = flags

    def __repr__(self):
        return reprFormat(self, "name", "typeParameters", "supertypes", "initializer",
                          "constructors", "fields", "methods", "flags")

    def __str__(self):
        buf = StringIO.StringIO()
        buf.write("%s class %s%s" % (" ".join(self.flags), self.name, self.id))
        buf.write("\n")
        for field in self.fields:
            buf.write("  %s\n" % str(field))
        if self.initializer is not None:
            buf.write("  initializer %s\n" % self.initializer.id)
        for ctor in self.constructors:
            buf.write("  constructor %s\n" % ctor.id)
        for method in self.methods:
            buf.write("  method %s\n" % method.id)
        return buf.getvalue()

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
        for i, m in enumerate(m for m in self.methods if flags.STATIC not in m.flags):
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

    # TODO: remove below when we no longer subclass data.Data
    propertyNames = IrTopDefn.propertyNames + ("typeParameters", "supertypes",
                     "initializer", "constructors", "fields", "methods", "flags")


class TypeParameter(IrTopDefn):
    """Represents a range of possible types between an upper and lower bound.

    In source, type parameters are always defined as part of a class or function definition.
    In IR though, they are global and can be used by multiple definitions (especially
    definitions which were nested in source).

    Type parameters are referenced through `VariableType`, which represents some type within
    bounds. Type parameters may have variance, which determines how the subtype relation works
    for parameterized types.

    Attributes:
        name (Name): the name of the type parameter.
        id (DefnId): unique identifier of the type parameter.
        astDefn (AstNode?): the location in source code where the type parameter is defined.
        upperBound (Type?): type arguments must be a subtype of this. May be `None` before
            type analysis.
        lowerBound (Type?): type arguments must be a supertype of this. May be `None` before
            type analysis.
        flags (frozenset[flag]): flags indicating how this type parameter may be used. Valid
            flags are `EXTERN`, `CONTRAVARIANT`, `COVARIANT`, `STATIC`.
    """

    def __init__(self, name, id, astDefn=None,
                 upperBound=None, lowerBound=None, flags=frozenset(), clas=None):
        # TODO: pass name, id, and astDefn to super when we no longer subclass data.Data
        self.name = name
        self.id = id
        self.astDefn = astDefn
        self.upperBound = upperBound
        self.lowerBound = lowerBound
        self.flags = flags
        self.clas = None

    def __repr__(self):
        return reprFormat(self, "name", "upperBound", "lowerBound", "flags")

    def __str__(self):
        return "%s type %s%s <: %s >: %s" % \
            (" ".join(self.flags), self.name, self.id, self.upperBound, self.lowerBound)

    def isEquivalent(self, other):
        return self.upperBound == other.upperBound and \
               self.lowerBound == other.lowerBound

    def isTypeDefn(self):
        return True

    def contains(self, ty):
        return ty.isSubtypeOf(self.upperBound) and self.lowerBound.isSubtypeOf(ty)

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

        if any(tp is other for tp in selfBounds):
            return other
        bound = other.upperBound
        while isinstance(bound, ir_types.VariableType):
            if any(tp is bound.typeParameter for tp in selfBounds):
                return bound.typeParameter
            bound = bound.typeParameter.upperBound
        return None

    # TODO: remove definitions below when we no longer subclass data.Data
    propertyNames = IrTopDefn.propertyNames + ("upperBound", "lowerBound", "flags")


# List of variable kinds
LOCAL = "local"
PARAMETER = "parameter"

class Variable(IrDefinition):
    def __init__(self, name, astDefn=None, type=None, kind=LOCAL, flags=frozenset()):
        # TODO: pass name and astDefn to super when we no longer subclass data.Data
        self.name = name
        self.astDefn = astDefn
        self.type = type
        self.kind = kind
        self.flags = flags

    def __repr__(self):
        return reprFormat(self, "name", "type", "kind", "flags")

    def __str__(self):
        flagsStr = " ".join(self.flags)
        return "%s %s %s: %s" % (flagsStr, self.kind, self.name, self.type)

    # TODO: remove definitions below when we no longer subclass data.Data
    propertyNames = IrDefinition.propertyNames + ("type", "kind", "flags")


class Field(IrDefinition):
    """Represents a data member of a class.

    Attributes:
        name (Name): the name of the field.
        astDefn (AstNode?): the location in source code where the field is defined.
        type (Type?): the type of the field. May be `None` before type analysis.
        flags (frozenset[flag]): flags indicating how this field is used. Valid flags are
            `LET`, `PUBLIC`, `PROTECTED`, `PRIVATE`, `STATIC`.
        index (int?): an integer indicating where this field is located with an instance of
            its class. 0 is the first field, 1 is the second, etc. Used to generate load / store
            instructions. This will usually be `None` before semantic analysis.
    """

    def __init__(self, name, astDefn=None, type=None, flags=frozenset(), index=None):
        # TODO: pass name and astDefn to super when we no longer subclass data.Data
        self.name = name
        self.astDefn = astDefn
        self.type = type
        self.flags = flags
        self.index = index

    def __repr__(self):
        return reprFormat(self, "name", "type", "flags")

    def __str__(self):
        return "%s field %s: %s" % (" ".join(self.flags), self.name, self.type)

    # TODO: remove definitions below when we no longer subclass data.Data
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


def getAllArgumentTypes(irFunction, receiverType, typeArgs, argTypes, importedTypeArgs):
    """Checks compatibility of arguments with the given function.

    Some type arguments may be implied. If the function was imported, these type arguments
    were specified in the import statement. If the function is defined inside a class with
    type parameters, type arguments are implied by the receiver (the enclosing scope
    in general). If the function is compatible, this function returns a
    ([Type], [Type]) tuple containing the full list of type arguments and argument types
    (including the receiver). If the function is not compatible, None is returned."""
    if receiverType is not None and \
       importedTypeArgs is None and \
       (flags.STATIC in irFunction.flags or flags.METHOD in irFunction.flags):
        # Method call: type args are implied by receiver.
        if isinstance(receiverType, ir_types.ObjectType):
            definingClass = irFunction.getDefiningClass(ir_types.getClassFromType(receiverType))
            receiverType = receiverType.substituteForBaseClass(definingClass)
        implicitTypeArgs = list(receiverType.getTypeArguments())
        allArgTypes = argTypes \
                      if flags.STATIC in irFunction.flags \
                      else [receiverType] + argTypes
    elif importedTypeArgs is not None:
        # Imported function call: type args are implied by import statement.
        implicitTypeArgs = importedTypeArgs
        allArgTypes = argTypes
    else:
        # Other function call: type args are implied by parent scope.
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

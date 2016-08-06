# Copyright 2014-2016, Jay Conrod. All rights reserved.
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
from utils import each, indexSame, hashList, reprFormat

import re
import StringIO

class Package(object):
    def __init__(self, id=None, name=None, version=None):
        if id is None:
            id = ids.PackageId(name=name)
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
        self.traits = []
        self.typeParameters = []
        self.strings = []
        self.entryFunction = None
        self.initFunction = None
        self.exports = None

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
        for t in self.traits:
            buf.write("%s\n\n" % t)
        for p in self.typeParameters:
            buf.write("%s\n\n" % p)
        buf.write("entry function: %s\n" % self.entryFunction)
        buf.write("init function: %s\n" % self.initFunction)
        return buf.getvalue()

    def addGlobal(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.GLOBAL, len(self.globals))
        self.addName(name)
        g = Global(name, id, *args, **kwargs)
        if g.sourceName is not None:
            self.findOrAddString(g.sourceName)
        self.globals.append(g)
        return g

    def addFunction(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.FUNCTION, len(self.functions))
        self.addName(name)
        f = Function(name, id, *args, **kwargs)
        if f.sourceName is not None:
            self.findOrAddString(f.sourceName)
        self.functions.append(f)
        return f

    def addClass(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.CLASS, len(self.classes))
        self.addName(name)
        c = Class(name, id, *args, **kwargs)
        if c.sourceName is not None:
            self.findOrAddString(c.sourceName)
        self.classes.append(c)
        return c

    def addTrait(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.TRAIT, len(self.traits))
        self.addName(name)
        t = Trait(name, id, *args, **kwargs)
        if t.sourceName is not None:
            self.findOrAddString(t.sourceName)
        self.traits.append(t)
        return t

    def addTypeParameter(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.TYPE_PARAMETER, len(self.typeParameters))
        self.addName(name)
        p = TypeParameter(name, id, *args, **kwargs)
        if p.sourceName is not None:
            self.findOrAddString(p.sourceName)
        self.typeParameters.append(p)
        return p

    def newField(self, name, **kwargs):
        self.addName(name)
        f = Field(name, **kwargs)
        if f.sourceName is not None:
            self.findOrAddString(f.sourceName)
        return f

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

        for g in self.globals:
            if flags.PUBLIC in g.flags:
                self.exports[g.name] = g
        for f in self.functions:
            if flags.PUBLIC in f.flags:
                self.exports[mangleFunctionName(f, self)] = f
        for c in self.classes:
            if flags.PUBLIC in c.flags:
                self.exports[c.name] = c
        for t in self.traits:
            if flags.PUBLIC in t.flags:
                self.exports[t.name] = t
        for p in self.typeParameters:
            if flags.PUBLIC in p.flags:
                self.exports[p.name] = p

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
            dep.linkedFunctions = [depExports[mangleFunctionName(f, self)]
                                   for f in dep.externFunctions]
            assert all(isinstance(f, Function) for f in dep.linkedFunctions)
            dep.linkedClasses = [depExports[c.name] for c in dep.externClasses]
            assert all(isinstance(c, Class) for c in dep.linkedClasses)
            dep.linkedTraits = [depExports[t.name] for t in dep.externTraits]
            assert all(isinstance(t, Trait) for t in dep.linkedTraits)
            dep.linkedTypeParameters = [depExports[p.name] for p in dep.externTypeParameters]
            assert all(isinstance(p, TypeParameter) for p in dep.linkedTypeParameters)

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

    def findTrait(self, **kwargs):
        return next(self.find(self.traits, kwargs))

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
                     return defn.definingClass is value
                 else:
                     assert isinstance(defn, TypeParameter)
                     return defn.clas is value
            else:
                return getattr(defn, key) == value
        def matchAll(defn):
            return all(matchItem(defn, k, v) for k, v in kwargs.iteritems())
        return (d for d in defns if matchAll(d))

    def getDefn(self, id):
        assert id.packageId is self.id
        if id.kind is ids.DefnId.GLOBAL:
            return self.globals[id.index]
        elif id.kind is ids.DefnId.FUNCTION:
            return self.functions[id.index]
        elif id.kind is ids.DefnId.CLASS:
            return self.classes[id.index]
        elif id.kind is ids.DefnId.TRAIT:
            return self.traits[id.index]
        else:
            assert id.kind is ids.DefnId.TYPE_PARAMETER
            return self.typeParameters[id.index]


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
ARRAY_LENGTH_SUFFIX = "$length"
EXISTENTIAL_SUFFIX = "$forsome"
BLANK_SUFFIX = "$blank"


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
        self.externTraits = []
        self.linkedTraits = None
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
        for t in self.externTraits:
            bug.write("%s\n\n" % t)
        for p in self.externTypeParameters:
            buf.write("%s\n\n" % p)
        return buf.getvalue()

    def __repr__(self):
        return "PackageDependency(%s, %s, %s)" % \
            (repr(self.name), repr(self.minVersion), repr(self.maxVersion))


class IrDefinition(object):
    def __init__(self, name, sourceName, astDefn):
        assert name is None or isinstance(name, Name)
        assert sourceName is None or isinstance(sourceName, str) \
            or isinstance(sourceName, unicode)
        assert astDefn is None or isinstance(astDefn, ast.Node)
        self.name = name
        self.sourceName = sourceName
        self.astDefn = astDefn

    def __eq__(self, other):
        raise NotImplementedError()

    def __ne__(self, other):
        return not (self == other)

    def isTypeDefn(self):
        return False

    def getLocation(self):
        return self.astDefn.location if self.astDefn is not None else None


class IrTopDefn(IrDefinition):
    def __init__(self, name, id, sourceName, astDefn):
        super(IrTopDefn, self).__init__(name, sourceName, astDefn)
        self.id = id

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
        sourceName (str?): name of the definition in source code
        astDefn (ast.Node?): the location in source code where the global is defined.
        type (Type?): the type of the global. May be `None` before type analysis.
        flags (frozenset[flag]): flags indicating how this global may be used. Valid flags are
            `EXTERN`, `LET`, `PUBLIC`.
    """

    def __init__(self, name, id, sourceName=None, astDefn=None, type=None, flags=frozenset()):
        super(Global, self).__init__(name, id, sourceName, astDefn)
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

    def __eq__(self, other):
        return self.name == other.name and \
               self.type == other.type and \
               self.flags == other.flags


class Function(ParameterizedDefn):
    """Represents a function or method.

    Attributes:
        name (Name): the name of the function.
        id (DefnId): unique identifier for the function.
        sourceName (str?): name of the definition in source code
        astDefn (ast.Node?): the location in source code where the function is defined.
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
        definingClass (ObjectTypeDefn?): the class or trait this method (static or not) was
            defined in (as opposed to any other class that inherits the method). For
            non-static methods, this could be derived from the receiver type, but there's
            no easy way to get it for static methods.
        overrides ([Function]?): methods in inherited traits or a base class that this
            method overrides. This must be set if the `METHOD` and `OVERRIDE` flags are set and
            the `STATIC`, `CONSTRUCTOR`, and `EXTERN` flags are not set. It must not be empty.
        overridenBy ({DefnId, Function}?): a map from class and trait ids to methods. Each
            entry describes an overriding function in a subclass or subtrait. This should only
            be set for non-constructor, non-static methods.
        insts (list[Instruction]?): a list of instructions to insert instead of calling this
            function. This is set for some (not all) builtin functions. For instance, the `+`
            method of `i64` has a list containing an `addi64` instruction.
        compileHint (symbol?): if set, the compiler will generate instructions for a specific
            kind of function (for example, an array getter) instead of generating instructions
            from a function body (which may not be present).
    """

    def __init__(self, name, id, sourceName=None, astDefn=None, returnType=None,
                 typeParameters=None, parameterTypes=None, variables=None, blocks=None,
                 flags=frozenset(), definingClass=None, overrides=None, overridenBy=None,
                 insts=None, compileHint=None):
        super(Function, self).__init__(name, id, sourceName, astDefn)
        self.returnType = returnType
        self.typeParameters = typeParameters
        self.parameterTypes = parameterTypes
        self.variables = variables
        self.blocks = blocks
        self.flags = flags
        self.definingClass = definingClass
        self.insts = insts
        self.overrides = overrides
        self.overridenBy = overridenBy
        self.compileHint = compileHint

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
        if self.definingClass is not None:
            buf.write("  class %s\n" % (self.definingClass.name))
        if self.blocks is not None:
            for block in self.blocks:
                buf.write("%d:\n" % block.id)
                for inst in block.instructions:
                    buf.write("  %s\n" % inst)
        return buf.getvalue()

    def __eq__(self, other):
        return self.name == other.name and \
               self.returnType == other.returnType and \
               self.typeParameters == other.typeParameters and \
               self.parameterTypes == other.parameterTypes and \
               self.variables == other.variables and \
               self.blocks == other.blocks and \
               self.flags == other.flags and \
               self.definingClass is other.definingClass and \
               self.insts == other.insts and \
               self.compileHint is other.compileHint

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
        if not self.isMethod() or self.isConstructor() or flags.FINAL in self.flags:
            return True
        receiverClass = self.definingClass
        return hasattr(receiverClass, "isPrimitive") and receiverClass.isPrimitive

    def mayOverride(self, other):
        if not self.isMethod() or self.isConstructor() or \
           not other.isMethod() or other.isConstructor():
            return False
        selfExplicitTypeParameters = getExplicitTypeParameters(self)
        otherExplicitTypeParameters = getExplicitTypeParameters(other)
        typeParametersAreCompatible = \
            len(selfExplicitTypeParameters) == len(otherExplicitTypeParameters) and \
            all(atp.isEquivalent(btp) for atp, btp in
                zip(selfExplicitTypeParameters, otherExplicitTypeParameters))
        selfParameterTypes = self.parameterTypes[1:]
        selfClass = self.definingClass
        otherClass = other.definingClass
        otherParameterTypes = [pty.substituteForInheritance(selfClass, otherClass) \
                               for pty in other.parameterTypes[1:]]
        parameterTypesAreCompatible = \
            len(selfParameterTypes) == len(otherParameterTypes) and \
            all(bt.isSubtypeOf(at) for at, bt in zip(selfParameterTypes, otherParameterTypes))
        return typeParametersAreCompatible and \
               parameterTypesAreCompatible


class ObjectTypeDefn(ParameterizedDefn):
    """Represents a definition that creates an interface for interacting with objects.
    Specifically, this is a class or trait.

    Attributes:
        name (Name): the name of the definition.
        id (DefnId): unique identifier for the definition.
        sourceName (str?): name of the definition in source code.
        astDefn (ast.Node?): the node from the AST where this definition comes from.
        typeParameters (list[TypeParameter]?): a list of type parameters used in this
            definition. `ClassType`s for this definition must have type arguments that
            correspond to these parameters. Note that this list should include not only the
            parameters from the definition, but also any parameters defined in outer
            scopes (which are "implicit" and should come first).
        supertypes (list[ClassType]?): a complete list of types from which this definition is
            derived. This list includes not only the direct base types of the definition, but
            also the transitive closure types. The list may be `None` or incomplete until
            inheritance analysis is finished. `Type.isSubtypeOf` may not be used until then.
            `VariableType`s in this list must correspond to parameters in `typeParameters`.
    """

    def __init__(self, name, id, sourceName=None, astDefn=None, typeParameters=None,
                 supertypes=None):
        super(ObjectTypeDefn, self).__init__(name, id, sourceName, astDefn)
        self.typeParameters = typeParameters
        self.supertypes = supertypes

    def isTypeDefn(self):
        return True

    def superclass(self):
        """Returns the `Class` which is the first base of this definition.

        `None` is returned if this definition has no bases.
        """
        assert self is not builtins.getNothingClass()
        if len(self.supertypes) == 0:
            return None
        else:
            return self.supertypes[0].clas

    def bases(self):
        """Returns a generator of definitions this definition is based on.

        The generator iterates over bases in depth-first pre-order. This definition is
        included (it is returned first).
        """
        assert self is not builtins.getNothingClass()
        yield self
        clas = self
        for supertype in self.supertypes:
            yield supertype.clas

    def findDistanceToBaseClass(self, base):
        """Returns the distance to `base` on the inheritance graph.

        Arguments:
            base (Class): a superclass of this class.

        Returns:
            (int): 0 if `self` is `base`, 1 if `base` is the direct superclass, 2 if it is
            the superclass of the direct superclass, and so on.
        """
        # This method is only used by builtins, and there are only builtin classes. It doesn't
        # really work for traits. If we want to use it for traits, we should store more
        # information with supertypes so we don't have to do a full depth-first-search of
        # the inheritance graph on every call.
        distance = 0
        clas = self
        while clas is not None and clas is not base:
            distance += 1
            clas = clas.superclass()
        assert clas is base
        return distance

    def isDerivedFrom(self, other):
        if self is other or self is builtins.getNothingClass():
            return True
        elif other is builtins.getNothingClass():
            return False
        else:
            return other in self.bases()

    def findBaseType(self, base):
        """Searches this definition's supertypes for a type of the same definition as `base`.

        This method may only be called after inheritance analysis has been performed.

        Arguments:
            base (ObjectTypeDefn): the class or trait of the type being searched for.

        Returns:
            ClassType?: returns the base type if this definition is derived from `base`. If
                `base` is `self`, returns the same as `ClassType.forReceiver(self)`. Returns
                `None` otherwise.
        """
        if self is base:
            return ir_types.ClassType.forReceiver(self)
        for sty in self.supertypes:
            if sty.clas is base:
                return sty
        return None

    def findMethodBySourceName(self, name):
        """Searches the method list.

        Arguments:
            name (str): the source name of the method. Methods without source names will not
                be considered.

        Returns:
            (Function, int)?: returns a pair of the method and the method index for the first
                matching method. If no match was found, `None` is returned.
        """
        assert isinstance(name, str)
        for i, m in enumerate(m for m in self.methods if flags.STATIC not in m.flags):
            if m.sourceName == name:
                return m, i
        return None

    def getMethodIndex(self, method):
        for i, m in enumerate(m for m in self.methods if flags.STATIC not in m.flags):
            if m is method:
                return i
        raise KeyError("method does not belong to this class")


class Class(ObjectTypeDefn):
    """Represents a class definition.

    Attributes:
        name (Name): the name of the definition.
        id (DefnId): unique identifier for the definition.
        sourceName (str?): name of the definition in source code.
        astDefn (ast.Node?): the node from the AST where this definition comes from.
        typeParameters (list[TypeParameter]?): a list of type parameters used in this
            definition. `ClassType`s for this definition must have type arguments that
            correspond to these parameters. Note that this list should include not only the
            parameters from the definition, but also any parameters defined in outer
            scopes (which are "implicit" and should come first).
        supertypes (list[ClassType]?): a complete list of types from which this definition is
            derived. This list includes not only the direct base types of the definition, but
            also the transitive closure types. The list may be `None` or incomplete until
            inheritance analysis is finished. `Type.isSubtypeOf` may not be used until then.
            `VariableType`s in this list must correspond to parameters in `typeParameters`.
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
        traits ({DefnId: [Function]}?): a method list for each trait this class inherits,
            directly or indirectly. This is set during class flattening.
        elementType (Type?): if this is an array class, this is the type of the elements. `None`
            for non-array classes. The `ARRAY` flag must be set if this is not `None`.
        flags (frozenset[flag]): flags indicating how this class is used. Valid flags are
            `ABSTRACT`, `ARRAY`, `EXTERN`, `PUBLIC`, `PROTECTED`, `PRIVATE`.
    """

    def __init__(self, name, id, sourceName=None, astDefn=None, typeParameters=None,
                 supertypes=None, initializer=None, constructors=None, fields=None,
                 methods=None, traits=None, elementType=None, flags=frozenset()):
        super(Class, self).__init__(name, id, sourceName, astDefn, typeParameters, supertypes)
        self.initializer = initializer
        self.constructors = constructors
        self.fields = fields
        self.methods = methods
        self.traits = traits
        self.elementType = elementType
        self.flags = flags

    def __repr__(self):
        return reprFormat(self, "name", "typeParameters", "supertypes", "initializer",
                          "constructors", "fields", "methods", "traits", "elementType", "flags")

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
        if self.traits is not None:
            for id, methods in self.traits:
                buf.write("  trait %s\n" % id)
                for method in methods:
                    buf.write("    method %s\n" % method.id)
        if self.elementType is not None:
            buf.write("  arrayelements %s\n" % str(self.elementType))
        return buf.getvalue()

    def __eq__(self, other):
        return self.name == other.name and \
               self.typeParameters == other.typeParameters and \
               self.supertypes == other.supertypes and \
               self.initializer == other.initializer and \
               self.constructors == other.constructors and \
               self.fields == other.fields and \
               self.methods == other.methods and \
               self.traits == other.traits and \
               self.elementType == other.elementType and \
               self.flags == other.flags

    def findCommonBaseClass(self, other):
        """Returns a class which (a) is a superclass of both classes, and (b) has no subclasses
        which are superclasses of both classes."""
        if self is other:
            return self
        if self is builtins.getNothingClass():
            return other
        if other is builtins.getNothingClass():
            return self

        selfBases = list(self.bases())
        otherBases = list(other.bases())
        if selfBases[-1] is not otherBases[-1]:
            return None

        selfLast = -len(selfBases) - 1
        otherLast = -len(otherBases) - 1
        i = -1
        while i > selfLast and i > otherLast and selfBases[i] is otherBases[i]:
            i -= 1
        return selfBases[i + 1]

    def findFieldBySourceName(self, name):
        for i, f in enumerate(self.fields):
            if name == f.sourceName:
                return f, i
        return None

    def getMember(self, name):
        method = self.findMethodBySourceName(name)[0]
        if method is not None:
            return method
        return self.findFieldBySourceName(name)[0]

    def getFieldIndex(self, field):
        for i, f in enumerate(self.fields):
            if f is field:
                return i
        raise KeyError("field does not belong to this class")

    def isFinal(self):
        return flags.FINAL in self.flags


class Trait(ObjectTypeDefn):
    """Represents a trait definition. A trait is like an interface from Java, but it allows
    methods to be defined and inherited.

    Attributes:
        name (Name): the name of the definition.
        id (DefnId): unique identifier for the definition.
        sourceName (str?): name of the definition in source code.
        astDefn (ast.Node?): the node from the AST where this definition comes from.
        typeParameters (list[TypeParameter]?): a list of type parameters used in this
            definition. `ClassType`s for this definition must have type arguments that
            correspond to these parameters. Note that this list should include not only the
            parameters from the definition, but also any parameters defined in outer
            scopes (which are "implicit" and should come first).
        supertypes (list[ClassType]?): a complete list of types from which this definition is
            derived. This list includes not only the direct base types of the definition, but
            also the transitive closure types. The list may be `None` or incomplete until
            inheritance analysis is finished. `Type.isSubtypeOf` may not be used until then.
            `VariableType`s in this list must correspond to parameters in `typeParameters`.
        methods (list[Function]): a list of functions that operate on instances of this trait.
            Inherited methods may be added to this list during class flattening.
        flags (frozenset[flag]): flags indicating how this trait is used. Valid flags are
            `PUBLIC`, `PROTECTED`, `PRIVATE`.
    """

    def __init__(self, name, id, sourceName=None, astDefn=None, typeParameters=None,
                 supertypes=None, methods=None, flags=None):
        super(Trait, self).__init__(name, id, sourceName, astDefn, typeParameters, supertypes)
        self.methods = methods
        self.flags = flags

    def __repr__(self):
        return reprFormat(self, "name", "typeParameters", "supertypes", "methods", "flags")

    def __str__(self):
        buf = StringIO.StringIO()
        buf.write("%s trait %s%s\n" % (" ".join(self.flags), self.name, self.id))
        for method in self.methods:
            buf.write("  method %s\n" % method.id)
        return buf.getvalue()

    def __eq__(self, other):
        return self.name == other.name and \
               self.typeParameters == other.typeParameters and \
               self.supertypes == other.supertypes and \
               self.methods == other.methods and \
               self.flags == other.flags


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
        sourceName (str?): name of the definition in source code
        astDefn (ast.Node?): the location in source code where the type parameter is defined.
        upperBound (Type?): type arguments must be a subtype of this. May be `None` before
            type analysis.
        lowerBound (Type?): type arguments must be a supertype of this. May be `None` before
            type analysis.
        flags (frozenset[flag]): flags indicating how this type parameter may be used. Valid
            flags are `EXTERN`, `CONTRAVARIANT`, `COVARIANT`, `STATIC`.
    """

    def __init__(self, name, id, sourceName=None, astDefn=None,
                 upperBound=None, lowerBound=None, flags=frozenset(), clas=None):
        super(TypeParameter, self).__init__(name, id, sourceName, astDefn)
        self.upperBound = upperBound
        self.lowerBound = lowerBound
        self.flags = flags
        self.clas = None

    def __repr__(self):
        return reprFormat(self, "name", "upperBound", "lowerBound", "flags")

    def __str__(self):
        return "%s type %s%s <: %s >: %s" % \
            (" ".join(self.flags), self.name, self.id, self.upperBound, self.lowerBound)

    def __eq__(self, other):
        return self.name == other.name and \
               self.upperBound == other.upperBound and \
               self.lowerBound == other.lowerBound and \
               self.flags == other.flags

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


# List of variable kinds
LOCAL = "local"
PARAMETER = "parameter"

class Variable(IrDefinition):
    def __init__(self, name, sourceName=None, astDefn=None, type=None, kind=LOCAL,
                 flags=frozenset()):
        super(Variable, self).__init__(name, sourceName, astDefn)
        self.type = type
        self.kind = kind
        self.flags = flags

    def __repr__(self):
        return reprFormat(self, "name", "type", "kind", "flags")

    def __str__(self):
        flagsStr = " ".join(self.flags)
        return "%s %s %s: %s" % (flagsStr, self.kind, self.name, self.type)

    def __eq__(self, other):
        return self.name == other.name and \
               self.type == other.type and \
               self.kind is other.kind and \
               self.flags == other.flags


class Field(IrDefinition):
    """Represents a data member of a class.

    Attributes:
        name (Name): the name of the field.
        sourceName (str?): name of the definition in source code
        astDefn (ast.Node?): the location in source code where the field is defined.
        type (Type?): the type of the field. May be `None` before type analysis.
        flags (frozenset[flag]): flags indicating how this field is used. Valid flags are
            `LET`, `PUBLIC`, `PROTECTED`, `PRIVATE`, `STATIC`.
        index (int?): an integer indicating where this field is located with an instance of
            its class. 0 is the first field, 1 is the second, etc. Used to generate load / store
            instructions. This will usually be `None` before semantic analysis.
    """

    def __init__(self, name, sourceName=None, astDefn=None, type=None, flags=frozenset(),
                 index=None):
        super(Field, self).__init__(name, sourceName, astDefn)
        self.type = type
        self.flags = flags
        self.index = index

    def __repr__(self):
        return reprFormat(self, "name", "type", "flags")

    def __str__(self):
        return "%s field %s: %s" % (" ".join(self.flags), self.name, self.type)

    def __eq__(self, other):
        return self.name == other.name and \
               self.type == other.type and \
               self.flags == other.flags


# Miscellaneous functions for dealing with arguments and parameters.
def getExplicitTypeParameterCount(irDefn):
    """Returns the number of type parameters declared by this definition.

    A definition's type parameter list include parameters from both the enclosing definitions
    and the definition itself. For example, a method's type parameter list includes parameters
    from its defining class. "Implicit" type parameters come from an enclosing definition and
    are at the beginning of the list; "explicit" type parameters come from the definition
    itself and are at the end of the list.

    Arguments:
        irDefn (ParameterizedDefn): a definition with type parameters.

    Returns:
        (int): the number of type parameters declared by this definition.
    """
    if isinstance(irDefn, Function):
        if irDefn.definingClass is not None:
            return len(irDefn.typeParameters) - len(irDefn.definingClass.typeParameters)
        elif irDefn.astDefn is not None:
            # Local functions may still have implicit type parameters. We peek at the AST,
            # to count the explicit type parameters, which is kind of a hack. Works as long
            # as we can't import local functions from other packages, since we wouldn't
            # have an AST.
            return len(irDefn.astDefn.typeParameters)
        else:
            return len(irDefn.typeParameters)
    else:
        # TODO: implement for classes and traits after nesting is supported.
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
            receiverType = receiverType.substituteForBase(irFunction.definingClass)
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


def mangleFunctionName(function, package):
    """Returns a mangled name string which can be used to link overloaded functions."""
    def mangleType(ty, variables):
        if ty is ir_types.UnitType:
            return "U"
        elif ty is ir_types.BooleanType:
            return "Z"
        elif ty is ir_types.I8Type:
            return "B"
        elif ty is ir_types.I16Type:
            return "S"
        elif ty is ir_types.I32Type:
            return "I"
        elif ty is ir_types.I64Type:
            return "L"
        elif ty is ir_types.F32Type:
            return "F"
        elif ty is ir_types.F64Type:
            return "D"
        elif isinstance(ty, ir_types.ClassType):
            if isinstance(ty.clas, Class):
                code = "C"
            else:
                assert isinstance(ty.clas, Trait)
                code = "T"
            nameStr = str(ty.clas.name)
            if ty.clas.isForeign():
                assert ty.clas.id.packageId.name is not None
                packageStr = str(ty.clas.id.packageId.name)
                prefixStr = "%d%s:%d%s" % (len(packageStr), packageStr, len(nameStr), nameStr)
            elif ty.clas.isLocal():
                prefixStr = ":%d%s" % (len(nameStr), nameStr)
            else:
                assert ty.clas.isBuiltin()
                prefixStr = "::%d%s" % (len(nameStr), nameStr)
            if len(ty.typeArguments) == 0:
                typeArgStr = ""
            else:
                typeArgParts = [mangleType(a, variables) for a in ty.typeArguments]
                typeArgStr = "[" + ",".join(typeArgParts) + "]"
            flagStr = "?" if ty.isNullable() else ""
            return code + prefixStr + typeArgStr + flagStr
        elif isinstance(ty, ir_types.VariableType):
            i = indexSame(variables, ty.typeParameter)
            assert i >= 0
            return "V%d%s" % (i, "?" if ty.isNullable() else "")
        else:
            assert isinstance(ty, ir_types.ExistentialType)
            allVars = variables + list(ty.variables)
            varParts = [mangleTypeParameter(p, allVars) for p in ty.variables]
            varStr = "[" + ",".join(varParts) + "]"
            tyStr = mangleType(ty.ty, allVars)
            return "E" + varStr + tyStr

    def mangleTypeParameter(typeParameter, variables):
        flagStr = "s" if flags.STATIC in typeParameter.flags else ""
        upperStr = mangleType(typeParameter.upperBound, variables)
        lowerStr = mangleType(typeParameter.lowerBound, variables)
        return "%s<%s>%s" % (flagStr, upperStr, lowerStr)

    if len(function.name.components) == 1:
        nameStr = "%d%s" % (len(function.name.components[0]), function.name.components[0])
    else:
        nameStr = "%s.%d%s" % (
            ".".join(function.name.components[:-1]),
            len(function.name.components[-1]),
            function.name.components[-1])
    if len(function.typeParameters) == 0:
        typeParamsStr = ""
    else:
        typeParamsParts = [mangleTypeParameter(tp, function.typeParameters)
                           for tp in function.typeParameters]
        typeParamsStr = "[" + ",".join(typeParamsParts) + "]"
    argsParts = [mangleType(pt, function.typeParameters) for pt in function.parameterTypes]
    argsStr = "(" + ",".join(argsParts) + ")"
    return nameStr + typeParamsStr + argsStr


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
    "ARRAY_LENGTH_SUFFIX",
    "EXISTENTIAL_SUFFIX",
    "BLANK_SUFFIX",
    "PackagePrefix",
    "PackageDependency",
    "Global",
    "Function",
    "Class",
    "Trait",
    "TypeParameter",
    "Variable",
    "Field",
    "LOCAL",
    "PARAMETER",
]

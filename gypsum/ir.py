# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.

import re
import StringIO


import ast
import builtins
import data
import flags
import ids
import ir_types
from name import Name
import bytecode
from utils import (
    each,
    flatMap,
    hashList,
    indexSame,
    oneOrNone,
    reprFormat,
)


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
        self.strings = []
        self.stringIndices = {}
        self.names = None
        self.nameIndices = None
        self.entryFunction = None
        self.initFunction = None
        self.typeParameters = []
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
        buf.write("entry function: %s\n" % self.entryFunction)
        buf.write("init function: %s\n" % self.initFunction)
        return buf.getvalue()

    def addGlobal(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.GLOBAL, len(self.globals))
        if self.names is not None:
            self.findOrAddName(name)
        g = Global(name, id, *args, **kwargs)
        if g.sourceName is not None:
            self.findOrAddString(g.sourceName)
        self.globals.append(g)
        return g

    def addFunction(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.FUNCTION, len(self.functions))
        if self.names is not None:
            self.findOrAddName(name)
        f = Function(name, id, *args, **kwargs)
        if f.sourceName is not None:
            self.findOrAddString(f.sourceName)
        self.functions.append(f)
        return f

    def addClass(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.CLASS, len(self.classes))
        if self.names is not None:
            self.findOrAddName(name)
        c = Class(name, id, *args, **kwargs)
        if c.sourceName is not None:
            self.findOrAddString(c.sourceName)
        self.classes.append(c)
        return c

    def addTrait(self, name, *args, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.TRAIT, len(self.traits))
        if self.names is not None:
            self.findOrAddName(name)
        t = Trait(name, id, *args, **kwargs)
        if t.sourceName is not None:
            self.findOrAddString(t.sourceName)
        self.traits.append(t)
        return t

    def addTypeParameter(self, defn, name, **kwargs):
        id = ids.DefnId(self.id, ids.DefnId.TYPE_PARAMETER, len(self.typeParameters))
        if self.names is not None:
            self.findOrAddName(name)
        if "index" not in kwargs and defn is not None:
            kwargs["index"] = len(defn.typeParameters)
        p = TypeParameter(name, id, **kwargs)
        if p.sourceName is not None:
            self.findOrAddString(p.sourceName)
        self.typeParameters.append(p)
        if defn is not None:
            defn.typeParameters.append(p)
            if isinstance(defn, ObjectTypeDefn):
                p.clas = defn
        return p

    def newField(self, name, **kwargs):
        if self.names is not None:
            self.findOrAddName(name)
        f = Field(name, **kwargs)
        if f.sourceName is not None:
            self.findOrAddString(f.sourceName)
        return f

    def addField(self, clas, name, **kwargs):
        f = self.newField(name, definingClass=clas, index=len(clas.fields), **kwargs)
        clas.fields.append(f)
        return f

    def newVariable(self, name, **kwargs):
        if self.names is not None:
            self.findOrAddName(name)
        v = Variable(name, **kwargs)
        if v.sourceName is not None:
            self.findOrAddString(v.sourceName)
        return v

    def addVariable(self, function, name, **kwargs):
        v = self.newVariable(name, **kwargs)
        function.variables.append(v)
        return v

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

        # Link function overrides.
        for f in self.functions:
            if f.overrides is not None:
                for i, override in enumerate(f.overrides):
                    if override.id.packageId is not self.id:
                        depIndex = override.id.packageId.index
                        externIndex = override.id.index
                        linkedOverride = \
                          self.dependencies[depIndex].linkedFunctions[externIndex]
                        f.overrides[i] = linkedOverride

    def findString(self, s):
        if isinstance(s, str):
            s = unicode(s)
        assert isinstance(s, unicode)
        return self.stringIndices.get(s)

    def findOrAddString(self, s):
        if isinstance(s, str):
            s = unicode(s)
        assert isinstance(s, unicode)
        index = self.findString(s)
        if index is None:
            index = len(self.strings)
            self.strings.append(s)
            self.stringIndices[s] = index
        return index

    def findName(self, name):
        assert self.names is not None
        return self.nameIndices.get(name)

    def findOrAddName(self, name):
        assert self.names is not None
        index = self.findName(name)
        if index is None:
            index = len(self.names)
            each(self.findOrAddString, name.components)
            self.names.append(name)
            self.nameIndices[name] = index
        return index

    def buildNameIndex(self):
        """Builds a table of names and a map from names to indices in that table.

        This is done after type declaration analysis, when functions are renamed using their
        type signatures.

        This is used by the serializer and by instructions (e.g., ldf, stf) that refer to
        definitions by name using the name's index.
        """
        assert self.names is None
        self.names = []
        self.nameIndices = {}

        def addName(defn):
            name = defn.name
            assert name is not None
            assert name not in self.nameIndices
            each(self.findOrAddString, name.components)
            self.nameIndices[name] = len(self.names)
            self.names.append(name)

        each(addName, self.globals)
        each(addName, self.functions)
        each(addName, self.classes)
        each(addName, flatMap(lambda c: c.fields, self.classes))
        each(addName, self.traits)
        each(addName, self.typeParameters)

    def findFunction(self, **kwargs):
        return oneOrNone(_findDefn(self.functions, kwargs))

    def findClass(self, **kwargs):
        return oneOrNone(_findDefn(self.classes, kwargs))

    def findTrait(self, **kwargs):
        return oneOrNone(_findDefn(self.traits, kwargs))

    def findGlobal(self, **kwargs):
        return oneOrNone(_findDefn(self.globals, kwargs))

    def findTypeParameter(self, **kwargs):
        return oneOrNone(_findDefn(self.typeParameters, kwargs))

    def findDependency(self, **kwargs):
        return oneOrNone(_findDefn(self.dependencies, kwargs))

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
    dependencySrc = "(%s)(?::(%s)?(?:(-)(%s)?)?)?" % \
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
        self.externMethods = []

    @staticmethod
    def fromString(s):
        m = PackageDependency.dependencyRex.match(s)
        if not m:
            raise ValueError("invalid package dependency: " + s)
        name = Name.fromString(m.group(1))
        minVersion = PackageVersion.fromString(m.group(2)) if m.group(2) else None
        dash = m.group(3) is not None
        if m.group(4) is not None:
            maxVersion = PackageVersion.fromString(m.group(4))
        elif dash:
            maxVersion = None
        else:
            maxVersion = minVersion
        return PackageDependency(name, minVersion, maxVersion)

    @staticmethod
    def fromPackage(package):
        dep = PackageDependency(package.name, package.version, None)
        dep.package = package
        return dep

    def dependencyString(self):
        minStr = str(self.minVersion) if self.minVersion is not None else ""
        maxStr = "-" + str(self.maxVersion) if self.maxVersion is not None else "-"
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

    def getSourceName(self):
        return self.sourceName if self.sourceName is not None else str(self.name)


class IrTopDefn(IrDefinition):
    def __init__(self, name, id, sourceName, astDefn):
        super(IrTopDefn, self).__init__(name, sourceName, astDefn)
        self.id = id
        self.isExternalized = False

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

    def findTypeParameter(self, **kwargs):
        return next(_findDefn(self.typeParameters, kwargs))


class Global(IrTopDefn):
    """Represents a global variable or constant.

    Attributes:
        name (Name): the name of the global
        id (DefnId): unique identifier for the global.
        sourceName (str?): name of the definition in source code
        astDefn (ast.Node?): the location in source code where the global is defined.
        astVarDefn (ast.Node?): the location in source code of the variable definition
            containing the field definition.
        type (Type?): the type of the global. May be `None` before type analysis.
        flags (frozenset[flag]): flags indicating how this global may be used. Valid flags are
            `EXTERN`, `LET`, `PUBLIC`.
    """

    def __init__(self, name, id, sourceName=None, astDefn=None, astVarDefn=None,
                 type=None, flags=frozenset()):
        super(Global, self).__init__(name, id, sourceName, astDefn)
        self.astVarDefn = astVarDefn
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
        overrides ([Function]?): methods in inherited traits or a base class that this
            method overrides. This must be set if the `METHOD` and `OVERRIDE` flags are set and
            the `STATIC`, `CONSTRUCTOR`, and `EXTERN` flags are not set. It must not be empty.
        flags (frozenset[flag]): a flags indicating how this function is used. Valid flags are
            `ABSTRACT`, `EXTERN`, `PUBLIC`, `PROTECTED`, `PRIVATE`, `STATIC`, `CONSTRUCTOR`,
            `METHOD`.
        definingClass (ObjectTypeDefn?): the class or trait this method (static or not) was
            defined in (as opposed to any other class that inherits the method). For
            non-static methods, this could be derived from the receiver type, but there's
            no easy way to get it for static methods.
        overriddenBy ({DefnId, Function}?): a map from class and trait ids to methods. Each
            entry describes an overriding function in a subclass or subtrait. This should only
            be set for non-constructor, non-static methods.
        insts (list[Instruction]?): a list of instructions to insert instead of calling this
            function. This is set for some (not all) builtin functions. For instance, the `+`
            method of `i64` has a list containing an `addi64` instruction.
        instTypes (list[Type]?): a list of types referenced by `insts`. Used for things like
            casting and pattern matching. Generated during semantic analysis along with
            `insts`. This does not affect the function's type signature.
        compileHint (symbol?): if set, the compiler will generate instructions for a specific
            kind of function (for example, an array getter) instead of generating instructions
            from a function body (which may not be present).
    """

    def __init__(self, name, id, sourceName=None, astDefn=None, returnType=None,
                 typeParameters=None, parameterTypes=None, variables=None, blocks=None,
                 overrides=None, flags=frozenset(), definingClass=None, overriddenBy=None,
                 insts=None, instTypes=None, compileHint=None):
        super(Function, self).__init__(name, id, sourceName, astDefn)
        self.returnType = returnType
        self.typeParameters = typeParameters
        self.parameterTypes = parameterTypes
        self.variables = variables
        self.blocks = blocks
        self.overrides = overrides
        self.flags = flags
        self.definingClass = definingClass
        self.insts = insts
        self.instTypes = instTypes
        self.overriddenBy = overriddenBy
        self.compileHint = compileHint

    def __repr__(self):
        return reprFormat(self, "name", "returnType", "typeParameters", "parameterTypes",
                          "variables", "blocks", "overrides", "flags")

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
               self.overrides == other.overrides and \
               self.flags == other.flags and \
               self.definingClass is other.definingClass and \
               self.insts == other.insts and \
               self.instTypes == other.instTypes and \
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

    def getOverriddenMethodIds(self):
        """Returns a set of function ids of the methods that this method overrides.

        The functions referenced by ids in the returned set do not override anything. If this
        is called on a method that doesn't override anything, a set containing the function's
        own id is returned.

        If a method is part of a chain of overrides (e.g., a overrides b overrides c), then
        only the top-most non-overriding id would be returned (c).

        Multiple ids may be returned if a method overrides different methods in different
        base definitions.

        This may only be called on functions with the `METHOD` flag. It may be called on
        methods with the `STATIC` flag.
        """
        assert flags.METHOD in self.flags
        if self.overrides is None:
            return set([self.id])
        else:
            allOverrideIds = set()
            for override in self.overrides:
                allOverrideIds.update(override.getOverriddenMethodIds())
            return allOverrideIds


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

    def findMethodBySourceName(self, sourceName):
        """Searches the method list for a non-static function with the given `sourceName`.
        Returns `None` if no such method is found."""
        assert isinstance(sourceName, str)
        for m in self.methods:
            if m.sourceName == sourceName and flags.STATIC not in m.flags:
                return m
        return None


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
            `None` before declaration analysis is complete. This list does not include
            inherited fields.
        methods (list[Function]?): a list of functions that operate on instances of this class.
            This may be `None` before declaration analysis is complete. This list does not
            include inherited methods.
        elementType (Type?): if this is an array class, this is the type of the elements. `None`
            for non-array classes. The `ARRAY` flag must be set if this is not `None`.
        flags (frozenset[flag]): flags indicating how this class is used. Valid flags are
            `ABSTRACT`, `ARRAY`, `EXTERN`, `PUBLIC`, `PROTECTED`, `PRIVATE`.
    """

    def __init__(self, name, id, sourceName=None, astDefn=None, typeParameters=None,
                 supertypes=None, initializer=None, constructors=None, fields=None,
                 methods=None, elementType=None, flags=frozenset()):
        super(Class, self).__init__(name, id, sourceName, astDefn, typeParameters, supertypes)
        self.initializer = initializer
        self.constructors = constructors
        self.fields = fields
        self.methods = methods
        self.elementType = elementType
        self.flags = flags

    def __repr__(self):
        return reprFormat(self, "name", "typeParameters", "supertypes", "initializer",
                          "constructors", "fields", "methods", "elementType", "flags")

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
        for f in self.fields:
            if name == f.sourceName:
                return f
        return None

    def getMember(self, name):
        method = self.findMethodBySourceName(name)
        if method is not None:
            return method
        return self.findFieldBySourceName(name)

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
            This list does not include inherited methods.
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


class TypeParameter(IrDefinition):
    """Parameterizes types used in a definition or existential type.

    A type parameter represents an "unknown" type, usually as part of a definition. For
    example, if we are defining a `List` class, we would define a type parameter which
    represents the type of elements in the list. Classes, traits, and functions are
    parameterized. Additionally, existential types can define type parameters for locally
    unknown types.

    Type parameters are referenced through `VariableType`s. They are only meaningful within
    the definitions that contain them.

    Each type parameter has an upper and lower bound. These are both non-nullable `ClassType`s
    or `VariableType`s. These bounds define the subtype relation for variable types. They
    must not cause the subtype graph to be cyclic, i.e., a type parameter may not be bounded
    by itself, directly or through a chain of other definitions.

    Note that `TypeParameter` is not a top-level definition. These objects are just values,
    and they may be copied or re-used as needed. Type parameters are not exported or linked.

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
        clas (ObjectTypeDefn?): the object type definition that defines this parameter. Used
            for checking variance.
        index (int?): an integer that distinguishes this type parameter from others in the
            same lexical context. Used for serialization, not otherwise meaningful.
    """

    def __init__(self, name, id, sourceName=None, astDefn=None,
                 upperBound=None, lowerBound=None, flags=frozenset(),
                 clas=None, index=None):
        super(TypeParameter, self).__init__(name, sourceName, astDefn)
        self.id = id
        self.upperBound = upperBound
        self.lowerBound = lowerBound
        self.flags = flags
        self.clas = clas
        self.index = index
        # HACK: in externalization, we can't skip a local type parameter because type parameters
        # don't have a concept of local or foreign. They can have cyclic references with other
        # type parameters (T <: S, S >: T), which causes infinite loops. Setting this field
        # helps us break out of that.
        self.isExternalized = False

    def __repr__(self):
        return reprFormat(self, "name", "upperBound", "lowerBound", "flags")

    def __str__(self):
        return "%s type %s <: %s >: %s" % \
            (" ".join(self.flags), self.name, self.upperBound, self.lowerBound)

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
    def __init__(self, name, sourceName=None, astDefn=None, astVarDefn=None,
                 type=None, kind=LOCAL, flags=frozenset()):
        super(Variable, self).__init__(name, sourceName, astDefn)
        self.astVarDefn = astVarDefn
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
        astVarDefn (ast.Node?): the location in source code of the variable definition
            containing the field definition.
        type (Type?): the type of the field. May be `None` before type analysis.
        flags (frozenset[flag]): flags indicating how this field is used. Valid flags are
            `LET`, `PUBLIC`, `PROTECTED`, `PRIVATE`, `STATIC`.
        definingClass (Class?): the class this field was defined in (as opposed to any other
            class that inherits this field).
        index (int?): an integer indicating where this field is located with an instance of
            its class. 0 is the first field, 1 is the second, etc. Used to generate load / store
            instructions. This will usually be `None` before semantic analysis.
    """

    def __init__(self, name, sourceName=None, astDefn=None, astVarDefn=None, type=None,
                 flags=frozenset(), definingClass=None, index=None):
        super(Field, self).__init__(name, sourceName, astDefn)
        self.astVarDefn = astVarDefn
        self.type = type
        self.flags = flags
        self.definingClass = definingClass
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
            if isinstance(irDefn.astDefn, ast.FunctionDefinition):
                # Local functions may still have implicit type parameters. We peek at the AST,
                # to count the explicit type parameters, which is kind of a hack. Works as long
                # as we can't import local functions from other packages, since we wouldn't
                # have an AST.
                return len(irDefn.astDefn.typeParameters)
            else:
                return 0
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
    in general).

    Arguments:
        irFunction (Function): the function being called.
        receiverType (Type?): the type of the receiver if this function is being called as
            a method.
        typeArgs ([Type]): a list of type arguments explicitly passed to the function.
            Does not include implicit type arguments from the receiver or the surrounding scope.
        argTypes ([Type]): a list of types of arguments explicitly passed to the function,
            not including the receiver type.
        importedTypeArgs ([Type]?): imported type arguments from the function's definition.

    Returns:
        ([Type], [Type])?: if the function is compatible, this returns a tuple containing the
        full list of type arguments and the full list of argument types. Otherwise, `None`
        is returned.
    """
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


def mangleFunctionShortName(function, package):
    """Returns a mangled name string which can be used when linking overloaded functions.

    This should be kept in sync with vm/src/index.cpp.
    """
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
                prefixStr = "%s:%s" % (packageStr, nameStr)
            elif ty.clas.isLocal():
                prefixStr = ":%s" % nameStr
            else:
                assert ty.clas.isBuiltin()
                prefixStr = "::%s" % nameStr
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

    shortName = function.name.short()
    nameStr = "%s" % shortName
    if len(function.typeParameters) == 0:
        typeParamsStr = ""
    else:
        typeParamsParts = [mangleTypeParameter(tp, function.typeParameters)
                           for tp in function.typeParameters]
        typeParamsStr = "[" + ",".join(typeParamsParts) + "]"
    if len(function.parameterTypes) == 0:
        argsStr = ""
    else:
        argsParts = [mangleType(pt, function.typeParameters) for pt in function.parameterTypes]
        argsStr = "(" + ",".join(argsParts) + ")"
    return nameStr + typeParamsStr + argsStr


def mangleFunctionName(function, package):
    components = function.name.components[:-1]
    components.append(mangleFunctionShortName(function, package))
    return Name(components)


def unmangleNameForTest(name):
    def unmangleComponent(component):
        if '$' in component:
            # Remove unique numbers from the end of internal names.
            numberIndex = component.rfind('_')
            if numberIndex >= 0:
                component = component[:numberIndex]

        paramTypesIndex = component.find('(')
        if paramTypesIndex == -1:
            paramTypesIndex = len(component)
        typeParamsIndex = component.find('[')
        if typeParamsIndex == -1:
            typeParamsIndex = len(component)
        endIndex = min(paramTypesIndex, typeParamsIndex)
        component = component[:endIndex]
        return component

    return Name(map(unmangleComponent, name.components))


def _findDefn(defns, kwargs):
    def matchItem(defn, key, value):
        if key == "name":
            if isinstance(value, str):
                name = Name.fromString(value)
            else:
                name = value
            return name == defn.name or \
                name == unmangleNameForTest(defn.name) or \
                (isinstance(value, str) and defn.sourceName == value)
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


__all__ = [
    "Package",
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

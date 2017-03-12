# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import ast
import builtins
import data
import errors
import ids
import ir
import ir_types
import location
from name import Name


CONTEXT_CONSTRUCTOR_HINT = "context-constructor-hint"
CLOSURE_CONSTRUCTOR_HINT = "closure-constructor-hint"
CLOSURE_CLASS_HINT = "closure-class-hint"
PACKAGE_INITIALIZER_HINT = "package-initializer-hint"
ARRAY_ELEMENT_GET_HINT = "array-element-get-hint"
ARRAY_ELEMENT_SET_HINT = "array-element-set-hint"
ARRAY_ELEMENT_LENGTH_HINT = "array-element-length-hint"

NORMAL_MODE = "normal-mode"
STD_MODE = "std-mode"
NOSTD_MODE = "nostd-mode"

STD_NAME = Name(["std"])

MAX_TUPLE_LENGTH = 10

class CompileInfo(object):
    """Contains state created and used by most compiler phases"""

    def __init__(self, ast_, package, packageLoader, isUsingStd=True):
        if isinstance(ast_, ast.Module):
            ast_ = ast.Package([ast_], location.NoLoc)
            ast_.id = ids.AstId(-1)
        assert package.id is ids.TARGET_PACKAGE_ID
        assert packageLoader is not None
        self.packageLoader = packageLoader
        self.packageNames = self.packageLoader.getPackageNames()
        self.ast = ast_
        self.package = package
        self.isUsingStd = isUsingStd
        self.scopes = {}  # keyed by ScopeId, AstId, and DefnId
        self.contextInfo = {}  # keyed by ScopeId
        self.closureInfo = {}  # keyed by ScopeId
        self.defnInfo = {}  # keyed by AstId
        self.useInfo = {}  # keyed by AstId
        self.typeInfo = {}  # keyed by AstId
        self.callInfo = {}  # keyed by AstId
        self.scopePrefixInfo = {}  # keyed by AstId
        self.stdExternInfo = {} # keyed by DefnId
        self.importInfo = {}  # keyed by AstId
        self.generatedNames = {}  # keyed by Name
        self.typeCheckFunction = None

    def languageMode(self):
        if self.isUsingStd:
            return NORMAL_MODE
        elif self.package.name == STD_NAME:
            return STD_MODE
        else:
            return NOSTD_MODE

    def getStdClass(self, name, loc):
        package = self._getStdPackage(loc)
        clas = package.findClass(name=name)
        if clas is None:
            raise errors.ScopeException(loc, "%s: class not found in std" % name)
        if self.languageMode() is NORMAL_MODE:
            self.setStdExternInfo(clas.id, clas)
            for ctor in clas.constructors:
                self.setStdExternInfo(ctor.id, ctor)
        return clas

    def getStdTrait(self, name, loc):
        package = self._getStdPackage(loc)
        trait = package.findTrait(name=name)
        if trait is None:
            raise errors.ScopeException(loc, "%s: trait not found in std" % name)
        if trait is not None and self.languageMode() is NORMAL_MODE:
            self.setStdExternInfo(trait.id, trait)
        return trait

    def getTupleClass(self, n, loc):
        # TODO(#33): remove this method when we have variadic type parameters.
        name = "Tuple%d" % n
        return self.getStdClass(name, loc)

    def getFunctionTrait(self, n, loc):
        # TODO(#33): remove this method when we have variadic type parameters.
        name = "Function%d" % n
        return self.getStdTrait(name, loc)

    def getFunctionTraitOrNone(self, n):
        # TODO(#33): remove this method when we have variadic type parameters.
        name = "Function%d" % n
        try:
            return self.getStdTrait(name, location.NoLoc)
        except errors.ScopeException:
            return None

    def makeUniqueName(self, name):
        assert '$' in name.short()
        if name not in self.generatedNames:
            self.generatedNames[name] = 1
        short = "%s_%d" % (name.short(), self.generatedNames[name])
        self.generatedNames[name] += 1
        return Name(name.components[:-1] + [short])

    def _getStdPackage(self, loc):
        langMode = self.languageMode()
        if langMode is NOSTD_MODE:
            raise errors.ScopeException(
                loc, "language feature not available without std library")
        elif langMode is NORMAL_MODE:
            return self.packageLoader.loadPackage(STD_NAME, loc)
        else:
            assert langMode is STD_MODE
            return self.package


_dictNames = [("Scope", "scopes", (ids.ScopeId, ids.AstId, ids.DefnId, ids.PackageId)),
              ("ContextInfo", "contextInfo", (ids.ScopeId,)),
              ("ClosureInfo", "closureInfo", (ids.ScopeId,)),
              ("DefnInfo", "defnInfo", (ids.AstId,)),
              ("UseInfo", "useInfo", (ids.AstId,)),
              ("Type", "typeInfo", (ids.AstId,)),
              ("CallInfo", "callInfo", (ids.AstId,)),
              ("ScopePrefixInfo", "scopePrefixInfo", (ids.AstId,)),
              ("StdExternInfo", "stdExternInfo", (ids.DefnId,)),
              ("ImportInfo", "importInfo", (ids.AstId,)),]

def _addDictMethods(elemName, dictName, types):
    def cleanKey(self, key):
        if isinstance(key, ast.Node):
            astId = key.id
            if ids.AstId in types:
                return astId
            elif ids.DefnId in types and astId in self.defnInfo:
                return self.defnInfo[astId][0].irDefn.id
            elif ids.ScopeId in types and astId in self.scopes:
                return self.scopes[astId][0].scopeId
            return astId
        elif isinstance(key, ir.IrDefinition):
            defnId = key.id
            if ids.AstId in types and key.astDefn is not None:
                return key.astDefn.id
            elif ids.DefnId in types:
                return defnId
            elif ids.ScopeId in types:
                if defnId in self.scopes:
                    return self.scopes[defnId][0].scopeId
                elif key.astDefn is not None and key.astDefn.id in self.scopes:
                    return self.scopes[key.astDefn.id][0].scopeId
            return defnId
        elif isinstance(key, ir.Package):
            packageId = key.id
            if ids.PackageId in types:
                return packageId
            elif ids.ScopeId in types:
                return self.scopes[packageId][0].scopeId
        return key

    def has(self, key):
        key = cleanKey(self, key)
        assert any(isinstance(key, type) for type in types)
        return key in getattr(self, dictName)
    setattr(CompileInfo, "has" + elemName, has)

    def get(self, key):
        key = cleanKey(self, key)
        assert any(isinstance(key, type) for type in types)
        values = getattr(self, dictName)[key]
        assert len(values) == 1
        return values[0]
    setattr(CompileInfo, "get" + elemName, get)

    def getAll(self, key):
        key = cleanKey(self, key)
        assert any(isinstance(key, type) for type in types)
        return getattr(self, dictName)[key]
    setattr(CompileInfo, "getAll" + elemName, getAll)

    def set(self, key, value):
        key = cleanKey(self, key)
        assert any(isinstance(key, type) for type in types)
        getattr(self, dictName)[key] = [value]
    setattr(CompileInfo, "set" + elemName, set)

    def add(self, key, value):
        key = cleanKey(self, key)
        assert any(isinstance(key, type) for type in types)
        table = getattr(self, dictName)
        if key not in table:
            table[key] = []
        table[key].append(value)
    setattr(CompileInfo, "add" + elemName, add)

    def iter(self):
        table = getattr(self, dictName)
        for vs in table.itervalues():
            for v in vs:
                yield v
    setattr(CompileInfo, "iter" + elemName, iter)

for _elemName, _dictName, _types in _dictNames:
    _addDictMethods(_elemName, _dictName, _types)


class ContextInfo(data.Data):
    """Created for every AST node which creates a scope.

    This includes classes and functions, but it also includes block expressions, lambda
    expressions, etc. It stores information about whether a context object is created for
    definitions in that scope and what context objects are used from parent scopes."""

    propertyNames = [
        # ScopeId: the scope this context info is about
        "id",

        # Class | None: a class which contains definitions made in this scope which may be used
        # by inner scopes (captured). This is None for functions unless definitions are actually
        # captured. For classes, this is the class definition itself.
        "irContextClass",
    ]

    def __init__(self, id, irContextClass=None):
        self.id = id
        self.irContextClass = irContextClass

    def __repr__(self):
        irContextClassStr = self.irContextClass.name if self.irContextClass else "None"
        return "ContextInfo(%s, %s)" % (self.id, irContextClassStr)


class ClosureInfo(data.Data):
    """Created for nodes which create a scope and use something defined in an outer scope.

    The use may be in an inner scope. Note that ClosureInfo is not created for local scopes."""

    propertyNames = [
        # Class | None: for classes, the IR class. For methods, the parent class. For functions
        # converted to closures, a closure class which contains references to captured
        # contexts.
        "irClosureClass",

        # {ScopeId -> Field | Variable | None}: for each outer scope containing a definition
        # used in this scope, this dictionary maps the outer scope id to a location where its
        # context may be loaded. Parent scopes are loaded out of a Field in irClosureClass.
        # Local function scopes are accessible with a Variable. Local class scopes are not
        # accessible, so None is stored for these.
        "irClosureContexts",

        # Variable | None: for functions converted to closures defined inside other functions,
        # a Variable in the parent function containing an instance of the closure class.
        "irClosureVar",
    ]

    def __init__(self, irClosureClass=None, irClosureContexts=None, irClosureVar=None):
        self.irClosureClass = irClosureClass
        self.irClosureContexts = irClosureContexts if irClosureContexts else {}
        self.irClosureVar = irClosureVar

    def __repr__(self):
        irClosureClassStr = self.irClosureClass.name if self.irClosureClass else "None"
        irClosureContextsStr = ", ".join("%s: %s" % kv
                                         for kv in self.irClosureContexts.iteritems())
        return "ClosureInfo(%s, {%s}, %s)" % \
            (irClosureClassStr, irClosureContextsStr, repr(self.irClosureVar))

    def capturedScopeIds(self):
        return sorted(scopeId
                      for scopeId, slot
                      in self.irClosureContexts.iteritems()
                      if isinstance(slot, ir.Field))


NOT_HERITABLE = -1

# DefnInfo is available for every AST node which defines something, such as classes, functions,
# parameters, and individual variables. This is used to map AST definitions to their
# IR definition stubs.
class DefnInfo(data.Data):
    """Created for every AST node which defines something.

    This includes classes, functions, and individual variables. This is used to map AST
    definitions to their IR definition stubs. This is also created for inherited definitions,
    so multiple DefnInfo objects can point to the same irDefn."""

    propertyNames = [
        # the IR definition created from this node
        "irDefn",

        # ScopeId: the id of the scope which contains this definition. If this definition is
        # inherited, this is the scope id of the inheriting class (the subclass).
        "scopeId",

        # bool: whether this definition can be accessed from an unrelated scope using a prefix.
        # For example, in a class, members are visible, but type parameters are not.
        "isVisible",

        # ScopeId: the id of the scope which contains this definition in source. If this definition
        # is inherited, this will be the id of the superclass containing the original
        # definition. If this definition is not inherited, this will be the same as `scopeId`.
        "inheritedScopeId",

        # int: how many scopes this definition was inherited through. 0 means this is the
        # original definition. NOT_HERITABLE (-1) indicates this is the original definition and
        # it is not heritable.
        "inheritanceDepth",

        # [Type]: a list of type arguments from the import statement. None for definitions
        # that weren't imported.
        "importedTypeArguments",
    ]

    def __init__(self, irDefn, scopeId, isVisible,
                 inheritedScopeId=None, inheritanceDepth=0, importedTypeArguments=None):
        if inheritedScopeId is None:
            inheritedScopeId = scopeId
        self.irDefn = irDefn
        self.scopeId = scopeId
        self.isVisible = isVisible
        self.inheritedScopeId = inheritedScopeId
        self.inheritanceDepth = inheritanceDepth
        self.importedTypeArguments = importedTypeArguments

    def __repr__(self):
        irDefnStr = repr(self.irDefn)
        return "DefnInfo(%s, %s, %s, %s, %d, %s)" % \
            (irDefnStr, self.scopeId, self.isVisible,
             self.inheritedScopeId, self.inheritanceDepth, self.importedTypeArguments)

    def isMethod(self):
        return isinstance(self.irDefn, Function) and self.irDefn.isMethod()

    def isHeritable(self):
        return self.inheritanceDepth != NOT_HERITABLE

    def inherit(self, scopeId):
        assert self.isHeritable() and self.isVisible
        return DefnInfo(self.irDefn, scopeId, self.isVisible,
                        self.inheritedScopeId, self.inheritanceDepth + 1)

    def importt(self, scopeId):
        assert self.isVisible and \
            (self.inheritanceDepth == NOT_HERITABLE or self.inheritanceDepth == 0)
        return DefnInfo(self.irDefn, scopeId, False, None, NOT_HERITABLE, None)


USE_AS_VALUE = "USE_AS_VALUE"
USE_AS_TYPE = "USE_AS_TYPE"
USE_AS_PROPERTY = "USE_AS_PROPERTY"
USE_AS_CONSTRUCTOR = "USE_AS_CONSTRUCTOR"


class UseInfo(data.Data):
    """Created for every AST node which refers to a definition using a symbol."""

    propertyNames = [
        # DefnInfo: Info about the definition being referenced.
        "defnInfo",

        # ScopeId: The scope id containing the symbol. If the symbol is used in a different
        # scope than the one it was defined, it may need to be captured.
        "useScopeId",

        # Describes how the definition is being used. This affects whether the definition is
        # captured.
        "kind",
    ]

    def shouldCapture(self, info):
        useScope = info.getScope(self.useScopeId)
        defnScope = info.getScope(self.defnInfo.scopeId)
        return self.kind is USE_AS_VALUE and \
               defnScope.requiresCapture() and \
               not useScope.isLocalWithin(defnScope)


class CallInfo(data.Data):
    """Defined at each call site during type/use analysis. Tracks information needed for the
    compiler to generate the call."""

    propertyNames = [
        # [Type]: A list of type arguments to be passed to the callee.
        "typeArguments",

        # Type?: The type of the receiver. This is only specified for non-static method calls.
        # TODO: this may be `None` for calls to closure functions, since they are regular
        # functions during type analysis. Closure conversion should add this.
        "receiverType",

        # bool: Whether the callee needs to be fully evaluated. This is true for things like
        # closures and bound methods. False for named functions and methods.
        "calleeIsValue",
    ]


class ScopePrefixInfo(data.Data):
    """Defined for variable and property expressions which are actually just scope names.
    This helps us access definitions inside packages later."""

    propertyNames = [
        # The package or class definition that defines the scope being referenced.
        "scopeDefn",

        # The id of the scope being referenced.
        "scopeId",
    ]


class ImportInfo(data.Data):
    """Defined for each import statement. Points to definitions that were imported."""

    propertyNames = [
        # [DefnInfo]: the definitions that were imported by this statement.
        "importedDefnInfos",
    ]



__all__ = [ "CompileInfo", "ContextInfo", "ClosureInfo", "DefnInfo", "UseInfo", "ImportInfo",
            "USE_AS_VALUE", "USE_AS_TYPE", "USE_AS_PROPERTY", "USE_AS_CONSTRUCTOR",
            "CONTEXT_CONSTRUCTOR_HINT", "CLOSURE_CONSTRUCTOR_HINT", "ARRAY_ELEMENT_GET_HINT",
            "ARRAY_ELEMENT_SET_HINT", "ARRAY_ELEMENT_LENGTH_HINT",
            "NORMAL_MODE", "STD_MODE", "NOSTD_MODE", "STD_NAME", "MAX_TUPLE_LENGTH",
            "NOT_HERITABLE"]

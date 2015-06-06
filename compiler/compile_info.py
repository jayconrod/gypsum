# Copyright 2014-2015, Jay Conrod. All rights reserved.
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


CONTEXT_CONSTRUCTOR_HINT = "context-constructor-hint"
CLOSURE_CONSTRUCTOR_HINT = "closure-constructor-hint"
CLOSURE_CLASS_HINT = "closure-class-hint"
PACKAGE_INITIALIZER_HINT = "package-initializer-hint"

NORMAL_MODE = "normal-mode"
STD_MODE = "std-mode"
NOSTD_MODE = "nostd-mode"

STD_NAME = ir.Name(["std"])

MAX_TUPLE_LENGTH = 2

class CompileInfo(object):
    """Contains state created and used by most compiler phases"""

    def __init__(self, ast_, package, packageLoader, isUsingStd=True):
        if isinstance(ast_, ast.AstModule):
            ast_ = ast.AstPackage([ast_], location.NoLoc)
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
        self.classInfo = {}  # keyed by DefnId
        self.typeInfo = {}  # keyed by AstId
        self.callInfo = {}  # keyed by AstId
        self.packageInfo = {}  # keyed by AstId
        self.stdExternInfo = {} # keyed by DefnId

    def languageMode(self):
        if self.isUsingStd:
            return NORMAL_MODE
        elif self.package.name == STD_NAME:
            return STD_MODE
        else:
            return NOSTD_MODE

    def getTupleClass(self, n, loc):
        langMode = self.languageMode()
        if langMode is NOSTD_MODE:
            raise errors.TypeException(loc, "tuples not supported without std library")
        elif langMode is NORMAL_MODE:
            package = self.packageLoader.loadPackage(STD_NAME, location.NoLoc)
        else:
            assert langMode is STD_MODE
            package = self.package

        tupleClassName = "Tuple%d" % n
        tupleClass = package.findClass(name=tupleClassName)

        if langMode is NORMAL_MODE:
            self.setStdExternInfo(tupleClass.id, tupleClass)
            ctor = tupleClass.constructors[0]
            self.setStdExternInfo(ctor.id, ctor)

        return tupleClass


_dictNames = [("Scope", "scopes", (ids.ScopeId, ids.AstId, ids.DefnId, ids.PackageId)),
              ("ContextInfo", "contextInfo", (ids.ScopeId,)),
              ("ClosureInfo", "closureInfo", (ids.ScopeId,)),
              ("DefnInfo", "defnInfo", (ids.AstId,)),
              ("UseInfo", "useInfo", (ids.AstId,)),
              ("ClassInfo", "classInfo", (ids.DefnId,)),
              ("Type", "typeInfo", (ids.AstId,)),
              ("CallInfo", "callInfo", (ids.AstId,)),
              ("PackageInfo", "packageInfo", (ids.AstId,)),
              ("StdExternInfo", "stdExternInfo", (ids.DefnId,))]

def _addDictMethods(elemName, dictName, types):
    def cleanKey(self, key):
        if isinstance(key, ast.AstNode):
            astId = key.id
            if ids.AstId in types:
                return astId
            elif ids.DefnId in types and astId in self.defnInfo:
                return self.defnInfo[astId].irDefn.id
            elif ids.ScopeId in types and astId in self.scopes:
                return self.scopes[astId].scopeId
            return astId
        elif isinstance(key, ir.IrDefinition):
            defnId = key.id
            if ids.DefnId in types:
                return defnId
            elif ids.ScopeId in types:
                if defnId in self.scopes:
                    return self.scopes[defnId].scopeId
                elif key.astDefn is not None and key.astDefn.id in self.scopes:
                    return self.scopes[key.astDefn.id].scopeId
            return defnId
        elif isinstance(key, ir.Package):
            packageId = key.id
            if ids.PackageId in types:
                return packageId
            elif ids.ScopeId in types:
                return self.scopes[packageId].scopeId
        return key

    def get(self, key):
        key = cleanKey(self, key)
        assert any(isinstance(key, type) for type in types)
        return getattr(self, dictName)[key]
    setattr(CompileInfo, "get" + elemName, get)

    def set(self, key, value):
        key = cleanKey(self, key)
        assert any(isinstance(key, type) for type in types)
        getattr(self, dictName)[key] = value
    setattr(CompileInfo, "set" + elemName, set)

    def has(self, key):
        key = cleanKey(self, key)
        assert any(isinstance(key, type) for type in types)
        return key in getattr(self, dictName)
    setattr(CompileInfo, "has" + elemName, has)

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
        return sorted(self.irClosureContexts.keys())


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

        # ScopeId: the id of the scope which contains this definition in source. If this definition
        # is inherited, this will be the id of the superclass containing the original
        # definition. If this definition is not inherited, this will be the same as `scopeId`.
        "inheritedScopeId",

        # int: how many scopes this definition was inherited through. 0 means this is the
        # original definition. NOT_HERITABLE (-1) indicates this is the original definition and
        # it is not heritable.
        "inheritanceDepth",
    ]

    def __init__(self, irDefn, scopeId, inheritedScopeId=None, inheritanceDepth=0):
        if inheritedScopeId is None:
            inheritedScopeId = scopeId
        self.irDefn = irDefn
        self.scopeId = scopeId
        self.inheritedScopeId = inheritedScopeId
        self.inheritanceDepth = inheritanceDepth

    def __repr__(self):
        irDefnStr = repr(self.irDefn)
        return "DefnInfo(%s, %s, %s, %d)" % \
            (irDefnStr, self.scopeId, self.inheritedScopeId, self.inheritanceDepth)

    def isMethod(self):
        return isinstance(self.irDefn, Function) and self.irDefn.isMethod()

    def inherit(self, scopeId):
        assert self.inheritanceDepth != NOT_HERITABLE
        return DefnInfo(self.irDefn, scopeId, self.inheritedScopeId, self.inheritanceDepth + 1)


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


class ClassInfo(data.Data):
    """Defined for each class. Keeps track of superclass."""

    propertyNames = [
        "irDefn",
        "superclassInfo",
    ]

    def __init__(self, irDefn, superclassInfo=None):
        self.irDefn = irDefn
        self.superclassInfo = superclassInfo

    def __repr__(self):
        irDefnStr = repr(self.irDefn)
        superclassInfoStr = self.superclassInfo.irDefn.name \
                            if self.superclassInfo is not None else "None"
        return "ClassInfo(%s, %s)" % (irDefnStr, superclassInfoStr)


class CallInfo(data.Data):
    """Defined at each call site during type/use analysis. Tracks information needed for the
    compiler to generate the call."""

    propertyNames = [
        # A list of type arguments to be passed to the callee.
        "typeArguments",

        # For property expressions, this is true if the receiver needs to be compiled. This is
        # false when the receiver expression is a package or namespace or when there is no
        # explicit receiver expression.
        "receiverExprNeeded",
    ]


class PackageInfo(data.Data):
    """Defined for variable and property expressions which are actually just package names.
    This helps us access definitions inside packages later."""

    propertyNames = [
        # The package being referenced.
        "package",

        # The PackageScope for the package being referenced.
        "scopeId",
    ]


__all__ = [ "CompileInfo", "ContextInfo", "ClosureInfo", "DefnInfo",
            "ClassInfo", "UseInfo",
            "USE_AS_VALUE", "USE_AS_TYPE", "USE_AS_PROPERTY", "USE_AS_CONSTRUCTOR",
            "CONTEXT_CONSTRUCTOR_HINT", "CLOSURE_CONSTRUCTOR_HINT",
            "NORMAL_MODE", "STD_MODE", "NOSTD_MODE", "STD_NAME", "MAX_TUPLE_LENGTH",
            "NOT_HERITABLE"]

# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from ast import *
from builtins import *
from data import *
import ir


BUILTIN_SCOPE_ID = -1
GLOBAL_SCOPE_ID = 0

CONTEXT_CONSTRUCTOR_HINT = "context-constructor-hint"
CLOSURE_CONSTRUCTOR_HINT = "closure-constructor-hint"
CLOSURE_CLASS_HINT = "closure-class-hint"


class CompileInfo(object):
    """Contains state created and used by most compiler phases"""

    def __init__(self, ast, package=None):
        if package is None:
            package = ir.Package()
        self.ast = ast
        self.package = package
        self.scopes = {}
        self.globalScope = None
        self.contextInfo = {}
        self.closureInfo = {}
        self.defnInfo = {}
        self.useInfo = {}
        self.classInfo = {}
        self.typeInfo = {}
        self.callInfo = {}

    def _get(self, key, dictionary):
        return dictionary[self._key(key)]

    def _set(self, key, value, dictionary):
        dictionary[self._key(key)] = value

    def _has(self, key, dictionary):
        return self._key(key) in dictionary

    def _key(self, k):
        if isinstance(k, int):
            return k
        elif isinstance(k, AstNode):
            return k.id
        elif hasattr(k, "astDefn"):
            return k.astDefn.id
        elif isBuiltinId(k.id) and isinstance(k, Function):
            # TODO: fix this hack. We add an offset to builtin function ids to disambiguate
            # them from class ids.
            return k.id - 100
        else:
            return k.id


_dictNames = [("Scope", "scopes"),
              ("ContextInfo", "contextInfo"),
              ("ClosureInfo", "closureInfo"),
              ("DefnInfo", "defnInfo"),
              ("UseInfo", "useInfo"),
              ("ClassInfo", "classInfo"),
              ("Type", "typeInfo"),
              ("CallInfo", "callInfo")]
def _addDictMethods(elemName, dictName):
    setattr(CompileInfo, "get" + elemName,
            lambda self, key: self._get(key, getattr(self, dictName)))
    setattr(CompileInfo, "set" + elemName,
            lambda self, key, value: self._set(key, value, getattr(self, dictName)))
    setattr(CompileInfo, "has" + elemName,
            lambda self, key: self._has(key, getattr(self, dictName)))
for _elemName, _dictName in _dictNames:
    _addDictMethods(_elemName, _dictName)


class ContextInfo(Data):
    """Created for every AST node which creates a scope.

    This includes classes and functions, but it also includes block expressions, lambda
    expressions, etc. It stores information about whether a context object is created for
    definitions in that scope and what context objects are used from parent scopes."""

    propertyNames = [
        # int: the AST id of the node that creates this scope.
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
        return "ContextInfo(%d, %s)" % (self.id, irContextClassStr)


class ClosureInfo(Data):
    """Created for nodes which create a scope and use something defined in an outer scope.

    The use may be in an inner scope. Note that ClosureInfo is not created for local scopes."""

    propertyNames = [
        # Class | None: for classes, the IR class. For methods, the parent class. For functions
        # converted to closures, a closure class which contains references to captured
        # contexts.
        "irClosureClass",

        # {ast id -> Field | Variable | None}: for each outer scope containing a definition
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
        irClosureContextsStr = ", ".join("%d: %s" % kv
                                         for kv in self.irClosureContexts.iteritems())
        return "ClosureInfo(%s, {%s}, %s)" % \
            (irClosureClassStr, irClosureContextsStr, repr(self.irClosureVar))

    def capturedScopeIds(self):
        return sorted(self.irClosureContexts.keys())


NOT_HERITABLE = -1

# DefnInfo is available for every AST node which defines something, such as classes, functions,
# parameters, and individual variables. This is used to map AST definitions to their
# IR definition stubs.
class DefnInfo(Data):
    """Created for every AST node which defines something.

    This includes classes, functions, and individual variables. This is used to map AST
    definitions to their IR definition stubs. This is also created for inherited definitions,
    so multiple DefnInfo objects can point to the same irDefn."""

    propertyNames = [
        # the IR definition created from this node
        "irDefn",

        # int: the id of the scope which contains this definition. If this definition is
        # inherited, this is the scope id of the inheriting class (the subclass).
        "scopeId",

        # int: the id of the scope which contains this definition in source. If this definition
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
        return "DefnInfo(%s, %d, %d, %d)" % \
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


class UseInfo(Data):
    """Created for every AST node which refers to a definition using a symbol."""

    propertyNames = [
        # Info about the definition being referenced.
        "defnInfo",

        # The scope id containing the symbol. If the symbol is used in a different scope than
        # the one it was defined, it may need to be captured.
        "useScopeId",

        # Describes how the definition is being used. This affects whether the definition is
        # captured.
        "kind",
    ]

    def shouldCapture(self, info):
        useScope = info.getScope(self.useScopeId)
        defnScope = info.getScope(self.defnInfo.scopeId)
        return self.kind is USE_AS_VALUE and \
               defnScope.scopeId != GLOBAL_SCOPE_ID and \
               defnScope.scopeId != BUILTIN_SCOPE_ID and \
               not useScope.isLocalWithin(defnScope)


class ClassInfo(Data):
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


class CallInfo(Data):
    """Defined at each call site during type/use analysis. Tracks information needed for the
    compiler to generate the call."""

    propertyNames = [
        # A list of type arguments to be passed to the callee.
        "typeArguments",
    ]


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
        if isinstance(receiverType, ObjectType):
            receiverType = receiverType.substituteForBaseClass(irFunction.clas)
        implicitTypeArgs = list(receiverType.getTypeArguments())
        allArgTypes = [receiverType] + argTypes
    else:
        implicitTypeParams = getImplicitTypeParameters(irFunction)
        implicitTypeArgs = [VariableType(t) for t in implicitTypeParams]
        allArgTypes = argTypes
    allTypeArgs = implicitTypeArgs + typeArgs

    if irFunction.canCallWith(allTypeArgs, allArgTypes):
        return (allTypeArgs, allArgTypes)
    else:
        return None


class InfoPrinter(AstPrinter):
    def __init__(self, out, info):
        super(InfoPrinter, self).__init__(out)
        self.info = info

    def visitDefault(self, node):
        super(InfoPrinter, self).visitDefault(node)
        indent = self.indentStr()
        infoStrs = []
        if self.info.hasScope(node):
            infoStrs.append(str(self.info.getScope(node)))
        if self.info.hasDefnInfo(node):
            infoStrs.append(str(self.info.getDefnInfo(node)))
        if self.info.hasContextInfo(node):
            infoStrs.append(str(self.info.getContextInfo(node)))
        if self.info.hasClosureInfo(node):
            infoStrs.append(str(self.info.getClosureInfo(node)))
        if self.info.hasUseInfo(node):
            infoStrs.append(str(self.info.getUseInfo(node)))
        if self.info.hasClassInfo(node):
            infoStrs.append(str(self.info.getClassInfo(node)))
        if self.info.hasType(node):
            infoStrs.append(str(self.info.getType(node)))

        for s in infoStrs:
            self.out.write("%s- %s\n" % (indent, s))

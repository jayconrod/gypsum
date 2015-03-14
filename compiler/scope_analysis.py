# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


# Scope analysis has two basic purposes:
#
# 1. Scan the AST for definitions and create stub IR definitions to be filled in later by
#    type analysis and semantic analysis.
# 2. Scan the AST for symbols and correlate them with the definitions they refer to.
#
# To accomplish this, we provide several passes (see doc strings for details):
# (performed after parsing)
# - analyzeDeclarations
# - analyzeInheritance
# - analyzeUses
# (performed after type analysis)
# - convertClosures
# - flattenClasses

import ast
from compile_info import ContextInfo, ClosureInfo, DefnInfo, ClassInfo, UseInfo, getAllArgumentTypes, USE_AS_VALUE, USE_AS_TYPE, USE_AS_PROPERTY, USE_AS_CONSTRUCTOR, CONTEXT_CONSTRUCTOR_HINT, CLOSURE_CONSTRUCTOR_HINT, NOT_HERITABLE
from data import Data
from errors import TypeException, ScopeException
from flags import *
from graph import Graph
from ids import DefnId, PackageId, ScopeId, BUILTIN_SCOPE_ID, GLOBAL_SCOPE_ID, PACKAGE_SCOPE_ID
import ir
from ir_types import getRootClassType, ClassType, UnitType
from location import Location, NoLoc
from builtins import registerBuiltins, getBuiltinClasses
from utils import Counter
from bytecode import BUILTIN_ROOT_CLASS_ID

def analyzeDeclarations(info):
    """Traverses the AST, creating a scope tree and DefnInfo for definition nodes.

    This is the first step of scope analysis. It traverses the AST recursively, performing
    the following tasks:
    - Create a Scope object for each node which creates a lexical scope. These objects can be
      accessed later using info.{has,get,set}Scope.
    - Create an IR definition and a DefnInfo object for each node which defines something.
      IR definitions are added to the package. DefnInfo can be accessed later using
      info.{get,set,has}DefnInfo.
    - Create a ClassInfo object for each node which defines a class. These can be accessed later
      using info.{get,set,has}ClassInfo."""
    packageScope = PackageScope(PACKAGE_SCOPE_ID, None, info, info.packageNames, [], None)
    builtinScope = BuiltinGlobalScope(packageScope)
    globalScope = GlobalScope(info.ast, builtinScope)
    info.setScope(info.ast.id, globalScope)

    visitor = DeclarationVisitor(globalScope)
    visitor.visitChildren(info.ast)


def analyzeInheritance(info):
    """Construct and analyze graph of inheritance between classes and type parameters.

    This pass visits classes and type parameters in the AST and links them with their base
    classes and bounds. It constructs a subtype graph and verifies there are no cycles.
    It also constructs an inheritance graph for classes only. This is used to copy symbol
    bindings from each class to its derived classes. At this point, overrides are treated as
    overloads, since we cannot distinguish between them until after type analysis."""

    # The inheritance graph is for classes only. The nodes are ScopeIds. If A is a subclass of
    # B, then A -> B will be an edge in the graph.
    inheritanceGraph = Graph()

    # The subtype graph is for all type definitions (classes and type parameters). The nodes
    # are DefnIds. The inheritance graph is a subgraph. Additional edges correspond to type
    # parameter bounds. Type arguments are ignored here.
    subtypeGraph = Graph()

    # Add edges for builtin classes. Still need to track these, even though they aren't in AST.
    def handleBuiltinInheritance(name, irDefn):
        if isinstance(irDefn, ir.Class):
            assert len(irDefn.supertypes) <= 1
            if len(irDefn.supertypes) == 1:
                classInfo = info.getClassInfo(irDefn)
                irSuperclass = irDefn.supertypes[0].clas
                classInfo.superclassInfo = info.getClassInfo(irSuperclass)
                classScopeId = info.getScope(irDefn.id).scopeId
                superclassScopeId = info.getScope(irSuperclass.id).scopeId
                inheritanceGraph.addEdge(classScopeId, superclassScopeId)
                subtypeGraph.addEdge(irDefn.id, irSuperclass.id)
        elif isinstance(irDefn, ir.TypeParameter):
            raise NotImplementedError
    registerBuiltins(handleBuiltinInheritance)

    # Populate the rest of the graph by traversing the AST.
    visitor = InheritanceVisitor(info.getScope(GLOBAL_SCOPE_ID), inheritanceGraph, subtypeGraph)
    visitor.visitChildren(info.ast)

    # Check for cycles.
    if subtypeGraph.isCyclic():
        # TODO: need to report an error for each cycle
        raise ScopeException(NoLoc, "inheritance cycle detected")

    # Copy bindings from superclasses to subclasses. This must be done in topological order
    # to ensure we don't miss anything, i.e., if S <: T, we must ensure all bindings have been
    # copied to T before copying from T to S. Builtin classes are already flattened, so this
    # is not necessary for them.
    topologicalClassIds = inheritanceGraph.reverseEdges().topologicalSort()
    for scopeId in topologicalClassIds:
        scope = info.getScope(scopeId)
        clas = scope.getIrDefn()
        classInfo = info.getClassInfo(clas)
        if clas.isBuiltin() or classInfo.superclassInfo is None:
            continue

        superclassScope = info.getScope(classInfo.superclassInfo.irDefn.id)
        for name, defnInfo in superclassScope.getBindings():
            if defnInfo.inheritanceDepth == NOT_HERITABLE:
                continue
            inheritedDefnInfo = defnInfo.inherit(scope.scopeId)
            if scope.isBound(name) and \
               not scope.getDefinition(name).isOverloadable(inheritedDefnInfo):
                raise ScopeException(defnInfo.irDefn.getLocation(),
                                     "%s: conflicts with inherited definition" % name)
            scope.bind(name, inheritedDefnInfo)
            scope.define(name)


def convertClosures(info):
    """Resolves references to free symbols by capturing their definitions in context and
    closure objects (defined below).

    High level example: `x` is defined in `f` but is free in `g`

        def f(x: i64) =
          def g = x + 2
          g()

    Terminology:
    - local: a relationship between two scopes. A scope is local inside another scope if
        both scopes come from the same definition, and definitions in the outer scope may
        be used without modification in the inner scope. Right now, this is only true for
        block expressions inside functions.
    - free symbol: a symbol used in a scope which is not local within the scope the symbo
        is defined in. In the example above, `x` is free in `g`, since it is defined in `f`.
    - captured definition: a definition which is referenced by a free symbol. In the example
        above, `x` is a captured definition.
    - context scope: a scope containing a captured definition
    - closure scope: a scope which uses a captured definition. Scopes between a context scope
        and a closure scope are also considered closure scopes even if they don't contain
        free symbols themselves.
    - context class: instances of a context class (context objects) store captured definitions.
        Context objects are created in context scopes and used in closure scopes.
    - closure class: instances of a closure class (closure objects) store context objects.
        Closure objects are created when a definition with a closure scope is declared.

    This pass eliminates references to free symbols by capturing their definitions. We
    search the use table for references to free symbols (see `UseInfo.shouldCapture`). For each
    of these references, we apply the algorithm below:

    1. Create a context class and capture the definition within it.
       a. If the context scope is a class scope, skip this step. The class itself acts as the
          context class, and all members are captured within it automatically.
       b. If the context scope is a function scope,
          i. If the captured definition is a function, convert it to a closure first. See below.
             After this point, any captured definition in a function starts off as a variable.
          ii. Create an empty context class and reference it in the scope's  `ContextInfo`.
          iii. Add a field to the context class, and change the captured definitions `DefnInfo`
               to reference the field. The old variable will be deleted.
          iv. Add a new variable to the function which contains the context scope. This will
              hold the context object.
    2. For each scope between the context scope and the closure scope, including the closure
       scope but not the context scope, convert the scope definition to a closure:
       a. Not supported for class scopes yet.
       b. For function scopes:
          i. Create an empty closure class with a single empty constructor. Reference the
             closure class in the scope's `ClosureInfo`.
          ii. Add the function as a method of the closure class. This adds a receiver parameter.
          iii. Add a field to the closure class for the context being captured.

    After this process is complete, a closure scope may access a captured definition by
    loading the appropriate context object from its closure object, then accessing the value
    in the context object. The example above becomes something like this:

        class Context-f
          var x: i64

        class Closure-g
          var context-f: Context-F

          def this(context-f: Context-F) =
            this.context-f = context-f

          def apply = this.context-f.x + 2

        def f(x: i64) =
          var context = f-context()
          context.x = x
          var closure-g = Closure-g(context)
          closure-g.apply()
    """
    # Do the actual closure conversion.
    for useInfo in info.useInfo.itervalues():
        if useInfo.shouldCapture(info):
            useScope = info.getScope(useInfo.useScopeId)
            useScope.capture(useInfo)

    # We are done modifying scopes, and we made a mess. Call finish on scopes in no
    # particular order.
    for scope in info.scopes.values():
        scope.finish()


def flattenClasses(info):
    """Copies inherited definitions from base classes to derived classes.

    Until this point, IR classes have only contained definitions defined inside them. Inherited
    definitions have been made available for the purpose of scope and type analysis through
    Scope objects. In order for the compiler to determine accurate object sizes and vtable
    indices, we need to actually copy these inherited definitions into derived classes."""

    # We cannot re-use the inheritance graph we built during inheritance analysis, since we may
    # have created new classes (especially contexts and closures) since then.
    allClasses = getBuiltinClasses(includePrimitives=False) + info.package.classes
    inheritanceGraph = Graph(clas.id for clas in allClasses)
    for clas in allClasses:
        for sty in clas.supertypes:
            # Edges point from base classes to derived classes. So the root class is a "source"
            # and leaf classes are "sinks".
            inheritanceGraph.addEdge(sty.clas.id, clas.id)

    topologicalClassIds = inheritanceGraph.topologicalSort()
    for id in topologicalClassIds:
        # TODO: this should already be done for builtin classes.
        if id.isBuiltin():
            continue
        irClass = info.package.classes[id.index]
        if len(irClass.supertypes) != 1:
            raise NotImplementedError
        irSuperclass = irClass.supertypes[0].clas
        irClass.fields = irSuperclass.fields + irClass.fields
        methods = list(irSuperclass.methods)
        for ownMethod in irClass.methods:
            if hasattr(ownMethod, "override"):
                foundOverride = False
                for i, inheritedMethod in enumerate(irSuperclass.methods):
                    if ownMethod.override is inheritedMethod:
                        foundOverride = True
                        methods[i] = ownMethod
                        break
                assert foundOverride
            else:
                methods.append(ownMethod)
        irClass.methods = methods
        if ABSTRACT not in irClass.flags:
            for m in methods:
                if ABSTRACT in m.flags:
                    raise ScopeException(irClass.getLocation(),
                                         "must override inherited abstract method `%s` in concrete class `%s`" %
                                         (m.name, irClass.name))


def isHeritable(irDefn):
    """Returns true if the given irDefn can be inherited from a base class by a
    deriving class."""
    if isinstance(irDefn, ir.Function) and irDefn.name == "$constructor":
        # Constructors are not heritable. At this time when this function is called, the
        # function may not have been made into a method yet, so Function.isConstructor would
        # return false. So we check the name instead, which is a hack.
        return False
    if isinstance(irDefn, ir.TypeParameter):
        return False
    return True


class NameInfo(object):
    def __init__(self, name):
        # A str for the name being tracked.
        self.name = name

        # A list of DefnInfo pairs that may be referenced by this symbol. In most cases, this
        # list will contain just one element.
        self.overloads = []

        # A map from function ids of overriden functions to ids of the overriding functions.
        # Only defined after `resolveOverrides` is called and only if `isOverloaded` is true.
        self.overrides = None

    def addOverload(self, defnInfo):
        self.overloads.append(defnInfo)

    def isOverloadable(self, otherDefnInfo):
        return isinstance(otherDefnInfo.irDefn, ir.Function) and \
               isinstance(self.overloads[0].irDefn, ir.Function)

    def isOverloaded(self):
        return len(self.overloads) > 1

    def getDefnInfo(self):
        assert not self.isOverloaded()
        return self.overloads[0]

    def iterOverloads(self):
        return iter(self.overloads)

    def isClass(self):
        return not self.isOverloaded() and isinstance(self.getDefnInfo().irDefn, ir.Class)

    def isPackagePrefix(self):
        return not self.isOverloaded() and \
               isinstance(self.getDefnInfo().irDefn, ir.PackagePrefix)

    def isPackage(self):
        return not self.isOverloaded() and \
               isinstance(self.getDefnInfo().irDefn, ir.Package)

    def getInfoForConstructors(self, info):
        assert self.isClass()
        irClass = self.getDefnInfo().irDefn
        classScope = info.getScope(irClass)
        ctorNameInfo = classScope.getDefinition("$constructor")
        assert ctorNameInfo is not None
        return ctorNameInfo

    def didResolveOverrides(self):
        return self.overrides is not None

    def resolveOverrides(self):
        """Decides which definitions are overloads and which are overrides.

        Called during or after type analysis so we know the return and parameter types of each
        definition. Compares each definition to other definitions. Function A is considered to
        override function B if all of the following are true:

        - A and B have the same number of parameters
        - Each parameter type of A is a supertype of the corresponding parameter type of B
        - The return type of A is a subtype of the return type of B
        - The inheritance depth of A is strictly less than B
        - There is no other function A' with inheritance depth between A and B that satisfies
          the above criteria.

        The overriding function (A) will have its `override` attribute set to point to the
        overriden function (B). `overrides` will have an entry added."""
        if self.didResolveOverrides():
            return
        self.overrides = {}
        if not self.isOverloaded():
            return

        # Sort overloads by depth to simplify the loop and avoid the last condition
        # mentioned above.
        overloadsByDepth = sorted(self.overloads,
                                  key=lambda defnInfo: defnInfo.inheritanceDepth)

        # Compare each function with each other function with greater depth.
        for (aIndex, aDefnInfo) in enumerate(overloadsByDepth):
            aDepth = aDefnInfo.inheritanceDepth
            overrideIndex = None
            for (bIndex, bDefnInfo) in enumerate(overloadsByDepth[aIndex + 1:]):
                bDepth = bDefnInfo.inheritanceDepth
                bIndex += aIndex + 1
                assert aDepth <= bDepth
                if aDepth == bDepth:
                    continue

                aIrDefn = aDefnInfo.irDefn
                bIrDefn = bDefnInfo.irDefn
                assert isinstance(aIrDefn, ir.Function) and isinstance(bIrDefn, ir.Function)
                if aIrDefn.mayOverride(bIrDefn):
                    # Check that nothing else is already overriding this function.
                    if bIrDefn.id in self.overrides:
                        raise TypeException(aIrDefn.getLocation(),
                                            "multiple definitions may override: %s" %
                                            self.name)

                    aIrDefn.override = bIrDefn
                    self.overrides[bIrDefn.id] = aIrDefn.id
                    overrideIndex = bIndex
                    break

            # If we found an override, check that there aren't other functions we could
            # override.
            if overrideIndex is not None:
                overrideDepth = overloadsByDepth[overrideIndex].inheritanceDepth
                for bDefnInfo in overloadsByDepth[overrideIndex + 1:]:
                    bDepth = bDefnInfo.inheritanceDepth
                    if bDepth > overrideDepth:
                        break
                    if aDefnInfo.irDefn.mayOverride(bDefnInfo.irDefn):
                        raise TypeException(aDefnInfo.irDefn.getLocation(),
                                            "may override multiple definitions: %s" %
                                            self.name)

    def findDefnInfoWithArgTypes(self, receiverType, receiverIsExplicit,
                                 typeArgs, argTypes, loc):
        """Determines which overloaded or overriding function should be called, based on
        argument types.

        This is safe to call on any NameInfo, even if it doesn't refer to a function. If there
        is exactly one match, returns (DefnInfo, list(Type), list(Type)) containing the matched
        definition, the full list of type arguments, and the full list of argument types. If
        there are zero or multiple matches, raises ScopeException."""
        self.resolveOverrides()
        candidate = None
        for defnInfo in self.overloads:
            irDefn = defnInfo.irDefn
            if isinstance(irDefn, ir.Function) and irDefn.id in self.overrides:
                continue

            if not isinstance(irDefn, ir.Function) and \
               len(typeArgs) == 0 and len(argTypes) == 0:
                # Non-function
                typesAndArgs = (None, None)
                match = True
            elif not receiverIsExplicit and \
                 isinstance(irDefn, ir.Function) and \
                 not irDefn.isMethod():
                # Regular function
                typesAndArgs = getAllArgumentTypes(irDefn, None, typeArgs, argTypes)
                match = typesAndArgs is not None
            elif isinstance(irDefn, ir.Function) and \
                 irDefn.isMethod():
                # Method call
                typesAndArgs = getAllArgumentTypes(irDefn, receiverType, typeArgs, argTypes)
                match = typesAndArgs is not None
            else:
                match = False

            if match:
                if candidate is not None:
                    raise TypeException(loc, "ambiguous call to overloaded function: %s" % \
                                        self.name)
                allTypeArgs, allArgTypes = typesAndArgs
                candidate = (defnInfo, allTypeArgs, allArgTypes)

        if candidate is None:
            raise TypeException(loc, "could not find compatible definition: %s" % self.name)
        return candidate


class Scope(ast.AstNodeVisitor):
    def __init__(self, ast, scopeId, parent, info):
        self.scopeId = scopeId
        self.ast = ast
        self.parent = parent
        self.info = info
        self.bindings = {}
        self.defined = set()
        self.childScopes = {}
        info.setScope(self.scopeId, self)
        if self.ast is not None:
            info.setScope(ast.id, self)
        info.setContextInfo(self.scopeId, ContextInfo(self.scopeId))
        if not self.isLocal() and \
           self.scopeId is not GLOBAL_SCOPE_ID and \
           not info.hasClosureInfo(scopeId):
            closureInfo = ClosureInfo()
            self.info.setClosureInfo(scopeId, closureInfo)

    def __str__(self):
        return self.__class__.__name__

    def getDefnInfo(self):
        scope = self.topLocalScope()
        return self.info.getDefnInfo(scope.ast)

    def getAstDefn(self):
        scope = self.topLocalScope()
        assert isinstance(scope.ast, ast.AstDefinition)
        return scope.ast

    def getIrDefn(self):
        return self.getDefnInfo().irDefn

    def declare(self, astDefn, astVarDefn=None):
        """Creates an IR definition to the package, adds it to the package, and
        adds DefinitionInfo."""
        # Create the definition, and add it to the package.
        irDefn = self.createIrDefn(astDefn, astVarDefn)
        if irDefn is None:
            return

        # Connect it with the AST so we can find it again.
        if astVarDefn is not None:
            irDefn.astVarDefn = astVarDefn
        inheritanceDepth = 0 if isHeritable(irDefn) else NOT_HERITABLE
        defnInfo = DefnInfo(irDefn, self.scopeId, self.scopeId, inheritanceDepth)
        self.info.setDefnInfo(astDefn, defnInfo)

        # If the definition has a user-specified name, bind it in this scope.
        name = irDefn.name
        if self.isBound(name) and not self.bindings[name].isOverloadable(defnInfo):
            raise ScopeException(astDefn.location, "%s: already declared" % name)
        self.bind(name, defnInfo)
        if self.isDefinedAutomatically(astDefn):
            self.define(name)

        # Associate the definition with this scope. Type/use analysis doesn't traverse the
        # AST in order, so this is important for finding the correct scope.
        self.info.setScope(astDefn, self)
        if astVarDefn is not None:
            if not self.info.hasScope(astVarDefn):
                self.info.setScope(astVarDefn, self)
            else:
                assert self.info.getScope(astVarDefn) is self

    def bind(self, name, defnInfo):
        """Binds a name in this scope.

        For non-function defintions, this requires the name is not already bound. For function
        definitions, multiple functions may share the same name. They are added to
        OverloadInfo."""
        if name not in self.bindings:
            self.bindings[name] = NameInfo(name)
        self.bindings[name].addOverload(defnInfo)

    def getBindings(self):
        """Returns a iterator, which returns (str, DefnInfo, int) pairs for each binding.

        The str is the name, and the int indicates how many classes the definition was inherited
        through (0 indicates it was inherited from this class). For overloaded functions, this
        will return the same name more than once."""
        for name, nameInfo in self.bindings.iteritems():
            for defnInfo in nameInfo.overloads:
                yield (name, defnInfo)

    def createIrDefn(self, astDefn, astVarDefn):
        """Creates an IR definition and adds it to the package."""
        raise NotImplementedError

    def getImplicitTypeParameters(self):
        """Returns a list of type parameters implied by this scope and outer scopes. These
        can be used when declaring or calling functions or classes. Outer-most type parameters
        are listed first."""
        if self.scopeId in [BUILTIN_SCOPE_ID, GLOBAL_SCOPE_ID, PACKAGE_SCOPE_ID]:
            return []
        else:
            return list(self.getIrDefn().typeParameters)

    def createIrClassDefn(self, astDefn):
        """Convenience method for creating a class definition."""
        implicitTypeParams = self.getImplicitTypeParameters()
        flags = getFlagsFromAstDefn(astDefn, None)
        checkFlags(flags, frozenset([ABSTRACT]), astDefn.location)
        irDefn = self.info.package.addClass(astDefn.name, astDefn, implicitTypeParams,
                                            None, None, [], [], [], flags)

        irInitializer = self.info.package.addFunction("$initializer", astDefn,
                                                      None, list(implicitTypeParams),
                                                      None, [], None, frozenset())
        self.makeMethod(irInitializer, irDefn)
        irDefn.initializer = irInitializer

        if not astDefn.hasConstructors():
            irDefaultCtor = self.info.package.addFunction("$constructor", astDefn,
                                                          None, list(implicitTypeParams),
                                                          None, [], None, frozenset())
            self.makeMethod(irDefaultCtor, irDefn)
            irDefn.constructors.append(irDefaultCtor)
        classInfo = ClassInfo(irDefn)
        self.info.setClassInfo(irDefn, classInfo)
        return irDefn

    def makeMethod(self, function, clas):
        """Convenience method which adds a "this" parameter and sets the "clas" attrib.

        Note that this does not add the function to the class's methods or constructors lists.
        """
        function.clas = clas
        this = ir.Variable("$this", function.astDefn, None, ir.PARAMETER, frozenset([LET]))
        function.variables.insert(0, this)

    def isDefinedAutomatically(self, astDefn):
        """Returns true if a definition is available as soon as the scope is entered.

        This is only false for local variables in function scopes."""
        raise NotImplementedError

    def lookup(self, name, loc, localOnly=False, mayBeAssignment=False, ignoreDefnOrder=False):
        """Resolves a reference to a symbol, possibly in a parent scope.

        Returns NameInfo. For overloaded symbols, there may be several functions in there."""
        defnScope = self
        while defnScope is not None and \
              not defnScope.isBound(name) and \
              (not localOnly or self.isLocalWithin(defnScope)):
            defnScope = defnScope.parent
        if defnScope is None or (localOnly and not self.isLocalWithin(defnScope)):
            if mayBeAssignment and name.endswith("=") and name != "==":
                return self.lookup(name[:-1], loc, localOnly=localOnly,
                                   mayBeAssignment=False, ignoreDefnOrder=ignoreDefnOrder)
            else:
                raise ScopeException(loc, "%s: not found" % name)
        if not ignoreDefnOrder and \
           not defnScope.isDefined(name) and \
           self.isLocalWithin(defnScope):
            raise ScopeException(loc, "%s: used before being defined" % name)
        return defnScope.getDefinition(name)

    def isBound(self, name):
        """Returns true if a symbol is defined in this scope."""
        return self.getDefinition(name) is not None

    def getDefinition(self, name):
        """Returns NameInfo for a symbol defined in this scope or None."""
        return self.bindings.get(name)

    def isDefined(self, name):
        """Returns whether a symbol has been defined yet."""
        return name in self.defined

    def define(self, name):
        self.defined.add(name)

    def use(self, defnInfo, useAstId, useKind, loc):
        """Creates, registers, and returns UseInfo for a given definition.

        Also checks whether the definition is allowed to be used in this scope and raises an
        Exception if not."""
        irDefn = defnInfo.irDefn
        if isinstance(irDefn, ir.Package):
            assert useKind in [USE_AS_VALUE, USE_AS_PROPERTY]
            self.info.package.addDependency(irDefn)

        if isinstance(irDefn, ir.IrTopDefn) and \
           irDefn.isForeign() and \
           not EXTERN in irDefn.flags:
            irDefn = defnInfo.irDefn = self.info.package.externalize(irDefn)

        if hasattr(irDefn, "flags") and \
           ((PRIVATE in irDefn.flags and \
             not self.isWithin(defnInfo.inheritedScopeId)) or \
            (PROTECTED in irDefn.flags and \
             not self.isWithin(defnInfo.scopeId))):
            raise ScopeException(loc, "%s: not allowed to be used in this scope" %
                                 irDefn.name)

        if useKind is USE_AS_CONSTRUCTOR and \
           ABSTRACT in irDefn.clas.flags:
            raise ScopeException(loc, "%s: cannot instantiate abstract class" %
                                 irDefn.clas.name)

        useInfo = UseInfo(defnInfo, self.scopeId, useKind)
        self.info.setUseInfo(useAstId, useInfo)
        return useInfo

    def resolveOverrides(self):
        """For bindings with multiple definitions, resolves which definitions are overloads and
        which are overriden (and by which others).

        All of the overloaded definitions must have type information before this is called."""
        for nameInfo in self.bindings.values():
            nameInfo.resolveOverrides()

    def isLocal(self):
        """Returns true if values defined in the parent scope are accessible in this scope."""
        return isinstance(self.ast, ast.AstBlockExpression) or \
               isinstance(self.ast, ast.AstPartialFunctionCase)

    def isLocalWithin(self, defnScope):
        """Returns true if values defined in defnScope may be accessed directly in this scope."""
        current = self
        while True:
            if current is defnScope:
                return True
            if not current.isLocal():
                return False
            current = current.parent

    def isWithin(self, scopeOrId):
        id = scopeOrId.scopeId if isinstance(scopeOrId, Scope) else scopeOrId
        current = self
        while current is not None and current.scopeId != id:
            current = current.parent
        return current is not None

    def topLocalScope(self, defnScope=None):
        """Returns the top-most local scope which is still below defnScope."""
        scope = self
        while scope != defnScope and scope.isLocal():
            scope = scope.parent
        return scope

    def findEnclosingClass(self):
        """Returns the IR class enclosing this scope, if there is one. If this is a class scope,
        this will return the class itself."""
        raise NotImplementedError

    def capture(self, useInfo):
        """Makes the named non-local value defined in defnScope accessible in this scope.

        If the value is defined in a scope without a context, a context is created for that
        scope. The context is made available in this scope and all top scopes in between."""
        defnInfo = useInfo.defnInfo
        defnScope = self.info.getScope(defnInfo.scopeId)
        useScope = self.topLocalScope(defnScope)
        if defnScope is useScope:
            # Base case: need to capture the definition in a context.
            defnScope.captureScopeContext()
            irCtxClass = self.info.getContextInfo(defnScope.scopeId).irContextClass
            defnScope.captureInContext(defnInfo, irCtxClass)
        else:
            # Recursive case: make sure the variable is captured and context is accessible in
            # parent scope. Then make it accessible in this scope.
            useScope.parent.capture(useInfo)
            useScope.makeClosure()
            useScope.closureCaptureContext(defnScope.scopeId)

    def requiresCapture(self):
        """Returns True if definitions in this scope must be captured to be available in other
        scopes, for example for function and class scopes. Returns False if those definitions
        are available in all scopes, for example for global, builtin, and package scopes."""
        raise NotImplementedError

    def captureScopeContext(self):
        """Makes this scope available for capturing.

        For functions, this creates an empty context class and a variable containing an in
        instance of that class (unless they have already been created). Nothing is done for
        classes since instances can already be used as contexts."""
        raise NotImplementedError

    def captureInContext(self, defnInfo, irContextClass):
        """Stores the given definition in the given context class.

        Returns the field storing the definition in the context class."""
        raise NotImplementedError

    def makeClosure(self):
        """Ensures this scope can store contexts from parent scopes."""
        raise NotImplementedError

    def closureCaptureContext(self, scopeId):
        """Ensures a closure class captures a specific context from a parent scope.

        Returns the field containing the captured context."""
        assert not self.isLocal()
        closureInfo = self.info.getClosureInfo(self.scopeId)
        if scopeId not in closureInfo.irClosureContexts:
            irClosureClass = closureInfo.irClosureClass
            irContextClass = self.info.getContextInfo(scopeId).irContextClass
            irContextType = ClassType(irContextClass)
            irContextField = self.info.package.newField("$context", None,
                                                        irContextType, frozenset())
            irClosureClass.fields.append(irContextField)
            irClosureClass.constructors[0].parameterTypes.append(irContextType)
            closureInfo.irClosureContexts[scopeId] = irContextField

    def localScope(self, ast):
        return self.getOrCreateScope(ast, self.newLocalScope)

    def newLocalScope(self, ast):
        """Creates a new scope for block expressions which may introduce definitions

        Must be implemented by subclasses.
        """
        raise NotImplementedError

    def scopeForFunction(self, ast):
        return self.getOrCreateScope(ast, self.newScopeForFunction)

    def newScopeForFunction(self, ast):
        """Creates a new scope for function definitions.

        Must be implemented by subclasses.
        """
        raise NotImplementedError

    def scopeForClass(self, ast):
        return self.getOrCreateScope(ast, self.newScopeForClass)

    def newScopeForClass(self, ast):
        """Creates a new scope for class definition.

        Must be implemented by subclasses.
        """
        raise NotImplementedError

    def getOrCreateScope(self, ast, create):
        if ast.id in self.childScopes:
            return self.childScopes[ast.id]
        else:
            scope = create(ast)
            self.childScopes[ast.id] = scope
            return scope

    def finish(self):
        """Adds final information to the scope's definition.

        May be overridden by subclasses.
        """
        pass


class GlobalScope(Scope):
    def __init__(self, astModule, parent):
        super(GlobalScope, self).__init__(astModule, GLOBAL_SCOPE_ID, parent, parent.info)

    def createIrDefn(self, astDefn, astVarDefn):
        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        if isinstance(astDefn, ast.AstVariablePattern):
            checkFlags(flags, frozenset([LET, PUBLIC, PROTECTED]), astDefn.location)
            irDefn = self.info.package.addGlobal(astDefn.name, astDefn, None, flags)
        elif isinstance(astDefn, ast.AstFunctionDefinition):
            checkFlags(flags, frozenset(), astDefn.location)
            if astDefn.body is None:
                raise ScopeException(astDefn.location,
                                     "%s: global function must have body" % astDefn.name)
            irDefn = self.info.package.addFunction(astDefn.name, astDefn,
                                                   None, self.getImplicitTypeParameters(),
                                                   None, [], None, flags)
            if astDefn.name == "main":
                assert self.info.package.entryFunction is None
                self.info.package.entryFunction = irDefn.id
        elif isinstance(astDefn, ast.AstClassDefinition):
            irDefn = self.createIrClassDefn(astDefn)
        else:
            raise NotImplementedError
        return irDefn

    def isDefinedAutomatically(self, astDefn):
        return True

    def findEnclosingClass(self):
        return None

    def requiresCapture(self):
        return False

    def captureScopeContext(self):
        raise NotImplementedError("global scope does not need a context")

    def captureInContext(self, irDefn, irContextClass):
        raise NotImplementedError("global definitions can't be captured")

    def makeClosure(self):
        raise NotImplementedError("global scopes can't capture anything")

    def newLocalScope(self, ast):
        raise NotImplementedError("global scopes can't have local contexts")

    def newScopeForFunction(self, ast):
        return FunctionScope(ast, self)

    def newScopeForClass(self, ast):
        return ClassScope(ast, self)


class FunctionScope(Scope):
    def __init__(self, ast, parent):
        super(FunctionScope, self).__init__(ast, ScopeId(ast.id), parent, parent.info)
        self.info.setScope(self.getIrDefn().id, self)

    def configureAsMethod(self, astClassScopeId, irClassDefn):
        defnInfo = self.getDefnInfo()
        irMethod = defnInfo.irDefn
        self.makeMethod(irMethod, irClassDefn)
        this = irMethod.variables[0]
        self.bind("this", DefnInfo(this, self.scopeId))
        self.define("this")
        closureInfo = self.info.getClosureInfo(self.scopeId)
        closureInfo.irClosureClass = irClassDefn
        closureInfo.irClosureContexts[astClassScopeId] = this

    def isMethod(self):
        irFunction = self.getIrDefn()
        return hasattr(irFunction, "clas")

    def createIrDefn(self, astDefn, astVarDefn):
        irScopeDefn = self.getIrDefn()
        if isinstance(irScopeDefn, ir.Class):
            irScopeDefn = irScopeDefn.initializer
        assert isinstance(irScopeDefn, ir.Function)

        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        if isinstance(astDefn, ast.AstTypeParameter):
            checkFlags(flags, frozenset([STATIC]), astDefn.location)
            if STATIC not in flags:
                raise NotImplementedError
            irDefn = self.info.package.addTypeParameter(astDefn.name, astDefn,
                                                        None, None, flags)
            irScopeDefn.typeParameters.append(irDefn)
        elif isinstance(astDefn, ast.AstParameter):
            checkFlags(flags, frozenset(), astDefn.location)
            if isinstance(astDefn.pattern, ast.AstVariablePattern):
                # If the parameter is a simple variable which doesn't need unpacking, we don't
                # need to create a separate definition here.
                irDefn = None
            else:
                irDefn = ir.Variable("$parameter", None, ir.PARAMETER, flags)
                irScopeDefn.variables.append(irDefn)
        elif isinstance(astDefn, ast.AstVariablePattern):
            checkFlags(flags, frozenset([LET]), astDefn.location)
            kind = ir.PARAMETER if isinstance(astVarDefn, ast.AstParameter) else ir.LOCAL
            irDefn = ir.Variable(astDefn.name, astDefn, None, kind, flags)
            irScopeDefn.variables.append(irDefn)
        elif isinstance(astDefn, ast.AstFunctionDefinition):
            checkFlags(flags, frozenset(), astDefn.location)
            if astDefn.body is None:
                raise ScopeException(astDefn.location,
                                     "%s: function must have body" % astDefn.name)
            implicitTypeParams = self.getImplicitTypeParameters()
            irDefn = self.info.package.addFunction(astDefn.name, astDefn,
                                                   None, implicitTypeParams,
                                                   None, [], None, flags)
        elif isinstance(astDefn, ast.AstClassDefinition):
            irDefn = self.createIrClassDefn(astDefn)
        else:
            raise NotImplementedError
        return irDefn

    def isDefinedAutomatically(self, astDefn):
        return isinstance(astDefn, ast.AstClassDefinition) or \
               isinstance(astDefn, ast.AstFunctionDefinition)

    def findEnclosingClass(self):
        return self.parent.findEnclosingClass()

    def requiresCapture(self):
        return True

    def captureScopeContext(self):
        contextInfo = self.info.getContextInfo(self.scopeId)
        if contextInfo.irContextClass is not None:
            return

        # Create the context class.
        implicitTypeParams = self.getImplicitTypeParameters()
        irContextClass = self.info.package.addClass("$context", None, list(implicitTypeParams),
                                                    [getRootClassType()], None,
                                                    [], [], [], frozenset())
        irContextType = ClassType(irContextClass, ())
        ctor = self.info.package.addFunction("$contextCtor", None,
                                             UnitType, list(implicitTypeParams),
                                             [irContextType],
                                             [ir.Variable("$this", None, irContextType,
                                                          ir.PARAMETER, frozenset([LET]))],
                                             [], frozenset())
        ctor.compileHint = CONTEXT_CONSTRUCTOR_HINT
        irContextClass.constructors.append(ctor)
        contextInfo.irContextClass = irContextClass

        # Create a variable to hold an instance of it.
        irContextVar = ir.Variable("$context", None, irContextType, ir.LOCAL, frozenset())
        self.getIrDefn().variables.append(irContextVar)
        closureInfo = self.info.getClosureInfo(self.scopeId)
        closureInfo.irClosureContexts[self.scopeId] = irContextVar

    def captureInContext(self, defnInfo, irContextClass):
        contextInfo = self.info.getContextInfo(self.scopeId)
        irDefn = defnInfo.irDefn
        if isinstance(irDefn, ir.Field):
            # variable which was already captured
            pass
        elif isinstance(irDefn, ir.Variable):
            defnInfo.irDefn.kind = None  # finish() will delete this
            irCaptureField = self.info.package.newField(irDefn.name, irDefn.astDefn,
                                                        irDefn.type, irDefn.flags)
            if hasattr(defnInfo.irDefn, "astDefn"):
                irCaptureField.astDefn = defnInfo.irDefn.astDefn
            if hasattr(defnInfo.irDefn, "astVarDefn"):
                irCaptureField.astVarDefn = defnInfo.irDefn.astVarDefn
            irContextClass.fields.append(irCaptureField)
            defnInfo.irDefn = irCaptureField
        elif isinstance(defnInfo.irDefn, ir.Function):
            # TODO
            raise NotImplementedError
        elif isinstance(defnInfo.irDefn, ir.Class):
            # TODO
            raise NotImplementedError
        else:
            raise NotImplementedError

    def makeClosure(self):
        # Check if the function is already a closure.
        assert not self.isLocal()
        closureInfo = self.info.closureInfo[self.scopeId]
        if closureInfo.irClosureClass:
            return

        # Create a closure class to hold this method and its contexts.
        implicitTypeParams = self.getImplicitTypeParameters()
        irClosureClass = self.info.package.addClass("$closure", None, list(implicitTypeParams),
                                                    [getRootClassType()], None,
                                                    [], [], [], frozenset())
        closureInfo.irClosureClass = irClosureClass
        irClosureType = ClassType(irClosureClass)
        irClosureCtor = self.info.package.addFunction("$closureCtor", None,
                                                      UnitType, list(implicitTypeParams),
                                                      [irClosureType],
                                                      [ir.Variable("$this", None,
                                                                   irClosureType,
                                                                   ir.PARAMETER,
                                                                   frozenset([LET]))],
                                                      None, frozenset())
        irClosureCtor.clas = irClosureClass
        irClosureCtor.compileHint = CLOSURE_CONSTRUCTOR_HINT
        irClosureClass.constructors.append(irClosureCtor)

        # Convert the function into a method of the closure class.
        irDefn = self.getIrDefn()
        assert not irDefn.isMethod()
        irDefn.clas = irClosureClass
        irDefn.variables.insert(0, ir.Variable("$this", None, irClosureType,
                                               ir.PARAMETER, frozenset([LET])))
        irDefn.parameterTypes.insert(0, irClosureType)
        irClosureClass.methods.append(irDefn)

        # If the parent is a function scope, define a local variable to hold the closure.
        if isinstance(self.parent, FunctionScope):
            irClosureVar = ir.Variable(irDefn.name, irDefn.astDefn,
                                       irClosureType, ir.LOCAL, frozenset())
            self.parent.getIrDefn().variables.append(irClosureVar)
            closureInfo.irClosureVar = irClosureVar

    def newLocalScope(self, ast):
        return FunctionScope(ast, self)

    def newScopeForFunction(self, ast):
        return FunctionScope(ast, self)

    def newScopeForClass(self, ast):
        return ClassScope(ast, self)

    def finish(self):
        if not self.isLocal():
            nextLocalId = Counter()
            scopeIrDefn = self.getIrDefn()
            oldVariables = scopeIrDefn.variables
            scopeIrDefn.variables = []
            for var in oldVariables:
                if var.kind is ir.LOCAL:
                    var.id = nextLocalId()
                if var.kind is not None:
                    # delete context variables
                    scopeIrDefn.variables.append(var)


class ClassScope(Scope):
    def __init__(self, ast, parent):
        super(ClassScope, self).__init__(ast, ScopeId(ast.id), parent, parent.info)
        irDefn = self.getIrDefn()
        self.info.setScope(irDefn.id, self)
        contextInfo = self.info.getContextInfo(self.scopeId)
        contextInfo.irContextClass = irDefn
        this = irDefn.initializer.variables[0]
        assert this.name == "$this"
        self.bind("this", DefnInfo(this, self.scopeId, self.scopeId, NOT_HERITABLE))
        self.define("this")
        closureInfo = self.info.getClosureInfo(self.scopeId)
        closureInfo.irClosureClass = irDefn
        closureInfo.irClosureContexts[self.scopeId] = this

        # Bind default constructors.
        for ctor in irDefn.constructors:
            defnInfo = DefnInfo(ctor, self.scopeId, self.scopeId, NOT_HERITABLE)
            self.bind(ctor.name, defnInfo)
            self.define(ctor.name)

    def createIrDefn(self, astDefn, astVarDefn):
        irScopeDefn = self.getIrDefn()
        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        if isinstance(astDefn, ast.AstVariablePattern):
            checkFlags(flags, frozenset([LET, PROTECTED, PRIVATE]), astDefn.location)
            irDefn = self.info.package.newField(astDefn.name, astDefn, None, flags)
            irScopeDefn.fields.append(irDefn)
        elif isinstance(astDefn, ast.AstFunctionDefinition):
            implicitTypeParams = self.getImplicitTypeParameters()
            if ABSTRACT in flags and astDefn.body is not None:
                raise ScopeException(astDefn.location,
                                     "%s: abstract method must not have body" % astDefn.name)
            if ABSTRACT not in flags and astDefn.body is None:
                raise ScopeException(astDefn.location,
                                     "%s: non-abstract method must have body" % astDefn.name)
            if ABSTRACT in flags and ABSTRACT not in irScopeDefn.flags:
                raise ScopeException(astDefn.location,
                                     "%s: abstract function not allowed in non-abstract class" %
                                     astDefn.name)
            if astDefn.name == "this":
                checkFlags(flags, frozenset([PROTECTED, PRIVATE]), astDefn.location)
                irDefn = self.info.package.addFunction("$constructor", astDefn,
                                                       None, implicitTypeParams,
                                                       None, [], None, flags)
                irScopeDefn.constructors.append(irDefn)
            else:
                checkFlags(flags, frozenset([ABSTRACT, PROTECTED, PRIVATE]), astDefn.location)
                irDefn = self.info.package.addFunction(astDefn.name, astDefn,
                                                       None, implicitTypeParams,
                                                       None, [], None, flags)
                irScopeDefn.methods.append(irDefn)
            # We don't need to call makeMethod here because the inner FunctionScope will do it.
        elif isinstance(astDefn, ast.AstPrimaryConstructorDefinition):
            checkFlags(flags, frozenset([PROTECTED, PRIVATE]), astDefn.location)
            implicitTypeParams = self.getImplicitTypeParameters()
            irDefn = self.info.package.addFunction("$constructor", astDefn,
                                                   None, implicitTypeParams,
                                                   None, [], None, flags)
            irScopeDefn.constructors.append(irDefn)
            self.makeMethod(irDefn, irScopeDefn)
        elif isinstance(astDefn, ast.AstTypeParameter):
            checkFlags(flags, frozenset([STATIC, COVARIANT, CONTRAVARIANT]), astDefn.location)
            if STATIC not in flags:
                raise NotImplementedError
            irDefn = self.info.package.addTypeParameter(astDefn.name, astDefn,
                                                        None, None, flags)
            irDefn.clas = irScopeDefn
            irScopeDefn.typeParameters.append(irDefn)
            irScopeDefn.initializer.typeParameters.append(irDefn)
            for ctor in irScopeDefn.constructors:
                ctor.typeParameters.append(irDefn)
        elif isinstance(astDefn, ast.AstParameter):
            # Parameters in a class scope can only belong to a primary constructor. They are
            # never treated as local variables, so we don't need to create definitions here.
            irDefn = None
        else:
            assert isinstance(astDefn, ast.AstClassDefinition)
            irDefn = self.createIrClassDefn(astDefn)
        return irDefn

    def isDefinedAutomatically(self, astDefn):
        return isinstance(astDefn, ast.AstPrimaryConstructorDefinition) or \
               isinstance(astDefn, ast.AstFunctionDefinition) or \
               isinstance(astDefn, ast.AstClassDefinition) or \
               isinstance(astDefn, ast.AstVariableDefinition) or \
               isinstance(astDefn, ast.AstVariablePattern)

    def findEnclosingClass(self):
        return self.getIrDefn()

    def requiresCapture(self):
        return True

    def captureScopeContext(self):
        pass

    def captureInContext(self, defnInfo, irContextClass):
        pass

    def makeClosure(self):
        pass

    def newLocalScope(self, ast):
        scope = FunctionScope(ast, self)
        return scope

    def newScopeForFunction(self, ast):
        scope = FunctionScope(ast, self)
        scope.configureAsMethod(self.scopeId, self.getIrDefn())
        return scope

    def newScopeForClass(self, ast):
        return ClassScope(ast, self)


class BuiltinGlobalScope(Scope):
    def __init__(self, parent):
        super(BuiltinGlobalScope, self).__init__(None, BUILTIN_SCOPE_ID, parent, parent.info)
        def bind(name, irDefn):
            defnInfo = DefnInfo(irDefn, self.scopeId)
            if isinstance(irDefn, ir.Class):
                # This scope will automatically be registered.
                BuiltinClassScope(defnInfo, self)
            self.bind(name, defnInfo)
            self.define(name)
        registerBuiltins(bind)

    def requiresCapture(self):
        return False


class BuiltinClassScope(Scope):
    def __init__(self, classDefnInfo, parent):
        irClass = classDefnInfo.irDefn
        super(BuiltinClassScope, self).__init__(None, ScopeId("builtin-" + irClass.name),
                                                parent, parent.info)
        self.info.setScope(irClass.id, self)
        self.defnInfo = classDefnInfo
        if not hasattr(irClass, "isPrimitive"):
            parent.info.setClassInfo(irClass, ClassInfo(irClass))
            for ctor in irClass.constructors:
                defnInfo = DefnInfo(ctor, self.scopeId, self.scopeId, NOT_HERITABLE)
                self.bind("$constructor", defnInfo)
                self.define("$constructor")
        for method in irClass.methods:
            inheritedScopeId = self.info.getScope(method.clas.id).scopeId
            inheritanceDepth = irClass.findDistanceToBaseClass(method.clas)
            defnInfo = DefnInfo(method, self.scopeId, inheritedScopeId, inheritanceDepth)
            self.bind(method.name, defnInfo)
            self.define(method.name)
        for field in irClass.fields:
            defnInfo = DefnInfo(field, self.scopeId)
            self.bind(field.name, defnInfo)
            self.define(field.name)

    def getDefnInfo(self):
        return self.defnInfo

    def requiresCapture(self):
        return False


class PackageScope(Scope):
    def __init__(self, scopeId, parent, info, packageNames, prefix, package):
        super(PackageScope, self).__init__(None, scopeId, parent, info)
        self.packageNames = []
        self.prefix = prefix
        self.package = package
        self.prefixScopes = {}

        if package is not None:
            for i, g in enumerate(package.globals):
                if PUBLIC in g.flags:
                    defnInfo = DefnInfo(g, scopeId)
                    self.bind(g.name, defnInfo)
                    self.define(g.name)

        packageBindings = {}
        for name in packageNames:
            if name.hasPrefix(prefix):
                nextComponent = name.components[len(prefix)]
                if self.isBound(nextComponent):
                    continue

                self.packageNames.append(name)
                nextPrefix = list(prefix) + [nextComponent]
                package = ir.PackagePrefix(ir.PackageName(nextPrefix))
                if nextComponent not in packageBindings or \
                   (isinstance(packageBindings[nextComponent], ir.PackagePrefix) and \
                    isinstance(package, ir.Package)):
                    packageBindings[nextComponent] = package

        for component, prefix in packageBindings.iteritems():
            defnInfo = DefnInfo(prefix, self.scopeId)
            self.bind(component, defnInfo)
            self.define(component)

    def scopeForPrefix(self, component):
        if component in self.prefixScopes:
            return self.prefixScopes

        prefix = self.prefix + [component]
        name = ir.PackageName(prefix)
        if name in self.packageNames:
            package = self.info.loader.loadPackage(name)
        else:
            package = None

        scope = PackageScope(ScopeId(".".join(prefix)), self, self.info,
                             self.packageNames, prefix, package)
        self.prefixScopes[component] = scope
        return scope

    def getDefinition(self, name):
        nameInfo = super(PackageScope, self).getDefinition(name)
        if nameInfo is None:
            return None
        defnInfo = nameInfo.getDefnInfo()
        irDefn = defnInfo.irDefn
        if isinstance(irDefn, ir.PackagePrefix):
            packageName = ir.PackageName(self.prefix + [name])
            if packageName in self.packageNames:
                defnInfo.irDefn = self.info.loader.loadPackage(packageName)
        return nameInfo

    def use(self, defnInfo, useAstId, useKind, loc):
        irDefn = defnInfo
        return super(PackageScope, self).use(defnInfo, useAstId, useKind, loc)

    def requiresCapture(self):
        return False


class ScopeVisitor(ast.AstNodeVisitor):
    """Abstract base class for scope analysis related visitor.

    This class takes care of the common tasks of entering lexical scopes in the AST. visit
    methods can be implemented or overriden to provide functionality."""

    def __init__(self, scope):
        self.scope = scope
        self.info = self.scope.info

    def createChildVisitor(self, scope):
        """Create a new visitor for a descendant scope. Subclasses must override."""
        raise NotImplementedError

    def visitAstVariableDefinition(self, node):
        if node.expression is not None:
            self.visit(node.expression)
        self.visit(node.pattern, node)

    def visitAstFunctionDefinition(self, node):
        scope = self.scope.scopeForFunction(node)
        visitor = self.createChildVisitor(scope)
        visitor.visitChildren(node)

    def visitAstClassDefinition(self, node):
        scope = self.scope.scopeForClass(node)
        visitor = self.createChildVisitor(scope)
        visitor.visitChildren(node)

    def visitAstTypeParameter(self, node):
        if node.upperBound is not None:
            self.visit(node.upperBound)
        if node.lowerBound is not None:
            self.visit(node.lowerBound)

    def visitAstParameter(self, node):
        self.visit(node.pattern, node)

    def visitAstBlockExpression(self, node):
        if isinstance(self.scope.ast, ast.AstFunctionDefinition) and \
           self.scope.ast.body is node:
            self.visitChildren(node)
        else:
            scope = self.scope.localScope(node)
            visitor = self.createChildVisitor(scope)
            visitor.visitChildren(node)

    def visitAstPartialFunctionCase(self, node):
        scope = self.scope.localScope(node)
        visitor = self.createChildVisitor(scope)
        visitor.visit(node.pattern, node)
        if node.condition is not None:
            visitor.visit(node.condition)
        visitor.visit(node.expression)

    def visitDefault(self, node, unused=None):
        self.visitChildren(node)


class DeclarationVisitor(ScopeVisitor):
    """Calls Scope.declare for each definition visited.

    If a definition is visited which has its own lexical scope, a new Scope object is created,
    and analyzeDeclarations is called on that."""

    def createChildVisitor(self, scope):
        return DeclarationVisitor(scope)

    def visitAstFunctionDefinition(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitAstFunctionDefinition(node)

    def visitAstClassDefinition(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitAstClassDefinition(node)

    def visitAstPrimaryConstructorDefinition(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitChildren(node)

    def visitAstTypeParameter(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitAstTypeParameter(node)

    def visitAstParameter(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitAstParameter(node)

    def visitAstVariablePattern(self, node, astVarDefn):
        self.scope.declare(node, astVarDefn)


class InheritanceVisitor(ScopeVisitor):
    def __init__(self, scope, inheritanceGraph, subtypeGraph):
        super(InheritanceVisitor, self).__init__(scope)
        self.inheritanceGraph = inheritanceGraph
        self.subtypeGraph = subtypeGraph

    def createChildVisitor(self, scope):
        return InheritanceVisitor(scope, self.inheritanceGraph, self.subtypeGraph)

    def visitAstClassDefinition(self, node):
        scope = self.scope.scopeForClass(node)
        scopeId = scope.scopeId
        irClass = scope.getIrDefn()
        classInfo = self.info.getClassInfo(irClass)
        if node.supertype is None:
            rootClassScopeId = self.info.getScope(BUILTIN_ROOT_CLASS_ID).scopeId
            classInfo.superclassInfo = self.info.getClassInfo(BUILTIN_ROOT_CLASS_ID)
            self.inheritanceGraph.addEdge(scopeId, rootClassScopeId)
            self.subtypeGraph.addEdge(irClass.id, BUILTIN_ROOT_CLASS_ID)
        else:
            supertype = node.supertype
            supertypeIrDefn = self.addTypeToSubtypeGraph(irClass.id, scope, supertype)
            if not isinstance(supertypeIrDefn, ir.Class):
                raise ScopeException(node.location, "inheritance from non-class type")

            superclassId = supertypeIrDefn.id
            superclassScopeId = self.info.getScope(superclassId).scopeId
            superclassInfo = self.info.getClassInfo(superclassId)
            if classInfo is superclassInfo:
                raise ScopeException(node.location, "class cannot inherit from itself")
            classInfo.superclassInfo = superclassInfo
            self.inheritanceGraph.addEdge(scopeId, superclassScopeId)

        super(InheritanceVisitor, self).visitAstClassDefinition(node)

    def visitAstTypeParameter(self, node):
        irParam = self.info.getDefnInfo(node).irDefn
        if node.upperBound is not None:
            self.addTypeToSubtypeGraph(irParam.id, self.scope, node.upperBound)
        if node.lowerBound is not None:
            self.addTypeToSubtypeGraph(irParam.id, self.scope, node.lowerBound, reverse=True)

    def addTypeToSubtypeGraph(self, id, scope, astType, reverse=False):
        assert isinstance(id, DefnId)
        if not isinstance(astType, ast.AstClassType):
            raise ScopeException(astType.location, "inheritance from non-class type")
        nameInfo = scope.lookup(astType.name, astType.location, ignoreDefnOrder=True)
        if nameInfo.isOverloaded():
            raise ScopeException(astType.location, "inheritance from overloaded symbol")
        defnInfo = nameInfo.getDefnInfo()
        irDefn = defnInfo.irDefn
        assert irDefn.isTypeDefn()
        if not reverse:
            self.subtypeGraph.addEdge(id, irDefn.id)
        else:
            self.subtypeGraph.addEdge(irDefn.id, id)
        return irDefn


def getFlagsFromAstDefn(astDefn, astVarDefn):
    flags = set()
    if isinstance(astDefn, ast.AstDefinition):
        attribs = astDefn.attribs
    elif isinstance(astVarDefn, ast.AstDefinition):
        attribs = astVarDefn.attribs
    else:
        attribs = []

    if isinstance(astDefn, ast.AstVariablePattern) and \
       not ((isinstance(astVarDefn, ast.AstVariableDefinition) and astVarDefn.keyword == "var") or \
            (isinstance(astVarDefn, ast.AstParameter) and astVarDefn.var == "var")):
        flags.add(LET)

    if isinstance(astDefn, ast.AstTypeParameter):
        if astDefn.variance == "+":
            flags.add(COVARIANT)
        elif astDefn.variance == "-":
            flags.add(CONTRAVARIANT)

    for attrib in attribs:
        flag = getFlagByName(attrib.name)
        if flag in flags:
            raise ScopeException(astDefn.location, "duplicate flag: %s" % attrib.name)
        flags.add(flag)
    return frozenset(flags)


def checkFlags(flags, mask, loc):
    conflict = checkFlagConflicts(flags)
    if conflict is not None:
        raise ScopeException(loc, "flags cannot be used together: %s" % ", ".join(conflict))
    diff = flags - mask
    if len(diff) > 0:
        raise ScopeException(loc, "invalid flags: %s" % ", ".join(diff))


def isInternalName(name):
    return name.startswith("$")

# Copyright 2014, Jay Conrod. All rights reserved.
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

from ast import *
from compile_info import *
from data import *
from errors import *
from flags import *
from graph import *
from ir import *
from ir_types import *
from builtins import *
from utils import *

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
    info.globalScope = GlobalScope(info.ast, info)
    info.setScope(GLOBAL_SCOPE_ID, info.globalScope)

    def createBuiltinInfo(name, irDefn):
        if isinstance(irDefn, Class):
            # This scope will automatically be added to info.scopes.
            BuiltinScope(irDefn, info.globalScope)
            # DefnInfos for the class and its members are created by BuiltinScope.
            defnInfo = info.getDefnInfo(irDefn)
        else:
            assert isinstance(irDefn, Function)
            defnInfo = DefnInfo(irDefn, BUILTIN_SCOPE_ID)
            info.setDefnInfo(irDefn, defnInfo)
        info.globalScope.bind(name, defnInfo)
        info.globalScope.define(name)
    registerBuiltins(createBuiltinInfo)

    visitor = DeclarationVisitor(info.globalScope)
    visitor.visitChildren(info.ast)


def analyzeInheritance(info):
    """Construct and analyze graph of inheritance between classes.

    This pass visits and links the class in the AST with their base classes. It constructs
    an inheritance graph and verifies there are no cycles. Finally, it copies symbol bindings
    to each class from its bases. At this point, overrides are treated as overloads, since
    we cannot distinguish between them until after type analysis."""

    # Add edges for inheritance between builtin classes, since these are in the AST.
    inheritanceGraph = Graph(info.classInfo.keys())
    def handleBuiltinInheritance(name, clas):
        if not isinstance(clas, Class):
            return
        assert len(clas.supertypes) <= 1
        if len(clas.supertypes) == 1:
            classInfo = info.getClassInfo(clas)
            irSuperclass = clas.supertypes[0].clas
            classInfo.superclassInfo = info.getClassInfo(irSuperclass)
            inheritanceGraph.addEdge(clas.id, irSuperclass.id)
    registerBuiltins(handleBuiltinInheritance)

    # Populate the graph by traversing the AST.
    visitor = InheritanceVisitor(info.globalScope, inheritanceGraph)
    visitor.visitChildren(info.ast)

    # Check for cycles.
    if inheritanceGraph.isCyclic():
        raise ScopeException("inheritance cycle detected")

    # Copy bindings from superclasses to subclasses. This must be done in topological order
    # to ensure we don't miss anything, i.e., if S <: T, we must ensure all bindings have been
    # copied to T before copying from T to S. Builtin classes are already flattened, so this
    # is not necessary for them.
    def bind(scope, name, defnInfo):
        if scope.isBound(name) and \
           not scope.getDefinition(name).isOverloadable(defnInfo):
            raise ScopeException("%s: conflicts with inherited definition" % name)
        scope.bind(name, defnInfo)
        scope.define(name)

    topologicalClassIds = inheritanceGraph.reverseEdges().topologicalSort()
    for classId in topologicalClassIds:
        if isBuiltinId(classId):
            continue
        classInfo = info.getClassInfo(classId)
        scope = info.getScope(classId)
        if classInfo.superclassInfo is None:
            continue
        superclassScope = info.getScope(classInfo.superclassInfo.irDefn)
        for name, defnInfo in superclassScope.getBindings():
            if defnInfo.inheritanceDepth == NOT_HERITABLE:
                continue
            bind(scope, name, defnInfo.inherit(scope.scopeId))


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
    for useInfo in info.useInfo.itervalues():
        if useInfo.shouldCapture(info):
            useScope = info.getScope(useInfo.useScopeId)
            useScope.capture(useInfo)


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
        if isBuiltinId(id):
            continue
        irClass = info.package.classes[id]
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


NOT_HERITABLE = -1


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
        return isinstance(otherDefnInfo.irDefn, Function) and \
               isinstance(self.overloads[0].irDefn, Function)

    def isOverloaded(self):
        return len(self.overloads) > 1

    def getDefnInfo(self):
        assert not self.isOverloaded()
        return self.overloads[0]

    def iterOverloads(self):
        return iter(self.overloads)

    def isClass(self):
        return not self.isOverloaded() and isinstance(self.getDefnInfo().irDefn, Class)

    def getInfoForConstructors(self, info):
        assert self.isClass()
        irClass = self.getDefnInfo().irDefn
        ctorNameInfo = NameInfo(self.name)
        ctorNameInfo.overloads = [info.getDefnInfo(ctor) for ctor in irClass.constructors]
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
                assert isinstance(aIrDefn, Function) and isinstance(bIrDefn, Function)
                if aIrDefn.mayOverride(bIrDefn):
                    # Check that nothing else is already overriding this function.
                    if bIrDefn.id in self.overrides:
                        raise TypeException("multiple definitions may override: %s" %
                                            self.name)

                    aIrDefn.override = bIrDefn
                    self.overrides[bIrDefn.id] = aIrDefn.id
                    overrideIndex = aIndex + bIndex
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
                        raise TypeException("may override multiple definitions: %s" %
                                            self.name)

    def findDefnInfoWithArgTypes(self, receiverType, receiverIsExplicit, typeArgs, argTypes):
        """Determines which overloaded or overriding function should be called, based on
        argument types.

        This is safe to call on any NameInfo, even if it doesn't refer to a function. Returns
        DefnInfo if there is exactly one match. Raises ScopeException if there zero or
        multiple matches."""
        self.resolveOverrides()
        candidate = None
        for defnInfo in self.overloads:
            irDefn = defnInfo.irDefn
            if isinstance(irDefn, Function) and irDefn.id in self.overrides:
                continue

            isNonFunction = not isinstance(irDefn, Function) and \
                            len(typeArgs) == 0 and len(argTypes) == 0
            isFunction = not receiverIsExplicit and \
                         isinstance(irDefn, Function) and \
                         not irDefn.isMethod() and \
                         irDefn.canCallWith(typeArgs, argTypes)
            isImplicitMethod = not receiverIsExplicit and \
                               isinstance(irDefn, Function) and \
                               irDefn.isMethod() and \
                               irDefn.canCallWith(typeArgs, [receiverType] + argTypes)
            isExplicitMethod = receiverIsExplicit and \
                               isinstance(irDefn, Function) and \
                               irDefn.isMethod() and \
                               irDefn.canCallWith(typeArgs, [receiverType] + argTypes)

            if isNonFunction or \
               isFunction or \
               isImplicitMethod or \
               isExplicitMethod:
                if candidate is not None:
                    raise TypeException("ambiguous call to overloaded function: %s" % \
                                        self.name)
                candidate = defnInfo

        if candidate is None:
            raise TypeException("could not find compatible definition: %s" % self.name)
        return candidate


class Scope(AstNodeVisitor):
    def __init__(self, ast, scopeId, parent, info):
        self.scopeId = scopeId
        self.ast = ast
        self.parent = parent
        self.info = info
        self.bindings = {}
        self.defined = set()
        self.childScopes = {}
        info.setScope(self.scopeId, self)
        info.setContextInfo(scopeId, ContextInfo(self.scopeId))
        if not self.isLocal() and \
           scopeId != GLOBAL_SCOPE_ID and \
           not info.hasClosureInfo(scopeId):
            closureInfo = ClosureInfo()
            self.info.setClosureInfo(scopeId, closureInfo)

    def __str__(self):
        return self.__class__.__name__

    def getDefnInfo(self):
        scope = self.topLocalScope()
        return self.info.getDefnInfo(scope.scopeId)

    def getAstDefn(self):
        scope = self.topLocalScope()
        assert isinstance(scope.ast, AstDefinition)
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
        irDefn.astDefn = astDefn
        if astVarDefn is not None:
            irDefn.astVarDefn = astVarDefn
        defnInfo = DefnInfo(irDefn, self.scopeId)
        self.info.setDefnInfo(astDefn, defnInfo)

        # If the definition has a user-specified name, bind it in this scope.
        name = irDefn.name
        if self.isBound(name) and not self.bindings[name].isOverloadable(defnInfo):
            raise ScopeException("%s: already declared" % name)
        self.bind(name, defnInfo)
        if self.isDefinedAutomatically(astDefn):
            self.define(name)

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

    def createIrClassDefn(self, astDefn):
        """Convenience method for creating a class definition."""
        flags = getFlagsFromAstDefn(astDefn, None)
        checkFlags(flags, frozenset())
        irDefn = Class(astDefn.name, None, None, None, [], [], [], flags)
        self.info.package.addClass(irDefn)

        irInitializer = Function("$initializer", None, [], None, [], None, frozenset())
        irInitializer.astDefn = astDefn
        self.makeMethod(irInitializer, irDefn)
        self.info.package.addFunction(irInitializer)
        irDefn.initializer = irInitializer

        if astDefn.hasConstructors():
            irDefaultCtor = None
        else:
            irDefaultCtor = Function("$constructor", None, [], None, [], None, frozenset())
            irDefaultCtor.astDefn = astDefn
            self.makeMethod(irDefaultCtor, irDefn)
            self.info.package.addFunction(irDefaultCtor)
            irDefn.constructors.append(irDefaultCtor)
        classInfo = ClassInfo(irDefn)
        self.info.setClassInfo(astDefn, classInfo)
        return irDefn

    def makeMethod(self, function, clas):
        """Convenience method which adds a "this" parameter and sets the "clas" attrib.

        Note that this does not add the function to the class's methods or constructors lists.
        """
        function.clas= clas
        this = Variable("$this", None, PARAMETER, frozenset())
        function.variables.insert(0, this)

    def isDefinedAutomatically(self, astDefn):
        """Returns true if a definition is available as soon as the scope is entered.

        This is only false for local variables in function scopes."""
        raise NotImplementedError

    def lookup(self, name, localOnly, mayBeAssignment):
        """Resolves a reference to a symbol, possibly in a parent scope.

        Returns NameInfo. For overloaded symbols, there may be several functions in there."""
        defnScope = self
        while defnScope is not None and \
              not defnScope.isBound(name) and \
              (not localOnly or self.isLocalWithin(defnScope)):
            defnScope = defnScope.parent
        if defnScope is None or (localOnly and not self.isLocalWithin(defnScope)):
            if mayBeAssignment and name.endswith("=") and name != "==":
                return self.lookup(name[:-1], localOnly, False)
            else:
                raise ScopeException("%s: not found" % name)
        if not defnScope.isDefined(name) and self.isLocalWithin(defnScope):
            raise ScopeException("%s: used before being defined" % name)
        return defnScope.bindings[name]

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

    def use(self, defnInfo, useAstId, useKind):
        """Creates, registers, and returns UseInfo for a given definition.

        Also checks whether the definition is allowed to be used in this scope and raises an
        Exception if not."""
        if (PRIVATE in defnInfo.irDefn.flags and \
            not self.isWithin(defnInfo.inheritedScopeId)) or \
           (PROTECTED in defnInfo.irDefn.flags and \
            not self.isWithin(defnInfo.scopeId)):
            raise ScopeException("%s: not allowed to be used in this scope" %
                                 defnInfo.irDefn.name)

        useInfo = UseInfo(defnInfo, self.scopeId, useKind)
        self.info.useInfo[useAstId] = useInfo
        return useInfo

    def resolveOverrides(self):
        """For bindings with multiple definitions, resolves which definitions are overloads and
        which are overriden (and by which others).

        All of the overloaded definitions must have type information before this is called."""
        for nameInfo in self.bindings.values():
            nameInfo.resolveOverrides()

    def isLocal(self):
        """Returns true if values defined in the parent scope are accessible in this scope."""
        return isinstance(self.ast, AstBlockExpression) or \
               isinstance(self.ast, AstPartialFunctionCase)

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
            irContextField = Field("$context", irContextType, frozenset())
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
    def __init__(self, astModule, info):
        super(GlobalScope, self).__init__(astModule, astModule.id, None, info)
        assert astModule.id == GLOBAL_SCOPE_ID

    def createIrDefn(self, astDefn, astVarDefn):
        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        if isinstance(astDefn, AstVariablePattern):
            checkFlags(flags, frozenset())
            irDefn = Global(astDefn.name, None, None, flags)
            self.info.package.addGlobal(irDefn)
        elif isinstance(astDefn, AstFunctionDefinition):
            checkFlags(flags, frozenset())
            irDefn = Function(astDefn.name, None, [], None, [], None, flags)
            self.info.package.addFunction(irDefn)
            if astDefn.name == "main":
                assert self.info.package.entryFunction == -1
                self.info.package.entryFunction = irDefn.id
        elif isinstance(astDefn, AstClassDefinition):
            irDefn = self.createIrClassDefn(astDefn)
        else:
            raise NotImplementedError
        return irDefn

    def isDefinedAutomatically(self, astDefn):
        return True

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
        super(FunctionScope, self).__init__(ast, ast.id, parent, parent.info)

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
        topScope = self.topLocalScope()
        irScopeDefn = self.info.getDefnInfo(topScope.scopeId).irDefn
        if isinstance(irScopeDefn, Class):
            irScopeDefn = irScopeDefn.initializer
        assert isinstance(irScopeDefn, Function)

        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        if isinstance(astDefn, AstTypeParameter):
            checkFlags(flags, frozenset([STATIC]))
            if STATIC not in flags:
                raise NotImplementedError
            irDefn = TypeParameter(astDefn.name, None, None, flags)
            self.info.package.addTypeParameter(irDefn)
            irScopeDefn.typeParameters.append(irDefn)
        elif isinstance(astDefn, AstParameter):
            checkFlags(flags, frozenset())
            if isinstance(astDefn.pattern, AstVariablePattern):
                # If the parameter is a simple variable which doesn't need unpacking, we don't
                # need to create a separate definition here.
                irDefn = None
            else:
                irDefn = Variable("$parameter", None, PARAMETER, flags)
                irScopeDefn.variables.append(irDefn)
        elif isinstance(astDefn, AstVariablePattern):
            checkFlags(flags, frozenset())
            kind = PARAMETER if isinstance(astVarDefn, AstParameter) else LOCAL
            irDefn = Variable(astDefn.name, None, kind, flags)
            irScopeDefn.variables.append(irDefn)
        elif isinstance(astDefn, AstFunctionDefinition):
            checkFlags(flags, frozenset())
            irDefn = Function(astDefn.name, None, [], None, [], None, flags)
            self.info.package.addFunction(irDefn)
        elif isinstance(astDefn, AstClassDefinition):
            irDefn = self.createIrClassDefn(astDefn)
        else:
            raise NotImplementedError
        return irDefn

    def isDefinedAutomatically(self, astDefn):
        return isinstance(astDefn, AstClassDefinition) or \
               isinstance(astDefn, AstFunctionDefinition)

    def captureScopeContext(self):
        contextInfo = self.info.getContextInfo(self.scopeId)
        if contextInfo.irContextClass is not None:
            return

        # Create the context class.
        irContextClass = Class("$context", [], [getRootClassType()], None,
                               [], [], [], frozenset())
        self.info.package.addClass(irContextClass)
        irContextType = ClassType(irContextClass, ())
        ctor = Function("$contextCtor", UnitType, [], [irContextType],
                        [Variable("$this", irContextType, PARAMETER, frozenset())],
                        [], frozenset())
        ctor.compileHint = CONTEXT_CONSTRUCTOR_HINT
        self.info.package.addFunction(ctor)
        irContextClass.constructors.append(ctor)
        contextInfo.irContextClass = irContextClass

        # Create a variable to hold an instance of it.
        irContextVar = Variable("$context", irContextType, LOCAL, frozenset())
        self.getIrDefn().variables.append(irContextVar)
        closureInfo = self.info.getClosureInfo(self.getAstDefn())
        closureInfo.irClosureContexts[self.scopeId] = irContextVar

    def captureInContext(self, defnInfo, irContextClass):
        contextInfo = self.info.getContextInfo(self.scopeId)
        irDefn = defnInfo.irDefn
        if isinstance(irDefn, Variable):
            defnInfo.irDefn.kind = None  # finish() will delete this
            irCaptureField = Field(defnInfo.irDefn.name, irDefn.type, frozenset())
            if hasattr(defnInfo.irDefn, "astDefn"):
                irCaptureField.astDefn = defnInfo.irDefn.astDefn
            if hasattr(defnInfo.irDefn, "astVarDefn"):
                irCaptureField.astVarDefn = defnInfo.irDefn.astVarDefn
            irContextClass.fields.append(irCaptureField)
            defnInfo.irDefn = irCaptureField
        elif isinstance(defnInfo.irDefn, Function):
            # TODO
            raise NotImplementedError
        elif isinstance(defnInfo.irDefn, Class):
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
        irClosureClass = Class("$closure", None, [getRootClassType()], None,
                               [], [], [], frozenset())
        self.info.package.addClass(irClosureClass)
        closureInfo.irClosureClass = irClosureClass
        irClosureType = ClassType(irClosureClass)
        irClosureCtor = Function("$closureCtor", UnitType, [], [irClosureType],
                                 [Variable("$this", irClosureType, PARAMETER, frozenset())],
                                 None, frozenset())
        irClosureCtor.clas = irClosureClass
        irClosureCtor.compileHint = CLOSURE_CONSTRUCTOR_HINT
        irClosureClass.constructors.append(irClosureCtor)
        self.info.package.addFunction(irClosureCtor)

        # Convert the function into a method of the closure class.
        irDefn = self.getIrDefn()
        assert not irDefn.isMethod()
        irDefn.clas = irClosureClass
        irDefn.variables.insert(0, Variable("$this", irClosureType, PARAMETER, frozenset()))
        irDefn.parameterTypes.insert(0, irClosureType)
        irClosureClass.methods.append(irDefn)

        # If the parent is a function scope, define a local variable to hold the closure.
        if isinstance(self.parent, FunctionScope):
            irClosureVar = Variable(irDefn.name, irClosureType, LOCAL, frozenset())
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
                if var.kind is LOCAL:
                    var.id = nextLocalId()
                if var.kind is not None:
                    # delete context variables
                    scopeIrDefn.variables.append(var)


class ClassScope(Scope):
    def __init__(self, ast, parent):
        super(ClassScope, self).__init__(ast, ast.id, parent, parent.info)
        irDefn = self.getIrDefn()
        contextInfo = self.info.getContextInfo(self.scopeId)
        contextInfo.irContextClass = irDefn
        this = irDefn.initializer.variables[0]
        assert this.name == "$this"
        self.bind("this", DefnInfo(this, self.scopeId, self.scopeId, NOT_HERITABLE))
        self.define("this")
        closureInfo = self.info.getClosureInfo(self.scopeId)
        closureInfo.irClosureContexts[self.scopeId] = this

        # Bind default constructors.
        for ctor in irDefn.constructors:
            defnInfo = DefnInfo(ctor, self.scopeId)
            self.bind(ctor.name, defnInfo)
            self.define(ctor.name)

    def createIrDefn(self, astDefn, astVarDefn):
        irScopeDefn = self.getIrDefn()
        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        if isinstance(astDefn, AstVariablePattern):
            checkFlags(flags, frozenset([PROTECTED, PRIVATE]))
            irDefn = Field(astDefn.name, None, flags, id=len(irScopeDefn.fields,))
            irScopeDefn.fields.append(irDefn)
        elif isinstance(astDefn, AstFunctionDefinition):
            checkFlags(flags, frozenset([PROTECTED, PRIVATE]))
            if astDefn.name == "this":
                irDefn = Function("$constructor", None, [], None, [], None, flags)
                irScopeDefn.constructors.append(irDefn)
            else:
                irDefn = Function(astDefn.name, None, [], None, [], None, flags)
                irScopeDefn.methods.append(irDefn)
            # We don't need to call makeMethod here because the inner FunctionScope will do it.
            self.info.package.addFunction(irDefn)
        elif isinstance(astDefn, AstPrimaryConstructorDefinition):
            checkFlags(flags, frozenset([PROTECTED, PRIVATE]))
            irDefn = Function("$constructor", None, [], None, [], None, flags)
            irScopeDefn.constructors.append(irDefn)
            self.makeMethod(irDefn, irScopeDefn)
            self.info.package.addFunction(irDefn)
        elif isinstance(astDefn, AstParameter):
            # Parameters in a class scope can only belong to a primary constructor. They are
            # never treated as local variables, so we don't need to create definitions here.
            irDefn = None
        else:
            assert isinstance(astDefn, AstClassDefinition)
            irDefn = self.createIrClassDefn(astDefn)
        return irDefn

    def isDefinedAutomatically(self, astDefn):
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


class BuiltinScope(Scope):
    def __init__(self, irClass, parent):
        super(BuiltinScope, self).__init__(None, irClass.id, parent, parent.info)
        self.irClass = irClass
        classDefnInfo = DefnInfo(irClass, BUILTIN_SCOPE_ID)
        self.info.setDefnInfo(irClass, classDefnInfo)
        if not hasattr(irClass, "isPrimitive"):
            parent.info.setClassInfo(irClass, ClassInfo(irClass))
            for ctor in irClass.constructors:
                defnInfo = DefnInfo(ctor, irClass.id)
                self.info.setDefnInfo(ctor, defnInfo)
                ctor.clas = irClass
        for method in irClass.methods:
            defnInfo = DefnInfo(method, irClass.id)
            self.info.setDefnInfo(method, defnInfo)
            self.bind(method.name, defnInfo)
            self.define(method.name)
            method.clas = irClass
        for field in irClass.fields:
            defnInfo = DefnInfo(field, irClass.id)
            self.bind(field.name, defnInfo)
            self.define(field.name)


class ScopeVisitor(AstNodeVisitor):
    """Abstract base class for scope analysis related visitor.

    This class takes care of the common tasks of entering lexical scopes in the AST. visit
    methods can be implemented or overriden to provide functionality."""

    def __init__(self, scope):
        self.scope = scope

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
        if isinstance(self.scope.ast, AstFunctionDefinition) and \
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
    def __init__(self, scope, graph):
        super(InheritanceVisitor, self).__init__(scope)
        self.graph = graph

    def createChildVisitor(self, scope):
        return InheritanceVisitor(scope, self.graph)

    def visitAstClassDefinition(self, node):
        classInfo = self.scope.info.getClassInfo(node)
        if len(node.supertypes) == 0:
            classInfo.superclassInfo = self.scope.info.getClassInfo(BUILTIN_ROOT_CLASS_ID)
            self.graph.addEdge(node.id, BUILTIN_ROOT_CLASS_ID)
        else:
            if len(node.supertypes) > 1:
                raise NotImplementedError
            supertype = node.supertypes[0]
            if not isinstance(supertype, AstClassType):
                raise ScopeException("inheritance from non-class type")
            supertypeDefnInfo = self.scope.lookup(supertype.name,
                                                  localOnly=False,
                                                  mayBeAssignment=False).getDefnInfo()
            self.scope.use(supertypeDefnInfo, supertype.id, USE_AS_TYPE)
            supertypeIrDefn = supertypeDefnInfo.irDefn

            superclassId = supertypeIrDefn.id
            if isBuiltinId(superclassId):
                classInfo.superclassInfo = self.scope.info.getClassInfo(superclassId)
                self.graph.addEdge(node.id, superclassId)
            else:
                superclassAst = supertypeIrDefn.astDefn
                superclassInfo = self.scope.info.getClassInfo(superclassAst)
                if classInfo is superclassInfo:
                    raise ScopeException("class cannot inherit from itself")
                classInfo.superclassInfo = superclassInfo
                self.graph.addEdge(node.id, superclassAst.id)
        super(InheritanceVisitor, self).visitAstClassDefinition(node)


def getFlagsFromAstDefn(astDefn, astVarDefn):
    if isinstance(astDefn, AstDefinition):
        attribs = astDefn.attribs
    elif isinstance(astVarDefn, AstDefinition):
        attribs = astVarDefn.attribs
    else:
        attribs = []

    flags = set()
    for attrib in attribs:
        flag = getFlagByName(attrib.name)
        if flag in flags:
            raise ScopeException("duplicate flag: %s" % attrib.name)
        flags.add(flag)
    return frozenset(flags)


def checkFlags(flags, mask):
    conflict = checkFlagConflicts(flags)
    if conflict is not None:
        raise ScopeException("flags cannot be used together: %s" % ", ".join(conflict))
    diff = flags - mask
    if len(diff) > 0:
        raise ScopeException("invalid flags: %s" % ", ".join(diff))


def isInternalName(name):
    return name.startswith("$")

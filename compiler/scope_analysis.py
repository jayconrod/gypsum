# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


# Scope analysis has two basic purposes:
#
# 1. Scan the AST for definitions and create stub IR definitions to be filled in later by
#    type analysis and semantic analysis.
# 2. Scan the AST for symbols and correlate them with the definitions they refer to.

import ast
from compile_info import ContextInfo, ClosureInfo, DefnInfo, UseInfo, ImportInfo, USE_AS_VALUE, USE_AS_TYPE, USE_AS_PROPERTY, USE_AS_CONSTRUCTOR, NORMAL_MODE, STD_MODE, NOSTD_MODE, CONTEXT_CONSTRUCTOR_HINT, CLOSURE_CONSTRUCTOR_HINT, NOT_HERITABLE
from data import Data
from errors import InheritanceException, ScopeException, TypeException
from flags import *
from graph import Graph
from ids import AstId, DefnId, PackageId, ScopeId, BUILTIN_SCOPE_ID, GLOBAL_SCOPE_ID, PACKAGE_SCOPE_ID
import ir
from ir_types import getRootClassType, ClassType, UnitType, I32Type
from location import Location, NoLoc
from builtins import registerBuiltins, getBuiltinClasses
from utils import Counter, each
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

    Import statements are processed after the AST has been fully traversed. This is necessary
    since we can't import symbols from a scope that hasn't been visited yet. Symbols are
    imported by adding additional DefnInfo records in the importing scope. The actual
    definitions are not duplicated though."""

    packageScope = PackageScope(PACKAGE_SCOPE_ID, None, info, info.packageNames, [], None)
    if info.languageMode() is NORMAL_MODE:
        # Load the std library so we can access class members even if there aren't any
        # explicit references to the std library.
        packageScope.scopeForPrefix("std", NoLoc)
    builtinScope = BuiltinGlobalScope(packageScope)
    globalScope = GlobalScope(info.ast, builtinScope)
    info.setScope(info.ast.id, globalScope)

    visitor = DeclarationVisitor(globalScope)
    visitor.visitChildren(info.ast)

    def processImportsForScope(scope):
        scope.processImports()
        for child in scope.childScopes.itervalues():
            processImportsForScope(child)
    processImportsForScope(globalScope)


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


def isHeritable(irDefn):
    """Returns true if the given irDefn can be inherited from a base class by a
    deriving class."""
    if isinstance(irDefn, ir.Function) and irDefn.isConstructor():
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

        # A list of DefnInfo that may be referenced by this symbol. In most cases, this
        # list will contain just one element.
        self.overloads = []

    def addOverload(self, defnInfo):
        self.overloads.append(defnInfo)

    def isHeritable(self):
        return all(isHeritable(o.irDefn) for o in self.overloads)

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

    def isFunction(self):
        return self.isOverloaded() or isinstance(self.getDefnInfo().irDefn, ir.Function)

    def isScope(self):
        return self.isClass() or self.isPackagePrefix() or self.isPackage()

    def getScope(self, info, loc):
        assert self.isScope()
        defnInfo = self.overloads[0]
        if isinstance(defnInfo.irDefn, ir.PackagePrefix):
            scope = info.getScope(defnInfo.scopeId).scopeForPrefix(self.name, loc)
        else:
            scope = info.getScope(defnInfo.irDefn)
        return scope

    def getInfoForConstructors(self, info):
        assert self.isClass()
        irClass = self.getDefnInfo().irDefn
        classScope = info.getScope(irClass)
        ctorNameInfo = classScope.getDefinition(ir.CONSTRUCTOR_SUFFIX)
        assert ctorNameInfo is not None
        return ctorNameInfo


class Scope(ast.NodeVisitor):
    def __init__(self, prefix, ast, scopeId, parent, info):
        self.prefix = prefix
        self.scopeId = scopeId
        self.ast = ast
        self.parent = parent
        self.info = info
        self.bindings = {}
        self.defined = set()
        self.childScopes = {}
        self.imports = []
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

    def getClass(self):
        return None

    def getAstDefn(self):
        scope = self.topLocalScope()
        assert isinstance(scope.ast, ast.Definition)
        return scope.ast

    def getIrDefn(self):
        return self.getDefnInfo().irDefn

    def makeName(self, short):
        return ir.Name(self.prefix + [short])

    def declare(self, astDefn, astVarDefn=None):
        """Creates an IR definition to the package, adds it to the package, and
        adds DefinitionInfo."""
        # Create the definition, and add it to the package.
        irDefn, shouldBind, isVisible = self.createIrDefn(astDefn, astVarDefn)
        if irDefn is None:
            return

        # Connect it with the AST so we can find it again.
        if astVarDefn is not None:
            irDefn.astVarDefn = astVarDefn
        inheritanceDepth = 0 if isHeritable(irDefn) else NOT_HERITABLE
        defnInfo = DefnInfo(irDefn, self.scopeId, isVisible, self.scopeId, inheritanceDepth)
        self.info.setDefnInfo(astDefn, defnInfo)

        # Associate the definition with this scope. Type/use analysis doesn't traverse the
        # AST in order, so this is important for finding the correct scope.
        self.info.setScope(astDefn, self)
        if astVarDefn is not None:
            if not self.info.hasScope(astVarDefn):
                self.info.setScope(astVarDefn, self)
            else:
                assert self.info.getScope(astVarDefn) is self

        # If the definition has a user-specified name, bind it in this scope.
        if not shouldBind:
            return
        name = irDefn.name.short()
        if self.isBound(name) and not self.bindings[name].isOverloadable(defnInfo):
            raise ScopeException(astDefn.location, "%s: already declared" % name)
        self.bind(name, defnInfo)
        if self.isDefinedAutomatically(astDefn):
            self.define(name)

    def addImport(self, node):
        """Adds an import statement to this scope's import list.

        This is called by DeclarationVisitor. Import statements can't be processed until the
        declaration analysis has visited the entire AST. processImports should be called for
        every scope after that."""
        self.imports.append(node)

    def processImports(self):
        """Processes import statements recorded with addImport.

        For each import statement, this method looks up the scope the prefix refers to, then
        copies DefnInfo records into this scope. The imported DefnInfo records are not visible
        outside this scope and are not heritable. They are bound to either the original
        symbols or the alternate symbols specified in the import statement. This method should
        be called for each AST scope at the end of declaration analysis."""
        for importStmt in self.imports:
            scope = self
            hasPrefix = False
            for component in importStmt.prefix:
                nameInfo = scope.lookup(component.name, component.location,
                                        fromExternal=hasPrefix)
                if not nameInfo.isScope():
                    raise ScopeException(component.location,
                                         "%s: import prefix does not refer to a scope" %
                                         component.name)
                scope = nameInfo.getScope(self.info, component.location)
                hasPrefix = True

            if importStmt.bindings is None:
                importedNames = [(name, name, importStmt.location)
                                 for name in scope.bindings.keys()]
                allowEmpty = True
            else:
                def getAsName(binding):
                    return binding.asName if binding.asName is not None else binding.name
                importedNames = [(binding.name, getAsName(binding), binding.location)
                                 for binding in importStmt.bindings]
                allowEmpty = False

            allImportedDefnInfos = []
            for name, asName, loc in importedNames:
                if name not in scope.bindings:
                    raise ScopeException(loc, "%s: undefined imported symbol" % name)
                nameInfo = scope.bindings[name]
                importedDefnInfos = []
                for defnInfo in nameInfo.overloads:
                    if defnInfo.isVisible and \
                       (not hasattr(defnInfo.irDefn, "flags") or
                        frozenset([PRIVATE, PROTECTED]).isdisjoint(defnInfo.irDefn.flags)):
                        importedDefnInfos.append(defnInfo.importt(self.scopeId))
                if not allowEmpty and len(importedDefnInfos) == 0:
                    raise ScopeException(loc,
                                         "%s: could not import private, protected, or inherited definition" %
                                         name)
                for importedDefnInfo in importedDefnInfos:
                    if self.isBound(asName) and \
                       not self.bindings[asName].isOverloadable(importedDefnInfo):
                        raise ScopeException(loc, "%s: already declared" % asName)
                    self.bind(asName, importedDefnInfo)
                    self.define(asName)
                allImportedDefnInfos.extend(importedDefnInfos)
            self.info.setImportInfo(importStmt, ImportInfo(allImportedDefnInfos))

    def canImport(self, defnInfo):
        """Returns whether a given definition can be imported from this class.

        Inherited and invisible definitions cannot be imported. In general, definitions that
        require context (such as non-static methods and fields) cannot be imported. Subclasses
        may override this."""
        return defnInfo.isVisible

    def bind(self, name, defnInfo):
        """Binds a name in this scope.

        For non-function defintions, this requires the name is not already bound. For function
        definitions, multiple functions may share the same name. They are added to
        OverloadInfo."""
        assert isinstance(name, str)
        assert isinstance(defnInfo, DefnInfo)
        if name not in self.bindings:
            self.bindings[name] = NameInfo(name)
        self.bindings[name].addOverload(defnInfo)

    def iterBindings(self):
        """Returns a iterator, which returns (str, DefnInfo, int) pairs for each binding.

        The str is the name, and the int indicates how many classes the definition was inherited
        through (0 indicates it was inherited from this class). For overloaded functions, this
        will return the same name more than once."""
        for name, nameInfo in self.bindings.iteritems():
            for defnInfo in nameInfo.overloads:
                yield (name, defnInfo)

    def iterNameInfo(self):
        """Returns an iterator which returns `NameInfo` for each binding."""
        return self.bindings.itervalues()

    def createIrDefn(self, astDefn, astVarDefn):
        """Creates an IR definition and adds it to the package. Returns a tuple containing
        the IrDefinition, a bool indicating whether it should be bound to its short name, and
        a bool indicating whether it should be visible to unrelated scopes."""
        raise NotImplementedError()

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
        checkFlags(flags, frozenset([ABSTRACT, FINAL, PUBLIC, PROTECTED]), astDefn.location)
        name = self.makeName(astDefn.name)
        irDefn = self.info.package.addClass(name, sourceName=astDefn.name, astDefn=astDefn,
                                            typeParameters=implicitTypeParams,
                                            constructors=[], fields=[], methods=[], flags=flags)

        irInitializerName = name.withSuffix(ir.CLASS_INIT_SUFFIX)
        irInitializer = self.info.package.addFunction(irInitializerName, astDefn=astDefn,
                                                      typeParameters=list(implicitTypeParams),
                                                      variables=[],
                                                      flags=frozenset([INITIALIZER]))
        self.makeMethod(irInitializer, irDefn)
        irDefn.initializer = irInitializer

        if not astDefn.hasConstructors():
            ctorFlags = flags & frozenset([PUBLIC, PROTECTED])
            irDefaultCtorName = name.withSuffix(ir.CONSTRUCTOR_SUFFIX)
            irDefaultCtor = self.info.package.addFunction(irDefaultCtorName, astDefn=astDefn,
                                                          typeParameters=
                                                              list(implicitTypeParams),
                                                          variables=[], flags=ctorFlags)
            self.makeConstructor(irDefaultCtor, irDefn)
            irDefn.constructors.append(irDefaultCtor)
        return irDefn, True

    def createIrTraitDefn(self, astDefn):
        """Convenience method for creating a trait definition."""
        implicitTypeParams = self.getImplicitTypeParameters()
        flags = getFlagsFromAstDefn(astDefn, None)
        checkFlags(flags, frozenset([PUBLIC, PROTECTED, PRIVATE]), astDefn.location)
        name = self.makeName(astDefn.name)
        irDefn = self.info.package.addTrait(name, sourceName=astDefn.name, astDefn=astDefn,
                                            typeParameters=implicitTypeParams,
                                            methods=[], flags=flags)
        return irDefn, True

    def makeMethod(self, function, clas):
        """Convenience method which turns a function into a method.

        Adds a "this" parameter, sets the "clas" attrib, and adds the METHOD flag. Note that
        this does not add the function to the class's methods or constructors lists. It just
        modifies the function itself.
        """
        function.flags |= frozenset([METHOD])
        function.definingClass = clas
        function.overridenBy = {}
        thisName = function.name.withSuffix(ir.RECEIVER_SUFFIX)
        this = ir.Variable(thisName, sourceName="this", astDefn=function.astDefn,
                           kind=ir.PARAMETER, flags=frozenset([LET]))
        function.variables.insert(0, this)

    def makeConstructor(self, function, clas):
        """Convenience method which turns a function into a constructor.

        Same as `makeMethod` but also adds the `CONSTRUCTOR` flag."""
        self.makeMethod(function, clas)
        function.flags |= frozenset([CONSTRUCTOR])

    def isDefinedAutomatically(self, astDefn):
        """Returns true if a definition is available as soon as the scope is entered.

        This is only false for local variables in function scopes."""
        raise NotImplementedError()

    def lookup(self, name, loc, fromExternal, mayBeAssignment=False, ignoreDefnOrder=None):
        """Resolves a reference to a symbol, either from this scope or an external scope."""
        if fromExternal:
            assert ignoreDefnOrder is not False
            return self.lookupFromExternal(name, loc, mayBeAssignment)
        else:
            if ignoreDefnOrder is None:
                ignoreDefnOrder = False
            return self.lookupFromSelf(name, loc, mayBeAssignment, ignoreDefnOrder)

    def lookupFromSelf(self, name, loc, mayBeAssignment=False, ignoreDefnOrder=False):
        """Resolves a reference to a symbol from this scope.

        The symbol may be defined in a parent scope. Returns NameInfo if the symbol is found.
        For overloaded symbols, there may be several functions in there. If the symbol is not
        found, a ScopeException is raised. Callers should call `use` if they actually use
        the symbol."""
        defnScope = self
        while defnScope is not None and not defnScope.isBound(name):
            defnScope = defnScope.parent
        if defnScope is None:
            if mayBeAssignment and name.endswith("=") and name != "==":
                return self.lookupFromSelf(name[:-1], loc, mayBeAssignment=False,
                                           ignoreDefnOrder=ignoreDefnOrder)
            else:
                raise ScopeException(loc, "%s: not found" % name)
        if not ignoreDefnOrder and \
           not defnScope.isDefined(name) and \
           self.isLocalWithin(defnScope):
            raise ScopeException(loc, "%s: used before being defined" % name)
        return defnScope.getDefinition(name)

    def lookupFromExternal(self, name, loc, mayBeAssignment=False):
        """Resolves a reference to a symbol from another scope.

        Unlike `lookupFromLocal`, the symbol must be defined in this scope. Returns NameInfo
        if the symbol is found. For overloaded symbols, there may be several functions in there.
        If the symbol is not found, a ScopeException is raised. Callers should call `use` if
        they actually use the symbol."""
        if not self.isBound(name):
            if mayBeAssignment and name.endswith("=") and name != "==":
                return self.lookupFromExternal(name[:-1], loc, mayBeAssignment=False)
            else:
                raise ScopeException(loc, "%s: not found" % name)

        nameInfo = self.getDefinition(name)
        if nameInfo.isOverloaded():
            assert all(o.isVisible for o in nameInfo.overloads)
        elif not nameInfo.getDefnInfo().isVisible:
            raise ScopeException(loc, "%s: cannot access definition outside of its scope" %
                                 name)
        return nameInfo

    def tryLookup(self, name, fromExternal, mayBeAssignment=False, ignoreDefnOrder=None):
        """Attempts to resolve a reference to a symbol from this scope or from another.

        Returns `NameInfo` if the symbol is found and can be accessed. Returns `None`
        otherwise."""
        if fromExternal:
            assert ignoreDefnOrder is None
            return self.tryLookupFromExternal(name, mayBeAssignment)
        else:
            if ignoreDefnOrder is None:
                ignoreDefnOrder = False
            return self.tryLookupFromSelf(name, mayBeAssignment, ignoreDefnOrder)

    def tryLookupFromSelf(self, name, mayBeAssignment=False, ignoreDefnOrder=False):
        """Attempts to resolve a reference to a symbol from this scope.

        This is like `lookupFromSelf` but returns `None` if the symbol is not found or
        can't be accessed."""
        defnScope = self
        while defnScope is not None and defnScope.isBound(name):
            defnScope = defnScope.parent
        if defnScope is None:
            if mayBeAssignment and name != "==":
                return self.tryLookupFromSelf(name[:-1], mayBeAssignment=False,
                                              ignoreDefnOrder=ignoreDefnOrder)
            else:
                return None
        if not ignoreDefnOrder and \
           not defnScope.isDefined(name) and \
           self.isLocalWithin(defnScope):
            return None
        return defnScope.getDefinition(name)

    def tryLookupFromExternal(self, name, mayBeAssignment=False):
        """Attempts to resolve a reference to a symbol from another scope.

        This is similar to `lookupFromExternal` but returns `None` if the symbol is not found
        or can't be accessed."""
        if not self.isBound(name):
            if mayBeAssignment and name.endswith("=") and name != "==":
                return self.tryLookupFromExternal(name[:-1], mayBeAssignment=False)
            else:
                return None

        nameInfo = self.getDefinition(name)
        if nameInfo.isOverloaded():
            assert all(o.isVisible for o in nameInfo.overloads)
        elif not nameInfo.getDefnInfo().isVisible:
            return None
        return nameInfo

    def isBound(self, name):
        """Returns true if a symbol is defined in this scope."""
        return self.getDefinition(name) is not None

    def isShadow(self, name):
        """Returns true if a symbol is defined in an outer scope."""
        assert self.isBound(name)
        scope = self.parent
        while scope is not None:
            if scope.isBound(name):
                return True
            scope = scope.parent
        return False

    def getDefinition(self, name):
        """Returns NameInfo for a symbol defined in this scope or None."""
        return self.bindings.get(name)

    def isDefined(self, name):
        """Returns whether a symbol has been defined yet."""
        return name in self.defined

    def define(self, name):
        assert isinstance(name, str)
        self.defined.add(name)

    def deleteVar(self, name):
        """Deletes the local variable defined at this node, and removes it from the scope's
        bindings and from the defnInfo table. This is useful when we don't know if an
        expression is defining or using a variable. We define it first, then delete it if we
        don't need it."""
        assert self.isBound(name) and not self.isDefined(name)
        nameInfo = self.bindings[name]
        assert not nameInfo.isOverloaded()
        defnInfo = nameInfo.getDefnInfo()
        assert isinstance(defnInfo.irDefn, ir.Variable) and defnInfo.irDefn.kind is not None
        defnInfo.irDefn.kind = None
        del(self.bindings[name])

    def use(self, defnInfo, useAstId, useKind, loc):
        """Creates, registers, and returns UseInfo for a given definition.

        This method should be called in the scope where a definition is used, not where
        the definition comes from or is looked up from. Consequently, this should never be
        called on a foreign or builtin scope. This method also checks whether the definition
        is allowed to be used in this scope and raises a ScopeException if not."""
        assert not self.isForeign()
        irDefn = defnInfo.irDefn
        if isinstance(irDefn, ir.Package):
            assert useKind in [USE_AS_VALUE, USE_AS_TYPE, USE_AS_PROPERTY]
            self.info.package.ensureDependency(irDefn)

        if hasattr(irDefn, "flags"):
            if PRIVATE in irDefn.flags and not self.isWithin(defnInfo.inheritedScopeId):
                raise ScopeException(loc,
                                     "%s: definition is private and cannot be used here" %
                                     irDefn.name)
            if PROTECTED in irDefn.flags and not self.isWithin(defnInfo.scopeId):
                raise ScopeException(loc,
                                     "%s: definition is protected and cannot be used here" %
                                     irDefn.name)
            if STATIC not in irDefn.flags and \
               useKind is USE_AS_VALUE and \
               self.isStaticWithin(defnInfo.scopeId):
                raise ScopeException(loc,
                                     "%s: definition is non-static, accessed from a static scope" %
                                     irDefn.name)

        if useKind is USE_AS_CONSTRUCTOR and \
           ABSTRACT in irDefn.definingClass.flags:
            raise ScopeException(loc, "%s: cannot instantiate abstract class" %
                                 irDefn.definingClass.name)

        useInfo = UseInfo(defnInfo, self.scopeId, useKind)
        self.info.setUseInfo(useAstId, useInfo)
        return useInfo

    def isLocal(self):
        """Returns true if values defined in the parent scope are accessible in this scope."""
        return isinstance(self.ast, ast.BlockExpression) or \
               isinstance(self.ast, ast.PartialFunctionCase)

    def isLocalWithin(self, defnScope):
        """Returns true if values defined in defnScope may be accessed directly in this scope."""
        current = self
        while True:
            if current is defnScope:
                return True
            if not current.isLocal():
                return False
            current = current.parent

    def isForeign(self):
        """Returns true if this scope is in a package other than the one being compiled."""
        return False

    def isWithin(self, scopeOrId):
        id = scopeOrId.scopeId if isinstance(scopeOrId, Scope) else scopeOrId
        current = self
        while current is not None and current.scopeId != id:
            current = current.parent
        return current is not None

    def isStaticWithin(self, scopeOrId):
        """Returns true if this scope is within the given scope and at least one top-level
        scope between the current scope and the given scope (including the current scope but
        not the given scope) is defined by a static definition. If this is true, then
        non-static definitions bound in the given scope cannot be used in this scope."""
        id = scopeOrId.scopeId if isinstance(scopeOrId, Scope) else scopeOrId
        if self.scopeId is id or self.scopeId is GLOBAL_SCOPE_ID:
            return False

        current = self
        isWithin = False
        isStatic = False
        isGlobal = False
        while True:
            if not self.isLocal():
                isStatic |= STATIC in self.getIrDefn().flags
            current = current.parent
            isWithin |= current.scopeId is id
            isGlobal |= isinstance(current, GlobalScope)
            if current.scopeId is id or isWithin or isGlobal:
                break

        return isWithin and isStatic and not isGlobal

    def topLocalScope(self, defnScope=None):
        """Returns the top-most local scope which is still below defnScope."""
        scope = self
        while scope != defnScope and scope.isLocal():
            scope = scope.parent
        return scope

    def findEnclosingClass(self):
        """Returns the IR class enclosing this scope, if there is one. If this is a class scope,
        this will return the class itself."""
        raise NotImplementedError()

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
        raise NotImplementedError()

    def captureScopeContext(self):
        """Makes this scope available for capturing.

        For functions, this creates an empty context class and a variable containing an in
        instance of that class (unless they have already been created). Nothing is done for
        classes since instances can already be used as contexts."""
        raise NotImplementedError()

    def captureInContext(self, defnInfo, irContextClass):
        """Stores the given definition in the given context class.

        Returns the field storing the definition in the context class."""
        raise NotImplementedError()

    def makeClosure(self):
        """Ensures this scope can store contexts from parent scopes."""
        raise NotImplementedError()

    def closureCaptureContext(self, scopeId):
        """Ensures a closure class captures a specific context from a parent scope.

        Returns the field containing the captured context."""
        assert not self.isLocal()
        closureInfo = self.info.getClosureInfo(self.scopeId)
        if scopeId not in closureInfo.irClosureContexts:
            irClosureClass = closureInfo.irClosureClass
            irContextClass = self.info.getContextInfo(scopeId).irContextClass
            irContextType = ClassType(irContextClass)
            contextFieldName = irClosureClass.name.withSuffix(ir.CONTEXT_SUFFIX)
            irContextField = self.info.package.newField(contextFieldName, type=irContextType)
            irClosureClass.fields.append(irContextField)
            irClosureClass.constructors[0].parameterTypes.append(irContextType)
            closureInfo.irClosureContexts[scopeId] = irContextField

    def localScope(self, ast):
        return self.getOrCreateScope(None, ast, self.newLocalScope)

    def newLocalScope(self, prefix, ast):
        """Creates a new scope for block expressions which may introduce definitions

        Must be implemented by subclasses.
        """
        raise NotImplementedError()

    def scopeForFunction(self, shortName, ast):
        return self.getOrCreateScope(shortName, ast, self.newScopeForFunction)

    def newScopeForFunction(self, prefix, ast):
        """Creates a new scope for function definitions.

        Must be implemented by subclasses.
        """
        raise NotImplementedError()

    def scopeForClass(self, shortName, ast):
        return self.getOrCreateScope(shortName, ast, self.newScopeForClass)

    def newScopeForClass(self, prefix, ast):
        """Creates a new scope for class definition.

        Must be implemented by subclasses.
        """
        raise NotImplementedError()

    def scopeForTrait(self, shortName, ast):
        return self.getOrCreateScope(shortName, ast, self.newScopeForTrait)

    def newScopeForTrait(self, prefix, ast):
        """Creates a new scope for trait definition.

        Must be implemented by subclasses.
        """
        raise NotImplementedError()

    def scopeForExistential(self, ast):
        return self.getOrCreateScope(ir.EXISTENTIAL_SUFFIX, ast, self.newScopeForExistential)

    def newScopeForExistential(self, prefix, ast):
        """Creates a new scope for an existential type."""
        return ExistentialTypeScope(prefix, ast, self)

    def getOrCreateScope(self, shortName, ast, create):
        if ast.id in self.childScopes:
            return self.childScopes[ast.id]
        else:
            prefix = list(self.prefix)
            if shortName is not None:
                prefix.append(shortName)
            scope = create(prefix, ast)
            self.childScopes[ast.id] = scope
            return scope

    def finish(self):
        """Adds final information to the scope's definition.

        May be overridden by subclasses.
        """
        pass


class GlobalScope(Scope):
    def __init__(self, astModule, parent):
        super(GlobalScope, self).__init__([], astModule, GLOBAL_SCOPE_ID, parent, parent.info)

    def createIrDefn(self, astDefn, astVarDefn):
        name = self.makeName(astDefn.name)
        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        shouldBind = True
        if isinstance(astDefn, ast.VariablePattern):
            checkFlags(flags, frozenset([LET, PUBLIC, PROTECTED]), astDefn.location)
            irDefn = self.info.package.addGlobal(name, sourceName=astDefn.name, astDefn=astDefn,
                                                 flags=flags)
        elif isinstance(astDefn, ast.FunctionDefinition):
            checkFlags(flags, frozenset([PUBLIC, NATIVE]), astDefn.location)
            if astDefn.body is None and NATIVE not in flags:
                raise ScopeException(astDefn.location,
                                     "%s: global function must have body" % astDefn.name)
            if astDefn.body is not None and NATIVE in flags:
                raise ScopeException(astDefn.location,
                                     "%s: native global function must not have body" %
                                     astDefn.name)
            irDefn = self.info.package.addFunction(name, sourceName=astDefn.name,
                                                   astDefn=astDefn,
                                                   typeParameters=
                                                       self.getImplicitTypeParameters(),
                                                   variables=[],
                                                   flags=flags)
            if astDefn.name == "main":
                assert self.info.package.entryFunction is None
                self.info.package.entryFunction = irDefn.id
        elif isinstance(astDefn, ast.ClassDefinition):
            irDefn, shouldBind = self.createIrClassDefn(astDefn)
        elif isinstance(astDefn, ast.TraitDefinition):
            irDefn, shouldBind = self.createIrTraitDefn(astDefn)
        else:
            raise NotImplementedError()
        return irDefn, shouldBind, True

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

    def newLocalScope(self, prefix, ast):
        raise NotImplementedError("global scopes can't have local contexts")

    def newScopeForFunction(self, prefix, ast):
        return FunctionScope(prefix, ast, self)

    def newScopeForClass(self, prefix, ast):
        return ClassScope(prefix, ast, self)

    def newScopeForTrait(self, prefix, ast):
        return TraitScope(prefix, ast, self)


class FunctionScope(Scope):
    def __init__(self, prefix, ast, parent):
        super(FunctionScope, self).__init__(prefix, ast, ScopeId(ast.id), parent, parent.info)
        self.info.setScope(self.getIrDefn().id, self)

    def configureAsMethod(self, astClassScopeId, irClassDefn):
        """Configures this scope as a method scope.

        Binds and defines the `this` parameter, and
        adds the given parent class to the closure context map. `makeMethod` should have already
        been called on the function definition; this method does not modify the method itself.
        """
        irMethod = self.getIrDefn()
        assert irMethod.isMethod()
        this = irMethod.variables[0]
        self.bind("this", DefnInfo(this, self.scopeId, isVisible=False))
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
        shouldBind = True
        if isinstance(astDefn, ast.TypeParameter):
            name = self.makeName(astDefn.name)
            checkFlags(flags, frozenset([STATIC]), astDefn.location)
            if STATIC not in flags:
                raise NotImplementedError()
            flags |= irScopeDefn.flags & frozenset([PUBLIC, PROTECTED, PRIVATE])
            irDefn = self.info.package.addTypeParameter(name, sourceName=astDefn.name,
                                                        astDefn=astDefn, flags=flags)
            irScopeDefn.typeParameters.append(irDefn)
        elif isinstance(astDefn, ast.Parameter):
            checkFlags(flags, frozenset(), astDefn.location)
            if isinstance(astDefn.pattern, ast.VariablePattern):
                # If the parameter is a simple variable which doesn't need unpacking, we don't
                # need to create a separate definition here.
                irDefn = None
            else:
                name = self.makeName(ir.ANON_PARAMETER_SUFFIX)
                irDefn = ir.Variable(name, astDefn=astDefn, kind=ir.PARAMETER, flags=flags)
                irScopeDefn.variables.append(irDefn)
        elif isinstance(astDefn, ast.VariablePattern):
            name = self.makeName(astDefn.name)
            checkFlags(flags, frozenset([LET]), astDefn.location)
            isParameter = isinstance(astVarDefn, ast.Parameter) and \
                          astVarDefn.pattern is astDefn
            kind = ir.PARAMETER if isParameter else ir.LOCAL
            irDefn = ir.Variable(name, sourceName=astDefn.name, astDefn=astDefn,
                                 kind=kind, flags=flags)
            irScopeDefn.variables.append(irDefn)
        elif isinstance(astDefn, ast.FunctionDefinition):
            name = self.makeName(astDefn.name)
            checkFlags(flags, frozenset([NATIVE]), astDefn.location)
            if astDefn.body is None and NATIVE not in flags:
                raise ScopeException(astDefn.location,
                                     "%s: function must have body" % astDefn.name)
            if astDefn.body is not None and NATIVE in flags:
                raise ScopeException(astDefn.location,
                                     "%s: native function must not have body" % astDefn.name)
            implicitTypeParams = self.getImplicitTypeParameters()
            irDefn = self.info.package.addFunction(name, sourceName=astDefn.name,
                                                   astDefn=astDefn,
                                                   typeParameters=implicitTypeParams,
                                                   variables=[], flags=flags)
        elif isinstance(astDefn, ast.ClassDefinition):
            irDefn, shouldBind = self.createIrClassDefn(astDefn)
        else:
            raise NotImplementedError()
        isVisible = False
        return irDefn, shouldBind, isVisible

    def isDefinedAutomatically(self, astDefn):
        return isinstance(astDefn, ast.ClassDefinition) or \
               isinstance(astDefn, ast.FunctionDefinition)

    def findEnclosingClass(self):
        return self.parent.findEnclosingClass()

    def requiresCapture(self):
        return True

    def captureScopeContext(self):
        contextInfo = self.info.getContextInfo(self.scopeId)
        if contextInfo.irContextClass is not None:
            return
        irDefn = self.getIrDefn()

        # Create the context class.
        implicitTypeParams = self.getImplicitTypeParameters()
        contextClassName = irDefn.name.withSuffix(ir.CONTEXT_SUFFIX)
        irContextClass = self.info.package.addClass(contextClassName,
                                                    typeParameters=list(implicitTypeParams),
                                                    supertypes=[getRootClassType()],
                                                    constructors=[], fields=[],
                                                    methods=[], flags=frozenset())
        irContextType = ClassType(irContextClass, ())
        ctorName = contextClassName.withSuffix(ir.CONSTRUCTOR_SUFFIX)
        receiverName = ctorName.withSuffix(ir.RECEIVER_SUFFIX)
        ctor = self.info.package.addFunction(ctorName, returnType=UnitType,
                                             typeParameters=list(implicitTypeParams),
                                             parameterTypes=[irContextType],
                                             variables=[
                                                 ir.Variable(receiverName, type=irContextType,
                                                             kind=ir.PARAMETER,
                                                             flags=frozenset([LET]))],
                                             flags=frozenset([METHOD]))
        ctor.compileHint = CONTEXT_CONSTRUCTOR_HINT
        irContextClass.constructors.append(ctor)
        contextInfo.irContextClass = irContextClass

        # Create a variable to hold an instance of it.
        irContextVar = ir.Variable(contextClassName, type=irContextType, kind=ir.LOCAL)
        irDefn.variables.append(irContextVar)
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
            irCaptureField = self.info.package.newField(irDefn.name, astDefn=irDefn.astDefn,
                                                        type=irDefn.type, flags=irDefn.flags)
            if hasattr(defnInfo.irDefn, "astDefn"):
                irCaptureField.astDefn = defnInfo.irDefn.astDefn
            if hasattr(defnInfo.irDefn, "astVarDefn"):
                irCaptureField.astVarDefn = defnInfo.irDefn.astVarDefn
            irContextClass.fields.append(irCaptureField)
            defnInfo.irDefn = irCaptureField
        elif isinstance(defnInfo.irDefn, ir.Function):
            # TODO
            raise NotImplementedError()
        elif isinstance(defnInfo.irDefn, ir.Class):
            # TODO
            raise NotImplementedError()
        else:
            raise NotImplementedError()

    def makeClosure(self):
        # Check if the function is already a closure.
        assert not self.isLocal()
        closureInfo = self.info.closureInfo[self.scopeId]
        if closureInfo.irClosureClass:
            return
        irDefn = self.getIrDefn()

        # Create a closure class to hold this method and its contexts.
        implicitTypeParams = self.getImplicitTypeParameters()
        closureName = irDefn.name.withSuffix(ir.CLOSURE_SUFFIX)
        irClosureClass = self.info.package.addClass(closureName,
                                                    typeParameters=list(implicitTypeParams),
                                                    supertypes=[getRootClassType()],
                                                    constructors=[], fields=[],
                                                    methods=[], flags=frozenset())
        closureInfo.irClosureClass = irClosureClass
        irClosureType = ClassType(irClosureClass)
        ctorName = closureName.withSuffix(ir.CONSTRUCTOR_SUFFIX)
        ctorReceiverName = ctorName.withSuffix(ir.RECEIVER_SUFFIX)
        irClosureCtor = self.info.package.addFunction(ctorName,
                                                      returnType=UnitType,
                                                      typeParameters=list(implicitTypeParams),
                                                      parameterTypes=[irClosureType],
                                                      variables=[ir.Variable(ctorReceiverName,
                                                                             type=irClosureType,
                                                                             kind=ir.PARAMETER,
                                                                             flags=frozenset([LET]))],
                                                      flags=frozenset([METHOD]),
                                                      definingClass=irClosureClass,
                                                      compileHint=CLOSURE_CONSTRUCTOR_HINT)
        irClosureClass.constructors.append(irClosureCtor)

        # Convert the function into a method of the closure class.
        # We don't use `makeMethod`, since it's intended to be used before type analysis, but
        # we do most of the same things.
        assert not irDefn.isMethod()
        irDefn.flags |= frozenset([METHOD])
        thisType = ClassType.forReceiver(irClosureClass)
        thisName = irDefn.name.withSuffix(ir.RECEIVER_SUFFIX)
        this = ir.Variable(thisName, astDefn=irDefn.astDefn, type=thisType,
                           kind=ir.PARAMETER, flags=frozenset([LET]))
        irDefn.variables.insert(0, this)
        irDefn.parameterTypes.insert(0, thisType)
        irDefn.definingClass = irClosureClass
        irClosureClass.methods.append(irDefn)

        # If the parent is a function scope, define a local variable to hold the closure.
        if isinstance(self.parent, FunctionScope):
            irClosureVar = ir.Variable(irDefn.name, astDefn=irDefn.astDefn,
                                       type=irClosureType, kind=ir.LOCAL)
            self.parent.getIrDefn().variables.append(irClosureVar)
            closureInfo.irClosureVar = irClosureVar

    def newLocalScope(self, prefix, ast):
        return FunctionScope(prefix, ast, self)

    def newScopeForFunction(self, prefix, ast):
        return FunctionScope(prefix, ast, self)

    def newScopeForClass(self, prefix, ast):
        return ClassScope(prefix, ast, self)

    def newScopeForTrait(self, prefix, ast):
        return TraitScope(prefix, ast, self)

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
    def __init__(self, prefix, ast, parent):
        super(ClassScope, self).__init__(prefix, ast, ScopeId(ast.id), parent, parent.info)
        irDefn = self.getIrDefn()
        self.info.setScope(irDefn.id, self)
        contextInfo = self.info.getContextInfo(self.scopeId)
        contextInfo.irContextClass = irDefn
        this = irDefn.initializer.variables[0]
        assert this.name.short() == ir.RECEIVER_SUFFIX
        self.bind("this", DefnInfo(this, self.scopeId, False, self.scopeId, NOT_HERITABLE))
        self.define("this")
        closureInfo = self.info.getClosureInfo(self.scopeId)
        closureInfo.irClosureClass = irDefn
        closureInfo.irClosureContexts[self.scopeId] = this

        # Bind default constructors.
        for ctor in irDefn.constructors:
            defnInfo = DefnInfo(ctor, self.scopeId, True, self.scopeId, NOT_HERITABLE)
            self.bind(ctor.name.short(), defnInfo)
            self.define(ctor.name.short())

    def getClass(self):
        return getIrDefn()

    def createIrDefn(self, astDefn, astVarDefn):
        irScopeDefn = self.getIrDefn()
        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        shouldBind = True
        isVisible = True
        if isinstance(astDefn, ast.VariablePattern):
            name = self.makeName(astDefn.name)
            checkFlags(flags, frozenset([LET, PUBLIC, PROTECTED, PRIVATE]), astDefn.location)
            irDefn = self.info.package.newField(name, sourceName=astDefn.name,
                                                astDefn=astDefn, flags=flags)
            irScopeDefn.fields.append(irDefn)
        elif isinstance(astDefn, ast.FunctionDefinition):
            implicitTypeParams = self.getImplicitTypeParameters()
            if astDefn.body is not None:
                if ABSTRACT in flags:
                    raise ScopeException(astDefn.location,
                                         "%s: abstract method must not have body" %
                                         astDefn.name)
                if NATIVE in flags:
                    raise ScopeException(astDefn.location,
                                         "%s: native method must not have body")
            else:
                if ABSTRACT not in flags and NATIVE not in flags:
                    raise ScopeException(astDefn.location,
                                         "%s: non-abstract method must have body" %
                                         astDefn.name)
            if ABSTRACT in flags and ABSTRACT not in irScopeDefn.flags:
                raise ScopeException(astDefn.location,
                                     "%s: abstract function not allowed in non-abstract class" %
                                     astDefn.name)

            if astDefn.name == "this":
                name = self.makeName(ir.CONSTRUCTOR_SUFFIX)
                checkFlags(flags, frozenset([PUBLIC, PROTECTED, PRIVATE, NATIVE]),
                           astDefn.location)
                irDefn = self.info.package.addFunction(name, astDefn=astDefn,
                                                       typeParameters=implicitTypeParams,
                                                       variables=[], flags=flags)
                self.makeConstructor(irDefn, irScopeDefn)
                irScopeDefn.constructors.append(irDefn)
            else:
                name = self.makeName(astDefn.name)
                if STATIC in flags:
                    checkFlags(flags, frozenset([STATIC, PUBLIC, PROTECTED, PRIVATE, NATIVE]),
                               astDefn.location)
                    flags |= frozenset([METHOD])
                    irDefn = self.info.package.addFunction(name, sourceName=astDefn.name,
                                                           astDefn=astDefn,
                                                           typeParameters=implicitTypeParams,
                                                           variables=[], flags=flags,
                                                           definingClass=irScopeDefn)
                else:
                    checkFlags(flags, frozenset([ABSTRACT, FINAL, PUBLIC,
                                                 PROTECTED, PRIVATE, OVERRIDE, NATIVE]),
                               astDefn.location)
                    irDefn = self.info.package.addFunction(name, sourceName=astDefn.name,
                                                           astDefn=astDefn,
                                                           typeParameters=implicitTypeParams,
                                                           variables=[], flags=flags)
                    self.makeMethod(irDefn, irScopeDefn)
                irScopeDefn.methods.append(irDefn)
        elif isinstance(astDefn, ast.PrimaryConstructorDefinition):
            name = self.makeName(ir.CONSTRUCTOR_SUFFIX)
            checkFlags(flags, frozenset([PUBLIC, PROTECTED, PRIVATE]), astDefn.location)
            if len(flags & frozenset([PUBLIC, PROTECTED, PRIVATE])) == 0:
                flags |= irScopeDefn.flags & frozenset([PUBLIC, PROTECTED, PRIVATE])
            implicitTypeParams = self.getImplicitTypeParameters()
            irDefn = self.info.package.addFunction(name, astDefn=astDefn,
                                                   typeParameters=implicitTypeParams,
                                                   variables=[], flags=flags)
            self.makeConstructor(irDefn, irScopeDefn)
            irScopeDefn.constructors.append(irDefn)
        elif isinstance(astDefn, ast.TypeParameter):
            name = self.makeName(astDefn.name)
            checkFlags(flags, frozenset([STATIC, COVARIANT, CONTRAVARIANT]), astDefn.location)
            if STATIC not in flags:
                raise NotImplementedError()
            flags |= irScopeDefn.flags & frozenset([PUBLIC, PROTECTED, PRIVATE])
            irDefn = self.info.package.addTypeParameter(name, sourceName=astDefn.name,
                                                        astDefn=astDefn, flags=flags)
            irDefn.clas = irScopeDefn
            irScopeDefn.typeParameters.append(irDefn)
            irScopeDefn.initializer.typeParameters.append(irDefn)
            for ctor in irScopeDefn.constructors:
                ctor.typeParameters.append(irDefn)
            isVisible = False
        elif isinstance(astDefn, ast.Parameter):
            # Parameters in a class scope can only belong to a primary constructor. They are
            # never treated as local variables. Note that these parameters usually result in
            # some field definitions. Those definitions will be created separately on
            # pattern nodes.
            name = self.makeName(astDefn.pattern.name) \
                   if isinstance(astDefn.pattern, ast.VariablePattern) \
                   else self.makeName(ir.ANON_PARAMETER_SUFFIX)
            irDefn = ir.Variable(name, sourceName=astDefn.pattern.name, astDefn=astDefn,
                                 kind=ir.PARAMETER, flags=frozenset([LET]))
            irCtor = self.info.getDefnInfo(self.ast.constructor).irDefn
            irCtor.variables.append(irDefn)
            shouldBind = False
        elif isinstance(astDefn, ast.ClassDefinition):
            irDefn, shouldBind = self.createIrClassDefn(astDefn)
        elif isinstance(astDefn, ast.ArrayElementsStatement):
            checkFlags(flags, frozenset([FINAL]), astDefn.location)
            irScopeDefn.flags |= frozenset([ARRAY])
            if FINAL in flags:
                irScopeDefn.flags |= frozenset([ARRAY_FINAL])
            name = self.makeName(ir.ARRAY_LENGTH_SUFFIX)
            irDefn = self.info.package.newField(name, astDefn=astDefn, type=I32Type,
                                                flags=frozenset([PRIVATE, LET, ARRAY]))
            irScopeDefn.fields.append(irDefn)
            shouldBind = False
        else:
            assert isinstance(astDefn, ast.ArrayAccessorDefinition)
            name = self.makeName(astDefn.name)
            checkFlags(flags, frozenset([FINAL, PUBLIC, PROTECTED, PRIVATE, OVERRIDE]),
                       astDefn.location)
            flags |= frozenset([ARRAY])
            implicitTypeParams = self.getImplicitTypeParameters()
            irDefn = self.info.package.addFunction(name, sourceName=astDefn.name,
                                                   astDefn=astDefn,
                                                   typeParameters=implicitTypeParams,
                                                   variables=[],
                                                   flags=flags)
            self.makeMethod(irDefn, irScopeDefn)
            irScopeDefn.methods.append(irDefn)
        return irDefn, shouldBind, isVisible

    def canImport(self, defnInfo):
        if not super(ClassScope, self).canImport(defnInfo):
            return False
        flags = defnInfo.irDefn.flags
        return PRIVATE not in flags and PROTECTED not in flags and STATIC in flags

    def isDefinedAutomatically(self, astDefn):
        return isinstance(astDefn, ast.PrimaryConstructorDefinition) or \
               isinstance(astDefn, ast.FunctionDefinition) or \
               isinstance(astDefn, ast.ClassDefinition) or \
               isinstance(astDefn, ast.VariableDefinition) or \
               isinstance(astDefn, ast.VariablePattern)

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

    def newLocalScope(self, prefix, ast):
        return FunctionScope(prefix, ast, self)

    def newScopeForFunction(self, prefix, ast):
        scope = FunctionScope(prefix, ast, self)
        irDefn = scope.getIrDefn()
        if irDefn.isMethod():
            scope.configureAsMethod(self.scopeId, self.getIrDefn())
        return scope

    def newScopeForClass(self, prefix, ast):
        return ClassScope(prefix, ast, self)

    def newScopeForTrait(self, prefix, ast):
        return TraitScope(prefix, ast, self)


class TraitScope(Scope):
    def __init__(self, prefix, ast, parent):
        super(TraitScope, self).__init__(prefix, ast, ScopeId(ast.id), parent, parent.info)
        irDefn = self.getIrDefn()
        self.info.setScope(irDefn.id, self)
        contextInfo = self.info.getContextInfo(self.scopeId)
        contextInfo.irContextClass = irDefn

    def getClass(self):
        return getIrDefn()

    def createIrDefn(self, astDefn, astVarDefn):
        irScopeDefn = self.getIrDefn()
        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        shouldBind = True
        isVisible = True
        if isinstance(astDefn, ast.VariablePattern):
            raise NotImplementedError()
        elif isinstance(astDefn, ast.FunctionDefinition):
            if astDefn.body is not None:
                if ABSTRACT in flags:
                    raise ScopeException(astDefn.location,
                                         "%s: abstract method must not have body" %
                                         astDefn.name)
                if NATIVE in flags:
                    raise ScopeException(astDefn.location,
                                         "%s: native method must not have body")
            else:
               flags |= frozenset([ABSTRACT])

            if astDefn.name == "this":
                raise ScopeException(astDefn.location,
                                     "constructors may not be defined for traits")

            name = self.makeName(astDefn.name)
            implicitTypeParams = self.getImplicitTypeParameters()
            if STATIC in flags:
                checkFlags(flags, frozenset([STATIC, PUBLIC, PROTECTED, PRIVATE, NATIVE]),
                           astDefn.location)
                flags |= frozenset([METHOD])
                irDefn = self.info.package.addFunction(name, sourceName=astDefn.name,
                                                       astDefn=astDefn,
                                                       typeParameters=implicitTypeParams,
                                                       variables=[], flags=flags,
                                                       definingClass=irScopeDefn)
            else:
                checkFlags(flags, frozenset([ABSTRACT, FINAL, PUBLIC, PROTECTED, PRIVATE,
                                             OVERRIDE, NATIVE]),
                           astDefn.location)
                irDefn = self.info.package.addFunction(name, sourceName=astDefn.name,
                                                       astDefn=astDefn,
                                                       typeParameters=implicitTypeParams,
                                                       variables=[], flags=flags)
                self.makeMethod(irDefn, irScopeDefn)
            irScopeDefn.methods.append(irDefn)
        else:
            assert isinstance(astDefn, ast.TypeParameter)
            name = self.makeName(astDefn.name)
            checkFlags(flags, frozenset([STATIC, COVARIANT, CONTRAVARIANT]), astDefn.location)
            if STATIC not in flags:
                raise NotImplementedError()
            flags |= irScopeDefn.flags & frozenset([PUBLIC, PROTECTED, PRIVATE])
            irDefn = self.info.package.addTypeParameter(name, sourceName=astDefn.name,
                                                        astDefn=astDefn, flags=flags)
            irDefn.clas = irScopeDefn
            irScopeDefn.typeParameters.append(irDefn)
            isVisible = False
        return irDefn, shouldBind, isVisible

    def canImport(self, defnInfo):
        if not super(TraitScope, self).canImport(defnInfo):
            return False
        flags = defnInfo.irDefn.flags
        return PRIVATE not in flags and PROTECTED not in flags and STATIC in flags

    def isDefinedAutomatically(self, astDefn):
        return isinstance(astDefn, ast.FunctionDefinition)

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

    def newLocalScope(self, prefix, ast):
        return FunctionScope(prefix, ast, self)

    def newScopeForFunction(self, prefix, ast):
        scope = FunctionScope(prefix, ast, self)
        irDefn = scope.getIrDefn()
        if irDefn.isMethod():
            scope.configureAsMethod(self.scopeId, self.getIrDefn())
        return scope

    def newScopeForClass(self, prefix, ast):
        return ClassScope(prefix, ast, self)

    def newScopeForTrait(self, prefix, ast):
        return TraitScope(prefix, ast, self)


class ExistentialTypeScope(Scope):
    def __init__(self, prefix, ast, parent):
        super(ExistentialTypeScope, self).__init__(prefix, ast, ScopeId(ast.id),
                                                   parent, parent.info)

    def createIrDefn(self, astDefn, astVarDefn):
        flags = getFlagsFromAstDefn(astDefn, astVarDefn)
        if isinstance(astDefn, ast.TypeParameter):
            name = self.makeName(astDefn.name)
            if len(flags) > 0:
                raise ScopeException(astDefn.location, "invalid flags: %s" % ", ".join(flags))
            irDefn = self.info.package.addTypeParameter(name, sourceName=astDefn.name,
                                                        astDefn=astDefn, flags=flags)
        else:
            raise NotImplementedError()

        shouldBind = True
        isVisible = False
        return irDefn, shouldBind, isVisible

    def isDefinedAutomatically(self, astDefn):
        return True

    def findEnclosingClass(self):
        return None

    def requiresCapture(self):
        # Types in this scope may be used by inner (existential) scopes.
        return False

    def captureScopeContext(self):
        # Nothing can be captured.
        raise NotImplementedError()

    def captureInContext(self, defnInfo, irContextClass):
        # Nothing can be captured.
        raise NotImplementedError()

    def makeClosure(self):
        # Nothing can be captured.
        raise NotImplementedError()

    def newLocalScope(self, prefix, ast):
        # Not syntactically possible.
        raise NotImplementedError()

    def newScopeForFunction(self):
        # Not syntactically possible.
        raise NotImplementedError()

    def newScopeForClass(self):
        # Not syntactically possible.
        raise NotImplementedError()

    def newScopeForTrait(self):
        # Not syntactically possible.
        raise NotImplementedError()


class BuiltinGlobalScope(Scope):
    def __init__(self, parent):
        super(BuiltinGlobalScope, self).__init__([], None, BUILTIN_SCOPE_ID,
                                                 parent, parent.info)
        def bind(name, irDefn):
            defnInfo = DefnInfo(irDefn, self.scopeId, isVisible=True)
            if isinstance(irDefn, ir.Class):
                # This scope will automatically be registered.
                BuiltinClassScope(defnInfo, self)
            self.bind(name.short(), defnInfo)
            self.define(name.short())
        registerBuiltins(bind)

    def requiresCapture(self):
        return False


class BuiltinClassScope(Scope):
    def __init__(self, classDefnInfo, parent):
        irClass = classDefnInfo.irDefn
        super(BuiltinClassScope, self).__init__(
            [], None, ScopeId("builtin-" + irClass.name.short()), parent, parent.info)
        self.info.setScope(irClass.id, self)
        self.defnInfo = classDefnInfo
        if not hasattr(irClass, "isPrimitive"):
            for ctor in irClass.constructors:
                defnInfo = DefnInfo(ctor, self.scopeId, True, self.scopeId, NOT_HERITABLE)
                self.bind(ir.CONSTRUCTOR_SUFFIX, defnInfo)
                self.define(ir.CONSTRUCTOR_SUFFIX)
        for method in irClass.methods:
            methodClass = method.definingClass
            inheritedScopeId = self.info.getScope(methodClass.id).scopeId
            inheritanceDepth = irClass.findDistanceToBaseClass(methodClass)
            defnInfo = DefnInfo(method, self.scopeId, True, inheritedScopeId, inheritanceDepth)
            self.bind(method.name.short(), defnInfo)
            self.define(method.name.short())
        for field in irClass.fields:
            defnInfo = DefnInfo(field, self.scopeId, True)
            self.bind(field.name.short(), defnInfo)
            self.define(field.name.short())

    def getDefnInfo(self):
        return self.defnInfo

    def getClass(self):
        return self.getIrDefn()

    def requiresCapture(self):
        return False


class PackageScope(Scope):
    def __init__(self, scopeId, parent, info, packageNames, prefix, package):
        assert package is None or \
               isinstance(package, ir.Package) or \
               isinstance(package, ir.PackagePrefix)
        super(PackageScope, self).__init__([], None, scopeId, parent, info)
        self.packageNames = []
        self.prefix = prefix
        self.package = package
        self.prefixScopes = {}

        if isinstance(package, ir.Package):
            info.setScope(package.id, self)
            exportedDefns = []
            exportedDefns.extend(g for g in package.globals if PUBLIC in g.flags)
            exportedDefns.extend(f for f in package.functions
                                 if PUBLIC in f.flags and METHOD not in f.flags)
            exportedClasses = [c for c in package.classes if PUBLIC in c.flags]
            exportedDefns.extend(exportedClasses)
            for defn in exportedDefns:
                defnInfo = DefnInfo(defn, scopeId, True)
                self.bind(defn.name.short(), defnInfo)
                self.define(defn.name.short())
            for clas in exportedClasses:
                scope = ExternClassScope(ScopeId(clas.name), self, info, clas)
                self.info.setScope(clas.id, scope)

        packageBindings = {}
        for name in packageNames:
            if name.hasPrefix(prefix):
                nextComponent = name.components[len(prefix)]
                if self.isBound(nextComponent):
                    continue

                self.packageNames.append(name)
                nextPrefix = list(prefix) + [nextComponent]
                package = ir.PackagePrefix(ir.Name(nextPrefix))
                if nextComponent not in packageBindings or \
                   (isinstance(packageBindings[nextComponent], ir.PackagePrefix) and \
                    isinstance(package, ir.Package)):
                    packageBindings[nextComponent] = package

        for component, prefix in packageBindings.iteritems():
            defnInfo = DefnInfo(prefix, self.scopeId, True)
            self.bind(component, defnInfo)
            self.define(component)

    def isForeign(self):
        return self.package is not None

    def scopeForPrefix(self, component, loc):
        if component in self.prefixScopes:
            return self.prefixScopes[component]

        defnInfo = self.bindings[component].getDefnInfo()
        name = defnInfo.irDefn.name
        if name in self.packageNames and isinstance(defnInfo.irDefn, ir.PackagePrefix):
            defnInfo.irDefn = self.info.packageLoader.loadPackage(name, loc)

        scope = PackageScope(ScopeId(str(name)), self, self.info,
                             self.packageNames, name.components, defnInfo.irDefn)
        self.prefixScopes[component] = scope
        return scope

    def getDefinition(self, name):
        nameInfo = super(PackageScope, self).getDefinition(name)
        if nameInfo is None:
            return None
        defnInfo = nameInfo.getDefnInfo()
        irDefn = defnInfo.irDefn
        if isinstance(irDefn, ir.PackagePrefix):
            packageName = ir.Name(self.prefix + [name])
            if packageName in self.packageNames:
                defnInfo.irDefn = self.info.packageLoader.loadPackage(packageName, NoLoc)
                self.scopeForPrefix(name, NoLoc)  # force scope to be created
        return nameInfo

    def requiresCapture(self):
        return False


class ExternClassScope(Scope):
    def __init__(self, scopeId, parent, info, clas):
        super(ExternClassScope, self).__init__(clas.name.components, None,
                                               scopeId, parent, info)
        for ctor in clas.constructors:
            if PUBLIC in ctor.flags:
                defnInfo = DefnInfo(ctor, self.scopeId, isVisible=True)
                self.bind(ir.CONSTRUCTOR_SUFFIX, defnInfo)
                self.define(ir.CONSTRUCTOR_SUFFIX)
        for member in clas.methods + clas.fields:
            if PUBLIC in member.flags:
                defnInfo = DefnInfo(member, self.scopeId, isVisible=True)
                self.bind(member.name.short(), defnInfo)
                self.define(member.name.short())

    def isForeign(self):
        return True

    def requiresCapture(self):
        return True


class ScopeVisitor(ast.NodeVisitor):
    """Abstract base class for scope analysis related visitor.

    This class takes care of the common tasks of entering lexical scopes in the AST. visit
    methods can be implemented or overriden to provide functionality."""

    def __init__(self, scope):
        self.scope = scope
        self.info = self.scope.info

    def createChildVisitor(self, scope):
        """Create a new visitor for a descendant scope. Subclasses must override."""
        raise NotImplementedError()

    def visitVariableDefinition(self, node):
        if node.expression is not None:
            self.visit(node.expression)
        self.visit(node.pattern, node)

    def visitFunctionDefinition(self, node):
        scopeName = ir.CONSTRUCTOR_SUFFIX if node.name == "this" else node.name
        scope = self.scope.scopeForFunction(scopeName, node)
        visitor = self.createChildVisitor(scope)
        visitor.visitChildren(node)

    def visitClassDefinition(self, node):
        scope = self.scope.scopeForClass(node.name, node)
        visitor = self.createChildVisitor(scope)
        visitor.visitChildren(node)

    def visitTraitDefinition(self, node):
        scope = self.scope.scopeForTrait(node.name, node)
        visitor = self.createChildVisitor(scope)
        visitor.visitChildren(node)

    def visitTypeParameter(self, node):
        if node.upperBound is not None:
            self.visit(node.upperBound)
        if node.lowerBound is not None:
            self.visit(node.lowerBound)

    def visitParameter(self, node):
        self.visit(node.pattern, node)

    def visitVariablePattern(self, node, astVarDefn):
        # Avoid passing `astVarDefn` to type visiting methods.
        self.visitChildren(node)

    def visitBlankPattern(self, node, astVarDefn):
        # Avoid passing `astVarDefn` to type visiting methods.
        self.visitChildren(node)

    def visitBlockExpression(self, node):
        if isinstance(self.scope.ast, ast.FunctionDefinition) and \
           self.scope.ast.body is node:
            self.visitChildren(node)
        else:
            scope = self.scope.localScope(node)
            visitor = self.createChildVisitor(scope)
            visitor.visitChildren(node)

    def visitPartialFunctionCase(self, node):
        scope = self.scope.localScope(node)
        visitor = self.createChildVisitor(scope)
        visitor.visit(node.pattern, node)
        if node.condition is not None:
            visitor.visit(node.condition)
        visitor.visit(node.expression)

    def visitExistentialType(self, node):
        scope = self.scope.scopeForExistential(node)
        visitor = self.createChildVisitor(scope)
        visitor.visitChildren(node)

    def visitDefault(self, node, *args):
        self.visitChildren(node, *args)


class DeclarationVisitor(ScopeVisitor):
    """Calls Scope.declare for each definition visited.

    If a definition is visited which has its own lexical scope, a new Scope object is created,
    and analyzeDeclarations is called on that."""

    def createChildVisitor(self, scope):
        return DeclarationVisitor(scope)

    def visitFunctionDefinition(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitFunctionDefinition(node)

    def visitClassDefinition(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitClassDefinition(node)

    def visitTraitDefinition(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitTraitDefinition(node)

    def visitPrimaryConstructorDefinition(self, node):
        self.scope.declare(node)
        self.visitChildren(node)

    def visitArrayElementsStatement(self, node):
        self.scope.declare(node)
        self.visitChildren(node)

    def visitArrayAccessorDefinition(self, node):
        self.scope.declare(node)

    def visitImportStatement(self, node):
        self.scope.addImport(node)

    def visitTypeParameter(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitTypeParameter(node)

    def visitParameter(self, node):
        self.scope.declare(node)
        super(DeclarationVisitor, self).visitParameter(node)

    def visitVariablePattern(self, node, astVarDefn):
        if node.ty is not None:
            self.visit(node.ty)
        self.scope.declare(node, astVarDefn)


def getFlagsFromAstDefn(astDefn, astVarDefn):
    flags = set()
    if isinstance(astDefn, ast.Definition):
        attribs = astDefn.attribs
    elif isinstance(astVarDefn, ast.Definition):
        attribs = astVarDefn.attribs
    else:
        attribs = []

    if isinstance(astDefn, ast.VariablePattern) and \
       not ((isinstance(astVarDefn, ast.VariableDefinition) and astVarDefn.keyword == "var") or \
            (isinstance(astVarDefn, ast.Parameter) and astVarDefn.var == "var")):
        flags.add(LET)

    if isinstance(astDefn, ast.TypeParameter):
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

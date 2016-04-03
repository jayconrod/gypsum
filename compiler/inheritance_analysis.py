# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import ast
from builtins import registerBuiltins, getNothingClass
from bytecode import BUILTIN_ROOT_CLASS_ID
from compile_info import NOT_HERITABLE
from errors import InheritanceException
from graph import Graph
from flags import *
from ids import GLOBAL_SCOPE_ID
import ir
from ir_types import ClassType, ExistentialType, VariableType, getRootClassType
from location import NoLoc
from scope_analysis import ScopeVisitor
from utils import each


def analyzeInheritance(info):
    """Constructs an analyzes the graph of inheritance between classes, traits, and
    type parameters.

    This pass runs after type declaration analysis, which assigns direct supertypes to
    classes and traits and upper/lower bounds to type parameters. This pass accomplishes
    the following tasks:

        1. Builds a subtype graph where the vertices are type definitions and the edges are
           direct subtype relations. Checks that the subtype graph contains no cycles. If
           it did contain cycles, we could have an infinite loop of inheritance, and
           Type.isSubtypeOf would not be a partial order.
        2. Copies bindings into class and trait scopes from the scopes of their bases.
           This is how inherited definitions become visible. If inherited definitions have the
           same names as other definitions, they are treated as overloads. Overrides are
           resolved later, once all types are known.
        3. Builds the full list of supertypes for classes and traits. This is compiled into
           the packages, since it's needed for many type and trait operations to be efficient.
    """

    # Check the inheritance graph for cycles. We only explicitly add nodes for definitions in
    # the package being compiled, but nodes from supertypes outside the package may also end
    # up there. Since cyclic package dependencies are not allowed, we don't worry about
    # subtype cycles across packages.
    subtypeGraph = buildSubtypeGraph(info)
    if subtypeGraph.isCyclic():
        # TODO: need to report an error for each cycle and say what types are in the cycle
        raise InheritanceException(NoLoc, "inheritance cycle detected")

    # Build full type lists for each definition. We process each class and trait in
    # topological order, so all supertype definitions are processed first.
    inheritanceGraph = buildInheritanceGraph(info)
    bases = buildFullTypeLists(info, inheritanceGraph)

    # Perform some type checks on type arguments for inherited types. These checks were
    # deferred from type declaration analysis. After these checks are performed, we can use
    # `Type.isSubtypeOf`.
    if info.typeCheckFunction is not None:
        info.typeCheckFunction()

    # Resolve overrides and copy inherited definitions into classes and traits from bases.
    copyInheritedDefinitions(info, inheritanceGraph, bases)


def buildSubtypeGraph(info):
    """Builds a directed graph of definitions and their subtype relations.

    Each class, trait, and type parameter defined in the package being compiled is a node in
    this graph. Edges go from classes and traits to their base classes and traits. Type
    parameters also have edges from their lower bounds and to their upper bounds.
    """
    subtypeGraph = Graph()

    def addTypeDefn(irTypeDefn):
        subtypeGraph.addVertex(irTypeDefn.id)
        if isinstance(irTypeDefn, ir.ObjectTypeDefn):
            for supertype in irTypeDefn.supertypes:
                if supertype.isNullable():
                    raise InheritanceException(irTypeDefn.getLocation(),
                                               "%s: cannot inherit nullable type" %
                                               irTypeDefn.name)
                if supertype.clas is getNothingClass():
                    raise InheritanceException(irTypeDefn.getLocation(),
                                               "%s: cannot inherit Nothing" %
                                               irTypeDefn.name)
                supertypeId = getIdForType(supertype)
                if irTypeDefn.id is supertypeId:
                    raise InheritanceException(irTypeDefn.getLocation(),
                                               "%s: cannot inherit from itself" %
                                               irTypeDefn.name)
                subtypeGraph.addEdge(irTypeDefn.id, supertypeId)
        elif isinstance(irTypeDefn, ir.TypeParameter):
            upperBoundId = getIdForType(irTypeDefn.upperBound)
            if irTypeDefn.id is upperBoundId:
                raise InheritanceException(irTypeDefn.getLocation(),
                                           "%s: cannot be upper bounded by itself" %
                                           irTypeDefn.name)
            subtypeGraph.addEdge(irTypeDefn.id, upperBoundId)
            lowerBoundId = getIdForType(irTypeDefn.lowerBound)
            if irTypeDefn.id is lowerBoundId:
                raise InheritanceException(irTypeDefn.getLocation(),
                                           "%s: cannot be lower bounded by itself" %
                                           irTypeDefn.name)
            subtypeGraph.addEdge(lowerBoundId, irTypeDefn.id)
        else:
            raise NotImplementedError()

    each(addTypeDefn, info.package.classes)
    each(addTypeDefn, info.package.traits)
    each(addTypeDefn, info.package.typeParameters)

    return subtypeGraph


def buildInheritanceGraph(info):
    """Builds a graph of definitions for inheritance.

    Each class and trait defined in the package being compiled has a node in this graph. There
    are also nodes for classes and traits in other packages that definitions in the package
    being compiled inherit. Edges go from base definitions to inheriting definitions (this
    is the opposite direction compared to the subtype graph.
    """
    inheritanceGraph = Graph()
    for irDefn in info.package.classes + info.package.traits:
        inheritanceGraph.addVertex(irDefn.id)
        for supertype in irDefn.supertypes:
            inheritanceGraph.addEdge(getIdForType(supertype), irDefn.id)
    return inheritanceGraph


def buildFullTypeLists(info, inheritanceGraph):
    """Builds full `supertypes` lists for each class and trait in the package being compiled.

    Each class and trait has a `supertypes` list, which contains exactly one type for each
    definition it inherits from, transitively. The types are ordered by visiting definitions
    in depth-first pre-order from left to right. This list is used in `Type.isSubtypeOf` among
    other things.

    Returns:
        ({DefnId: [DefnId]}): a list of direct base definitions for each class or trait.
        Redundant inheritances are removed. For example, if Foo inherits Bar and Baz, but
        Bar already inherits Baz, the list for Foo will just contain Bar.
    """
    topologicalIds = inheritanceGraph.topologicalSort()
    bases = {}
    for id in topologicalIds:
        if not id.isLocal():
            continue
        irDefn = info.package.getDefn(id)
        bases[id] = []

        # This maps class and trait ids to inherited types. It's possible to inherit a class or
        # trait through multiple paths (diamond inheritance). This is used to make sure the
        # types that are inherited are the same along all paths.
        inheritedTypeMap = {}

        # This will be the full list of inherited types. Types are added in depth-first
        # pre-order, but since each base has been processed earlier, we don't need to do a
        # full graph traversal to build this.
        inheritedTypes = []

        # Check that we don't explicitly inherit the same definition more than once.
        explicitInheritedIds = set()
        for supertype in irDefn.supertypes:
            if supertype.clas.id in explicitInheritedIds:
                raise InheritanceException(irDefn.getLocation(),
                                           "%s: inherited same definition more than once: %s" %
                                           (irDefn.name, supertype.clas.name))
            explicitInheritedIds.add(supertype.clas.id)

        # Ensure that the first inherited type is from a class. This need not be explicit in
        # source code. For classes, if the first supertype in source code is a trait, the
        # real first supertype is the root type. For traits, the real first supertype will be
        # the first supertype of the first inherited trait.
        supertypes = []
        assert len(irDefn.supertypes) > 0
        if isinstance(irDefn.supertypes[0].clas, ir.Trait):
            if isinstance(irDefn, ir.Class):
                baseClassType = getRootClassType()
            else:
                baseClassType = irDefn.supertypes[0].clas.supertypes[0].substitute(
                    irDefn.supertypes[0].clas.typeParameters,
                    irDefn.supertypes[0].typeArguments)
            supertypes = [baseClassType] + irDefn.supertypes
        else:
            baseClassType = irDefn.supertypes[0]
            supertypes = irDefn.supertypes
        assert isinstance(baseClassType.clas, ir.Class)

        # Inherit supertypes.
        isFirstSupertype = True
        for supertype in supertypes:
            irSuperDefn = supertype.clas

            # Perform some basic checks.
            if FINAL in irSuperDefn.flags:
                raise InheritanceException(irDefn.getLocation(),
                                           "%s: cannot inherit from final class %s" %
                                           (irDefn.name, irSuperDefn.name))

            if not isFirstSupertype and isinstance(irSuperDefn, ir.Class):
                raise InheritanceException(irDefn.getLocation(),
                                           "%s: only first supertype may be a class" %
                                           irDefn.name)
            irSuperClass = irSuperDefn \
                           if isinstance(irSuperDefn, ir.Class) \
                           else irSuperDefn.supertypes[0].clas
            if not baseClassType.clas.isDerivedFrom(irSuperClass):
                raise InheritanceException(irDefn.getLocation(),
                                           "%s: base class %s of supertype %s not a superclass of base class %s" %
                                           (irDefn.name, irSuperClass.name,
                                            supertype, baseClassType.clas.name))
            isFirstSupertype = False

            if irSuperDefn.id in inheritedTypeMap:
                # We have already inherited this type along a different path. We do not need
                # to copy bindings.
                if inheritedTypeMap[irSuperDefn.id] != supertype:
                    raise InheritanceException(irDefn.getLocation(),
                                               "%s: inherited %s multiple times with different types" %
                                               (irDefn.name, irSuperDefn.name))
            else:
                # We have not inherited this type yet.
                inheritedTypeMap[irSuperDefn.id] = supertype
                inheritedTypes.append(supertype)
                bases[id].append(irSuperDefn.id)

                # Inherit types from the supertype ("ubertypes"). We don't need to do this
                # recursively we processed the supertype in a previous iteration. Its supertypes
                # list already contains everything it inherits from.
                for ubertype in irSuperDefn.supertypes:
                    irUberDefn = ubertype.clas
                    substitutedUbertype = supertype.substituteForBase(irUberDefn)
                    if irUberDefn.id in inheritedTypeMap:
                        # We have already inherited this definition along a different path.
                        if inheritedTypeMap[irUberDefn.id] != substitutedUbertype:
                            raise InheritanceException(irDefn.getLocation(),
                                                       "%s: inherited %s multiple times with different types" %
                                                       (irDefn.name, irUberDefn.name))
                    else:
                        # We have not inherited this definition yet.
                        inheritedTypeMap[irUberDefn.id] = substitutedUbertype
                        inheritedTypes.append(substitutedUbertype)

        irDefn.supertypes = inheritedTypes

    return bases


def copyInheritedDefinitions(info, inheritanceGraph, bases):
    """Resolves overrides and copies inherited definitions from bases to classes and traits.

    A method may override methods in inherited scopes if it has the same name and compatible
    types. Overriden methods are not inherited, so we resolve this first. We build the
    `overrides` list for functions here. Note that overriding methods must have the
    `override` attribute, and overriden methods must not be `final`. Note also that return
    types are not known until after type definition analysis, so return type compatibility
    is checked there.

    After overrides are resolved, we inherit bindings from direct bases. This is done in
    topological order and we use `bases` which has redundant bases removed so we don't
    inherit the same definition more than once.

    For classes specifically, we also copy the ARRAY and ARRAY_FINAL flags from base classes.
    """
    topologicalIds = inheritanceGraph.topologicalSort()
    for id in topologicalIds:
        if not id.isLocal():
            continue
        scope = info.getScope(id)
        superScopes = [info.getScope(baseId) for baseId in bases[id]]
        overridenIds = set()

        for name, defnInfo in scope.iterBindings():
            if name == "this":
                continue
            irDefn = defnInfo.irDefn
            overrides = []
            for superScope in superScopes:
                nameInfo = superScope.getDefinition(name)
                if nameInfo is None or not nameInfo.isHeritable():
                    continue
                if not nameInfo.isOverloadable(defnInfo):
                    raise InheritanceException(irDefn.astDefn.location,
                                               "%s: cannot overload definition in base" % name)
                if isinstance(irDefn, ir.Function) and \
                   not irDefn.isConstructor() and \
                   not STATIC in irDefn.flags:
                    for superDefnInfo in nameInfo.overloads:
                        assert superDefnInfo.isHeritable()
                        superIrDefn = superDefnInfo.irDefn
                        if irDefn.mayOverride(superIrDefn):
                            if superIrDefn.isFinal():
                                raise InheritanceException(irDefn.astDefn.location,
                                                           "%s: cannot override a final method" %
                                                           name)
                            if id in superIrDefn.overridenBy:
                                raise InheritanceException(irDefn.astDefn.location,
                                                           "%s: multiple methods in this class override the same base method" %
                                                           name)
                            overrides.append(superIrDefn)
                            overridenIds.add(superIrDefn.id)
                            superIrDefn.overridenBy[id] = irDefn
            if OVERRIDE in irDefn.flags and len(overrides) == 0:
                raise InheritanceException(irDefn.astDefn.location,
                                           "%s: doesn't actually override anything" % name)
            if OVERRIDE not in irDefn.flags and len(overrides) > 0:
                raise InheritanceException(irDefn.astDefn.location,
                                           "%s: overrides methods without `override` attribute" %
                                           name)
            if len(overrides) > 0:
                irDefn.overrides = overrides

        for superScope in superScopes:
            for name, defnInfo in superScope.iterBindings():
                if not defnInfo.isHeritable() or \
                   (isinstance(defnInfo.irDefn, ir.IrTopDefn) and \
                    defnInfo.irDefn.id in overridenIds):
                    continue
                scope.bind(name, defnInfo.inherit(scope.scopeId))

        irScopeDefn = scope.getIrDefn()
        if isinstance(irScopeDefn, ir.Class):
            superclass = irScopeDefn.superclass()
            if ARRAY in superclass.flags:
                irScopeDefn.flags |= frozenset([ARRAY])
            if ARRAY_FINAL in superclass.flags:
                irScopeDefn.flags |= frozenset([ARRAY_FINAL])


def getIdForType(ty):
    if isinstance(ty, ClassType):
        return ty.clas.id
    elif isinstance(ty, VariableType):
        return ty.typeParameter.id
    elif isinstance(ty, ExistentialType):
        return getIdForType(id.id)
    else:
        raise NotImplementedError()

# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from copy import copy

from graph import Graph
from ids import DefnId
import ir
from utils import each


def flattenTypeDefinitions(info):
    """Builds final field lists, method lists, and trait method lists for classes and traits
    by copying from base classes and traits.

    Until this point, IR classes and traits have only contained definitions defined inside
    them. Inherited definitions have been made available for type and use analysis through
    `Scope` objects.

    In order for semantic analysis to determine accurate field and method vtable indices,
    we need to actually copy the inherited definitions into derived classes. The VM needs
    these lists, too, so they will be serialized. We perform this flattening for both classes
    and traits. Although the VM never directly calls a method in a trait, we still need the
    flat method list to build external trait scopes when a trait is loaded from a package.
    """

    # Build an inheritance graph containing local definitions. We may have introduced new
    # classes and traits since inheritance analysis (due to closure conversion), so we can't
    # reuse the graph created in that phase.
    inheritanceGraph = Graph()
    def addDefnToGraph(defn):
        inheritanceGraph.addVertex(defn.id)
        for sty in defn.supertypes:
            if sty.clas.isLocal():
                inheritanceGraph.addEdge(sty.clas.id, defn.id)
    each(addDefnToGraph, info.package.classes)
    each(addDefnToGraph, info.package.traits)

    # Process the definitions in topological order.
    topologicalIds = inheritanceGraph.topologicalSort()
    for id in topologicalIds:
        assert id.isLocal()
        flattenTypeDefinition(info.package.getDefn(id))


def flattenTypeDefinition(defn):
    """Builds and sets a flat method list for a class or trait. For classes, this also builds
    and sets trait method lists and fields.
    """
    ownMethods, baseMethods = flattenMethodList(defn)
    defn.methods = ownMethods
    if isinstance(defn, ir.Class):
        defn.traits = {baseId: methods for baseId, methods
                       in baseMethods.iteritems()
                       if baseId.kind is DefnId.TRAIT}
        defn.fields = flattenFields(defn)


def flattenMethodList(defn):
    """Builds and returns flat method list and base method lists for a class or trait.

    Arguments:
        defn (ObjectTypeDefn): the definition to process.

    Returns:
        ([Function], {DefnId: [Function]}): A flat method list that can be used directly in
        a class or trait, followed by a dictionary mapping base definition ids (of inherited
        classes and traits) to method lists. The base method lists may be assigned to class
        trait method maps after they are filtered to only include traits.
    """

    # Maps class and trait ids to method lists for those bases. We return this at the end.
    superMethods = {}  # {DefnId (CLASS|TRAIT): [Function]}

    # Maps ids of non-overriding methods (returned by getOverridenMethodIds) to lists of
    # slots where those methods are referenced (possibly by overrides). When we override a
    # method, we look in here for all the places we need to replace it.
    superMethodIndices = {}  # {DefnId (FUNCTION): [(DefnId (CLASS|TRAIT), int)]}

    # The method list for this definition. We return this at the end.
    ownMethods = []  # [Function]

    # Maps ids of non-overriding methods (returned by getOverridenMethodIds) to slots in
    # `ownMethods` that contain those methods or overrides. This works similarly to
    # `superMethodIndices`, but for `ownMethods` instead of `superMethods`.
    ownMethodIndices = {}  # {DefnId (FUNCTION): [int]}

    # Build the initial `superMethods` and `superMethodIndices`. This is just copied from
    # supertype definitions.
    for supertype in defn.supertypes:
        superDefn = supertype.clas
        superMethods[superDefn.id] = list(superDefn.methods)
        for index, method in enumerate(superDefn.methods):
            for overrideId in getOverridenMethodIds(method):
                if overrideId not in superMethodIndices:
                    superMethodIndices[overrideId] = []
                superMethodIndices[overrideId].append((superDefn.id, index))

    # This function will either replace overriden methods with the given method, or will
    # append the method onto the end of the method list.
    def addMethod(method, defnId):
        indices = []
        if method.overrides is None:
            # This method overrides nothing. Just append it to the end of the method list.
            ownMethods.append(method)
            ownMethodIndices[method.id] = [len(ownMethods) - 1]
        else:
            # This method overrides at least one other method.
            didOverrideOwnMethod = False
            for overrideId in getOverridenMethodIds(method):
                # Replace the method in function lists for each base.
                for baseId, index in superMethodIndices[overrideId]:
                    superMethods[baseId][index] = method

                # Replace the method in our own method list if it is there. There may be
                # no overriden method in our method list if we haven't found a path to inherit
                # the base method though.
                for index in ownMethodIndices.get(overrideId, ()):
                    ownMethods[index] = method
                    didOverrideOwnMethod = True

            if not didOverrideOwnMethod:
                ownMethods.append(method)
                index = len(ownMethods) - 1
                for overrideId in getOverridenMethodIds(method):
                    if overrideId not in ownMethodIndices:
                        ownMethodIndices[overrideId] = []
                    ownMethodIndices[overrideId].append(index)

    # Process overrides in each base definition. Make sure we don't process overrides
    # in a base definition we've already inherited indirectly.
    inheritedBaseIds = set()
    inheritedMethodIds = set()
    for supertype in defn.supertypes:
        baseDefn = supertype.clas
        if baseDefn.id in inheritedBaseIds:
            continue
        inheritedBaseIds.add(baseDefn.id)
        for baseSupertype in baseDefn.supertypes:
            inheritedBaseIds.add(baseSupertype.clas.id)

        for method in baseDefn.methods:
            if method.id in inheritedMethodIds:
                continue
            inheritedMethodIds.add(method.id)
            addMethod(method, baseDefn.id)

    # Process overrides for methods defined in this class or trait.
    for method in defn.methods:
        addMethod(method, defn.id)

    return ownMethods, superMethods


def flattenFields(clas):
    """Builds and returns a flat set of fields for a class. Type substitution is performed
    on inherited fields.

    Arguments:
        clas (Class): the class to process.

    Returns:
        ([Field]): a flat list of fields.
    """
    assert isinstance(clas, ir.Class)

    # Perform type substitution on inherited fields.
    superclass = clas.superclass()
    fields = []
    for field in superclass.fields:
        inheritedField = copy(field)
        field.type = field.type.substituteForInheritance(clas, superclass)
        fields.append(field)

    # Add our own fields.
    fields.extend(clas.fields)
    return fields


def getOverridenMethodIds(method):
    """Returns the ids of non-overriding methods that this method overrides, directly or
    indirectly.

    If this is called on a static or non-overriding method, it just returns the method's id.

    Returns:
        set(DefnId): non-overriding methods this method overrides.
    """
    if method.overrides is None:
        return set([method.id])
    else:
        overridenIds = set()
        for override in method.overrides:
            overridenIds.update(getOverridenMethodIds(override))
        return overridenIds

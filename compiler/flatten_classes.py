# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from copy import copy

from graph import Graph
from ids import DefnId
import ir
from utils import each


def flattenClasses(info):
    """Builds final field lists, method lists, and trait method lists for classes by copying
    from base classes and traits.

    Until this point, IR classes and traits have only contained definitions defined inside
    them. Inherited definitions have been made available for type and use analysis through
    `Scope` objects.

    In order for semantic analysis to determine accurate field and method vtable indices,
    we need to actually copy the inherited definitions into derived classes.

    We don't need to do this for traits. Traits just contain methods defined inside them.
    This is essentially an interface: these methods can only be called through classes.
    Each class has a list of methods for each trait it inherits, and these are the methods
    that actually get called. These lists are built here.
    """

    # Build a class inheritance graph. We need to process classes in topological order. Traits
    # do not get modified and are not part of this graph. Note that we may have introduced
    # new classes since inheritance analysis (due to closure conversion), so we can't reuse
    # the subtype graph from there.
    inheritanceGraph = Graph()
    for clas in info.package.classes:
        inheritanceGraph.addEdge(clas.superclass().id, clas.id)

    # Process the classes in topological order.
    topologicalIds = inheritanceGraph.topologicalSort()
    for id in topologicalIds:
        if not id.isLocal():
            # Builtin and foreign classes are already flattened.
            continue

        clas = info.package.classes[id.index]
        superclass = clas.superclass()

        # Keep track of where each method is. Both of these dictionaries are keyed by
        # non-overriding method ids.
        methodIndices = {}  # {DefnId: int}
        traitMethodIndices = {}  # {DefnId: [(DefnId, int)]}

        # Copy method list from base class. Keep track of the index of each method so overrides
        # can replace them.
        methods = copy(superclass.methods)
        for index, method in enumerate(methods):
            for overridenId in getOverridenMethodIds(method):
                assert overridenId not in methodIndices
                methodIndices[overridenId] = index

        # Copy trait method lists from all inherited traits. Keep track of the index of each
        # method in each trait.
        traitMethods = {}
        for sty in clas.supertypes:
            base = sty.clas
            if isinstance(base, ir.Trait):
                traitMethods[base.id] = copy(base.methods)
                for index, method in enumerate(traitMethods[base.id]):
                    for overridenId in getOverridenMethodIds(method):
                        if overridenId not in traitMethodIndices:
                            traitMethodIndices[overridenId] = []
                        traitMethodIndices[overridenId].append((base.id, index))

        # Keep track of which traits we've inherited methods from in the main method list
        # so far. We already have everything from the superclass.
        inheritedIds = set(uty.clas.id for uty in superclass.supertypes)
        inheritedIds.add(superclass.id)

        # Add methods from traits and from the class itself.
        def addMethod(method):
            if method.overrides is not None:
                didOverrideClassMethod = False
                overridenIds = getOverridenMethodIds(method)
                for overridenId in overridenIds:
                    if overridenId in methodIndices:
                        index = methodIndices[overridenId]
                        methods[index] = method
                        didOverrideClassMethod = True
                    for traitId, index in traitMethodIndices.get(overridenId, ()):
                        traitMethods[traitId][index] = method
                if not didOverrideClassMethod:
                    methods.append(method)
                    for overridenId in overridenIds:
                        methodIndices[overridenId] = len(methods) - 1
            else:
                methods.append(method)
                methodIndices[method.id] = len(methods) - 1


        for sty in clas.supertypes:
            base = sty.clas
            if base.id in inheritedIds:
                continue
            inheritedIds.update(uty.clas.id for uty in base.supertypes)
            inheritedIds.add(base.id)

            each(addMethod, base.methods)

        each(addMethod, clas.methods)

        # Copy field list from base class. We need to perform type substitution on these.
        fields = []
        for field in superclass.fields:
            inheritedField = copy(field)
            field.type = field.type.substituteForInheritance(clas, superclass)
            fields.append(field)

        # Add our own fields.
        fields.extend(clas.fields)

        # Set flattened attributes on the class.
        clas.fields = fields
        clas.methods = methods
        clas.traits = traitMethods


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

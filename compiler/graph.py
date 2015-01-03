# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


WHITE = "white"
GREY = "grey"
BLACK = "black"

class Graph(object):
    def __init__(self, vertices=None, edges=None):
        self.associations = {}
        if vertices is not None:
            for v in vertices:
                self.addVertex(v)
        if edges is not None:
            for v, w in edges:
                self.addEdge(v, w)

    def addVertex(self, v):
        if v not in self.associations:
            self.associations[v] = set()

    def addEdge(self, v, w):
        self.addVertex(v)
        self.addVertex(w)
        self.associations[v].add(w)

    def vertices(self):
        return self.associations.keys()

    def edges(self):
        for v in self.vertices():
            for w in self.neighbors(v):
                yield (v, w)

    def neighbors(self, v):
        return self.associations[v]

    def __eq__(self, other):
        return self.associations == other.associations

    def __ne__(self, other):
        return not (self == other)

    def __str__(self):
        return "\n".join("%s: %s" % (str(v), ", ".join(map(str, self.neighbors(v))))
                         for v in self.vertices())

    def __repr__(self):
        return "Graph(%s, %s)" % (repr(self.vertices()), repr(list(self.edges())))

    def __contains__(self, v):
        return v in self.associations

    def reverseEdges(self):
        """Returns a new graph with all the edges reversed.

        So if there is an edge v -> w in the original graph, the new graph will have w -> v."""
        V = self.vertices()
        E = list((w, v) for (v, w) in self.edges())
        return Graph(V, E)

    def stronglyConnectedComponents(self):
        """Tarjan's strongly connected components algorithm.

        Returns an acyclic graph where each vertex represents a set of vertices in this graph
        which form a strongly connnected component. This is basically copy-pasted from the
        Wikipedia pseudocode."""

        # Wrap state variables in a local context class due to the annoying lack of "nonlocal"
        # in Python 2.
        class Context(object): pass
        ctx = Context()
        ctx.index = 0
        ctx.indices = {}
        ctx.lowLinks = {}
        ctx.stack = []
        ctx.sccsToVertices = []
        ctx.verticesToSccs = {}

        def strongConnect(v):
            # Set the depth index for v to the smallest unused index.
            ctx.indices[v] = ctx.index
            ctx.lowLinks[v] = ctx.index
            ctx.index += 1
            ctx.stack.append(v)

            # Consider successors of v.
            for w in self.neighbors(v):
                if w not in ctx.indices:
                    # Successor w has not yet been visited; recurse on it.
                    strongConnect(w)
                    ctx.lowLinks[v] = min(ctx.lowLinks[v], ctx.lowLinks[w])
                elif w in ctx.stack:
                    # Successor w is in stack and hence in the current SCC.
                    ctx.lowLinks[v] = min(ctx.lowLinks[v], ctx.indices[w])

            # If v is a root node, pop the stack and generate an SCC.
            if ctx.lowLinks[v] == ctx.indices[v]:
                scc = set()
                sccId = len(ctx.sccsToVertices)
                ctx.sccsToVertices.append(scc)
                while True:
                    w = ctx.stack.pop()
                    scc.add(w)
                    ctx.verticesToSccs[w] = sccId
                    if w == v:
                        break

        for v in self.vertices():
            if v not in ctx.indices:
                strongConnect(v)

        sccGraph = Graph()
        for i in xrange(len(ctx.sccsToVertices)):
            sccVertex = frozenset(ctx.sccsToVertices[i])
            ctx.sccsToVertices[i] = sccVertex
            sccGraph.addVertex(sccVertex)
        for vSccId, vScc in enumerate(ctx.sccsToVertices):
            for v in vScc:
                for w in self.neighbors(v):
                    wSccId = ctx.verticesToSccs[w]
                    if vSccId != wSccId:
                        wScc = ctx.sccsToVertices[wSccId]
                        sccGraph.addEdge(vScc, wScc)

        return sccGraph

    def topologicalSort(self):
        """Returns a list of vertices in topological order."""
        # Collect a list of vertices with no incoming edges.
        roots = set(self.vertices())
        for v in self.vertices():
            for w in self.neighbors(v):
                roots.remove(w)

        # Generate a list by performing a depth-first-search on each root.
        colors = {v: WHITE for v in self.vertices()}
        order = []
        def visit(v):
            order.append(v)
        for root in roots:
            self.depthFirstSearch(root, visit, colors)
        assert len(order) == len(self.vertices())
        return order

    def depthFirstSearch(self, v, callback, colors=None):
        if colors is None:
            colors = {v: WHITE for v in self.vertices()}
        if colors[v] is not WHITE:
            return
        colors[v] = GREY
        callback(v)
        for w in self.neighbors(v):
            self.depthFirstSearch(w, callback, colors)
        colors[v] = BLACK

    def isCyclic(self):
        sccGraph = self.stronglyConnectedComponents()
        return len(sccGraph.vertices()) < len(self.vertices())

__all__ = ["Graph"]

# Copyright 2014,2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from graph import *

class TestGraph(unittest.TestCase):
    def testReverseEdges(self):
        G = Graph([1, 2], [(1, 2)])
        reversed = G.reverseEdges()
        expected = Graph([1, 2], [(2, 1)])
        self.assertEquals(expected, reversed)

    def testStronglyConnectedComponents(self):
        G = Graph([1, 2, 3, 4, 5],
                  [(1, 2), (2, 3), (3, 1), (3, 2), (4, 5)])
        SCCGraph = G.stronglyConnectedComponents()
        expectedSCCs = [frozenset([1, 2, 3]), frozenset([4]), frozenset([5])]
        a, b, c = expectedSCCs
        expectedSCCGraph = Graph(expectedSCCs,
                                 [(b, c)])
        self.assertEquals(expectedSCCGraph, SCCGraph)

    def testTopologicalSort(self):
        G = Graph([1, 2, 3, 4, 5],
                  [(1, 2), (2, 3), (2, 4), (3, 5), (4, 5)])
        sort = G.topologicalSort()
        self.assertTrue([1, 2, 3, 4, 5] == sort or [1, 2, 4, 3, 5] == sort)

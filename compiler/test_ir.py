# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from builtins import *
from ir import *
from ir_types import *


class TestIntermediateRepresentation(unittest.TestCase):
    registerBuiltins(lambda name, ir: None)

    def testFindCommonBaseClass(self):
        package = Package()
        baseClass = Class("Base", [], [getRootClassType()], None, [], [], [], frozenset())
        package.addClass(baseClass)
        baseTy = ClassType(baseClass)
        aClass = Class("A", [], [baseTy], None, [], [], [], frozenset())
        package.addClass(aClass)
        bClass = Class("B", [], [baseTy], None, [], [], [], frozenset())
        package.addClass(bClass)
        commonClass = aClass.findCommonBaseClass(bClass)
        self.assertIs(baseClass, commonClass)

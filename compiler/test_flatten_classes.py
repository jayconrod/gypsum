# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from lexer import *
from layout import layout
from parser import *
from ast import *
from scope_analysis import *
from type_analysis import *
from ids import *
from ir import *
from builtins import *
from compile_info import CompileInfo
from errors import *
from utils_test import MockPackageLoader

class TestFlattenClasses(unittest.TestCase):
    def setUp(self):
        self.rootMethodNames = [m.name.short() for m in getRootClass().methods]

    def analyzeFromSource(self, source):
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        package = Package(TARGET_PACKAGE_ID)
        packageLoader = MockPackageLoader([])
        info = CompileInfo(ast, package, packageLoader, isUsingStd=False)
        analyzeDeclarations(info)
        analyzeInheritance(info)
        analyzeTypes(info)
        convertClosures(info)
        flattenClasses(info)
        return info

    def testSimpleClass(self):
        info = self.analyzeFromSource("class C")
        C = info.package.findClass(name="C")
        self.assertEquals(self.rootMethodNames, [m.name.short() for m in C.methods])

    def testDerivedClassWithMethods(self):
        source = "class A\n" + \
                 "  def f = 12\n" + \
                 "class B <: A\n" + \
                 "  def g = 12\n"
        info = self.analyzeFromSource(source)
        B = info.package.findClass(name="B")
        self.assertEquals(self.rootMethodNames + ["f", "g"],
                          [m.name.short() for m in B.methods])

    def testDerivedClassWithOverloads(self):
        source = "class A\n" + \
                 "  def f(x: i32) = x\n" + \
                 "class B <: A\n" + \
                 "  def f(x: i64) = x\n"
        info = self.analyzeFromSource(source)
        B = info.package.findClass(name="B")
        self.assertEquals(self.rootMethodNames + ["f", "f"],
                          [m.name.short() for m in B.methods])

    def testDerivedClassWithOverride(self):
        source = "class A\n" + \
                 "  def f(x: i32) = x\n" + \
                 "class B <: A\n" + \
                 "  def f(x: i32) = 12i32\n"
        info = self.analyzeFromSource(source)
        B = info.package.findClass(name="B")
        self.assertEquals(self.rootMethodNames + ["f"],
                          [m.name.short() for m in B.methods])

    def testMustOverrideInheritedAbstractMethod(self):
        source = "abstract class A\n" + \
                 "  abstract def f: i64\n" + \
                 "class B <: A"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from ast import *
from compile_info import *
from ir import *
from layout import *
from lexer import *
from parser import *
from scope_analysis import *

class TestInheritanceAnalysis(unittest.TestCase):
    def parseFromSource(self, source):
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        return ast

    def analyzeFromSource(self, source):
        ast = self.parseFromSource(source)
        info = CompileInfo(ast)
        analyzeDeclarations(info)
        analyzeInheritance(info)
        return info

    def testClassInfoForBuiltin(self):
        info = self.analyzeFromSource("")
        rootClassInfo = info.getClassInfo(getRootClass())
        self.assertIs(getRootClass(), rootClassInfo.irDefn)
        self.assertIs(None, rootClassInfo.superclassInfo)

    def testNoBaseClass(self):
        info = self.analyzeFromSource("class Foo")
        ast = info.ast
        self.assertEquals(BUILTIN_ROOT_CLASS_ID,
                          info.getClassInfo(ast.definitions[0]).superclassInfo.irDefn.id)

    def testWithBaseClass(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo"
        info = self.analyzeFromSource(source)
        ast = info.ast
        fooClassInfo = info.getClassInfo(ast.definitions[0])
        barClassInfo = info.getClassInfo(ast.definitions[1])
        self.assertIs(fooClassInfo, barClassInfo.superclassInfo)

    def testInheritFromException(self):
        info = self.analyzeFromSource("class Foo <: Exception")
        ast = info.ast
        classInfo = info.getClassInfo(ast.definitions[0])
        superclassInfo = classInfo.superclassInfo
        self.assertIs(getExceptionClass(), superclassInfo.irDefn)

    def testInheritFromSelf(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "class Foo <: Foo")

    def testInheritCycle(self):
        source = "class Foo <: Bar\n" + \
                 "class Bar <: Foo"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testInheritedDefinitionsAreBound(self):
        source = "class Foo\n" + \
                 "  var x = 12\n" + \
                 "class Bar <: Foo"
        info = self.analyzeFromSource(source)
        ast = info.ast
        barScope = info.getScope(ast.definitions[1])
        self.assertTrue(barScope.isBound("x"))

    def testInheritedBuiltinDefinitionsAreBound(self):
        info = self.analyzeFromSource("class Foo")
        scope = info.getScope(info.ast.definitions[0])
        self.assertTrue(scope.isBound("typeof"))

    def testFieldNamesConflict(self):
        source = "class Foo\n" + \
                 "  var x\n" + \
                 "class Bar <: Foo\n" + \
                 "  var x"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testFieldsAndMethodsConflict(self):
        source = "class Foo\n" + \
                 "  var x\n" + \
                 "class Bar <: Foo\n" + \
                 "  def x = 12"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testMethodsDoNotConflict(self):
        source = "class Foo\n" + \
                 "  def f = 12\n" + \
                 "class Bar <: Foo\n" + \
                 "  def f = 34"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.definitions[1])
        self.assertEquals(2, len(scope.getDefinition("f").overloads))

    def testOverrideBuiltin(self):
        source = "class Foo\n" + \
                 "  def typeof = 12"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.definitions[0])
        self.assertEquals(2, len(scope.getDefinition("typeof").overloads))

    def testConstructorsNotHeritable(self):
        source = "class A[static S](value: S)\n" + \
                 "class B[static T] <: A[T]\n" + \
                 "  def this(value: T) = super(value)\n" + \
                 "class C[static U] <: B[U]\n" + \
                 "  def this(value: U) = super(value)\n"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.definitions[0])
        self.assertEquals(1, len(scope.getDefinition("$constructor").overloads))

    def testTypeParametersNotHeritable(self):
        source = "class A[static T]\n" + \
                 "class B <: A[String]"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.definitions[1])
        self.assertIs(None, scope.getDefinition("T"))

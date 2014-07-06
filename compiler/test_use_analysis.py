# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from lexer import *
from layout import *
from parser import *
from ast import *
from scope_analysis import *
from type_analysis import *
from ir import *


class TestUseAnalysis(unittest.TestCase):
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

    def testUndefinedReference(self):
        info = self.analyzeFromSource("var x = y")
        self.assertRaises(ScopeException,
                          info.getScope(GLOBAL_SCOPE_ID).lookup, "y", False, False)

    def testUseVarBeforeDefinition(self):
        info = self.analyzeFromSource("def f = { var x = y; var y = 12; }")
        scope = info.getScope(info.ast.definitions[0])
        self.assertRaises(ScopeException, scope.lookup, "y", False, False)

    def testUseFunctionBeforeDefinition(self):
        info = self.analyzeFromSource("def f = g; def g = 12;")
        gDefnInfo = info.getDefnInfo(info.ast.definitions[1])
        gNameInfo = info.getScope(info.ast.definitions[0]).lookup("g", False, False)
        self.assertIs(gDefnInfo, gNameInfo.getDefnInfo())

    def testUseCapturedVarBeforeDefinition(self):
        info = self.analyzeFromSource("def f =\n" + \
                                      "  def g = i = 1\n" + \
                                      "  var i: i64 = 0\n")
        statements = info.ast.definitions[0].body.statements
        iDefnInfo = info.getDefnInfo(statements[1].pattern)
        gScope = info.getScope(statements[0])
        iNameInfo = gScope.lookup("i", False, False)
        self.assertIs(iDefnInfo, iNameInfo.getDefnInfo())

    def testUseClassBeforeDefinition(self):
        info = self.analyzeFromSource("def f = C; class C;")
        cDefnInfo = info.getDefnInfo(info.ast.definitions[1])
        cNameInfo = info.getScope(GLOBAL_SCOPE_ID).lookup("C", False, False)
        self.assertIs(cDefnInfo, cNameInfo.getDefnInfo())

    def testUseInLocalScope(self):
        info = self.analyzeFromSource("def f(x: i64) = { { x; }; };")
        fScope = info.getScope(info.ast.definitions[0])
        fScope.define("x")
        xDefnInfo = info.getDefnInfo(info.ast.definitions[0].parameters[0].pattern)
        localScope = info.getScope(info.ast.definitions[0].body.statements[0])
        xNameInfo = localScope.lookup("x", False, False)
        self.assertIs(xDefnInfo, xNameInfo.getDefnInfo())

    def testUseThisInInitializer(self):
        info = self.analyzeFromSource("class Foo { var x = this; };")
        classScope = info.getScope(info.ast.definitions[0])
        thisNameInfo = classScope.lookup("this", False, False)
        self.assertEquals(DefnInfo(Variable("$this", None, PARAMETER), classScope.scopeId),
                          thisNameInfo.getDefnInfo())

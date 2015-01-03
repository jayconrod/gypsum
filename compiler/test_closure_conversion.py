# Copyright 2014, Jay Conrod. All rights reserved.
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
from ir import *


class TestClosureConversion(unittest.TestCase):
    def analyzeFromSource(self, source):
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        info = CompileInfo(ast)
        analyzeDeclarations(info)
        analyzeInheritance(info)
        analyzeTypes(info)
        convertClosures(info)
        return info

    def testUseFunctionVarInFunction(self):
        source = "def f =\n" + \
                 "  var x = 12\n" + \
                 "  def g = x\n"
        info = self.analyzeFromSource(source)
        fAst = info.ast.definitions[0]
        gAst = fAst.body.statements[1]

        fContextInfo = info.getContextInfo(fAst)
        fContextClass = fContextInfo.irContextClass
        self.assertEquals([Field("x", I64Type, frozenset())], fContextClass.fields)
        xDefnInfo = info.getDefnInfo(fAst.body.statements[0].pattern)
        self.assertEquals(Field("x", I64Type, frozenset()), xDefnInfo.irDefn)
        self.assertIs(xDefnInfo, info.getUseInfo(gAst.body).defnInfo)
        gClosureInfo = info.getClosureInfo(gAst)
        gClosureClass = gClosureInfo.irClosureClass
        self.assertEquals([Field("$context", ClassType(fContextClass), frozenset())],
                          gClosureClass.fields)
        self.assertEquals({fAst.id: gClosureClass.fields[0]}, gClosureInfo.irClosureContexts)
        self.assertEquals(Variable("g", ClassType(gClosureClass), LOCAL, frozenset()),
                          gClosureInfo.irClosureVar)

    def testUseFieldInMethod(self):
        source = "class C\n" + \
                 "  var x = 12\n" + \
                 "  def f = x"
        info = self.analyzeFromSource(source)
        cAst = info.ast.definitions[0]
        C = info.package.findClass(name="C")

        cContextInfo = info.getContextInfo(cAst)
        self.assertIs(C, cContextInfo.irContextClass)
        xDefnInfo = info.getDefnInfo(cAst.members[0].pattern)
        xUseInfo = info.getUseInfo(cAst.members[1].body)
        self.assertIs(xDefnInfo, xUseInfo.defnInfo)

    def testUseMethodInMethod(self):
        source = "class C\n" + \
                 "  def f = 12\n" + \
                 "  def g = f\n"
        info = self.analyzeFromSource(source)
        cAst = info.ast.definitions[0]
        C = info.package.findClass(name="C")

        cContextInfo = info.getContextInfo(cAst)
        self.assertIs(C, cContextInfo.irContextClass)
        fDefnInfo = info.getDefnInfo(cAst.members[0])
        fUseInfo = info.getUseInfo(cAst.members[1].body)
        self.assertIs(fDefnInfo, fUseInfo.defnInfo)

    def testCaptureThis(self):
        source = "class C\n" + \
                 "  def f =\n" + \
                 "    def g = this"
        info = self.analyzeFromSource(source)
        cAst = info.ast.definitions[0]
        C = info.package.findClass(name="C")
        CType = ClassType(C)
        fContextInfo = info.getContextInfo(cAst.members[0])
        fContextClass = info.package.findClass(name="$context")
        f = info.package.findFunction(name="f")
        self.assertIs(fContextClass, fContextInfo.irContextClass)
        self.assertEquals(1, len(fContextClass.constructors))
        self.assertEquals([Field("$this", CType, frozenset())], fContextClass.fields)
        gClosureInfo = info.getClosureInfo(cAst.members[0].body.statements[0])
        gClosureClass = info.package.findClass(name="$closure")
        self.assertIs(gClosureClass, gClosureInfo.irClosureClass)
        self.assertEquals({cAst.members[0].id: gClosureClass.fields[0]},
                          gClosureInfo.irClosureContexts)
        g = info.package.findFunction(name="g")
        self.assertTrue(gClosureInfo.irClosureVar in f.variables)
        self.assertEquals(1, len(gClosureClass.constructors))
        self.assertEquals([ClassType(gClosureClass), ClassType(fContextClass)],
                          gClosureClass.constructors[0].parameterTypes)
        self.assertEquals([Field("$context", ClassType(fContextClass), frozenset())],
                          gClosureClass.fields)

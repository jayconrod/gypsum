# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from lexer import *
from layout import layout
from parser import *
from ast import *
from compile_info import *
from scope_analysis import *
from ir import *
from ids import *
from errors import *
from flags import *
from utils_test import TestCaseWithDefinitions


class TestDeclarationAnalysis(TestCaseWithDefinitions):
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
        return info

    def testDefineGlobalVar(self):
        info = self.analyzeFromSource("var a = 12")
        ast = info.ast
        astDefn = ast.definitions[0].pattern
        self.assertEquals(DefnInfo(self.makeGlobal("a"), GLOBAL_SCOPE_ID),
                          info.getDefnInfo(astDefn))

    def testDefineGlobalConst(self):
        info = self.analyzeFromSource("let a = 12")
        ast = info.ast
        astDefn = ast.definitions[0].pattern
        self.assertEquals(DefnInfo(self.makeGlobal("a", flags=frozenset([LET])),
                                   GLOBAL_SCOPE_ID),
                          info.getDefnInfo(astDefn))

    def testDefineGlobalFunction(self):
        info = self.analyzeFromSource("def f = 12")
        ast = info.ast
        astDefn = ast.definitions[0]
        defnInfo = info.getDefnInfo(astDefn)
        expected = self.makeFunction("f")

        self.assertEquals(DefnInfo(expected, GLOBAL_SCOPE_ID), info.getDefnInfo(astDefn))
        self.assertTrue(info.getScope(GLOBAL_SCOPE_ID).isDefined("f"))

    def testDefineGlobalClass(self):
        info = self.analyzeFromSource("class C")
        ast = info.ast
        astDefn = ast.definitions[0]
        defnInfo = info.getDefnInfo(astDefn)
        irClass = defnInfo.irDefn
        self.assertEquals("C", irClass.name)
        self.assertEquals(GLOBAL_SCOPE_ID, defnInfo.scopeId)
        self.assertTrue(info.getScope(GLOBAL_SCOPE_ID).isDefined("C"))
        self.assertTrue(irClass.initializer is not None)
        self.assertEquals(1, len(irClass.constructors))
        irCtor = irClass.constructors[0]
        self.assertTrue(irCtor.isConstructor())
        self.assertIs(astDefn, irCtor.astDefn)
        classInfo = info.getClassInfo(irClass)
        self.assertIs(irClass, classInfo.irDefn)

    def testDefineClassWithPrimaryAndSecondaryCtors(self):
        source = "class C()\n" + \
                 "  def this = {}"
        info = self.analyzeFromSource(source)
        clas = info.package.findClass(name="C")
        self.assertEquals(2, len(clas.constructors))
        self.assertTrue(all(ctor.isConstructor() for ctor in clas.constructors))
        self.assertIs(info.getDefnInfo(info.ast.definitions[0].constructor).irDefn,
                      clas.constructors[0])
        self.assertIs(info.getDefnInfo(info.ast.definitions[0].members[0]).irDefn,
                      clas.constructors[1])
        classInfo = info.getClassInfo(clas)
        self.assertIs(clas, classInfo.irDefn)
        self.assertIsNot(None, clas.initializer)

    def testDefineClassWithPrimaryCtor(self):
        source = "class C(x: i32, y: i32)"
        info = self.analyzeFromSource(source)
        ast = info.ast.definitions[0]
        ctor = info.getDefnInfo(ast.constructor).irDefn
        clas = info.package.findClass(name="C")
        self.assertEquals(1, len(clas.constructors))
        self.assertIs(ctor, clas.constructors[0])
        self.assertEquals([self.makeVariable("$this", kind=PARAMETER, flags=frozenset([LET]))],
                          ctor.variables)
        self.assertEquals([self.makeField("x", flags=frozenset([LET])),
                           self.makeField("y", flags=frozenset([LET]))],
                          clas.fields)
        xDefnInfo = info.getDefnInfo(ast.constructor.parameters[0].pattern)
        self.assertEquals(self.makeField("x", flags=frozenset([LET])), xDefnInfo.irDefn)

    def testDefineClassWithPrimaryCtorWithVarParam(self):
        source = "class C(var x: i32)"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.assertEquals([self.makeField("x")], C.fields)

    def testDefineFunctionParameter(self):
        info = self.analyzeFromSource("def f(x: i32) = x")
        ast = info.ast
        astDefn = ast.definitions[0].parameters[0].pattern
        scopeId = info.getScope(ast.definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("x", kind=PARAMETER,
                                                     flags=frozenset([LET])),
                                   scopeId),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionParameterVar(self):
        info = self.analyzeFromSource("def f(var x: i32) = x")
        ast = info.ast
        astDefn = ast.definitions[0].parameters[0].pattern
        scopeId = info.getScope(ast.definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("x", kind=PARAMETER), scopeId),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionVar(self):
        info = self.analyzeFromSource("def f = { var x = 12; }")
        ast = info.ast
        astDefn = ast.definitions[0].body.statements[0].pattern
        scopeId = info.getScope(ast.definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("x", kind=LOCAL), scopeId),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionConst(self):
        info = self.analyzeFromSource("def f = { let x = 12; }")
        ast = info.ast
        astDefn = ast.definitions[0].body.statements[0].pattern
        scopeId = info.getScope(ast.definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("x", kind=LOCAL, flags=frozenset([LET])),
                                   scopeId),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionFunction(self):
        info = self.analyzeFromSource("def f = { def g = 12; };")
        ast = info.ast
        astDefn = ast.definitions[0].body.statements[0]
        scopeId = info.getScope(ast.definitions[0]).scopeId
        expected = self.makeFunction("g")
        self.assertEquals(DefnInfo(expected, scopeId), info.getDefnInfo(astDefn))

    def testDefineFunctionClass(self):
        info = self.analyzeFromSource("def f = { class C {}; };")
        ast = info.ast
        astDefn = ast.definitions[0].body.statements[0]
        scopeId = info.getScope(ast.definitions[0]).scopeId
        defnInfo = info.getDefnInfo(astDefn)
        self.assertEquals("C", defnInfo.irDefn.name)
        self.assertEquals(scopeId, defnInfo.scopeId)

    def testDefineClassVar(self):
        info = self.analyzeFromSource("class C { var x: i32; };")
        ast = info.ast
        astDefn = ast.definitions[0].members[0].pattern
        scopeId = info.getScope(ast.definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeField("x"), scopeId),
                          info.getDefnInfo(astDefn))

    def testDefineClassConst(self):
        info = self.analyzeFromSource("class C { let x: i32; };")
        ast = info.ast
        astDefn = ast.definitions[0].members[0].pattern
        scopeId = info.getScope(ast.definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeField("x", flags=frozenset([LET])), scopeId),
                          info.getDefnInfo(astDefn))


    def testDefineClassVarWithLocals(self):
        source = "class C() { var x = { var y = 12; y; }; };"
        info = self.analyzeFromSource(source)
        irInitializer = info.package.findClass(name="C").initializer
        self.assertEquals([self.makeVariable("$this", kind=PARAMETER, flags=frozenset([LET])),
                           self.makeVariable("y", kind=LOCAL)],
                          irInitializer.variables)

    def testDefineClassFunction(self):
        info = self.analyzeFromSource("class C { def f = 12; };")
        ast = info.ast
        astDefn = ast.definitions[0].members[0]
        scopeId = info.getScope(ast.definitions[0]).scopeId
        this = self.makeVariable("$this", kind=PARAMETER, flags=frozenset([LET]))
        expectedFunction = self.makeFunction("f", variables=[this])
        expectedDefnInfo = DefnInfo(expectedFunction, scopeId)
        self.assertEquals(expectedDefnInfo, info.getDefnInfo(astDefn))
        expectedClosureInfo = ClosureInfo(info.package.findClass(name="C"),
                                          {scopeId:
                                             self.makeVariable("$this", kind=PARAMETER,
                                                               flags=frozenset([LET]))})
        self.assertEquals(expectedClosureInfo, info.getClosureInfo(scopeId))

    @unittest.skip("inner classes not supported yet")
    def testDefineClassClass(self):
        info = self.analyzeFromSource("class C { class D; };")
        ast = info.ast
        astDefn = ast.definitions[0].members[0]
        self.assertEquals(DefnInfo(self.makeClass("D")), info.getDefnInfo(astDefn))

    def testVarDefinedInBlock(self):
        info = self.analyzeFromSource("def f = {\n" +
                                      "  {\n" +
                                      "    var x = 12;\n" +
                                      "  };\n" +
                                      "};")
        ast = info.ast
        defnInfo = info.getDefnInfo(ast.definitions[0].body.statements[0].statements[0].pattern)
        self.assertEquals(self.makeVariable("x", kind=LOCAL), defnInfo.irDefn)
        self.assertIs(info.getScope(ast.definitions[0].body.statements[0].id),
                      info.getScope(defnInfo.scopeId))

    def testVarDefinedInCatch(self):
        info = self.analyzeFromSource("def f = try 12 catch\n" +
                                      "  case x => x\n")
        ast = info.ast
        defnInfo = info.getDefnInfo(ast.definitions[0].body.catchHandler.cases[0].pattern)
        self.assertEquals(self.makeVariable("x", kind=LOCAL, flags=frozenset([LET])),
                          defnInfo.irDefn)
        self.assertIs(info.getScope(ast.definitions[0].body.catchHandler.cases[0].id),
                      info.getScope(defnInfo.scopeId))

    def testOverloadedFunctions(self):
        self.analyzeFromSource("def f = 12; def f(x: i32) = x")

    def testFunctionAndGlobalOverload(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "var f = 12; def f = 34;")

    def testRedefinedVar(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "var x = 12; var x = 34;")

    def testDuplicateAttribute(self):
        self.assertRaises(ScopeException, self.analyzeFromSource,
                          "class C\n" +
                          "  private private def f = {}")

    def testConflictingAttributes(self):
        self.assertRaises(ScopeException, self.analyzeFromSource,
                          "class C\n" +
                          "  private protected def f = {}")

    def testPrivateGlobalFunction(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "private def f = {}")

    def testPrivateClassFunction(self):
        info = self.analyzeFromSource("class C\n" +
                                      "  private def f = {}")
        f = info.package.findFunction(name="f")
        self.assertEquals(frozenset([PRIVATE]), f.flags)

    def testFunctionMustHaveBody(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "def f: i64")

    def testFunctionMayNotBeAbstract(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "abstract def f: i64 = 12")
        self.assertRaises(ScopeException, self.analyzeFromSource, "abstract def f: i64")

    def testMethodWithoutBodyMustBeAbstract(self):
        self.assertRaises(ScopeException, self.analyzeFromSource,
                          "abstract class C\n" +
                          "  def f: i64")
        info = self.analyzeFromSource("abstract class C\n" +
                                      "  abstract def f: i64")
        C = info.package.findClass(name="C")
        self.assertEquals(frozenset([ABSTRACT]), C.flags)
        f = info.package.findFunction(name="f")
        self.assertEquals(frozenset([ABSTRACT]), f.flags)

    def testAbstractMethodMustNotHaveBody(self):
        self.assertRaises(ScopeException, self.analyzeFromSource,
                          "abstract class C\n" +
                          "  abstract def f = 12")

    def testClassWithAbstractMethodMustBeAbstract(self):
        self.assertRaises(ScopeException, self.analyzeFromSource,
                          "class C\n" +
                          "  abstract def f: i64")

    def testConstructorMustNotBeAbstract(self):
        self.assertRaises(ScopeException, self.analyzeFromSource,
                          "abstract class C\n" +
                          "  abstract def this = {}")
        self.assertRaises(ScopeException, self.analyzeFromSource,
                          "abstract class C abstract ()")

    def testFunctionTypeParameterStatic(self):
        info = self.analyzeFromSource("def f[static T] = {}")
        T = info.package.findTypeParameter(name="T")
        self.assertEquals("T", T.name)
        self.assertEquals(frozenset([STATIC]), T.flags)
        f = info.package.findFunction(name="f")
        self.assertEquals(1, len(f.typeParameters))
        self.assertIs(T, f.typeParameters[0])

    def testFunctionOuterTypeParameter(self):
        info = self.analyzeFromSource("def f[static T](t: T) =\n" +
                                      "  def g = t")
        g = info.package.findFunction(name="g")
        self.assertEquals(1, len(g.typeParameters))
        T = info.package.findTypeParameter(name="T")
        self.assertIs(T, g.typeParameters[0])

    def testFunctionVariantTypeParameter(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "def f[static +T] = {}")
        self.assertRaises(ScopeException, self.analyzeFromSource, "def f[static -T] = {}")

    def testClassTypeParameter(self):
        source = "class Box[static T](x: T)\n" + \
                 "  def get = x\n" + \
                 "  def set(y: T) =\n" + \
                 "    x = y\n" + \
                 "    {}"
        info = self.analyzeFromSource(source)
        Box = info.package.findClass(name="Box")
        T = info.package.findTypeParameter(name="T")
        get = info.package.findFunction(name="get")
        set = info.package.findFunction(name="set")
        self.assertEquals([T], Box.typeParameters)
        self.assertEquals([T], Box.initializer.typeParameters)
        self.assertEquals([T], Box.constructors[0].typeParameters)
        self.assertEquals([T], get.typeParameters)
        self.assertEquals([T], set.typeParameters)

    def testClassVariantTypeParameter(self):
        source = "class Foo[static +S, static -T, static U]"
        info = self.analyzeFromSource(source)
        Foo = info.package.findClass(name="Foo")
        [S, T, U] = Foo.typeParameters
        self.assertEquals(frozenset([COVARIANT, STATIC]), S.flags)
        self.assertEquals(frozenset([CONTRAVARIANT, STATIC]), T.flags)
        self.assertEquals(frozenset([STATIC]), U.flags)

    def testBuiltinConflict(self):
        self.analyzeFromSource("let String = 42")
        # pass if no error

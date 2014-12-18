# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from lexer import *
from layout import *
from parser import *
from ast import *
from compile_info import *
from scope_analysis import *
from ir import *

class TestDeclarations(unittest.TestCase):
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
        self.assertEquals(DefnInfo(Global("a", None, None, frozenset()), ast.id),
                          info.getDefnInfo(astDefn))

    def testDefineGlobalConst(self):
        info = self.analyzeFromSource("let a = 12")
        ast = info.ast
        astDefn = ast.definitions[0].pattern
        self.assertEquals(DefnInfo(Global("a", None, None, frozenset([LET])), ast.id),
                          info.getDefnInfo(astDefn))

    def testDefineGlobalFunction(self):
        info = self.analyzeFromSource("def f = 12")
        ast = info.ast
        astDefn = ast.definitions[0]
        defnInfo = info.getDefnInfo(astDefn)
        self.assertEquals(DefnInfo(Function("f", None, [], None, [], None, frozenset()),
                                   ast.id),
                          info.getDefnInfo(astDefn))
        self.assertTrue(info.getScope(GLOBAL_SCOPE_ID).isDefined("f"))

    def testDefineGlobalClass(self):
        info = self.analyzeFromSource("class C")
        ast = info.ast
        astDefn = ast.definitions[0]
        defnInfo = info.getDefnInfo(astDefn)
        irClass = defnInfo.irDefn
        self.assertEquals("C", irClass.name)
        self.assertEquals(ast.id, defnInfo.scopeId)
        self.assertTrue(info.getScope(GLOBAL_SCOPE_ID).isDefined("C"))
        self.assertTrue(irClass.initializer is not None)
        self.assertEquals(1, len(irClass.constructors))
        irCtor = irClass.constructors[0]
        self.assertTrue(irCtor.isConstructor())
        self.assertIs(astDefn, irCtor.astDefn)
        classInfo = info.getClassInfo(astDefn)
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
        classInfo = info.getClassInfo(info.ast.definitions[0])
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
        self.assertEquals([Variable("$this", None, PARAMETER, frozenset([LET]))],
                          ctor.variables)
        self.assertEquals([Field("x", None, frozenset([LET])),
                           Field("y", None, frozenset([LET]))], clas.fields)
        xDefnInfo = info.getDefnInfo(ast.constructor.parameters[0].pattern)
        self.assertEquals(Field("x", None, frozenset([LET])), xDefnInfo.irDefn)

    def testDefineClassWithPrimaryCtorWithVarParam(self):
        source = "class C(var x: i32)"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.assertEquals([Field("x", None, frozenset())], C.fields)

    def testDefineFunctionParameter(self):
        info = self.analyzeFromSource("def f(x: i32) = x")
        ast = info.ast
        astDefn = ast.definitions[0].parameters[0].pattern
        self.assertEquals(DefnInfo(Variable("x", None, PARAMETER, frozenset([LET])),
                                   ast.definitions[0].id),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionParameterVar(self):
        info = self.analyzeFromSource("def f(var x: i32) = x")
        ast = info.ast
        astDefn = ast.definitions[0].parameters[0].pattern
        self.assertEquals(DefnInfo(Variable("x", None, PARAMETER, frozenset()),
                                   ast.definitions[0].id),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionVar(self):
        info = self.analyzeFromSource("def f = { var x = 12; }")
        ast = info.ast
        astDefn = ast.definitions[0].body.statements[0].pattern
        self.assertEquals(DefnInfo(Variable("x", None, LOCAL, frozenset()),
                                   ast.definitions[0].id),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionConst(self):
        info = self.analyzeFromSource("def f = { let x = 12; }")
        ast = info.ast
        astDefn = ast.definitions[0].body.statements[0].pattern
        self.assertEquals(DefnInfo(Variable("x", None, LOCAL, frozenset([LET])),
                                   ast.definitions[0].id),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionFunction(self):
        info = self.analyzeFromSource("def f = { def g = 12; };")
        ast = info.ast
        astDefn = ast.definitions[0].body.statements[0]
        self.assertEquals(DefnInfo(Function("g", None, [], None, [], None, frozenset()),
                                   ast.definitions[0].id),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionClass(self):
        info = self.analyzeFromSource("def f = { class C {}; };")
        ast = info.ast
        astDefn = ast.definitions[0].body.statements[0]
        defnInfo = info.getDefnInfo(astDefn)
        self.assertEquals("C", defnInfo.irDefn.name)
        self.assertEquals(ast.definitions[0].id, defnInfo.scopeId)

    def testDefineClassVar(self):
        info = self.analyzeFromSource("class C { var x: i32; };")
        ast = info.ast
        astDefn = ast.definitions[0].members[0].pattern
        self.assertEquals(DefnInfo(Field("x", None, frozenset()), ast.definitions[0].id),
                          info.getDefnInfo(astDefn))

    def testDefineClassConst(self):
        info = self.analyzeFromSource("class C { let x: i32; };")
        ast = info.ast
        astDefn = ast.definitions[0].members[0].pattern
        self.assertEquals(DefnInfo(Field("x", None, frozenset([LET])), ast.definitions[0].id),
                          info.getDefnInfo(astDefn))


    def testDefineClassVarWithLocals(self):
        source = "class C() { var x = { var y = 12; y; }; };"
        info = self.analyzeFromSource(source)
        irInitializer = info.package.findClass(name="C").initializer
        self.assertEquals([Variable("$this", None, PARAMETER, frozenset([LET])),
                           Variable("y", None, LOCAL, frozenset())],
                          irInitializer.variables)

    def testDefineClassFunction(self):
        info = self.analyzeFromSource("class C { def f = 12; };")
        ast = info.ast
        astDefn = ast.definitions[0].members[0]
        expectedFunction = Function("f", None, [], None,
                                    [Variable("$this", None, PARAMETER, frozenset([LET]))],
                                    None, frozenset())
        expectedDefnInfo = DefnInfo(expectedFunction, ast.definitions[0].id)
        self.assertEquals(expectedDefnInfo, info.getDefnInfo(astDefn))
        expectedClosureInfo = ClosureInfo(info.package.findClass(name="C"),
                                          {ast.definitions[0].id:
                                             Variable("$this", None, PARAMETER, frozenset([LET]))})
        self.assertEquals(expectedClosureInfo, info.getClosureInfo(astDefn))

    @unittest.skip("inner classes not supported yet")
    def testDefineClassClass(self):
        info = self.analyzeFromSource("class C { class D; };")
        ast = info.ast
        astDefn = ast.definitions[0].members[0]
        self.assertEquals(DefnInfo(Class("D", None, None, [], [], [], frozenset())),
                          info.getDefnInfo(astDefn))

    def testVarDefinedInBlock(self):
        info = self.analyzeFromSource("def f = {\n" +
                                      "  {\n" +
                                      "    var x = 12;\n" +
                                      "  };\n" +
                                      "};")
        ast = info.ast
        defnInfo = info.getDefnInfo(ast.definitions[0].body.statements[0].statements[0].pattern)
        self.assertEquals(Variable("x", None, LOCAL, frozenset()), defnInfo.irDefn)
        self.assertEquals(ast.definitions[0].body.statements[0].id, defnInfo.scopeId)

    def testVarDefinedInCatch(self):
        info = self.analyzeFromSource("def f = try 12 catch\n" +
                                      "  case x => x\n")
        ast = info.ast
        defnInfo = info.getDefnInfo(ast.definitions[0].body.catchHandler.cases[0].pattern)
        self.assertEquals(Variable("x", None, LOCAL, frozenset([LET])), defnInfo.irDefn)
        self.assertEquals(ast.definitions[0].body.catchHandler.cases[0].id, defnInfo.scopeId)

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

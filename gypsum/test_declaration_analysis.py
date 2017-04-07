# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

import ast
from lexer import *
from layout import layout
from parser import *
from compile_info import *
from scope_analysis import *
from ir import *
from ids import *
from errors import *
from flags import *
from utils_test import FakePackageLoader, TestCaseWithDefinitions
from location import NoLoc
from name import (
    ARRAY_LENGTH_SUFFIX,
    BLANK_SUFFIX,
    CLASS_INIT_SUFFIX,
    CONSTRUCTOR_SUFFIX,
    EXISTENTIAL_SUFFIX,
    Name,
    RECEIVER_SUFFIX,
)


class TestDeclarationAnalysis(TestCaseWithDefinitions):
    def parseFromSource(self, source):
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        return ast

    def analyzeFromSource(self, source, packageLoader=None):
        ast = self.parseFromSource(source)
        package = Package(id=TARGET_PACKAGE_ID)
        if packageLoader is None:
            packageLoader = FakePackageLoader([])
        info = CompileInfo(ast, package, packageLoader, isUsingStd=False)
        analyzeDeclarations(info)
        return info

    def testDefineGlobalVar(self):
        source = "var a = 12"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].pattern
        scopeId = info.getScope(ast.modules[0]).scopeId
        self.assertEquals(DefnInfo(self.makeGlobal("a"), scopeId, True),
                          info.getDefnInfo(astDefn))

    def testDefineGlobalConst(self):
        source = "let a = 12"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].pattern
        scopeId = info.getScope(ast.modules[0]).scopeId
        self.assertEquals(DefnInfo(self.makeGlobal("a", flags=frozenset([LET])),
                                   scopeId, True),
                          info.getDefnInfo(astDefn))

    def testDefineGlobalFunction(self):
        source = "def f = 12"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0]
        scope = info.getScope(info.ast.modules[0])
        defnInfo = info.getDefnInfo(astDefn)
        expected = self.makeFunction("f")

        self.assertEquals(DefnInfo(expected, scope.scopeId, True), info.getDefnInfo(astDefn))
        self.assertTrue(scope.isDefined("f"))
        self.assertTrue(info.getScope(GLOBAL_SCOPE_ID).isDefined("f"))

    def testDefineGlobalClass(self):
        source = "class C"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0]
        defnInfo = info.getDefnInfo(astDefn)
        scope = info.getScope(ast.modules[0])
        scopeId = scope.scopeId
        irClass = defnInfo.irDefn
        self.assertEquals(Name(["C"]), irClass.name)
        self.assertEquals(scopeId, defnInfo.scopeId)
        self.assertTrue(scope.isDefined("C"))
        self.assertTrue(info.getScope(GLOBAL_SCOPE_ID).isDefined("C"))
        self.assertTrue(irClass.initializer is not None)
        self.assertEquals(1, len(irClass.constructors))
        irCtor = irClass.constructors[0]
        self.assertTrue(irCtor.isConstructor())
        self.assertIs(astDefn, irCtor.astDefn)

    def testDefineClassWithPrimaryAndSecondaryCtors(self):
        source = "class C()\n" + \
                 "  def this = {}"
        info = self.analyzeFromSource(source)
        clas = info.package.findClass(name="C")
        self.assertEquals(2, len(clas.constructors))
        self.assertTrue(all(ctor.isConstructor() for ctor in clas.constructors))
        self.assertIs(info.getDefnInfo(info.ast.modules[0].definitions[0].constructor).irDefn,
                      clas.constructors[0])
        self.assertIs(info.getDefnInfo(info.ast.modules[0].definitions[0].members[0]).irDefn,
                      clas.constructors[1])
        self.assertIsNot(None, clas.initializer)

    def testDefineClassWithPrimaryCtor(self):
        source = "class C(x: i32, y: i32)"
        info = self.analyzeFromSource(source)
        ast = info.ast.modules[0].definitions[0]
        ctor = info.getDefnInfo(ast.constructor).irDefn
        clas = info.package.findClass(name="C")
        self.assertEquals(1, len(clas.constructors))
        self.assertIs(ctor, clas.constructors[0])
        self.assertEquals([self.makeVariable(Name(["C", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                             kind=PARAMETER, flags=frozenset([LET])),
                           self.makeVariable(Name(["C", CONSTRUCTOR_SUFFIX, "x"]),
                                             kind=PARAMETER, flags=frozenset([LET])),
                           self.makeVariable(Name(["C", CONSTRUCTOR_SUFFIX, "y"]),
                                             kind=PARAMETER, flags=frozenset([LET]))],
                          ctor.variables)
        self.assertEquals([self.makeField("C.x", flags=frozenset([LET])),
                           self.makeField("C.y", flags=frozenset([LET]))],
                          clas.fields)
        xDefnInfo = info.getDefnInfo(ast.constructor.parameters[0].pattern)
        self.assertEquals(self.makeField("C.x", flags=frozenset([LET])), xDefnInfo.irDefn)

    def testDefineClassWithPrimaryCtorWithVarParam(self):
        source = "class C(var x: i32)"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.assertEquals([self.makeField("C.x")], C.fields)

    def testDefineFunctionParameter(self):
        source = "def f(x: i32) = x"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].parameters[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("f.x", kind=PARAMETER,
                                                     flags=frozenset([LET])),
                                   scopeId, False),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionParameterVar(self):
        source = "def f(var x: i32) = x"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].parameters[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("f.x", kind=PARAMETER), scopeId, False),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionVar(self):
        source = "def f = { var x = 12; }"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].body.statements[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("f.x", kind=LOCAL), scopeId, False),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionConst(self):
        source = "def f = { let x = 12; }"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].body.statements[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("f.x", kind=LOCAL, flags=frozenset([LET])),
                                   scopeId, False),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionFunction(self):
        source = "def f = { def g = 12; };"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].body.statements[0]
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        expected = self.makeFunction("f.g")
        self.assertEquals(DefnInfo(expected, scopeId, False), info.getDefnInfo(astDefn))

    def testDefineFunctionClass(self):
        source = "def f = { class C {}; };"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].body.statements[0]
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        defnInfo = info.getDefnInfo(astDefn)
        self.assertEquals(Name(["f", "C"]), defnInfo.irDefn.name)
        self.assertEquals(scopeId, defnInfo.scopeId)

    def testDefineClassVar(self):
        source = "class C { var x: i32; };"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].members[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeField("C.x"), scopeId, True),
                          info.getDefnInfo(astDefn))

    def testDefineClassConst(self):
        source = "class C { let x: i32; };"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].members[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeField("C.x", flags=frozenset([LET])),
                                   scopeId, True),
                          info.getDefnInfo(astDefn))

    def testDefineClassVarWithLocals(self):
        source = "class C() { var x = { var y = 12; y; }; };"
        info = self.analyzeFromSource(source)
        irInitializer = info.package.findClass(name="C").initializer
        self.assertEquals([self.makeVariable(Name(["C", CLASS_INIT_SUFFIX, RECEIVER_SUFFIX]),
                                             kind=PARAMETER, flags=frozenset([LET])),
                           self.makeVariable(Name(["C", LOCAL_SUFFIX, "y"]), kind=LOCAL)],
                          irInitializer.variables)

    def testDefineClassFunction(self):
        source = "class C { def f = 12; };"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].members[0]
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        this = self.makeVariable(Name(["C", "f", RECEIVER_SUFFIX]),
                                 kind=PARAMETER, flags=frozenset([LET]))
        expectedFunction = self.makeFunction("C.f", variables=[this], flags=frozenset([METHOD]))
        expectedDefnInfo = DefnInfo(expectedFunction, scopeId, True)
        self.assertEquals(expectedDefnInfo, info.getDefnInfo(astDefn))
        expectedClosureInfo = ClosureInfo(info.package.findClass(name="C"),
                                          {scopeId: self.makeVariable(Name(["C", CLASS_INIT_SUFFIX, RECEIVER_SUFFIX]),
                                                                      kind=PARAMETER,
                                                                      flags=frozenset([LET]))})
        self.assertEquals(expectedClosureInfo, info.getClosureInfo(scopeId))

    def testDefineClassStaticFunction(self):
        source = "class C { static def f = 12; };"
        info = self.analyzeFromSource(source)
        astDefn = info.ast.modules[0].definitions[0].members[0]
        scopeId = info.getScope(info.ast.modules[0].definitions[0]).scopeId
        expectedFunction = self.makeFunction("C.f", flags=frozenset([STATIC, METHOD]))
        expectedDefnInfo = DefnInfo(expectedFunction, scopeId, True)
        self.assertEquals(expectedDefnInfo, info.getDefnInfo(astDefn))

    @unittest.skip("inner classes not supported yet")
    def testDefineClassClass(self):
        source = "class C { class D; };"
        info = self.analyzeFromSource(source)
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].members[0]
        self.assertEquals(DefnInfo(self.makeClass("D")), info.getDefnInfo(astDefn))

    def testDefineGlobalTrait(self):
        source = "public trait Tr"
        info = self.analyzeFromSource(source)
        astDefn = info.ast.modules[0].definitions[0]
        defnInfo = info.getDefnInfo(astDefn)
        scope = info.getScope(info.ast.modules[0])
        scopeId = scope.scopeId
        irTrait = defnInfo.irDefn
        self.assertEquals(Name(["Tr"]), irTrait.name)
        self.assertEquals(frozenset([PUBLIC]), irTrait.flags)
        self.assertEquals(scopeId, defnInfo.scopeId)
        self.assertTrue(scope.isDefined("Tr"))
        self.assertTrue(info.getScope(GLOBAL_SCOPE_ID).isDefined("Tr"))

    def testDefineTraitFunction(self):
        source = "trait Tr\n" + \
                 "  def f = 12"
        info = self.analyzeFromSource(source)
        astDefn = info.ast.modules[0].definitions[0].members[0]
        scopeId = info.getScope(info.ast.modules[0].definitions[0]).scopeId
        this = self.makeVariable(Name(["Tr", "f", RECEIVER_SUFFIX]),
                                 kind=PARAMETER, flags=frozenset([LET]))
        expectedFunction = self.makeFunction("Tr.f", variables=[this], flags=frozenset([METHOD]))
        expectedDefnInfo = DefnInfo(expectedFunction, scopeId, True)
        self.assertEquals(expectedDefnInfo, info.getDefnInfo(astDefn))

    def testDefineTraitTypeParameter(self):
        source = "public trait Tr[static T]\n" + \
                 "  def f(x: T) = 12"
        info = self.analyzeFromSource(source)
        Tr = info.package.findTrait(name="Tr")
        T = info.package.findTypeParameter(name="Tr.T")
        f = info.package.findFunction(name="Tr.f")
        self.assertEquals([T], Tr.typeParameters)
        self.assertEquals([T], f.typeParameters)
        self.assertEquals(frozenset([PUBLIC, STATIC]), T.flags)
        self.assertEquals(0, T.index)

    def testDefineAbstractTrait(self):
        source = "abstract trait Tr"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testTraitMethodWithoutBodyIsAbstract(self):
        source = "trait Tr\n" + \
                 "  def f: unit"
        info = self.analyzeFromSource(source)
        f = info.package.findFunction(name="Tr.f")
        self.assertIn(ABSTRACT, f.flags)

    def testVarDefinedInBlock(self):
        info = self.analyzeFromSource("def f = {\n" +
                                      "  {\n" +
                                      "    var x = 12;\n" +
                                      "  };\n" +
                                      "};")
        ast = info.ast
        defnInfo = info.getDefnInfo(ast.modules[0].definitions[0].body.statements[0].statements[0].pattern)
        self.assertEquals(self.makeVariable(Name(["f", LOCAL_SUFFIX, "x"]), kind=LOCAL),
                          defnInfo.irDefn)
        self.assertIs(info.getScope(ast.modules[0].definitions[0].body.statements[0].id),
                      info.getScope(defnInfo.scopeId))

    def testVarDefinedInCatch(self):
        info = self.analyzeFromSource("def f = try 12 catch\n" +
                                      "  case x => x\n")
        ast = info.ast
        defnInfo = info.getDefnInfo(ast.modules[0].definitions[0].body.catchHandler.cases[0].pattern)
        self.assertEquals(self.makeVariable(Name(["f", LOCAL_SUFFIX, "x"]),
                                            kind=LOCAL, flags=frozenset([LET])),
                          defnInfo.irDefn)
        self.assertIs(info.getScope(ast.modules[0].definitions[0].body.catchHandler.cases[0].id),
                      info.getScope(defnInfo.scopeId))

    def testLambdaExpr(self):
        source = "let f = lambda (x) { let y = x; y; }"
        info = self.analyzeFromSource(source)
        astLambda = info.ast.modules[0].definitions[0].expression
        irDefn = info.getDefnInfo(astLambda).irDefn
        self.assertEquals(
            self.makeFunction(
                Name([LAMBDA_SUFFIX]),
                variables=[
                    self.makeVariable(
                        Name([LAMBDA_SUFFIX, "x"]), kind=PARAMETER, flags=frozenset([LET])),
                    self.makeVariable(
                        Name([LAMBDA_SUFFIX, LOCAL_SUFFIX, "y"]),
                        kind=LOCAL, flags=frozenset([LET]))],
                flags=frozenset()),
            irDefn)
        lambdaScope = info.getScope(astLambda)
        self.assertTrue(lambdaScope.isBound("x"))
        localScope = info.getScope(astLambda.body)
        self.assertTrue(localScope.isBound("y"))

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
        f = info.package.findFunction(name="C.f")
        self.assertEquals(frozenset([METHOD, PRIVATE]), f.flags)

    def testPublicClassDefaultConstructor(self):
        source = "public class C"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.assertIn(PUBLIC, C.flags)
        self.assertEquals(1, len(C.constructors))
        self.assertIn(PUBLIC, C.constructors[0].flags)

    def testPublicClassPrimaryConstructor(self):
        source = "public class C(x: i64)"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.assertIn(PUBLIC, C.flags)
        self.assertEquals(1, len(C.constructors))
        self.assertIn(PUBLIC, C.constructors[0].flags)

    def testPublicClassPrivatePrimaryConstructor(self):
        source = "public class C private (x: i64)"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.assertIn(PRIVATE, C.constructors[0].flags & frozenset([PUBLIC, PROTECTED, PRIVATE]))

    def testAbstractFinalClass(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "abstract final class C")

    @unittest.skip("private classes not supported yet")
    def testPrivateClassPrimaryConstructor(self):
        source = "private class C(x: i64)"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.assertIn(PRIVATE, C.flags)
        self.assertEquals(1, len(C.constructors))
        self.assertEquals(frozenset([]),
                          C.constructors[0].flags & frozenset([PUBLIC, PROTECTED, PRIVATE]))

    def testFunctionMustHaveBody(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "def f: i64")

    def testNativeFunctionMustNotHaveBody(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "native def f = 12")

    def testNativeFunctionCannotOverload(self):
        source = "native def f(x: i32): unit\n" + \
                 "native def f(x: i64): unit"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

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
        f = info.package.findFunction(name="C.f")
        self.assertEquals(frozenset([ABSTRACT, METHOD]), f.flags)

    def testAbstractMethodMustNotHaveBody(self):
        self.assertRaises(ScopeException, self.analyzeFromSource,
                          "abstract class C\n" +
                          "  abstract def f = 12")

    def testNativeMethodMustNotHaveBody(self):
        self.assertRaises(ScopeException, self.analyzeFromSource,
                          "class C\n" + \
                          "  native def f = 12")

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
        source = "def f[static T] = {}"
        info = self.analyzeFromSource(source)
        T = info.package.findTypeParameter(name="f.T")
        self.assertEquals(Name.fromString("f.T"), T.name)
        self.assertEquals(frozenset([STATIC]), T.flags)
        f = info.package.findFunction(name="f")
        self.assertEquals(1, len(f.typeParameters))
        self.assertIs(T, f.typeParameters[0])
        self.assertEquals(0, T.index)

    def testFunctionOuterTypeParameter(self):
        info = self.analyzeFromSource("def f[static T](t: T) =\n" +
                                      "  def g = t")
        g = info.package.findFunction(name="f.g")
        T = info.package.findTypeParameter(name="f.T")
        self.assertEquals([T], g.typeParameters)

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
        T = info.package.findTypeParameter(name="Box.T")
        get = info.package.findFunction(name="Box.get")
        set = info.package.findFunction(name="Box.set")
        self.assertEquals([T], Box.typeParameters)
        self.assertEquals([T], Box.initializer.typeParameters)
        self.assertEquals([T], Box.constructors[0].typeParameters)
        self.assertEquals([T], get.typeParameters)
        self.assertEquals([T], set.typeParameters)
        self.assertEquals(0, T.index)

    def testClassVariantTypeParameter(self):
        source = "class Foo[static +S, static -T, static U]"
        info = self.analyzeFromSource(source)
        Foo = info.package.findClass(name="Foo")
        [S, T, U] = Foo.typeParameters
        self.assertEquals(frozenset([COVARIANT, STATIC]), S.flags)
        self.assertEquals(frozenset([CONTRAVARIANT, STATIC]), T.flags)
        self.assertEquals(frozenset([STATIC]), U.flags)
        self.assertEquals([0, 1, 2], [p.index for p in Foo.typeParameters])

    def testBuiltinConflict(self):
        self.analyzeFromSource("let String = 42")
        # pass if no error

    def testClassWithArrayElements(self):
        source = "final class Foo[static T]\n" + \
                 "  final arrayelements T, public get, public set, public length"
        info = self.analyzeFromSource(source)
        Foo = info.package.findClass(name="Foo")
        self.assertIn(ARRAY, Foo.flags)
        T = info.package.findTypeParameter(name="Foo.T")
        getter = info.package.findFunction(name="Foo.get")
        setter = info.package.findFunction(name="Foo.set")
        length = info.package.findFunction(name="Foo.length")
        for accessor in [getter, setter, length]:
            self.assertEquals([T], accessor.typeParameters)
            self.assertEquals(frozenset([PUBLIC, METHOD, ARRAY]), accessor.flags)
            self.assertIn(accessor, Foo.methods)
        lengthField = Foo.fields[-1]
        self.assertEquals(self.makeField(Name(["Foo", ARRAY_LENGTH_SUFFIX]),
                                         flags=frozenset([PRIVATE, LET, ARRAY])),
                          lengthField)

    def testClassWithArrayElementsWithBadFlag(self):
        source = "final class Foo[static T]\n" + \
                 "  private arrayelements T, get, set, length"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testImportStaticMethod(self):
        source = "class Foo\n" + \
                 "  static def m = 12\n" + \
                 "import Foo.m as x\n" + \
                 "def x(y: i64) = y"
        info = self.analyzeFromSource(source)
        m = info.package.findFunction(name="Foo.m")
        scope = info.getScope(info.ast.modules[0])
        nameInfo = scope.lookupFromSelf("x", NoLoc)
        self.assertTrue(nameInfo.isOverloaded())
        importedDefnInfo = next(di for di in nameInfo.iterOverloads() if di.irDefn is m)
        self.assertEquals(scope.scopeId, importedDefnInfo.scopeId)
        self.assertFalse(importedDefnInfo.isVisible)
        self.assertIsNone(importedDefnInfo.importedTypeArguments)

    def testImportGlobalFromPackage(self):
        foo = Package(name=Name(["foo"]))
        foo.addGlobal(Name(["bar"]), sourceName="bar", flags=frozenset([PUBLIC, LET]))
        foo.addGlobal(Name(["baz"]), sourceName="baz", flags=frozenset([LET]))
        source = "import foo._"
        info = self.analyzeFromSource(source, packageLoader=FakePackageLoader([foo]))
        scope = info.getScope(info.ast.modules[0])
        self.assertFalse(scope.isBound("baz"))
        scope.define("bar")
        nameInfo = scope.lookupFromSelf("bar", NoLoc)
        importedDefnInfo = nameInfo.getDefnInfo()
        self.assertEquals(scope.scopeId, importedDefnInfo.scopeId)
        self.assertFalse(importedDefnInfo.isVisible)
        self.assertIsNone(importedDefnInfo.importedTypeArguments)

    def testImportGlobalFromPackageMultipleModules(self):
        foo = Package(name=Name(["foo"]))
        foo.addGlobal(Name(["bar"]), sourceName="bar", flags=frozenset([PUBLIC, LET]))
        loader = FakePackageLoader([foo])

        source1 = "import foo.bar"
        mod1 = self.parseFromSource(source1)
        source2 = "import foo.bar\n" + \
                  "let baz = 12"
        mod2 = self.parseFromSource(source2)
        astt = ast.Package([mod1, mod2], NoLoc)
        astt.id = AstId(-1)
        package = Package(id=TARGET_PACKAGE_ID)
        info = CompileInfo(astt, package, loader, isUsingStd=False)
        analyzeDeclarations(info)

        baz = info.package.findGlobal(name="baz")
        bazScope = info.getScope(baz.astDefn)
        self.assertIs(info.getScope(mod2), bazScope)

    def testImportFromGlobal(self):
        source = "let x = 12\n" + \
                 "import x.y"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testImportNonPublicFromPackage(self):
        foo = Package(name=Name(["foo"]))
        foo.addGlobal(Name(["baz"]), None, None, frozenset([LET]))
        source = "import foo.baz"
        self.assertRaises(ScopeException, self.analyzeFromSource, source,
                          packageLoader=FakePackageLoader([foo]))

    def testImportPrivateFromClass(self):
        source = "class Foo\n" + \
                 "  private static def f = 12\n" + \
                 "import Foo.f"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testImportMissingFromPackage(self):
        foo = Package(name=Name(["foo"]))
        source = "import foo.baz"
        self.assertRaises(ScopeException, self.analyzeFromSource, source,
                          packageLoader=FakePackageLoader([foo]))

    def testImportRedeclare(self):
        foo = Package(name=Name(["foo"]))
        foo.addGlobal(Name(["bar"]), None, None, frozenset([PUBLIC, LET]))
        source = "let bar = 12\n" + \
                 "import foo.bar"
        self.assertRaises(ScopeException, self.analyzeFromSource, source,
                          packageLoader=FakePackageLoader([foo]))

    def testImportInherited(self):
        source = "class Foo\n" + \
                 "  static def f = 12\n" + \
                 "class Bar <: Foo\n" + \
                 "import Bar.f"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testExistentialType(self):
        source = "let x: forsome [X] X"
        info = self.analyzeFromSource(source)
        astType = info.ast.modules[0].definitions[0].pattern.ty
        scope = info.getScope(astType)
        self.assertTrue(isinstance(scope, ExistentialTypeScope))
        X = info.getDefnInfo(astType.typeParameters[0]).irDefn
        expected = self.makeTypeParameter(Name([EXISTENTIAL_SUFFIX, "X"]),
                                          flags=frozenset(), index=0)
        self.assertEquals(expected, X)

    def testExistentialTypeWithFlags(self):
        source = "let x: forsome [static +X] X"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testBlankTypeAlone(self):
        source = "let x: _"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testClassBlankType(self):
        source = "let x: Foo[_, _]"
        info = self.analyzeFromSource(source)
        astType = info.ast.modules[0].definitions[0].pattern.ty
        scope = info.getScope(astType)
        self.assertTrue(isinstance(scope, ExistentialTypeScope))
        for i in xrange(2):
            blank = info.getDefnInfo(astType.typeArguments[i]).irDefn
            expected = self.makeTypeParameter(Name([EXISTENTIAL_SUFFIX, BLANK_SUFFIX]),
                                              flags=frozenset(), index=i)
            self.assertEquals(expected, blank)

    def testTupleBlankType(self):
        source = "let x: (_, _)"
        info = self.analyzeFromSource(source)
        astType = info.ast.modules[0].definitions[0].pattern.ty
        scope = info.getScope(astType)
        self.assertTrue(isinstance(scope, ExistentialTypeScope))
        for i in xrange(2):
            blank = info.getDefnInfo(astType.types[i]).irDefn
            expected = self.makeTypeParameter(Name([EXISTENTIAL_SUFFIX, BLANK_SUFFIX]),
                                              flags=frozenset(), index=i)
            self.assertEquals(expected, blank)

    def testTypeParameterIndices(self):
        source = "class Foo[static A]\n" + \
                 "  def m[static B](x: forsome [C] forsome [D] D) = {}\n"
        info = self.analyzeFromSource(source)
        A = info.package.findTypeParameter(name="Foo.A")
        B = info.package.findTypeParameter(name="Foo.m.B")
        C = info.package.findTypeParameter(name=Name(["Foo", "m", EXISTENTIAL_SUFFIX, "C"]))
        D = info.package.findTypeParameter(name=Name(["Foo", "m", EXISTENTIAL_SUFFIX,
                                                      EXISTENTIAL_SUFFIX, "D"]))
        self.assertEquals([0, 1, 2, 3], [p.index for p in [A, B, C, D]])


class TestPackageScope(unittest.TestCase):
    def infoAndScopeWithPackageNames(self, args):
        packageNameFromString = lambda s: Name.fromString(s, isPackageName=True)
        names = map(packageNameFromString, args)
        package = Package(id=TARGET_PACKAGE_ID)
        packageLoader = FakePackageLoader(names)
        info = CompileInfo(None, package, packageLoader)
        scope = PackageScope(PACKAGE_SCOPE_ID, None, info, names, [], None)
        return info, scope

    def testNotFoundWhenEmpty(self):
        info, scope = self.infoAndScopeWithPackageNames([])
        self.assertRaises(ScopeException, scope.lookupFromSelf, "foo", NoLoc)

    def testFoundSimple(self):
        info, scope = self.infoAndScopeWithPackageNames(["foo", "bar"])
        nameInfo = scope.lookupFromSelf("foo", NoLoc)
        self.assertTrue(nameInfo.isPackage())

    def testFoundSimplePrefix(self):
        info, scope = self.infoAndScopeWithPackageNames(["foo.bar"])
        nameInfo = scope.lookupFromSelf("foo", NoLoc)
        self.assertFalse(nameInfo.isPackage())
        self.assertTrue(nameInfo.isPackagePrefix())

    def testFoundSimpleWithPrefix(self):
        info, scope = self.infoAndScopeWithPackageNames(["foo", "foo.bar"])
        nameInfo = scope.lookupFromSelf("foo", NoLoc)
        self.assertTrue(nameInfo.isPackage())

    def testNotFoundCompound(self):
        info, scope = self.infoAndScopeWithPackageNames(["foo.bar"])
        scope = scope.scopeForPrefix("foo", NoLoc)
        self.assertRaises(ScopeException, scope.lookupFromSelf, "baz", NoLoc)

    def testFoundCompound(self):
        info, scope = self.infoAndScopeWithPackageNames(["foo.bar"])
        scope = scope.scopeForPrefix("foo", NoLoc)
        nameInfo = scope.lookupFromSelf("bar", NoLoc)
        self.assertTrue(nameInfo.isPackage())

    def testFoundCompoundPrefix(self):
        info, scope = self.infoAndScopeWithPackageNames(["foo.bar.baz"])
        scope = scope.scopeForPrefix("foo", NoLoc)
        nameInfo = scope.lookupFromSelf("bar", NoLoc)
        self.assertFalse(nameInfo.isPackage())
        self.assertTrue(nameInfo.isPackagePrefix())

    def testFoundCompoundWithPrefix(self):
        info, scope = self.infoAndScopeWithPackageNames(["foo.bar", "foo.bar.baz"])
        scope = scope.scopeForPrefix("foo", NoLoc)
        nameInfo = scope.lookupFromSelf("bar", NoLoc)
        self.assertTrue(nameInfo.isPackage())

    def testPackageClassHasScope(self):
        package = Package(name=Name(["foo"]))
        clas = package.addClass(Name(["C"]), sourceName="C", typeParameters=[],
                                supertypes=[getRootClassType()], flags=frozenset([PUBLIC]))
        classType = ClassType(clas)
        publicCtor = package.addFunction(Name(["C", CONSTRUCTOR_SUFFIX]),
                                         sourceName=CONSTRUCTOR_SUFFIX,
                                         returnType=UnitType,
                                         typeParameters=[], parameterTypes=[classType],
                                         flags=frozenset([PUBLIC, METHOD, EXTERN]))
        privateCtor = package.addFunction(Name(["C", CONSTRUCTOR_SUFFIX]),
                                          sourceName=CONSTRUCTOR_SUFFIX,
                                          returnType=UnitType,
                                          typeParameters=[], parameterTypes=[classType],
                                          flags=frozenset([PRIVATE, METHOD, EXTERN]))
        clas.constructors = [publicCtor, privateCtor]
        publicMethod = package.addFunction(Name(["C", "m1"]), sourceName="m1",
                                           returnType=UnitType,
                                           typeParameters=[], parameterTypes=[classType],
                                           flags=frozenset([PUBLIC, METHOD, EXTERN]))
        privateMethod = package.addFunction(Name(["C", "m2"]), sourceName="m2",
                                            returnType=UnitType,
                                            typeParameters=[], parameterTypes=[classType],
                                            flags=frozenset([PRIVATE, METHOD, EXTERN]))
        clas.methods = [publicMethod, privateMethod]
        publicField = package.newField(Name(["C", "x"]), sourceName="x", type=UnitType,
                                       flags=frozenset([PUBLIC, EXTERN]))
        privateField = package.newField(Name(["C", "y"]), sourceName="y", type=UnitType,
                                        flags=frozenset([PRIVATE, EXTERN]))
        clas.fields = [publicField, privateField]

        packageLoader = FakePackageLoader([package])
        info = CompileInfo(None, Package(id=TARGET_PACKAGE_ID), packageLoader)
        topPackageScope = PackageScope(PACKAGE_SCOPE_ID, None, info,
                                       packageLoader.getPackageNames(), [], None)
        builtinScope = BuiltinGlobalScope(topPackageScope)
        fooPackageScope = topPackageScope.scopeForPrefix("foo", NoLoc)

        defnInfo = fooPackageScope.lookupFromSelf("C", NoLoc).getDefnInfo()
        self.assertIs(clas, defnInfo.irDefn)
        self.assertIs(fooPackageScope.scopeId, defnInfo.scopeId)
        self.assertFalse(info.hasScope(clas.id))
        classScope = NonLocalObjectTypeDefnScope.ensureForDefn(clas, info)
        self.assertIs(classScope, info.getScope(clas.id))
        defnInfo = classScope.lookupFromSelf(CONSTRUCTOR_SUFFIX, NoLoc).getDefnInfo()
        self.assertIs(publicCtor, defnInfo.irDefn)
        self.assertIs(classScope.scopeId, defnInfo.scopeId)
        defnInfo = classScope.lookupFromSelf("m1", NoLoc).getDefnInfo()
        self.assertIs(publicMethod, defnInfo.irDefn)
        self.assertRaises(ScopeException, classScope.lookupFromSelf, "m2", NoLoc)
        defnInfo = classScope.lookupFromSelf("x", NoLoc).getDefnInfo()
        self.assertIs(publicField, defnInfo.irDefn)
        self.assertRaises(ScopeException, classScope.lookupFromSelf, "y", NoLoc)


if __name__ == "__main__":
    unittest.main()

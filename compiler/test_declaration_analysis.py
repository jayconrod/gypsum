# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

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
        info = self.analyzeFromSource("var a = 12")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].pattern
        self.assertEquals(DefnInfo(self.makeGlobal("a"), GLOBAL_SCOPE_ID, True),
                          info.getDefnInfo(astDefn))

    def testDefineGlobalConst(self):
        info = self.analyzeFromSource("let a = 12")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].pattern
        self.assertEquals(DefnInfo(self.makeGlobal("a", flags=frozenset([LET])),
                                   GLOBAL_SCOPE_ID, True),
                          info.getDefnInfo(astDefn))

    def testDefineGlobalFunction(self):
        info = self.analyzeFromSource("def f = 12")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0]
        defnInfo = info.getDefnInfo(astDefn)
        expected = self.makeFunction("f")

        self.assertEquals(DefnInfo(expected, GLOBAL_SCOPE_ID, True), info.getDefnInfo(astDefn))
        self.assertTrue(info.getScope(GLOBAL_SCOPE_ID).isDefined("f"))

    def testDefineGlobalClass(self):
        info = self.analyzeFromSource("class C")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0]
        defnInfo = info.getDefnInfo(astDefn)
        irClass = defnInfo.irDefn
        self.assertEquals(Name(["C"]), irClass.name)
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
        self.assertIs(info.getDefnInfo(info.ast.modules[0].definitions[0].constructor).irDefn,
                      clas.constructors[0])
        self.assertIs(info.getDefnInfo(info.ast.modules[0].definitions[0].members[0]).irDefn,
                      clas.constructors[1])
        classInfo = info.getClassInfo(clas)
        self.assertIs(clas, classInfo.irDefn)
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
                           self.makeVariable(Name(["C", "x"]), kind=PARAMETER,
                                             flags=frozenset([LET])),
                           self.makeVariable(Name(["C", "y"]), kind=PARAMETER,
                                             flags=frozenset([LET]))],
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
        info = self.analyzeFromSource("def f(x: i32) = x")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].parameters[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("f.x", kind=PARAMETER,
                                                     flags=frozenset([LET])),
                                   scopeId, False),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionParameterVar(self):
        info = self.analyzeFromSource("def f(var x: i32) = x")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].parameters[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("f.x", kind=PARAMETER), scopeId, False),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionVar(self):
        info = self.analyzeFromSource("def f = { var x = 12; }")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].body.statements[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("f.x", kind=LOCAL), scopeId, False),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionConst(self):
        info = self.analyzeFromSource("def f = { let x = 12; }")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].body.statements[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeVariable("f.x", kind=LOCAL, flags=frozenset([LET])),
                                   scopeId, False),
                          info.getDefnInfo(astDefn))

    def testDefineFunctionFunction(self):
        info = self.analyzeFromSource("def f = { def g = 12; };")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].body.statements[0]
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        expected = self.makeFunction("f.g")
        self.assertEquals(DefnInfo(expected, scopeId, False), info.getDefnInfo(astDefn))

    def testDefineFunctionClass(self):
        info = self.analyzeFromSource("def f = { class C {}; };")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].body.statements[0]
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        defnInfo = info.getDefnInfo(astDefn)
        self.assertEquals(Name(["f", "C"]), defnInfo.irDefn.name)
        self.assertEquals(scopeId, defnInfo.scopeId)

    def testDefineClassVar(self):
        info = self.analyzeFromSource("class C { var x: i32; };")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].members[0].pattern
        scopeId = info.getScope(ast.modules[0].definitions[0]).scopeId
        self.assertEquals(DefnInfo(self.makeField("C.x"), scopeId, True),
                          info.getDefnInfo(astDefn))

    def testDefineClassConst(self):
        info = self.analyzeFromSource("class C { let x: i32; };")
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
                           self.makeVariable("C.y", kind=LOCAL)],
                          irInitializer.variables)

    def testDefineClassFunction(self):
        info = self.analyzeFromSource("class C { def f = 12; };")
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
        info = self.analyzeFromSource("class C { static def f = 12; };")
        astDefn = info.ast.modules[0].definitions[0].members[0]
        scopeId = info.getScope(info.ast.modules[0].definitions[0]).scopeId
        expectedFunction = self.makeFunction("C.f", flags=frozenset([STATIC, METHOD]))
        expectedDefnInfo = DefnInfo(expectedFunction, scopeId, True)
        self.assertEquals(expectedDefnInfo, info.getDefnInfo(astDefn))

    @unittest.skip("inner classes not supported yet")
    def testDefineClassClass(self):
        info = self.analyzeFromSource("class C { class D; };")
        ast = info.ast
        astDefn = ast.modules[0].definitions[0].members[0]
        self.assertEquals(DefnInfo(self.makeClass("D")), info.getDefnInfo(astDefn))

    def testDefineGlobalTrait(self):
        info = self.analyzeFromSource("public trait Tr")
        astDefn = info.ast.modules[0].definitions[0]
        defnInfo = info.getDefnInfo(astDefn)
        irTrait = defnInfo.irDefn
        self.assertEquals(Name(["Tr"]), irTrait.name)
        self.assertEquals(frozenset([PUBLIC]), irTrait.flags)
        self.assertEquals(GLOBAL_SCOPE_ID, defnInfo.scopeId)
        self.assertTrue(info.getScope(GLOBAL_SCOPE_ID).isDefined("Tr"))
        classInfo = info.getClassInfo(irTrait)
        self.assertIs(irTrait, classInfo.irDefn)

    def testVarDefinedInBlock(self):
        info = self.analyzeFromSource("def f = {\n" +
                                      "  {\n" +
                                      "    var x = 12;\n" +
                                      "  };\n" +
                                      "};")
        ast = info.ast
        defnInfo = info.getDefnInfo(ast.modules[0].definitions[0].body.statements[0].statements[0].pattern)
        self.assertEquals(self.makeVariable("f.x", kind=LOCAL), defnInfo.irDefn)
        self.assertIs(info.getScope(ast.modules[0].definitions[0].body.statements[0].id),
                      info.getScope(defnInfo.scopeId))

    def testVarDefinedInCatch(self):
        info = self.analyzeFromSource("def f = try 12 catch\n" +
                                      "  case x => x\n")
        ast = info.ast
        defnInfo = info.getDefnInfo(ast.modules[0].definitions[0].body.catchHandler.cases[0].pattern)
        self.assertEquals(self.makeVariable("f.x", kind=LOCAL, flags=frozenset([LET])),
                          defnInfo.irDefn)
        self.assertIs(info.getScope(ast.modules[0].definitions[0].body.catchHandler.cases[0].id),
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
        f = info.package.findFunction(name="C.f")
        self.assertEquals(frozenset([METHOD, PRIVATE]), f.flags)

    def testPublicClassDefaultConstructor(self):
        info = self.analyzeFromSource("public class C")
        C = info.package.findClass(name="C")
        self.assertIn(PUBLIC, C.flags)
        self.assertEquals(1, len(C.constructors))
        self.assertIn(PUBLIC, C.constructors[0].flags)

    def testPublicClassPrimaryConstructor(self):
        info = self.analyzeFromSource("public class C(x: i64)")
        C = info.package.findClass(name="C")
        self.assertIn(PUBLIC, C.flags)
        self.assertEquals(1, len(C.constructors))
        self.assertIn(PUBLIC, C.constructors[0].flags)

    def testPublicClassPrivatePrimaryConstructor(self):
        info = self.analyzeFromSource("public class C private (x: i64)")
        C = info.package.findClass(name="C")
        self.assertIn(PRIVATE, C.constructors[0].flags & frozenset([PUBLIC, PROTECTED, PRIVATE]))

    def testAbstractFinalClass(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "abstract final class C")

    @unittest.skip("private classes not supported yet")
    def testPrivateClassPrimaryConstructor(self):
        info = self.analyzeFromSource("private class C(x: i64)")
        C = info.package.findClass(name="C")
        self.assertIn(PRIVATE, C.flags)
        self.assertEquals(1, len(C.constructors))
        self.assertEquals(frozenset([]),
                          C.constructors[0].flags & frozenset([PUBLIC, PROTECTED, PRIVATE]))

    def testFunctionMustHaveBody(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "def f: i64")

    def testNativeFunctionMustNotHaveBody(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "native def f = 12")

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
        info = self.analyzeFromSource("def f[static T] = {}")
        T = info.package.findTypeParameter(name="f.T")
        self.assertEquals(Name.fromString("f.T"), T.name)
        self.assertEquals(frozenset([STATIC]), T.flags)
        f = info.package.findFunction(name="f")
        self.assertEquals(1, len(f.typeParameters))
        self.assertIs(T, f.typeParameters[0])

    def testFunctionOuterTypeParameter(self):
        info = self.analyzeFromSource("def f[static T](t: T) =\n" +
                                      "  def g = t")
        g = info.package.findFunction(name="f.g")
        self.assertEquals(1, len(g.typeParameters))
        T = info.package.findTypeParameter(name="f.T")
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
        T = info.package.findTypeParameter(name="Box.T")
        get = info.package.findFunction(name="Box.get")
        set = info.package.findFunction(name="Box.set")
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
        scope = info.getScope(GLOBAL_SCOPE_ID)
        nameInfo = scope.lookupFromSelf("x", NoLoc)
        self.assertTrue(nameInfo.isOverloaded())
        importedDefnInfo = next(di for di in nameInfo.iterOverloads() if di.irDefn is m)
        self.assertEquals(GLOBAL_SCOPE_ID, importedDefnInfo.scopeId)
        self.assertFalse(importedDefnInfo.isVisible)
        self.assertIsNone(importedDefnInfo.importedTypeArguments)

    def testImportGlobalFromPackage(self):
        foo = Package(name=Name(["foo"]))
        foo.addGlobal(Name(["bar"]), flags=frozenset([PUBLIC, LET]))
        foo.addGlobal(Name(["baz"]), flags=frozenset([LET]))
        source = "import foo._"
        info = self.analyzeFromSource(source, packageLoader=FakePackageLoader([foo]))
        scope = info.getScope(GLOBAL_SCOPE_ID)
        self.assertFalse(scope.isBound("baz"))
        scope.define("bar")
        nameInfo = scope.lookupFromSelf("bar", NoLoc)
        importedDefnInfo = nameInfo.getDefnInfo()
        self.assertEquals(GLOBAL_SCOPE_ID, importedDefnInfo.scopeId)
        self.assertFalse(importedDefnInfo.isVisible)
        self.assertIsNone(importedDefnInfo.importedTypeArguments)

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
        expected = self.makeTypeParameter(Name([EXISTENTIAL_SUFFIX, "X"]), flags=frozenset())
        self.assertEquals(expected, X)

    def testExistentialTypeWithFlags(self):
        source = "let x: forsome [static +X] X"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)


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
        clas = package.addClass(Name(["C"]), typeParameters=[],
                                supertypes=[getRootClassType()], flags=frozenset([PUBLIC]))
        classType = ClassType(clas)
        publicCtor = package.addFunction(Name(["C", CONSTRUCTOR_SUFFIX]), returnType=UnitType,
                                         typeParameters=[], parameterTypes=[classType],
                                         flags=frozenset([PUBLIC, METHOD, EXTERN]))
        privateCtor = package.addFunction(Name(["C", CONSTRUCTOR_SUFFIX]), returnType=UnitType,
                                          typeParameters=[], parameterTypes=[classType],
                                          flags=frozenset([PRIVATE, METHOD, EXTERN]))
        clas.constructors = [publicCtor, privateCtor]
        publicMethod = package.addFunction(Name(["C", "m1"]), returnType=UnitType,
                                           typeParameters=[], parameterTypes=[classType],
                                           flags=frozenset([PUBLIC, METHOD, EXTERN]))
        privateMethod = package.addFunction(Name(["C", "m2"]), returnType=UnitType,
                                            typeParameters=[], parameterTypes=[classType],
                                            flags=frozenset([PRIVATE, METHOD, EXTERN]))
        clas.methods = [publicMethod, privateMethod]
        publicField = package.newField(Name(["C", "x"]), type=UnitType,
                                       flags=frozenset([PUBLIC, EXTERN]))
        privateField = package.newField(Name(["C", "y"]), type=UnitType,
                                        flags=frozenset([PRIVATE, EXTERN]))
        clas.fields = [publicField, privateField]

        packageLoader = FakePackageLoader([package])
        info = CompileInfo(None, Package(id=TARGET_PACKAGE_ID), packageLoader)
        topPackageScope = PackageScope(PACKAGE_SCOPE_ID, None, info,
                                       packageLoader.getPackageNames(), [], None)
        fooPackageScope = topPackageScope.scopeForPrefix("foo", NoLoc)

        defnInfo = fooPackageScope.lookupFromSelf("C", NoLoc).getDefnInfo()
        self.assertIs(clas, defnInfo.irDefn)
        self.assertIs(fooPackageScope.scopeId, defnInfo.scopeId)
        classScope = info.getScope(clas.id)
        defnInfo = classScope.lookupFromSelf(CONSTRUCTOR_SUFFIX, NoLoc).getDefnInfo()
        self.assertIs(publicCtor, defnInfo.irDefn)
        self.assertIs(classScope.scopeId, defnInfo.scopeId)
        defnInfo = classScope.lookupFromSelf("m1", NoLoc).getDefnInfo()
        self.assertIs(publicMethod, defnInfo.irDefn)
        self.assertRaises(ScopeException, classScope.lookupFromSelf, "m2", NoLoc)
        defnInfo = classScope.lookupFromSelf("x", NoLoc).getDefnInfo()
        self.assertIs(publicField, defnInfo.irDefn)
        self.assertRaises(ScopeException, classScope.lookupFromSelf, "y", NoLoc)

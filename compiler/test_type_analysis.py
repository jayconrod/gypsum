# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from ast import *
from compile_info import *
from errors import *
from ir import *
from ir_types import *
from layout import layout
from lexer import *
from parser import *
from scope_analysis import *
from type_analysis import *


class TestTypeAnalysis(unittest.TestCase):
    def analyzeFromSource(self, source):
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        info = CompileInfo(ast)
        analyzeDeclarations(info)
        analyzeInheritance(info)
        analyzeTypes(info)
        return info

    # Module
    def testRecursiveNoType(self):
        source = "def f(x) = f(x)"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testRecursiveParamTypeOnly(self):
        source = "def f(x: i32) = f(x)"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testRecursiveReturnTypeOnly(self):
        source = "def f(x): i32 = f(x)"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testRecursiveGlobal(self):
        source = "def f = x\n" + \
                 "let x = f"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testRecursiveFullType(self):
        source = "def f(x: i32): i32 = f(x)"
        info = self.analyzeFromSource(source)
        f = info.package.functions[0]
        self.assertEquals(I32Type, f.returnType)
        self.assertEquals([I32Type], f.parameterTypes)

    def testMutuallyRecursiveNoType(self):
        source = "def f(x) = g(x)\n" + \
                 "def g(x) = f(x)"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testMutuallyRecursiveWithType(self):
        source = "def f(x: i32): i32 = g(x)\n" + \
                 "def g(x: i32): i32 = f(x)"
        info = self.analyzeFromSource(source)
        # pass if this does not raise an error

    def testNoReturnTypeRequiresBody(self):
        source = "abstract class C\n" + \
                 "  abstract def f"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    # Definitions
    def testConstructorsMayNotHaveReturnType(self):
        source = "class Foo\n" + \
                 "  def this: i32 = 12"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testConstructorsMayNotReturnValue(self):
        source = "class Foo\n" + \
                 "  def this = return 12"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testPrimaryConstructorsReturnUnit(self):
        source = "class Foo()"
        info = self.analyzeFromSource(source)
        clas = info.package.findClass(name="Foo")
        self.assertEquals(UnitType, clas.constructors[0].returnType)

    def testPrimaryUnaryConstructorReturnUnit(self):
        source = "class Foo(x: i32)"
        info = self.analyzeFromSource(source)
        clas = info.package.findClass(name="Foo")
        ctor = clas.constructors[0]
        self.assertEquals(UnitType, ctor.returnType)
        self.assertEquals([ClassType(clas), I32Type], ctor.parameterTypes)
        self.assertEquals([ClassType(clas)], [v.type for v in ctor.variables])

    def testSecondaryConstructorsReturnUnit(self):
        source = "class Foo\n" + \
                 "  def this = 12"
        info = self.analyzeFromSource(source)
        clas = info.package.findClass(name="Foo")
        self.assertEquals(UnitType, clas.constructors[0].returnType)

    def testInitializerAndDefaultConstructorThisType(self):
        source = "class Foo"
        info = self.analyzeFromSource(source)
        clas = info.package.findClass(name="Foo")
        thisType = ClassType(clas)
        ctor = clas.constructors[0]
        self.assertEquals([thisType], clas.constructors[0].parameterTypes)
        self.assertEquals(Variable("$this", thisType, PARAMETER, frozenset([LET])),
                          ctor.variables[0])
        init = clas.initializer
        self.assertEquals([thisType], clas.constructors[0].parameterTypes)
        self.assertEquals(Variable("$this", thisType, PARAMETER, frozenset([LET])),
                          init.variables[0])

    def testVariableWithoutType(self):
        self.assertRaises(TypeException, self.analyzeFromSource, "var x")

    def testConstructorBeforeField(self):
        source = "class Foo\n" + \
                 "  def this(x: i32) =\n" + \
                 "    this.x = x\n" + \
                 "  var x: i32\n"
        info = self.analyzeFromSource(source)
        body = info.ast.definitions[0].members[0].body
        self.assertEquals(I32Type, info.getType(body.statements[0].left))

    def testUnusedDefaultConstructor(self):
        source = "class Foo"
        info = self.analyzeFromSource(source)
        clas = info.package.findClass(name="Foo")
        ctor = clas.constructors[0]
        self.assertEquals([ClassType(clas)], ctor.parameterTypes)
        self.assertEquals(UnitType, ctor.returnType)

    def testUseBeforeCapturedVar(self):
        source = "def f =\n" + \
                 "  def g = i = 1\n" + \
                 "  var i = 0"
        info = self.analyzeFromSource(source)
        statements = info.ast.definitions[0].body.statements
        self.assertEquals(I64Type, info.getType(statements[0].body.left))
        self.assertEquals(I64Type, info.getType(statements[1].pattern))

    # Expressions
    def testIntLiteral(self):
        source = "var x = 12"
        info = self.analyzeFromSource(source)
        self.assertEquals(I64Type, info.package.findGlobal(name="x").type)

    def testIntLiteralBounds(self):
        self.assertRaises(TypeException, self.analyzeFromSource, "var x = 128i8")
        self.assertRaises(TypeException, self.analyzeFromSource, "var x = -129i8")

    def testIntLiteralWidths(self):
        self.assertRaises(TypeException, self.analyzeFromSource, "var x = 0i7")

    def testFloatLiteral(self):
        source = "var x = 1.2f64\n" + \
                 "var y = 3.4f32"
        info = self.analyzeFromSource(source)
        self.assertEquals(F64Type, info.package.findGlobal(name="x").type)
        self.assertEquals(F32Type, info.package.findGlobal(name="y").type)

    def testFloatLiteralWidths(self):
        source = "var x = 1.2f42"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testStringLiteral(self):
        source = "var s = \"foo\""
        info = self.analyzeFromSource(source)
        self.assertEquals(getStringType(), info.package.findGlobal(name="s").type)

    def testBooleanLiteral(self):
        source = "var x = true"
        info = self.analyzeFromSource(source)
        self.assertEquals(BooleanType, info.package.findGlobal(name="x").type)

    def testNullLiteral(self):
        source = "var x = null"
        info = self.analyzeFromSource(source)
        self.assertEquals(getNullType(), info.package.findGlobal(name="x").type)

    def testLocalVariable(self):
        source = "def f(x: i32) = x"
        info = self.analyzeFromSource(source)
        self.assertEquals(I32Type, info.package.findFunction(name="f").variables[0].type)
        self.assertEquals(I32Type, info.getType(info.ast.definitions[0].body))

    def testFunctionVariable(self):
        source = "def f: i32 = 12i32\n" + \
                 "def g = f\n"
        info = self.analyzeFromSource(source)
        self.assertEquals(I32Type, info.package.findFunction(name="g").returnType)
        self.assertEquals(I32Type, info.getType(info.ast.definitions[1].body))

    def testThisExpr(self):
        source = "class Foo\n" + \
                 "  var x = this"
        info = self.analyzeFromSource(source)
        clas = info.package.findClass(name="Foo")
        self.assertEquals(ClassType(clas),
                          info.getType(info.ast.definitions[0].members[0].expression))

    def testSuperExpr(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo\n" + \
                 "  def this = super()"
        info = self.analyzeFromSource(source)
        foo = info.package.findClass(name="Foo")
        expr = info.ast.definitions[1].members[0].body.callee
        self.assertEquals(ClassType(foo), info.getType(expr))

    def testBlockEmpty(self):
        source = "def f = {}"
        info = self.analyzeFromSource(source)
        self.assertEquals(UnitType, info.package.findFunction(name="f").returnType)
        self.assertEquals(UnitType, info.getType(info.ast.definitions[0].body))

    def testBlockSingleExpr(self):
        source = "def f = { 12; };"
        info = self.analyzeFromSource(source)
        self.assertEquals(I64Type, info.package.findFunction(name="f").returnType)
        self.assertEquals(I64Type, info.getType(info.ast.definitions[0].body))

    def testBlockEndsWithDefn(self):
        source = "def f =\n" + \
                 "  12\n" + \
                 "  var x = 34\n"
        info = self.analyzeFromSource(source)
        self.assertEquals(UnitType, info.package.findFunction(name="f").returnType)
        self.assertEquals(UnitType, info.getType(info.ast.definitions[0].body))
        self.assertFalse(info.hasType(info.ast.definitions[0].body.statements[1]))

    def testAssign(self):
        source = "def f(x: i64) = x = 12\n"
        info = self.analyzeFromSource(source)
        self.assertEquals(I64Type, info.package.findFunction(name="f").returnType)
        self.assertEquals(I64Type, info.getType(info.ast.definitions[0].body))

    def testAssignWrongType(self):
        source = "def f(x: i32) = x = true\n"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testPropertyNonExistant(self):
        source = "class Foo\n" + \
                 "def f(o: Foo) = o.x"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testPropertyField(self):
        source = "class Foo\n" + \
                 "  var x: boolean\n" + \
                 "def f(o: Foo) = o.x"
        info = self.analyzeFromSource(source)
        self.assertEquals(BooleanType, info.package.findFunction(name="f").returnType)
        self.assertEquals(BooleanType, info.getType(info.ast.definitions[1].body))

    def testPropertyNullaryMethod(self):
        source = "class Foo\n" + \
                 "  def m = false\n" + \
                 "def f(o: Foo) = o.m"
        info = self.analyzeFromSource(source)
        self.assertEquals(BooleanType, info.package.findFunction(name="f").returnType)
        self.assertEquals(BooleanType, info.package.findFunction(name="m").returnType)
        self.assertEquals(BooleanType, info.getType(info.ast.definitions[1].body))

    def testCallMethodWithNullableReceiver(self):
        source = "class Foo\n" + \
                 "  def m = {}\n" + \
                 "def f(o: Foo?) = o.m"
        info = self.analyzeFromSource(source)
        self.assertEquals(UnitType, info.package.findFunction(name="f").returnType)

    def testIntegerMethod(self):
        source = "def f(n: i64) = n.to-i32"
        info = self.analyzeFromSource(source)
        self.assertEquals(I32Type, info.package.findFunction(name="f").returnType)

    def testCall(self):
        source = "def f(x: i64, y: boolean) = x\n" + \
                 "def g = f(1, true)"
        info = self.analyzeFromSource(source)
        self.assertEquals(I64Type, info.package.findFunction(name="g").returnType)
        self.assertEquals(I64Type, info.getType(info.ast.definitions[1].body))

    def testCallWrongNumberOfArgs(self):
        source = "def f(x: i32, y: boolean) = x\n" + \
                 "def g = f(1)\n"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testCtorWrongNumberOfArgs(self):
        source = "class Foo\n" + \
                 "  def this(x: i32, y: i32) = {}\n" + \
                 "def f = Foo(12)\n"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testMethodWrongNumberOfArgs(self):
        source = "class Foo\n" + \
                 "  def m(x: i32) = x\n" + \
                 "def f(o: Foo) = o.m(1, 2)"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testCallWrongArgs(self):
        source = "def f(x: i32, y: boolean) = x\n" + \
                 "def g = f(true, 1)"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testCallNullaryCtor(self):
        info = self.analyzeFromSource("class Foo\n" +
                                      "  def this = {}\n" +
                                      "def f = Foo")
        clas = info.package.findClass(name="Foo")
        function = info.package.findFunction(name="f")
        self.assertEquals(ClassType(clas, ()), function.returnType)
        self.assertEquals(ClassType(clas, ()), info.getType(info.ast.definitions[1].body))

    def testCallCtorWithArgs(self):
        info = self.analyzeFromSource("class Foo\n" +
                                      "  def this(x: i64, y: i64) = {}\n" +
                                      "def f = Foo(1, 2)\n")
        clas = info.package.findClass(name="Foo")
        function = info.package.findFunction(name="f")
        self.assertEquals(ClassType(clas, ()), function.returnType)
        self.assertEquals(ClassType(clas, ()), info.getType(info.ast.definitions[1].body))

    def testNegExpr(self):
        info = self.analyzeFromSource("def f = -12")
        self.assertEquals(I64Type, info.package.findFunction(name="f").returnType)
        self.assertEquals(I64Type, info.getType(info.ast.definitions[0].body))

    def testAddExpr(self):
        info = self.analyzeFromSource("def f = 12 + 34")
        self.assertEquals(I64Type, info.getType(info.ast.definitions[0].body))

    def testStringConcatExpr(self):
        info = self.analyzeFromSource("def f = \"foo\" + \"bar\"")
        self.assertEquals(getStringType(), info.getType(info.ast.definitions[0].body))

    def testBinopAssignExpr(self):
        info = self.analyzeFromSource("def f =\n" +
                                      "  var x = 12\n" +
                                      "  x += 34\n")
        self.assertEquals(I64Type, info.package.findFunction(name="f").returnType)
        self.assertEquals(I64Type, info.getType(info.ast.definitions[0].body.statements[1]))

    def testAndExpr(self):
        info = self.analyzeFromSource("def f = true && false")
        self.assertEquals(BooleanType, info.getType(info.ast.definitions[0].body))

    def testOrExpr(self):
        info = self.analyzeFromSource("def f = true || false")
        self.assertEquals(BooleanType, info.getType(info.ast.definitions[0].body))

    def testIfExpr(self):
        info = self.analyzeFromSource("def f = if (true) 12 else 34")
        self.assertEquals(I64Type, info.getType(info.ast.definitions[0].body))

    def testIfExprNonBooleanCondition(self):
        source = "def f = if (-1) 12 else 34"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testIfExprNonCombineableBranches(self):
        source = "def f = if (true) false else 34"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testIfExprReturn(self):
        info = self.analyzeFromSource("def f = if (true) return 34 else 12")
        self.assertEquals(I64Type, info.getType(info.ast.definitions[0].body))

    def testIfExprWithoutElse(self):
        info = self.analyzeFromSource("def f = if (true) 12")
        self.assertEquals(UnitType, info.getType(info.ast.definitions[0].body))

    def testWhileExpr(self):
        info = self.analyzeFromSource("def f = while (true) 12")
        self.assertEquals(UnitType, info.getType(info.ast.definitions[0].body))

    def testThrowExpr(self):
        info = self.analyzeFromSource("def f(exn: Exception) = throw exn")
        self.assertEquals(ClassType(getNothingClass()),
                          info.package.findFunction(name="f").returnType)
        self.assertEquals(NoType, info.getType(info.ast.definitions[0].body))

    def testThrowNonException(self):
        self.assertRaises(TypeException, self.analyzeFromSource, "def f = throw 12")

    def testThrowInIfCondition(self):
        info = self.analyzeFromSource("def f(exn: Exception) = if (throw exn) 12")
        self.assertEquals(NoType, info.getType(info.ast.definitions[0].body.condition))

    def testThrowInWhileCondition(self):
        source = "def f(exn: Exception) = while (throw exn) {}"
        info = self.analyzeFromSource(source)
        self.assertEquals(NoType, info.getType(info.ast.definitions[0].body.condition))

    def testTryExpr(self):
        info = self.analyzeFromSource("class Base\n" +
                                      "class A <: Base\n" +
                                      "  def this = {}\n" +
                                      "class B <: Base\n" +
                                      "  def this = {}\n" +
                                      "def f = try A catch\n" +
                                      "    case exn => B\n" +
                                      "  finally 12")
        baseTy = ClassType(info.package.findClass(name="Base"), ())
        astBody = info.ast.definitions[3].body
        self.assertEquals(baseTy, info.getType(astBody))
        self.assertEquals(ClassType(getExceptionClass()),
                          info.getType(astBody.catchHandler.cases[0].pattern))
        self.assertEquals(I64Type, info.getType(astBody.finallyHandler))

    def testTryCombineBadCatch(self):
        self.assertRaises(TypeException, self.analyzeFromSource,
                          "def f = try 12 catch\n" +
                          "    case exn => exn")

    def testTryBadCondition(self):
        self.assertRaises(TypeException, self.analyzeFromSource,
                          "def f = try 12 catch\n" +
                          "  case exn if 34 => 56")

    def testTryCatchSubtype(self):
        source = "class Foo <: Exception\n" + \
                 "def f = try 12 catch { case exn: Foo => 34; }"
        info = self.analyzeFromSource(source)
        exnClass = info.package.findClass(name="Foo")
        exnTy = info.getType(info.ast.definitions[1].body.catchHandler.cases[0].pattern)
        self.assertIs(exnClass, exnTy.clas)
        self.assertIs(exnClass, info.package.findFunction(name="f").variables[0].type.clas)

    def testWhileExprNonBooleanCondition(self):
        self.assertRaises(TypeException, self.analyzeFromSource, "def f = while (-1) 12")

    def testReturnExpression(self):
        info = self.analyzeFromSource("def f = return 12")
        self.assertEquals(NoType, info.getType(info.ast.definitions[0].body))
        self.assertEquals(I64Type, info.package.findFunction(name="f").returnType)

    def testReturnExpressionInGlobal(self):
        source = "var g = return 12"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testReturnExpressionInClass(self):
        source = "class C\n" + \
                 "  var x = return 12"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testReturnEmpty(self):
        info = self.analyzeFromSource("def f = return")
        self.assertEquals(NoType, info.getType(info.ast.definitions[0].body))
        self.assertEquals(UnitType, info.package.findFunction(name="f").returnType)

    def testConstructRootClass(self):
        info = self.analyzeFromSource("def f = Object")
        rootClass = getRootClass()
        self.assertEquals(ClassType(rootClass, ()), info.getType(info.ast.definitions[0].body))

    # Types
    def testUnitType(self):
        info = self.analyzeFromSource("var g: unit")
        self.assertEquals(UnitType, info.package.findGlobal(name="g").type)

    def testI32Type(self):
        info = self.analyzeFromSource("var g: i32")
        self.assertEquals(I32Type, info.package.findGlobal(name="g").type)

    def testF32Type(self):
        info = self.analyzeFromSource("var g: f32")
        self.assertEquals(F32Type, info.package.findGlobal(name="g").type)

    def testBooleanType(self):
        info = self.analyzeFromSource("var g: boolean")
        self.assertEquals(BooleanType, info.package.findGlobal(name="g").type)

    def testRootClassType(self):
        info = self.analyzeFromSource("var g: Object")
        rootClass = getRootClass()
        self.assertEquals(ClassType(rootClass, ()), info.package.findGlobal(name="g").type)

    def testNullableType(self):
        info = self.analyzeFromSource("var g: Object?")
        expected = ClassType(getRootClass(), (), frozenset([NULLABLE_TYPE_FLAG]))
        self.assertEquals(expected, info.package.findGlobal(name="g").type)

    def testCallBuiltin(self):
        info = self.analyzeFromSource("def f = print(\"foo\")")
        self.assertEquals(UnitType, info.getType(info.ast.definitions[0].body))

    # Closures
    def testFunctionContextFields(self):
        source = "def f(x: i32) =\n" + \
                 "  def g = x\n" + \
                 "  g"
        info = self.analyzeFromSource(source)
        self.assertEquals(I32Type,
                          info.getType(info.ast.definitions[0].body.statements[0].body))
        self.assertEquals(I32Type, info.getType(info.ast.definitions[0].body.statements[1]))
        self.assertEquals(I32Type, info.package.findFunction(name="f").returnType)
        self.assertEquals(I32Type, info.package.findFunction(name="g").returnType)

    # Inheritance
    def testSupertypes(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo"
        info = self.analyzeFromSource(source)
        fooClass = info.package.findClass(name="Foo")
        self.assertEquals([ClassType(getRootClass(), ())], fooClass.supertypes)
        barClass = info.package.findClass(name="Bar")
        self.assertEquals([ClassType(fooClass, ())], barClass.supertypes)

    def testNullableSupertype(self):
        source = "class Foo <: Object?"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testNullableBounds(self):
        upperSource = "class Foo[static T <: Object?]"
        self.assertRaises(TypeException, self.analyzeFromSource, upperSource)
        lowerSource = "class Bar[static T >: Nothing?]"
        self.assertRaises(TypeException, self.analyzeFromSource, lowerSource)

    def testCallWithSubtype(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo\n" + \
                 "def f(foo: Foo) = foo\n" + \
                 "def g(bar: Bar) =\n" + \
                 "  var x = f(bar)\n"
        info = self.analyzeFromSource(source)
        fooClass = info.package.findClass(name="Foo")
        barClass = info.package.findClass(name="Bar")
        astCall = info.ast.definitions[3].body.statements[0].expression
        self.assertEquals(ClassType(barClass, ()), info.getType(astCall.arguments[0]))
        self.assertEquals(ClassType(fooClass, ()), info.getType(astCall))

    def testFunctionReturnBodyWithSubtype(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo\n" + \
                 "def f(bar: Bar): Foo = bar"
        info = self.analyzeFromSource(source)
        f = info.package.findFunction(name="f")
        fooClass = info.package.findClass(name="Foo")
        barClass = info.package.findClass(name="Bar")
        self.assertEquals(ClassType(fooClass, ()), f.returnType)
        self.assertEquals(ClassType(barClass, ()), info.getType(info.ast.definitions[2].body))

    def testFunctionReturnStatementWithSubtype(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo\n" + \
                 "def f(bar: Bar): Foo = return bar"
        info = self.analyzeFromSource(source)
        f = info.package.findFunction(name="f")
        fooClass = info.package.findClass(name="Foo")
        barClass = info.package.findClass(name="Bar")
        self.assertEquals(ClassType(fooClass, ()), f.returnType)

    def testCallInheritedMethod(self):
        source = "class Foo\n" + \
                 "  def m = 12\n" + \
                 "class Bar <: Foo\n" + \
                 "def f(bar: Bar) = bar.m"
        info = self.analyzeFromSource(source)
        f = info.package.findFunction(name="f")
        self.assertEquals(I64Type, f.returnType)

    def testSimpleOverride(self):
        source = "class A\n" + \
                 "class Foo\n" + \
                 "  def m(a: A) = a\n" + \
                 "class Bar\n" + \
                 "  def m(a: A) = a\n"
        info = self.analyzeFromSource(source)
        fooClass = info.package.findClass(name="Foo")
        barClass = info.package.findClass(name="Bar")
        self.assertEquals(len(fooClass.methods), len(barClass.methods))
        self.assertIs(barClass, barClass.methods[-1].clas)

    def testBuiltinOverride(self):
        source = "def f = \"foo\".to-string"
        info = self.analyzeFromSource(source)
        useInfo = info.getUseInfo(info.ast.definitions[0].body)
        receiverClass = useInfo.defnInfo.irDefn.clas
        self.assertIs(getStringClass(), receiverClass)

    def testRecursiveOverrideBuiltinWithoutReturnType(self):
        source = "class List(value: String, next: List?)\n" + \
                 "  def to-string = value + if (next !== null) next.to-string else \"\""
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testRecursiveOverrideBuiltin(self):
        source = "class List(value: String, next: List?)\n" + \
                 "  def to-string: String = value + if (next !== null) next.to-string else \"\""
        info = self.analyzeFromSource(source)
        List = info.package.findClass(name="List")
        useInfo = info.getUseInfo(info.ast.definitions[0].members[0].body.right.trueExpr)
        receiverClass = useInfo.defnInfo.irDefn.clas
        self.assertIs(List, receiverClass)

    def testOverrideWithImplicitTypeParameters(self):
        source = "class A[static T]\n" + \
                 "  def to-string = \"A\"\n"
        info = self.analyzeFromSource(source)
        A = info.package.findClass(name="A")
        toString = A.getMethod("to-string")
        self.assertIs(A, toString.clas)
        self.assertIs(toString.override, getRootClass().getMethod("to-string"))

    def testOverrideCovariantParameters(self):
        source = "class A\n" + \
                 "class B <: A\n" + \
                 "class Foo\n" + \
                 "  def m(b: B) = this\n" + \
                 "class Bar <: Foo\n" + \
                 "  def m(a: A) = this\n"
        info = self.analyzeFromSource(source)
        fooClass = info.package.findClass(name="Foo")
        barClass = info.package.findClass(name="Bar")
        self.assertEquals(len(fooClass.methods), len(barClass.methods))
        self.assertIs(fooClass.methods[-1], barClass.methods[-1].override)

    def testOverrideContravariantReturn(self):
        source = "class A\n" + \
                 "  def this = {}\n" + \
                 "class B <: A\n" + \
                 "  def this = {}\n" + \
                 "class Foo\n" + \
                 "  def m = A\n" + \
                 "class Bar <: Foo\n" + \
                 "  def m = B\n"
        info = self.analyzeFromSource(source)
        fooClass = info.package.findClass(name="Foo")
        barClass = info.package.findClass(name="Bar")
        self.assertIs(fooClass.methods[-1], barClass.methods[-1].override)

    def testAmbiguousOverloadWithoutCall(self):
        source = "def f = 12\n" + \
                 "def f = 34\n"
        self.analyzeFromSource(source)
        # pass if no error

    def testAmbiguousOverloadWithoutCallInClass(self):
        source = "class Foo\n" + \
                 "  def f = 12\n" + \
                 "  def f = 34\n"
        self.analyzeFromSource(source)
        # pass if no error

    def testAmbiguousOverload(self):
        source = "def f = 12\n" + \
                 "def f = 34\n" + \
                 "var x = f"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testAmbiguousOverride(self):
        source = "class Foo\n" + \
                 "  def f = 12\n" + \
                 "  def f = 34\n" + \
                 "class Bar <: Foo\n" + \
                 "  def f = 56"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    @unittest.skip("compiler bug")
    def testBrokenOverride(self):
        # This test exposes a bug in the compiler. Before we resolve overloads/overrides, we
        # call ensureParamTypeInfoForDefn, which only processes the body of possible target
        # functions. Maybe we should call ensureTypeInfo. Or we should require functions
        # with the `override` keyword to have explicit types. Or we should assume `override`
        # functions have the same type if not specified.
        source = "class A\n" + \
                 "class B <: A\n" + \
                 "def test(d: D) = d.f\n" + \
                 "class C\n" + \
                 "  def f: B = B\n" + \
                 "class D\n" + \
                 "  def f = A"
        self.analyzeFromSource(source)

    def testAmbiguousOverload(self):
        source = "class Foo\n" + \
                 "  def f = 12\n" + \
                 "class Bar <: Foo\n" + \
                 "  def f = 34\n" + \
                 "  def f = 56"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testSimpleOverload(self):
        source = "def f(x: i32) = 2i32 * x\n" + \
                 "def f(x: f32) = 2.f32 * x\n" + \
                 "def g =\n" + \
                 "  f(1i32)\n" + \
                 "  f(1f32)\n"
        info = self.analyzeFromSource(source)
        statements = info.ast.definitions[2].body.statements
        self.assertEquals(I32Type, info.getType(statements[0]))
        self.assertEquals(F32Type, info.getType(statements[1]))

    def testOverloadWithTypeParameter(self):
        source = "def f[static T] = {}\n" + \
                 "def f = {}\n" + \
                 "def g = f[Object]"
        info = self.analyzeFromSource(source)
        use = info.getUseInfo(info.ast.definitions[2].body)
        f = info.package.findFunction(name="f", pred=lambda fn: len(fn.typeParameters) == 1)
        self.assertIs(use.defnInfo.irDefn, f)

    def testOverloadOnTypeParameterBounds(self):
        source = "class A\n" + \
                 "def f[static T] = {}\n" + \
                 "def f[static T <: A] = {}\n" + \
                 "def g = f[Object]"
        info = self.analyzeFromSource(source)
        use = info.getUseInfo(info.ast.definitions[3].body)
        A = info.package.findClass(name="A")
        pred = lambda fn: len(fn.typeParameters) == 1 and \
                          fn.typeParameters[0].upperBound.clas is getRootClass()
        f = info.package.findFunction(name="f", pred=pred)
        self.assertIs(use.defnInfo.irDefn, f)

    def testIdentityTypeParameter(self):
        source = "def id[static T](o: T) = o\n" + \
                 "def f(o: String) = id[String](o)\n"
        info = self.analyzeFromSource(source)
        f = info.package.findFunction(name="f")
        self.assertEquals(getStringType(), f.returnType)

    def testTypeParametersDependInOrder(self):
        source = "def f[static S, static T <: S, static U <: T] = {}"
        info = self.analyzeFromSource(source)
        # pass if no exception
        source = "def f[static U <: T, static T <: S, static S] = {}"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testTypeParameterLookup(self):
        source = "class C\n" + \
                 "  var x: i64\n" + \
                 "def f[static T <: C](o: T) = o.x"
        info = self.analyzeFromSource(source)
        f = info.package.findFunction(name="f")
        self.assertEquals(I64Type, f.returnType)

    def testPrimitiveTypeArguments(self):
        source = "def id[static T](x: T) = x\n" + \
                 "def f = id[i64](12)"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testUseTypeParameterInnerFunctionExplicit(self):
        source = "def id-outer[static T] =\n" + \
                 "  def id-inner(x: T) = x"
        info = self.analyzeFromSource(source)
        paramType = info.getType(info.ast.definitions[0].body.statements[0].parameters[0])
        T = info.package.findTypeParameter(name="T")
        self.assertEquals(VariableType(T), paramType)
        retTy = info.package.findFunction(name="id-inner").returnType
        self.assertEquals(VariableType(T), retTy)

    def testUseTypeParameterInnerFunctionImplicit(self):
        source = "def id-outer[static T](x: T) =\n" + \
                 "  def id-inner = x"
        info = self.analyzeFromSource(source)
        xType = info.getType(info.ast.definitions[0].body.statements[0].body)
        T = info.package.findTypeParameter(name="T")
        self.assertEquals(VariableType(T), xType)

    def testCallInnerFunctionWithImplicitTypeParameter(self):
        source = "def id-outer[static T](x: T) =\n" + \
                 "  def id-inner = x\n" + \
                 "  id-inner"
        info = self.analyzeFromSource(source)
        callType = info.getType(info.ast.definitions[0].body.statements[1])
        T = info.package.findTypeParameter(name="T")
        self.assertEquals(VariableType(T), callType)

    def testClassWithTypeParameter(self):
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
        TTy = VariableType(T)
        BoxTy = ClassType(Box, (TTy,), None)

        self.assertEquals([BoxTy], Box.initializer.parameterTypes)
        self.assertEquals([BoxTy, TTy], Box.constructors[0].parameterTypes)
        self.assertEquals([BoxTy], get.parameterTypes)
        self.assertEquals(TTy, get.returnType)
        self.assertEquals([BoxTy, TTy], set.parameterTypes)
        self.assertEquals(UnitType, set.returnType)

    def testCallCtorWithTypeParameter(self):
        source = "class C\n" + \
                 "class Box[static T](x: T)\n" + \
                 "def f(c: C) = Box[C](c)"
        info = self.analyzeFromSource(source)
        Box = info.package.findClass(name="Box")
        C = info.package.findClass(name="C")
        f = info.package.findFunction(name="f")
        ty = ClassType(Box, (ClassType(C),))
        self.assertEquals(ty, f.returnType)
        self.assertEquals(ty, info.getType(info.ast.definitions[2].body))

    def testLoadFieldWithTypeParameter(self):
        source = "class Box[static T](value: T)\n" + \
                 "def f(box: Box[String]) = box.value"
        info = self.analyzeFromSource(source)
        self.assertEquals(getStringType(), info.getType(info.ast.definitions[1].body))

    def testLoadInheritedFieldWithTypeParameter(self):
        source = "class Box[static T](value: T)\n" + \
                 "class SubBox <: Box[String]\n" + \
                 "  def this(s: String) = super(s)\n" + \
                 "def f(box: SubBox) = box.value"
        info = self.analyzeFromSource(source)
        self.assertEquals(getStringType(), info.getType(info.ast.definitions[2].body))

    def testStoreSubtypeToTypeParameterField(self):
        source = "class A\n" + \
                 "class B <: A\n" + \
                 "class Box[static T](value: T)\n" + \
                 "def f(box: Box[A], b: B) = box.value = b"
        info = self.analyzeFromSource(source)
        A = info.package.findClass(name="A")
        self.assertEquals(ClassType(A), info.getType(info.ast.definitions[3].body))

    def testCallInheritedMethodWithTypeParameter(self):
        source = "class A[static T](val: T)\n" + \
                 "  def get = val\n" + \
                 "class B <: A[Object]\n" + \
                 "  def this(val: Object) =\n" + \
                 "    super(val)\n" + \
                 "def f(b: B) = b.get"
        info = self.analyzeFromSource(source)
        f = info.package.findFunction(name="f")
        ty = getRootClassType()
        self.assertEquals(ty, f.returnType)
        self.assertEquals([ty], info.getCallInfo(info.ast.definitions[2].body).typeArguments)

    def testOverrideInheritedMethodWithTypeParameter(self):
        source = "abstract class Function[static P, static R]\n" + \
                 "  abstract def apply(x: P): R\n" + \
                 "class AppendString <: Function[String, String]\n" + \
                 "  def apply(x: String): String = x + \"foo\"\n"
        info = self.analyzeFromSource(source)
        abstractApply = info.package.findFunction(name="apply", flag=ABSTRACT)
        AppendString = info.package.findClass(name="AppendString")
        concreteApply = info.package.findFunction(name="apply", clas=AppendString)
        self.assertIs(abstractApply, concreteApply.override)

    def testTypeParameterWithReversedBounds(self):
        source = "class A[static T <: Nothing >: Object]"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testTypeParameterWithUnrelatedBound(self):
        source = "class A[static S, static T, static U <: S >: T]"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testCovariantTypeParameterInConstField(self):
        source = "class Foo[static +T](x: T)"
        self.analyzeFromSource(source)
        # pass if no error

    def testCovariantTypeParameterInVarField(self):
        source = "class Foo[static +T](var x: T)"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testCovariantTypeParameterInMethodParam(self):
        source = "class Foo[static +T]\n" + \
                 "  def m(x: T) = {}"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testCovariantTypeParameterInMethodReturn(self):
        source = "abstract class Foo[static +T]\n" + \
                 "  abstract def m: T"
        self.analyzeFromSource(source)
        # pass if no error

    def testCovariantTypeParameterInCtor(self):
        source = "class Foo[static +T]\n" + \
                 "  def this(x: T) = {}"
        self.analyzeFromSource(source)
        # pass if no error

    def testContravariantTypeParameterInField(self):
        source = "class Foo[static -T](x: T)\n"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testContravariantTypeParameterInMethodParam(self):
        source = "class Foo[static -T]\n" + \
                 "  def m(x: T) = {}"
        self.analyzeFromSource(source)
        # pass if no error

    def testContravariantTypeParameterInMethodReturn(self):
        source = "abstract class Foo[static -T]\n" + \
                 "  abstract def m: T"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testContravariantTypeParameterInInferredMethodReturn(self):
        source = "class Foo[static -T]\n" + \
                 "  def m(x: T) = x"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testCovariantParamInCovariantClass(self):
        source = "class Source[static +S]\n" + \
                 "class Foo[static +T]\n" + \
                 "  def m(x: Source[T]) = {}"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testCovariantParamInContravariantClass(self):
        source = "class Source[static +S]\n" + \
                 "class Foo[static -T]\n" + \
                 "  def m(x: Source[T]) = {}"
        self.analyzeFromSource(source)
        # pass if no error

    def testCovariantReturnInCovariantClass(self):
        source = "class Source[static +S]\n" + \
                 "abstract class Foo[static +T]\n" + \
                 "  abstract def m: Source[T]"
        self.analyzeFromSource(source)
        # pass if no error

    def testCovariantReturnInContravariantClass(self):
        source = "class Source[static +S]\n" + \
                 "abstract class Foo[static -T]\n" + \
                 "  abstract def m: Source[T]"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testContravariantParamInCovariantClass(self):
        source = "class Sink[static -S]\n" + \
                 "class Foo[static +T]\n" + \
                 "  def m(x: Sink[T]) = {}"
        self.analyzeFromSource(source)
        # pass if no error

    def testContravariantParamInContravariantClass(self):
        source = "class Sink[static -S]\n" + \
                 "class Foo[static -T]\n" + \
                 "  def m(x: Sink[T]) = {}"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testContravariantReturnInCovariantClass(self):
        source = "class Sink[static -S]\n" + \
                 "abstract class Foo[static +T]\n" + \
                 "  abstract def m: Sink[T]"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testContravariantReturnInContravariantClass(self):
        source = "class Sink[static -S]\n" + \
                 "abstract class Foo[static -T]\n" + \
                 "  abstract def m: Sink[T]"
        self.analyzeFromSource(source)
        # pass if no error

    def testNoDefaultSuperCtor(self):
        source = "class Foo(x: i64)\n" + \
                 "class Bar <: Foo"
        self.assertRaises(TypeException, self.analyzeFromSource, source)

    def testOverloadedDefaultSuperCtor(self):
        source = "class Foo\n" + \
                 "  def this(x: i64) = {}\n" + \
                 "  def this(x: boolean) = {}\n" + \
                 "class Bar <: Foo(true)"
        info = self.analyzeFromSource(source)
        Foo = info.package.findClass(name="Foo")
        superctor = Foo.constructors[1]
        self.assertIs(superctor, info.getUseInfo(info.ast.definitions[1]).defnInfo.irDefn)

    def testOverloadedPrimarySuperCtor(self):
        source = "class Foo\n" + \
                 "  def this(x: i64) = {}\n" + \
                 "  def this(x: boolean) = {}\n" + \
                 "class Bar(x: boolean) <: Foo(x)"
        info = self.analyzeFromSource(source)
        Foo = info.package.findClass(name="Foo")
        superctor = Foo.constructors[1]
        self.assertIs(superctor, info.getUseInfo(info.ast.definitions[1]).defnInfo.irDefn)

    def testOverloadedAlternateCtor(self):
        source = "class Foo\n" + \
                 "  def this = this(true)\n" + \
                 "  def this(x: i64) = {}\n" + \
                 "  def this(x: boolean) = {}\n"
        info = self.analyzeFromSource(source)
        Foo = info.package.findClass(name="Foo")
        call = info.ast.definitions[0].members[0].body
        calleeCtor = Foo.constructors[2]
        self.assertIs(calleeCtor, info.getUseInfo(call).defnInfo.irDefn)

    # Tests for usage
    def testUseClassBeforeDefinition(self):
        source = "def f = C\n" + \
                 "class C\n" + \
                 "  def this = {}"
        info = self.analyzeFromSource(source)
        ty = ClassType(info.package.findClass(name="C"))
        self.assertEquals(ty, info.getType(info.ast.definitions[0].body))

    def testRedefinedSymbol(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "var x = 12; var x = 34;")

    def testUseGlobalVarInGlobal(self):
        info = self.analyzeFromSource("var x = 12; var y = x;")
        self.assertIs(info.getDefnInfo(info.ast.definitions[0].pattern),
                      info.getUseInfo(info.ast.definitions[1].expression).defnInfo)

    def testUseGlobalVarInFunction(self):
        info = self.analyzeFromSource("var x = 12; def f = x;")
        self.assertIs(info.getDefnInfo(info.ast.definitions[0].pattern),
                      info.getUseInfo(info.ast.definitions[1].body).defnInfo)

    def testUseGlobalVarInClass(self):
        info = self.analyzeFromSource("var x = 12; class C { var y = x; };")
        ast = info.ast
        self.assertIs(info.getDefnInfo(ast.definitions[0].pattern),
                      info.getUseInfo(ast.definitions[1].members[0].expression).defnInfo)

    def testUseGlobalFunctionInGlobal(self):
        info = self.analyzeFromSource("def f = 12; var x = f;")
        self.assertIs(info.getDefnInfo(info.ast.definitions[0]),
                      info.getUseInfo(info.ast.definitions[1].expression).defnInfo)

    def testUseGlobalFunctionInFunction(self):
        info = self.analyzeFromSource("def f = 12; def g = f;")
        self.assertIs(info.getDefnInfo(info.ast.definitions[0]),
                      info.getUseInfo(info.ast.definitions[1].body).defnInfo)

    def testUseGlobalFunctionInClass(self):
        info = self.analyzeFromSource("def f = 12; class C { var x = f; }")
        self.assertIs(info.getDefnInfo(info.ast.definitions[0]),
                      info.getUseInfo(info.ast.definitions[1].members[0].expression).defnInfo)

    def testUseGlobalClassInGlobal(self):
        source = "class C\n" + \
                 "  def this = {}\n" + \
                 "var x = C\n"
        info = self.analyzeFromSource(source)
        self.assertIs(info.getDefnInfo(info.ast.definitions[0].members[0]),
                      info.getUseInfo(info.ast.definitions[1].expression).defnInfo)

    def testUseGlobalClassInFunction(self):
        source = "class C\n" + \
                 "  def this = {}\n" + \
                 "def f = C"
        info = self.analyzeFromSource(source)
        self.assertIs(info.getDefnInfo(info.ast.definitions[0].members[0]),
                      info.getUseInfo(info.ast.definitions[1].body).defnInfo)

    def testUseGlobalClassInClass(self):
        source = "class C\n" + \
                 "  def this = {}\n" + \
                 "class D\n" + \
                 "  var x = C\n"
        info = self.analyzeFromSource(source)
        self.assertIs(info.getDefnInfo(info.ast.definitions[0].members[0]),
                      info.getUseInfo(info.ast.definitions[1].members[0].expression).defnInfo)

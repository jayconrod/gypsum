# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from ast import *
from parser import *
from layout import *
from lexer import *
from combinators import *


class TestParser(unittest.TestCase):
    def parseFromSource(self, parser, text):
        filename = "test"
        rawTokens = lex(filename, text)
        layoutTokens = layout(rawTokens, skipAnalysis=True)
        reader = Reader(filename, layoutTokens)
        result = Phrase(parser)(reader)
        if not result:
            raise ParseException(result.location, result.message)
        return result.value

    def checkParse(self, expected, parser, text):
        value = self.parseFromSource(parser, text)
        self.assertEqual(expected, value)

    # Module
    def testModuleEmpty(self):
        self.checkParse(AstModule([]), module(), "")

    # Definitions
    def testVarDefnEmpty(self):
        self.checkParse(AstVariableDefinition([], AstVariablePattern("x", None), None),
                        varDefn(),
                        "var x;")

    def testVarDefn(self):
        self.checkParse(AstVariableDefinition([],
                                              AstVariablePattern("x", None),
                                              AstVariableExpression("y")),
                        varDefn(),
                        "var x = y;")

    def testVarDefnWithAttribs(self):
        self.checkParse(AstVariableDefinition([AstAttribute("public")],
                                              AstVariablePattern("x", None),
                                              None),
                        varDefn(),
                        "public var x;")

    def testFunctionDefnSimple(self):
        self.checkParse(AstFunctionDefinition([], "f", [], [], None, None),
                        functionDefn(),
                        "def f;")

    def testFunctionDefn(self):
        self.checkParse(AstFunctionDefinition([], "f",
                                              [AstTypeParameter([], "S", None, None),
                                               AstTypeParameter([], "T", None, None)],
                                              [AstParameter([], AstVariablePattern("x", AstClassType("S", []))),
                                               AstParameter([], AstVariablePattern("y", AstClassType("T", [])))],
                                              AstClassType("A", []),
                                              AstVariableExpression("x")),
                        functionDefn(),
                        "def f[S, T](x: S, y: T): A = x;")

    def testFunctionDefnWithAttribs(self):
        self.checkParse(AstFunctionDefinition([AstAttribute("public")],
                                              "f", [], [], None, None),
                        functionDefn(),
                        "public def f;")

    def testClassDefnSimple(self):
        self.checkParse(AstClassDefinition([], "C", [], None, [], []),
                        classDefn(),
                        "class C;")

    def testClassDefnSimpleWithBody(self):
        ctorAst = AstFunctionDefinition([], "this", [], [], None,
                                        AstLiteralExpression(AstIntegerLiteral(12)))
        ast = AstClassDefinition([], "C", [], None, [], [ctorAst])
        self.checkParse(ast, classDefn(), "class C { def this = 12; };")

    def testClassDefnWithAttribs(self):
        self.checkParse(AstClassDefinition([AstAttribute("public")], "C", [], None, [], []),
                        classDefn(),
                        "public class C;")

    def testSubclass(self):
        ast = AstClassDefinition([], "Sub", [], None, [AstClassType("Base", [])], [])
        self.checkParse(ast, classDefn(), "class Sub <: Base;")

    def testClassWithNullaryCtor(self):
        ast = AstClassDefinition([], "C", [],
                                 AstPrimaryConstructorDefinition([], []),
                                 [], [])
        self.checkParse(ast, classDefn(), "class C();")

    def testClassWithUnaryCtor(self):
        ast = AstClassDefinition([], "C", [],
                                 AstPrimaryConstructorDefinition([],
                                                                 [AstParameter([], AstVariablePattern("x", AstI32Type()))]),
                                 [], [])
        self.checkParse(ast, classDefn(), "class C(x: i32);")

    def testClassWithBinaryCtor(self):
        ast = AstClassDefinition([], "C", [],
                                 AstPrimaryConstructorDefinition([],
                                                                 [AstParameter([], AstVariablePattern("x", AstI32Type())),
                                                                  AstParameter([], AstVariablePattern("y", AstI32Type()))]),
                                 [], [])
        self.checkParse(ast, classDefn(), "class C(x: i32, y: i32);")

    def testClassWithCtorWithAttribs(self):
        self.checkParse(AstClassDefinition([], "C", [],
                                           AstPrimaryConstructorDefinition([AstAttribute("public")], []),
                                           [], []),
                        classDefn(),
                        "class C public ();")

    def testTypeParametersEmpty(self):
        self.checkParse([], typeParameters(), "")

    def testTypeParameters(self):
        self.checkParse([AstTypeParameter([], "S", None, None),
                         AstTypeParameter([], "T", None, None)],
                        typeParameters(),
                        "[S, T]")

    def testTypeParameter(self):
        self.checkParse(AstTypeParameter([], "T", AstClassType("U", []), AstClassType("L", [])),
                        typeParameter(),
                        "T <: U >: L")

    def testTypeParameterSimple(self):
        self.checkParse(AstTypeParameter([], "T", None, None),
                        typeParameter(),
                        "T")

    def testTypeParametersWithFlags(self):
        self.checkParse(AstTypeParameter([AstAttribute("static")], "T", None, None),
                        typeParameter(),
                        "static T")
        self.checkParse(AstTypeParameter([AstAttribute("public"), AstAttribute("private")],
                                         "T", None, None),
                        typeParameter(),
                        "public private T")

    def testParametersEmpty(self):
        self.checkParse([], parameters(), "")

    def testParameters(self):
        self.checkParse([AstParameter([], AstVariablePattern("x", None)),
                         AstParameter([], AstVariablePattern("y", None))],
                        parameters(),
                        "(x, y)")

    # Patterns
    def testVarPatternNoType(self):
        self.checkParse(AstVariablePattern("x", None), varPattern(), "x")

    def testVarPatternWithType(self):
        self.checkParse(AstVariablePattern("x", AstClassType("T", [])), varPattern(), "x: T")

    # Types
    def testSimpleTypes(self):
        self.checkParse(AstUnitType(), ty(), "unit")
        self.checkParse(AstBooleanType(), ty(), "boolean")
        self.checkParse(AstI8Type(), ty(), "i8")
        self.checkParse(AstI16Type(), ty(), "i16")
        self.checkParse(AstI32Type(), ty(), "i32")
        self.checkParse(AstI64Type(), ty(), "i64")
        self.checkParse(AstF32Type(), ty(), "f32")
        self.checkParse(AstF64Type(), ty(), "f64")

    def testClassTypeSimple(self):
        self.checkParse(AstClassType("C", []), classType(), "C")

    def testClassTypeArgs(self):
        self.checkParse(AstClassType("A", [AstClassType("B", [AstClassType("C", [])]),
                                           AstClassType("D", [])]),
                        classType(),
                        "A[B[C], D]")

    def testNullableClassType(self):
        self.checkParse(AstClassType("C", [], set(["?"])), classType(), "C?")

    # Expressions
    def testIntExpr(self):
        values = [("-42", -42, 64),
                  ("42", 42, 64),
                  ("0", 0, 64),
                  ("12i16", 12, 16),
                  ("123i64", 123, 64),
                  ("-1i32", -1, 32),
                  ("-123i8", -123, 8),
                  ("-0i32", -0, 32),
                  ("0x0", 0x0, 64),
                  ("0xabc", 0xabc, 64),
                  ("0xABC", 0xABC, 64),
                  ("0xAb0", 0xAb0, 64),
                  ("-0xAb1", -0xAb1, 64),
                  ("0xAb1i32", 0xAb1, 32),
                  ("-0xAb1i32", -0xAb1, 32),
                  ("0b0", 0b0, 64),
                  ("0b1000101", 0b1000101, 64),
                  ("-0B1", -0B1, 64),
                  ("0b1i32", 0b1, 32),
                  ("-0b1i16", -0b1, 16)]
        for source, value, width in values:
            self.checkParse(AstLiteralExpression(AstIntegerLiteral(value, width)),
                            expression(), source)

    def testFloatExpr(self):
        values = [("1.", 1., 64),
                  (".1", .1, 64),
                  ("123.45", 123.45, 64),
                  ("-1.", -1., 64),
                  ("+.1", +.1, 64),
                  ("1.2e3", 1.2e3, 64),
                  ("-1.2E34", -1.2E34, 64),
                  ("9e8", 9e8, 64),
                  ("-9e+8", -9e8, 64),
                  ("-9e-8", -9e-8, 64),
                  ("1.2f32", 1.2, 32),
                  ("-1.2f64", -1.2, 64)]
        for source, value, width in values:
            self.checkParse(AstLiteralExpression(AstFloatLiteral(value, width)),
                            expression(), source)

    def testStringExpr(self):
        self.checkParse(AstLiteralExpression(AstStringLiteral("foo\nbar")),
                        expression(), r'"foo\nbar"')

    def testVarExpr(self):
        self.checkParse(AstVariableExpression("x"), expression(), "x")

    def testThisExpr(self):
        self.checkParse(AstThisExpression(), expression(), "this")

    def testSuperExpr(self):
        self.checkParse(AstSuperExpression(), expression(), "super")

    def testBlockExpr(self):
        self.checkParse(AstBlockExpression([AstVariableExpression("x"),
                                            AstVariableExpression("y")]),
                        expression(), "{x;y;}")

    def testProp(self):
        self.checkParse(AstPropertyExpression(AstVariableExpression("o"), "x"),
                        expression(), "o.x")

    def testPropChain(self):
        self.checkParse(AstPropertyExpression(AstPropertyExpression(AstVariableExpression("a"),
                                                                    "b"),
                                              "c"),
                        expression(), "a.b.c")

    def testCallExpr1(self):
        self.checkParse(AstCallExpression(AstVariableExpression("f"),
                                          [],
                                          []),
                        expression(), "f()")

    def testCallExpr2(self):
        self.checkParse(AstCallExpression(AstVariableExpression("f"),
                                          [],
                                          [AstVariableExpression("a"),
                                           AstVariableExpression("b")]),
                        expression(), "f(a, b)")

    def testCallExpr3(self):
        self.checkParse(AstCallExpression(AstVariableExpression("f"),
                                          [AstClassType("T", [])],
                                          []),
                        expression(), "f[T]")

    def testCallExpr4(self):
        self.checkParse(AstCallExpression(AstVariableExpression("f"),
                                          [AstClassType("T", [])],
                                          [AstVariableExpression("a")]),
                        expression(), "f[T](a)")

    def testCallMethod1(self):
        self.checkParse(AstCallExpression(AstPropertyExpression(AstVariableExpression("o"),
                                                                "f"),
                                          [AstClassType("T", [])],
                                          []),
                        expression(), "o.f[T]")

    def testCallMethod2(self):
        self.checkParse(AstCallExpression(AstPropertyExpression(AstVariableExpression("o"),
                                                                "f"),
                                          [],
                                          [AstVariableExpression("a")]),
                        expression(), "o.f(a)")

    def testCallMethod3(self):
        self.checkParse(AstCallExpression(AstPropertyExpression(AstVariableExpression("o"),
                                                                "f"),
                                          [AstClassType("T", [])],
                                          [AstVariableExpression("a")]),
                        expression(), "o.f[T](a)")

    def testCallMethod4(self):
        self.checkParse(AstCallExpression(AstPropertyExpression(AstVariableExpression("o"),
                                                                "f"),
                                          [],
                                          []),
                        expression(), "o.f()")

    def testFunctionValue1(self):
        self.checkParse(AstFunctionValueExpression(AstPropertyExpression(AstVariableExpression("o"),
                                                                         "f")),
                        expression(), "o.f _")

    def testUnaryExpr(self):
        self.checkParse(AstUnaryExpression("-", AstVariableExpression("x")),
                        expression(), "-x")

    def testUnaryUnaryExpr(self):
        self.checkParse(AstUnaryExpression("!",
                                           AstUnaryExpression("-",
                                                              AstVariableExpression("x"))),
                        expression(), "! -x")

    def testBinaryExpr(self):
        self.checkParse(AstBinaryExpression("+",
                                            AstVariableExpression("x"),
                                            AstVariableExpression("y")),
                        expression(), "x + y")

    def testNestedBinaryExpr(self):
        self.checkParse(AstBinaryExpression("+",
                                            AstVariableExpression("x"),
                                            AstBinaryExpression("*",
                                                                AstVariableExpression("y"),
                                                                AstVariableExpression("z"))),
                        expression(), "x + y * z")

    def testNestedBinaryExpr2(self):
        self.checkParse(AstBinaryExpression("+",
                                            AstBinaryExpression("*",
                                                                AstVariableExpression("x"),
                                                                AstVariableExpression("y")),
                                            AstVariableExpression("z")),
                        expression(), "x * y + z")

    def testBinaryMultipleOfExpr(self):
        self.checkParse(AstBinaryExpression("==",
                                            AstBinaryExpression("%",
                                                                AstVariableExpression("x"),
                                                                AstLiteralExpression(AstIntegerLiteral(3, 64))),
                                            AstLiteralExpression(AstIntegerLiteral(0, 64))),
                        expression(), "x % 3 == 0")

    def testLeftAssociativeBinaryExpr(self):
        self.checkParse(AstBinaryExpression("+",
                                            AstBinaryExpression("+",
                                                                AstVariableExpression("x"),
                                                                AstVariableExpression("y")),
                                            AstVariableExpression("z")),
                        expression(), "x + y + z")

    def testRightAssociativeBinaryExpr(self):
        self.checkParse(AstBinaryExpression("::",
                                            AstVariableExpression("x"),
                                            AstBinaryExpression("::",
                                                                AstVariableExpression("y"),
                                                                AstVariableExpression("z"))),
                        expression(), "x :: y :: z")

    def testLogicExpr(self):
        self.checkParse(AstBinaryExpression("||",
                                            AstBinaryExpression("&&",
                                                                AstVariableExpression("x"),
                                                                AstVariableExpression("y")),
                                            AstVariableExpression("z")),
                        expression(), "x && y || z")

    def testAssignExpr(self):
        self.checkParse(AstAssignExpression(AstVariableExpression("x"),
                                            AstBinaryExpression("+",
                                                                AstVariableExpression("y"),
                                                                AstVariableExpression("z"))),
                        expression(), "x = y + z")

    def testAssignLowPrecedence(self):
        self.checkParse(AstAssignExpression(AstVariableExpression("x"),
                                            AstBinaryExpression("|",
                                                                AstVariableExpression("y"),
                                                                AstVariableExpression("z"))),
                        expression(), "x = y | z")

    def testAssignRightAssociative(self):
        self.checkParse(AstAssignExpression(AstVariableExpression("x"),
                                            AstAssignExpression(AstVariableExpression("y"),
                                                                AstVariableExpression("z"))),
                        expression(), "x = y = z")

    def testAssignBinop(self):
        self.checkParse(AstBinaryExpression("+=",
                                            AstVariableExpression("x"),
                                            AstVariableExpression("y")),
                        expression(), "x += y")

    def testIfExpr(self):
        self.checkParse(AstIfExpression(AstVariableExpression("x"),
                                        AstVariableExpression("y"),
                                        AstVariableExpression("z")),
                        expression(), "if (x) y else z")

    def testIfExprNoElse(self):
        self.checkParse(AstIfExpression(AstVariableExpression("x"),
                                        AstVariableExpression("y"),
                                        None),
                        expression(), "if (x) y")

    def testIfExprWithBinop(self):
        self.checkParse(AstIfExpression(AstVariableExpression("x"),
                                        AstBinaryExpression("+",
                                                            AstVariableExpression("y"),
                                                            AstVariableExpression("z")),
                                        None),
                        expression(), "if (x) y + z")

    def testWhileExpr(self):
        self.checkParse(AstWhileExpression(AstVariableExpression("x"),
                                           AstVariableExpression("y")),
                        expression(), "while (x) y")

    def testWhileBlockEmptyExpr(self):
        self.checkParse(AstWhileExpression(AstVariableExpression("c"),
                                           AstBlockExpression([])),
                        expression(), "while (c) {}")

    def testWhileBlockExpr(self):
        self.checkParse(AstWhileExpression(AstBinaryExpression(">",
                                                               AstVariableExpression("n"),
                                                               AstLiteralExpression(AstIntegerLiteral(0, 64))),
                                           AstAssignExpression(AstVariableExpression("n"),
                                                               AstBinaryExpression("-",
                                                                                   AstVariableExpression("n"),
                                                                                   AstLiteralExpression(AstIntegerLiteral(1, 64))))),
                        expression(), "while (n > 0)\n" + \
                                      "  n = n - 1")

    def testBreakExpr(self):
        self.checkParse(AstBreakExpression(),
                        expression(), "break")

    def testContinueExpr(self):
        self.checkParse(AstContinueExpression(),
                        expression(), "continue")

    def testPartialFunctionExpr(self):
        self.checkParse(AstPartialFunctionExpression([AstPartialFunctionCase(AstVariablePattern("x", None),
                                                                             AstVariableExpression("b"),
                                                                             AstVariableExpression("x")),
                                                      AstPartialFunctionCase(AstVariablePattern("y", None),
                                                                             None,
                                                                             AstVariableExpression("y"))]),
                        expression(),
                        "{ case x if b => x; case y => y; }")

    def testMatchExpr(self):
        self.checkParse(AstMatchExpression(AstVariableExpression("x"),
                                           AstPartialFunctionExpression([AstPartialFunctionCase(AstVariablePattern("x", None),
                                                                                                None,
                                                                                                AstVariableExpression("x"))])),
                        expression(),
                        "match (x) { case x => x; }")

    def testThrowExpr(self):
        self.checkParse(AstThrowExpression(AstVariableExpression("x")),
                        expression(), "throw x")

    def testTryCatchExpr(self):
        self.checkParse(AstTryCatchExpression(AstVariableExpression("x"),
                                              AstPartialFunctionExpression([AstPartialFunctionCase(AstVariablePattern("x", None),
                                                                                                   None,
                                                                                                   AstVariableExpression("x"))]),
                                              None),
                        expression(),
                        "try x catch { case x => x; }")

    def testTryCatchFinallyExpr(self):
        self.checkParse(AstTryCatchExpression(AstVariableExpression("x"),
                                              AstPartialFunctionExpression([AstPartialFunctionCase(AstVariablePattern("x", None),
                                                                                                   None,
                                                                                                   AstVariableExpression("x"))]),
                                              AstVariableExpression("x")),
                        expression(),
                        "try x catch { case x => x; } finally x")

    def testTryFinallyExpr(self):
        self.checkParse(AstTryCatchExpression(AstVariableExpression("x"),
                                              None,
                                              AstVariableExpression("x")),
                        expression(),
                        "try x finally x")

    def testTryCatchSimpleExpr(self):
        self.checkParse(AstTryCatchExpression(AstVariableExpression("x"),
                                              AstPartialFunctionExpression([AstPartialFunctionCase(AstVariablePattern("x", None),
                                                                                                   None,
                                                                                                   AstVariableExpression("x"))]),
                                              None),
                        expression(),
                        "try x catch (x) x")

    def testTryExprError(self):
        self.assertRaises(ParseException, self.parseFromSource, expression(),
                          "try x")

    def testLambdaExpr(self):
        self.checkParse(AstLambdaExpression(None,
                                            [],
                                            [AstVariablePattern("x", None),
                                             AstVariablePattern("y", None)],
                                            AstVariableExpression("x")),
                        expression(),
                        "lambda (x, y) x")

    def testLambdaSimple(self):
        self.checkParse(AstLambdaExpression(None,
                                            [],
                                            [],
                                            AstVariableExpression("x")),
                        expression(),
                        "lambda () x")

    def testLambdaExprWithTypeParams(self):
        self.checkParse(AstLambdaExpression(None,
                                            [AstTypeParameter([], "S", None, None),
                                             AstTypeParameter([], "T", None, None)],
                                            [AstVariablePattern("x", AstClassType("S", [])),
                                             AstVariablePattern("y", AstClassType("T", []))],
                                            AstVariableExpression("x")),
                        expression(),
                        "lambda [S, T](x: S, y: T) x")

    def testLambdaExprWithName(self):
        self.checkParse(AstLambdaExpression("f",
                                            [AstTypeParameter([], "T", None, None)],
                                            [AstVariablePattern("x", AstClassType("T", []))],
                                            AstCallExpression(AstVariableExpression("f"),
                                                              [],
                                                              [AstVariableExpression("x")])),
                        expression(),
                        "lambda f[T](x: T) f(x)")

    def testReturnExpr(self):
        self.checkParse(AstReturnExpression(AstVariableExpression("x")),
                        expression(), "return x")

    def testGroupExpr(self):
        self.checkParse(AstBinaryExpression("*",
                                            AstBinaryExpression("+",
                                                                AstVariableExpression("x"),
                                                                AstVariableExpression("y")),
                                            AstVariableExpression("z")),
                        expression(), "(x + y) * z")

    # Literals
    def testBooleanLits(self):
        self.checkParse(AstBooleanLiteral(True), literal(), "true")
        self.checkParse(AstBooleanLiteral(False), literal(), "false")

    def testIntLits(self):
        self.checkParse(AstIntegerLiteral(123), literal(), "123")

    def testNullLit(self):
        self.checkParse(AstNullLiteral(), literal(), "null")

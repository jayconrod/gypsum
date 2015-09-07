# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

execfile("parser.py")


import ast
from errors import *
from layout import layout
from lexer import *
from location import Location
from combinators import *


# This code defines functions which call AST constructors with a dummy location. This is a hack
# to avoid specifying locations for all AST classes. Locations aren't part of the equality test,
# so we don't really care about them here.
nloc = Location("<test>", 1, 1, 1, 1)

for k, v in ast.__dict__.iteritems():
    if k.startswith("Ast") and type(v) is type:
        altName = "ast" + k[3:]
        altCtor = (lambda ctor: (lambda *args: ctor(*(args + (nloc,)))))(v)
        globals()[altName] = altCtor


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

    def checkParseError(self, parser, text):
        self.assertRaises(ParseException, self.parseFromSource, parser, text)

    # Module
    def testModuleEmpty(self):
        self.checkParse(astModule([]), module(), "")

    # Definitions
    def testVarDefnEmpty(self):
        self.checkParse(astVariableDefinition([], "var", astVariablePattern("x", None), None),
                        varDefn(),
                        "var x;")

    def testLetDefnEmpty(self):
        self.checkParse(astVariableDefinition([], "let", astVariablePattern("x", None), None),
                        varDefn(),
                        "let x;")

    def testVarDefn(self):
        self.checkParse(astVariableDefinition([], "var",
                                              astVariablePattern("x", None),
                                              astVariableExpression("y")),
                        varDefn(),
                        "var x = y;")

    def testLetDefn(self):
        self.checkParse(astVariableDefinition([], "let",
                                              astVariablePattern("x", None),
                                              astVariableExpression("y")),
                        varDefn(),
                        "let x = y;")

    def testVarDefnWithAttribs(self):
        self.checkParse(astVariableDefinition([astAttribute("public")], "var",
                                              astVariablePattern("x", None),
                                              None),
                        varDefn(),
                        "public var x;")

    def testFunctionDefnSimple(self):
        self.checkParse(astFunctionDefinition([], "f", [], [], None, None),
                        functionDefn(),
                        "def f;")

    def testFunctionDefn(self):
        self.checkParse(astFunctionDefinition([], "f",
                                              [astTypeParameter([], None, "S", None, None),
                                               astTypeParameter([], None, "T", None, None)],
                                              [astParameter([], None, astVariablePattern("x", astClassType([], "S", [], set()))),
                                               astParameter([], None, astVariablePattern("y", astClassType([], "T", [], set())))],
                                              astClassType([], "A", [], set()),
                                              astVariableExpression("x")),
                        functionDefn(),
                        "def f[S, T](x: S, y: T): A = x;")

    def testFunctionDefnWithVarParam(self):
        self.checkParse(astFunctionDefinition([], "f", [],
                                              [astParameter([], "var", astVariablePattern("x", astUnitType()))],
                                              None, None),
                        functionDefn(),
                        "def f(var x: unit);")

    def testFunctionDefnWithAttribs(self):
        self.checkParse(astFunctionDefinition([astAttribute("public")],
                                              "f", [], [], None, None),
                        functionDefn(),
                        "public def f;")

    def testOperatorFunctionDefn(self):
        self.checkParse(astFunctionDefinition([], "+", [], [], None, None),
                        functionDefn(),
                        "def +;")

    def testClassDefnSimple(self):
        self.checkParse(astClassDefinition([], "C", [], None, None, None, []),
                        classDefn(),
                        "class C;")

    def testClassDefnSimpleWithBody(self):
        ctorast = astFunctionDefinition([], "this", [], [], None,
                                        astLiteralExpression(astIntegerLiteral(12, 64)))
        ast = astClassDefinition([], "C", [], None, None, None, [ctorast])
        self.checkParse(ast, classDefn(), "class C { def this = 12; };")

    def testClassDefnWithAttribs(self):
        self.checkParse(astClassDefinition([astAttribute("public")], "C", [],
                                           None, None, None, []),
                        classDefn(),
                        "public class C;")

    def testSubclass(self):
        ast = astClassDefinition([], "Sub", [], None, astClassType([], "Base", [], set()), None, [])
        self.checkParse(ast, classDefn(), "class Sub <: Base;")

    def testSubclassWithTypeArgs(self):
        ast = astClassDefinition([], "Sub", [], None,
                                 astClassType([], "Base", [astClassType([], "X", [], set()),
                                                           astClassType([], "Y", [], set())], set()),
                                 None, [])
        self.checkParse(ast, classDefn(), "class Sub <: Base[X, Y];")

    def testSubclassWithSuperArgs(self):
        ast = astClassDefinition([], "Sub", [], None,
                                 astClassType([], "Base", [], set()),
                                 [astVariableExpression("x"), astVariableExpression("y")], [])
        self.checkParse(ast, classDefn(), "class Sub <: Base(x, y);")

    def testClassWithNullaryCtor(self):
        ast = astClassDefinition([], "C", [],
                                 astPrimaryConstructorDefinition([], []),
                                 None, None, [])
        self.checkParse(ast, classDefn(), "class C();")

    def testClassWithUnaryCtor(self):
        ast = astClassDefinition([], "C", [],
                                 astPrimaryConstructorDefinition([],
                                                                 [astParameter([], None, astVariablePattern("x", astI32Type()))]),
                                 None, None, [])
        self.checkParse(ast, classDefn(), "class C(x: i32);")

    def testClassWithUnaryCtorWithVarParam(self):
        ast = astClassDefinition([], "C", [],
                                 astPrimaryConstructorDefinition([],
                                                                 [astParameter([], "var", astVariablePattern("x", astI32Type()))]),
                                 None, None, [])
        self.checkParse(ast, classDefn(), "class C(var x: i32);")

    def testClassWithBinaryCtor(self):
        ast = astClassDefinition([], "C", [],
                                 astPrimaryConstructorDefinition([],
                                                                 [astParameter([], None, astVariablePattern("x", astI32Type())),
                                                                  astParameter([], None, astVariablePattern("y", astI32Type()))]),
                                 None, None, [])
        self.checkParse(ast, classDefn(), "class C(x: i32, y: i32);")

    def testClassWithCtorWithAttribs(self):
        self.checkParse(astClassDefinition([], "C", [],
                                           astPrimaryConstructorDefinition([astAttribute("public")], []),
                                           None, None, []),
                        classDefn(),
                        "class C public ();")

    def testOperatorClassDefn(self):
        self.checkParse(astClassDefinition([], "::", [], None, None, None, []),
                        classDefn(),
                        "class ::;")

    def testTypeParametersEmpty(self):
        self.checkParse([], typeParameters(), "")

    def testTypeParameters(self):
        self.checkParse([astTypeParameter([], None, "S", None, None),
                         astTypeParameter([], None, "T", None, None)],
                        typeParameters(),
                        "[S, T]")

    def testTypeParameter(self):
        self.checkParse(astTypeParameter([], None, "T",
                                         astClassType([], "U", [], set()),
                                         astClassType([], "L", [], set())),
                        typeParameter(),
                        "T <: U >: L")

    def testTypeParameterSimple(self):
        self.checkParse(astTypeParameter([], None, "T", None, None),
                        typeParameter(),
                        "T")

    def testTypeParametersWithFlags(self):
        self.checkParse(astTypeParameter([astAttribute("static")], None, "T", None, None),
                        typeParameter(),
                        "static T")
        self.checkParse(astTypeParameter([astAttribute("public"), astAttribute("private")],
                                         None, "T", None, None),
                        typeParameter(),
                        "public private T")

    def testTypeParametersWithVariance(self):
        self.checkParse(astTypeParameter([], "+", "T", None, None),
                        typeParameter(),
                        "+T")
        self.checkParse(astTypeParameter([], "-", "T", None, None),
                        typeParameter(),
                        "-T")

    def testParametersEmpty(self):
        self.checkParse([], parameters(), "")

    def testParameters(self):
        self.checkParse([astParameter([], None, astVariablePattern("x", None)),
                         astParameter([], None, astVariablePattern("y", None))],
                        parameters(),
                        "(x, y)")

    def testVarParameter(self):
        self.checkParse(astParameter([], "var", astVariablePattern("x", None)),
                        parameter(),
                        "var x")

    def testImportBlank(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None),
                                            astScopePrefixComponent("bar", [astErasedType()])],
                                           None),
                        importStmt(),
                        "import foo.bar[_]._;")

    def testImportSingle(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None)],
                                           [astImportBinding("x", None)]),
                        importStmt(),
                        "import foo.x;")

    def testImportSingleAs(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None)],
                                           [astImportBinding("x", "y")]),
                        importStmt(),
                        "import foo.x as y;")

    def testImportMultiple(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None)],
                                           [astImportBinding("x", None),
                                            astImportBinding("y", None)]),
                        importStmt(),
                        "import foo.x, y;")

    def testImportMultipleAs(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None)],
                                           [astImportBinding("a", "b"),
                                            astImportBinding("c", "d")]),
                        importStmt(),
                        "import foo.a as b, c as d;")

    def testImportNoPrefix(self):
        self.checkParseError(importStmt(), "import foo;")

    def testImportWithTypeArgs(self):
        self.checkParseError(importStmt(), "import foo.bar[_];")

    def testImportInClass(self):
        self.checkParse(astClassDefinition([], "C", [], None, None, None,
                                           [astImportStatement([astScopePrefixComponent("foo", None)],
                                                               None)]),
                        classDefn(),
                        "class C { import foo._; };")

    def testImportInBlockExpr(self):
        self.checkParse(astBlockExpression([astImportStatement([astScopePrefixComponent("foo", None)],
                                                               None)]),
                        expression(),
                        "{ import foo._; }")

    def testImportInModule(self):
        self.checkParse(astModule([astImportStatement([astScopePrefixComponent("foo", None)],
                                                      None)]),
                        module(),
                        "import foo._;")

    # Patterns
    def testVarPatternNoType(self):
        self.checkParse(astVariablePattern("x", None), pattern(), "x")

    def testVarPatternWithType(self):
        self.checkParse(astVariablePattern("x", astClassType([], "T", [], set())),
                        pattern(), "x: T")

    def testBlankPatternNoType(self):
        self.checkParse(astBlankPattern(None), pattern(), "_")

    def testBlankPatternWithType(self):
        self.checkParse(astBlankPattern(astClassType([], "T", [], set())), pattern(), "_: T")

    def testLiteralPatterns(self):
        self.checkParse(astLiteralPattern(astIntegerLiteral(12, 64)), pattern(), "12")
        self.checkParse(astLiteralPattern(astBooleanLiteral(True)), pattern(), "true")
        self.checkParse(astLiteralPattern(astNullLiteral()), pattern(), "null")

    def testParenthesizedPattern(self):
        self.checkParse(astBlankPattern(None), pattern(), "(_)")

    def testTuplePattern(self):
        self.checkParse(astTuplePattern([astBlankPattern(None), astBlankPattern(None)]),
                        pattern(), "(_, _)")

    def testTuplePatternWithoutParenthesis(self):
        self.checkParse(astTuplePattern([astBlankPattern(None), astBlankPattern(None)]),
                        pattern(), "_, _")

    def testValuePattern(self):
        self.checkParse(astValuePattern([astScopePrefixComponent("foo", None)], "bar"),
                        pattern(), "foo.bar")

    def testDestructurePatternSimple(self):
        self.checkParse(astDestructurePattern([astScopePrefixComponent("Foo", None)],
                                              [astVariablePattern("x", None)]),
                        pattern(), "Foo(x)")

    def testDestructurePatternAdvanced(self):
        self.checkParse(astDestructurePattern([astScopePrefixComponent("foo", None),
                                               astScopePrefixComponent("Bar", [astErasedType()])],
                                              [astVariablePattern("x", None),
                                               astVariablePattern("y", None)]),
                        pattern(), "foo.Bar[_](x, y)")

    def testUnaryPattern(self):
        self.checkParse(astUnaryPattern("~", astBlankPattern(None)),
                        pattern(), "~_")

    def testUnaryTuplePattern(self):
        self.checkParse(astTuplePattern([astUnaryPattern("-", astVariablePattern("x", None)),
                                         astUnaryPattern("-", astVariablePattern("y", None))]),
                        pattern(), "-x, -y")

    def testDestructureUnaryPatterns(self):
        self.checkParse(astDestructurePattern([astScopePrefixComponent("Foo", None)],
                                              [astUnaryPattern("-", astVariablePattern("x", None)),
                                               astUnaryPattern("-", astVariablePattern("y", None))]),
                        pattern(), "Foo(-x, -y)")

    def testBinaryPatternSimple(self):
        self.checkParse(astBinaryPattern("+", astBlankPattern(None), astBlankPattern(None)),
                        pattern(), "_ + _")

    def testBinaryPatternPrecedence(self):
        self.checkParse(astBinaryPattern("+", astBlankPattern(None),
                                         astBinaryPattern("*", astBlankPattern(None),
                                                          astBlankPattern(None))),
                        pattern(), "_ + _ * _")

    def testBinaryPatternAssociativityLeft(self):
        self.checkParse(astBinaryPattern("+",
                                         astBinaryPattern("+",
                                                          astBlankPattern(None),
                                                          astBlankPattern(None)),
                                         astBlankPattern(None)),
                        pattern(), "_ + _ + _")

    def testBinaryPatternAssociativityRight(self):
        self.checkParse(astBinaryPattern("::",
                                         astBlankPattern(None),
                                         astBinaryPattern("::",
                                                          astBlankPattern(None),
                                                          astBlankPattern(None))),
                        pattern(), "_ :: _ :: _")

    def testBinaryPatternAssociativityMixed(self):
        self.checkParseError(pattern(), "_ +: _ + _")

    # Scope prefix
    def testScopePrefixSimple(self):
        self.checkParse([astScopePrefixComponent("A", None)],
                        scopePrefix(), "A")

    def testScopePrefixCompound(self):
        self.checkParse([astScopePrefixComponent("A", None),
                         astScopePrefixComponent("B", None)],
                        scopePrefix(), "A.B")

    def testScopePrefixCompoundWithTypeArgs(self):
        self.checkParse([astScopePrefixComponent("A", [astI8Type()]),
                         astScopePrefixComponent("B", [astI32Type()])],
                        scopePrefix(), "A[i8].B[i32]")

    def testScopePrefixComponent(self):
        self.checkParse(astScopePrefixComponent("A", None),
                        scopePrefixComponent(), "A")

    def testScopePrefixComponentWithTypeArgs(self):
        self.checkParse(astScopePrefixComponent("A", [astI8Type(), astI16Type()]),
                        scopePrefixComponent(), "A[i8, i16]")

    # Types
    def testSimpleTypes(self):
        self.checkParse(astUnitType(), ty(), "unit")
        self.checkParse(astBooleanType(), ty(), "boolean")
        self.checkParse(astI8Type(), ty(), "i8")
        self.checkParse(astI16Type(), ty(), "i16")
        self.checkParse(astI32Type(), ty(), "i32")
        self.checkParse(astI64Type(), ty(), "i64")
        self.checkParse(astF32Type(), ty(), "f32")
        self.checkParse(astF64Type(), ty(), "f64")

    def testClassTypeSimple(self):
        self.checkParse(astClassType([], "C", [], set()), classType(), "C")

    def testClassTypeArgs(self):
        self.checkParse(astClassType([], "A", [astClassType([], "B", [astClassType([], "C", [], set())], set()),
                                               astClassType([], "D", [], set())], set()),
                        classType(),
                        "A[B[C], D]")

    def testNullableClassType(self):
        self.checkParse(astClassType([], "C", [], set(["?"])), classType(), "C?")

    def testSimpleScopedType(self):
        self.checkParse(astClassType([astScopePrefixComponent("A", None)],
                                     "B", [], set()),
                        ty(),
                        "A.B")

    def testCompoundScopedType(self):
        self.checkParse(astClassType([astScopePrefixComponent("A", None),
                                      astScopePrefixComponent("B", None)],
                                     "C", [], set()),
                        ty(),
                        "A.B.C")

    def testScopedTypeWithArgs(self):
        self.checkParse(astClassType([astScopePrefixComponent("A", [astClassType([], "B", [], set())])],
                                     "C", [astClassType([], "D", [], set())], set()),
                        ty(),
                        "A[B].C[D]")

    def testScopeFromPrimitive(self):
        self.assertRaises(ParseException, self.parseFromSource, ty(),
                          "i64.A")

    def testPrimitiveInScope(self):
        self.assertRaises(ParseException, self.parseFromSource, ty(),
                          "A.i64")

    def testTupleType(self):
        self.checkParse(astTupleType([astClassType([], "A", [], set()),
                                      astClassType([], "B", [astClassType([], "C", [], set())], set())],
                                     set(["?"])),
                        ty(),
                        "(A, B[C])?")

    def testErasedType(self):
        self.checkParse(astErasedType(), ty(), "_")

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
            self.checkParse(astLiteralExpression(astIntegerLiteral(value, width)),
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
            self.checkParse(astLiteralExpression(astFloatLiteral(value, width)),
                            expression(), source)

    def testStringExpr(self):
        self.checkParse(astLiteralExpression(astStringLiteral("foo\nbar")),
                        expression(), r'"foo\nbar"')

    def testVarExpr(self):
        self.checkParse(astVariableExpression("x"), expression(), "x")

    def testThisExpr(self):
        self.checkParse(astThisExpression(), expression(), "this")

    def testSuperExpr(self):
        self.checkParse(astSuperExpression(), expression(), "super")

    def testBlockExpr(self):
        self.checkParse(astBlockExpression([astVariableExpression("x"),
                                            astVariableExpression("y")]),
                        expression(), "{x;y;}")

    def testProp(self):
        self.checkParse(astPropertyExpression(astVariableExpression("o"), "x"),
                        expression(), "o.x")

    def testPropChain(self):
        self.checkParse(astPropertyExpression(astPropertyExpression(astVariableExpression("a"),
                                                                    "b"),
                                              "c"),
                        expression(), "a.b.c")

    def testCallExpr1(self):
        self.checkParse(astCallExpression(astVariableExpression("f"),
                                          None,
                                          []),
                        expression(), "f()")

    def testCallExpr2(self):
        self.checkParse(astCallExpression(astVariableExpression("f"),
                                          None,
                                          [astVariableExpression("a"),
                                           astVariableExpression("b")]),
                        expression(), "f(a, b)")

    def testCallExpr3(self):
        self.checkParse(astCallExpression(astVariableExpression("f"),
                                          [astClassType([], "T", [], set())],
                                          None),
                        expression(), "f[T]")

    def testCallExpr4(self):
        self.checkParse(astCallExpression(astVariableExpression("f"),
                                          [astClassType([], "T", [], set())],
                                          [astVariableExpression("a")]),
                        expression(), "f[T](a)")

    def testCallMethod1(self):
        self.checkParse(astCallExpression(astPropertyExpression(astVariableExpression("o"),
                                                                "f"),
                                          [astClassType([], "T", [], set())],
                                          None),
                        expression(), "o.f[T]")

    def testCallMethod2(self):
        self.checkParse(astCallExpression(astPropertyExpression(astVariableExpression("o"),
                                                                "f"),
                                          None,
                                          [astVariableExpression("a")]),
                        expression(), "o.f(a)")

    def testCallMethod3(self):
        self.checkParse(astCallExpression(astPropertyExpression(astVariableExpression("o"),
                                                                "f"),
                                          [astClassType([], "T", [], set())],
                                          [astVariableExpression("a")]),
                        expression(), "o.f[T](a)")

    def testCallMethod4(self):
        self.checkParse(astCallExpression(astPropertyExpression(astVariableExpression("o"),
                                                                "f"),
                                          None,
                                          []),
                        expression(), "o.f()")

    def testFunctionValue1(self):
        self.checkParse(astFunctionValueExpression(astPropertyExpression(astVariableExpression("o"),
                                                                         "f")),
                        expression(), "o.f _")

    def testTupleExprs(self):
        self.checkParse(astTupleExpression([astVariableExpression("a"),
                                            astVariableExpression("b")]),
                        expression(), "a, b")
        self.checkParse(astTupleExpression([astVariableExpression("a"),
                                            astTupleExpression([astVariableExpression("b"),
                                                                astVariableExpression("c")]),
                                            astVariableExpression("d")]),
                        expression(), "a, (b, c), d")
        self.checkParse(astCallExpression(astVariableExpression("a"),
                                          None,
                                          [astTupleExpression([astVariableExpression("b"),
                                                               astVariableExpression("c")])]),
                        expression(), "a((b, c))")

    def testUnaryExpr(self):
        self.checkParse(astUnaryExpression("-", astVariableExpression("x")),
                        expression(), "-x")

    def testUnaryUnaryExpr(self):
        self.checkParse(astUnaryExpression("!",
                                           astUnaryExpression("-",
                                                              astVariableExpression("x"))),
                        expression(), "! -x")

    def testUnaryArgsExpr(self):
        self.checkParse(astCallExpression(astVariableExpression("Vector"),
                                          None,
                                          [astUnaryExpression("-", astVariableExpression("x")),
                                           astUnaryExpression("-", astVariableExpression("y"))]),
                        expression(), "Vector(-x, -y)")

    def testUnaryTupleExpr(self):
        self.checkParse(astTupleExpression([astUnaryExpression("-", astVariableExpression("x")),
                                            astUnaryExpression("-", astVariableExpression("y"))]),
                        expression(), "-x, -y")

    def testBinaryExpr(self):
        self.checkParse(astBinaryExpression("+",
                                            astVariableExpression("x"),
                                            astVariableExpression("y")),
                        expression(), "x + y")

    def testNestedBinaryExpr(self):
        self.checkParse(astBinaryExpression("+",
                                            astVariableExpression("x"),
                                            astBinaryExpression("*",
                                                                astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        expression(), "x + y * z")

    def testNestedBinaryExpr2(self):
        self.checkParse(astBinaryExpression("+",
                                            astBinaryExpression("*",
                                                                astVariableExpression("x"),
                                                                astVariableExpression("y")),
                                            astVariableExpression("z")),
                        expression(), "x * y + z")

    def testBinaryMultipleOfExpr(self):
        self.checkParse(astBinaryExpression("==",
                                            astBinaryExpression("%",
                                                                astVariableExpression("x"),
                                                                astLiteralExpression(astIntegerLiteral(3, 64))),
                                            astLiteralExpression(astIntegerLiteral(0, 64))),
                        expression(), "x % 3 == 0")

    def testLeftAssociativeBinaryExpr(self):
        self.checkParse(astBinaryExpression("+",
                                            astBinaryExpression("+",
                                                                astVariableExpression("x"),
                                                                astVariableExpression("y")),
                                            astVariableExpression("z")),
                        expression(), "x + y + z")

    def testRightAssociativeBinaryExpr(self):
        self.checkParse(astBinaryExpression("::",
                                            astVariableExpression("x"),
                                            astBinaryExpression("::",
                                                                astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        expression(), "x :: y :: z")

    def testLogicExpr(self):
        self.checkParse(astBinaryExpression("||",
                                            astBinaryExpression("&&",
                                                                astVariableExpression("x"),
                                                                astVariableExpression("y")),
                                            astVariableExpression("z")),
                        expression(), "x && y || z")

    def testAssignExpr(self):
        self.checkParse(astAssignExpression(astVariableExpression("x"),
                                            astBinaryExpression("+",
                                                                astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        expression(), "x = y + z")

    def testAssignLowPrecedence(self):
        self.checkParse(astAssignExpression(astVariableExpression("x"),
                                            astBinaryExpression("|",
                                                                astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        expression(), "x = y | z")

    def testAssignRightAssociative(self):
        self.checkParse(astAssignExpression(astVariableExpression("x"),
                                            astAssignExpression(astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        expression(), "x = y = z")

    def testAssignBinop(self):
        self.checkParse(astBinaryExpression("+=",
                                            astVariableExpression("x"),
                                            astVariableExpression("y")),
                        expression(), "x += y")

    def testEqLogicBinopPrecedence(self):
        self.checkParse(astBinaryExpression("&&",
                                            astBinaryExpression("!==",
                                                                astVariableExpression("a"),
                                                                astVariableExpression("b")),
                                            astBinaryExpression("===",
                                                                astVariableExpression("c"),
                                                                astVariableExpression("d"))),
                        expression(), "a !== b && c === d")

    def testIfExpr(self):
        self.checkParse(astIfExpression(astVariableExpression("x"),
                                        astVariableExpression("y"),
                                        astVariableExpression("z")),
                        expression(), "if (x) y else z")

    def testIfExprNoElse(self):
        self.checkParse(astIfExpression(astVariableExpression("x"),
                                        astVariableExpression("y"),
                                        None),
                        expression(), "if (x) y")

    def testIfExprWithBinop(self):
        self.checkParse(astIfExpression(astVariableExpression("x"),
                                        astBinaryExpression("+",
                                                            astVariableExpression("y"),
                                                            astVariableExpression("z")),
                                        None),
                        expression(), "if (x) y + z")

    def testWhileExpr(self):
        self.checkParse(astWhileExpression(astVariableExpression("x"),
                                           astVariableExpression("y")),
                        expression(), "while (x) y")

    def testWhileBlockEmptyExpr(self):
        self.checkParse(astWhileExpression(astVariableExpression("c"),
                                           astBlockExpression([])),
                        expression(), "while (c) {}")

    def testWhileBlockExpr(self):
        self.checkParse(astWhileExpression(astBinaryExpression(">",
                                                               astVariableExpression("n"),
                                                               astLiteralExpression(astIntegerLiteral(0, 64))),
                                           astAssignExpression(astVariableExpression("n"),
                                                               astBinaryExpression("-",
                                                                                   astVariableExpression("n"),
                                                                                   astLiteralExpression(astIntegerLiteral(1, 64))))),
                        expression(), "while (n > 0)\n" + \
                                      "  n = n - 1")

    def testBreakExpr(self):
        self.checkParse(astBreakExpression(),
                        expression(), "break")

    def testContinueExpr(self):
        self.checkParse(astContinueExpression(),
                        expression(), "continue")

    def testPartialFunctionExpr(self):
        self.checkParse(astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                             astVariableExpression("b"),
                                                                             astVariableExpression("x")),
                                                      astPartialFunctionCase(astVariablePattern("y", None),
                                                                             None,
                                                                             astVariableExpression("y"))]),
                        expression(),
                        "{ case x if b => x; case y => y; }")

    def testMatchExpr(self):
        self.checkParse(astMatchExpression(astVariableExpression("x"),
                                           astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                                                None,
                                                                                                astVariableExpression("x"))])),
                        expression(),
                        "match (x) { case x => x; }")

    def testThrowExpr(self):
        self.checkParse(astThrowExpression(astVariableExpression("x")),
                        expression(), "throw x")

    def testTryCatchExpr(self):
        self.checkParse(astTryCatchExpression(astVariableExpression("x"),
                                              astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                                                   None,
                                                                                                   astVariableExpression("x"))]),
                                              None),
                        expression(),
                        "try x catch { case x => x; }")

    def testTryCatchFinallyExpr(self):
        self.checkParse(astTryCatchExpression(astVariableExpression("x"),
                                              astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                                                   None,
                                                                                                   astVariableExpression("x"))]),
                                              astVariableExpression("x")),
                        expression(),
                        "try x catch { case x => x; } finally x")

    def testTryFinallyExpr(self):
        self.checkParse(astTryCatchExpression(astVariableExpression("x"),
                                              None,
                                              astVariableExpression("x")),
                        expression(),
                        "try x finally x")

    def testTryCatchSimpleExpr(self):
        self.checkParse(astTryCatchExpression(astVariableExpression("x"),
                                              astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                                                   None,
                                                                                                   astVariableExpression("x"))]),
                                              None),
                        expression(),
                        "try x catch (x) x")

    def testTryExprError(self):
        self.assertRaises(ParseException, self.parseFromSource, expression(),
                          "try x")

    def testLambdaExpr(self):
        self.checkParse(astLambdaExpression(None,
                                            [],
                                            [astVariablePattern("x", None),
                                             astVariablePattern("y", None)],
                                            astVariableExpression("x")),
                        expression(),
                        "lambda (x, y) x")

    def testLambdaSimple(self):
        self.checkParse(astLambdaExpression(None,
                                            [],
                                            [],
                                            astVariableExpression("x")),
                        expression(),
                        "lambda () x")

    def testLambdaExprWithTypeParams(self):
        self.checkParse(astLambdaExpression(None,
                                            [astTypeParameter([], None, "S", None, None),
                                             astTypeParameter([], None, "T", None, None)],
                                            [astVariablePattern("x", astClassType([], "S", [], set())),
                                             astVariablePattern("y", astClassType([], "T", [], set()))],
                                            astVariableExpression("x")),
                        expression(),
                        "lambda [S, T](x: S, y: T) x")

    def testLambdaExprWithName(self):
        self.checkParse(astLambdaExpression("f",
                                            [astTypeParameter([], None, "T", None, None)],
                                            [astVariablePattern("x", astClassType([], "T", [], set()))],
                                            astCallExpression(astVariableExpression("f"),
                                                              None,
                                                              [astVariableExpression("x")])),
                        expression(),
                        "lambda f[T](x: T) f(x)")

    def testReturnExpr(self):
        self.checkParse(astReturnExpression(astVariableExpression("x")),
                        expression(), "return x")

    def testGroupExpr(self):
        self.checkParse(astBinaryExpression("*",
                                            astBinaryExpression("+",
                                                                astVariableExpression("x"),
                                                                astVariableExpression("y")),
                                            astVariableExpression("z")),
                        expression(), "(x + y) * z")

    # Literals
    def testBooleanLits(self):
        self.checkParse(astBooleanLiteral(True), literal(), "true")
        self.checkParse(astBooleanLiteral(False), literal(), "false")

    def testIntLits(self):
        self.checkParse(astIntegerLiteral(123, 64), literal(), "123")

    def testNullLit(self):
        self.checkParse(astNullLiteral(), literal(), "null")

    # Symbols
    def testQuotedSymbol(self):
        self.checkParse("fo`o", symbol, r"`fo\`o`")

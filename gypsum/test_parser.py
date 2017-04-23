# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from errors import ParseException
from layout import layout
from lexer import lex
from location import (Location, NoLoc)
from parser import Parser
import ast


# This code defines functions which call AST constructors with a dummy location. This is a hack
# to avoid specifying locations for all AST classes. Locations aren't part of the equality test,
# so we don't really care about them here.
for k, v in ast.__dict__.iteritems():
    if type(v) is type and issubclass(v, ast.Node):
        altName = "ast" + k
        altCtor = (lambda ctor: (lambda *args: ctor(*(args + (NoLoc,)))))(v)
        globals()[altName] = altCtor


class TestParser(unittest.TestCase):
    def parseFromSource(self, parseMethod, text):
        fileName = "test"
        rawTokens = lex(fileName, text)
        layoutTokens = layout(rawTokens, skipAnalysis=True)
        parser = Parser(fileName, layoutTokens)
        result = parseMethod(parser)
        if not parser.atEnd():
            raise ParseException(parser.location, "did not parse entire source")
        return result

    def checkParse(self, expected, parseMethod, text):
        value = self.parseFromSource(parseMethod, text)
        self.assertEqual(expected, value)

    def checkParseError(self, parseMethod, text):
        self.assertRaises(ParseException, self.parseFromSource, parseMethod, text)

    # Module
    def testModuleEmpty(self):
        self.checkParse(astModule([]), Parser.module, "")

    # Definitions
    def testVarDefnEmpty(self):
        self.checkParse(astVariableDefinition([], "var", astVariablePattern("x", None), None),
                        Parser.defn,
                        "var x;")

    def testLetDefnEmpty(self):
        self.checkParse(astVariableDefinition([], "let", astVariablePattern("x", None), None),
                        Parser.defn,
                        "let x;")

    def testVarDefn(self):
        self.checkParse(astVariableDefinition([], "var",
                                              astVariablePattern("x", None),
                                              astVariableExpression("y")),
                        Parser.defn,
                        "var x = y;")

    def testLetDefnMultiple(self):
        self.checkParseError(Parser.defn, "let x = 0, y = 0;")

    def testLetDefnTuplePattern(self):
        self.checkParse(astVariableDefinition([], "let",
                                              astTuplePattern([
                                                  astVariablePattern("x", None),
                                                  astVariablePattern("y", None)]),
                                              astVariableExpression("z")),
                        Parser.defn,
                        "let x, y = z;")

    def testLetDefnTupleExpr(self):
        self.checkParse(astVariableDefinition([], "let",
                                              astVariablePattern("x", None),
                                              astTupleExpression([
                                                  astVariableExpression("y"),
                                                  astVariableExpression("z")])),
                        Parser.defn,
                        "let x = y, z;")

    def testLetDefn(self):
        self.checkParse(astVariableDefinition([], "let",
                                              astVariablePattern("x", None),
                                              astVariableExpression("y")),
                        Parser.defn,
                        "let x = y;")

    def testVarDefnWithAttribs(self):
        self.checkParse(astVariableDefinition([astAttribute("public")], "var",
                                              astVariablePattern("x", None),
                                              None),
                        Parser.defn,
                        "public var x;")

    def testFunctionDefnSimple(self):
        self.checkParse(astFunctionDefinition([], "f", None, None, None, None),
                        Parser.defn,
                        "def f;")

    def testFunctionDefn(self):
        self.checkParse(astFunctionDefinition([], "f",
                                              [astTypeParameter([], None, "S", None, None),
                                               astTypeParameter([], None, "T", None, None)],
                                              [astParameter([], None, astVariablePattern("x", astClassType([], "S", [], set()))),
                                               astParameter([], None, astVariablePattern("y", astClassType([], "T", [], set())))],
                                              astClassType([], "A", [], set()),
                                              astVariableExpression("x")),
                        Parser.defn,
                        "def f[S, T](x: S, y: T): A = x;")

    def testFunctionDefnWithVarParam(self):
        self.checkParse(astFunctionDefinition([], "f", None,
                                              [astParameter([], "var", astVariablePattern("x", astUnitType()))],
                                              None, None),
                        Parser.defn,
                        "def f(var x: unit);")

    def testFunctionDefnWithAttribs(self):
        self.checkParse(astFunctionDefinition([astAttribute("public")],
                                              "f", None, None, None, None),
                        Parser.defn,
                        "public def f;")

    def testOperatorFunctionDefn(self):
        self.checkParse(astFunctionDefinition([], "+", None, None, None, None),
                        Parser.defn,
                        "def +;")

    def testClassDefnSimple(self):
        self.checkParse(astClassDefinition([], "C", None, None, None, None, None, None),
                        Parser.defn,
                        "class C;")

    def testClassDefnSimpleWithBody(self):
        ctorast = astFunctionDefinition([], "this", None, None, None,
                                        astLiteralExpression(astIntegerLiteral("12", 12, 64)))
        ast = astClassDefinition([], "C", None, None, None, None, None, [ctorast])
        self.checkParse(ast, Parser.defn, "class C { def this = 12; };")

    def testClassDefnWithAttribs(self):
        self.checkParse(astClassDefinition([astAttribute("public")], "C", None,
                                           None, None, None, None, None),
                        Parser.defn,
                        "public class C;")

    def testSubclass(self):
        ast = astClassDefinition([], "Sub", None, None, astClassType([], "Base", [], set()), None, None, None)
        self.checkParse(ast, Parser.defn, "class Sub <: Base;")

    def testSubclassWithTypeArgs(self):
        ast = astClassDefinition([], "Sub", None, None,
                                 astClassType([], "Base", [astClassType([], "X", [], set()),
                                                           astClassType([], "Y", [], set())], set()),
                                 None, None, None)
        self.checkParse(ast, Parser.defn, "class Sub <: Base[X, Y];")

    def testSubclassWithSuperArgs(self):
        ast = astClassDefinition([], "Sub", None, None,
                                 astClassType([], "Base", [], set()),
                                 [astVariableExpression("x"), astVariableExpression("y")],
                                 None, None)
        self.checkParse(ast, Parser.defn, "class Sub <: Base(x, y);")

    def testSubclassWithTrait(self):
        ast = astClassDefinition([], "Sub", None, None,
                                 astClassType([], "Base", [], set()),
                                 [astVariableExpression("x"), astVariableExpression("y")],
                                 [astClassType([], "Tr", [], set())],
                                 None)
        self.checkParse(ast, Parser.defn, "class Sub <: Base(x, y), Tr;")

    def testSubclassWithTraits(self):
        ast = astClassDefinition([], "Sub", None, None,
                                 astClassType([], "Base", [], set()),
                                 [astVariableExpression("x"), astVariableExpression("y")],
                                 [astClassType([], "Tr1", [], set()),
                                  astClassType([], "Tr2", [], set())],
                                 None)
        self.checkParse(ast, Parser.defn, "class Sub <: Base(x, y), Tr1, Tr2;")

    def testClassWithNullaryCtor(self):
        ast = astClassDefinition([], "C", None,
                                 astPrimaryConstructorDefinition([], []),
                                 None, None, None, None)
        self.checkParse(ast, Parser.defn, "class C();")

    def testClassWithUnaryCtor(self):
        ast = astClassDefinition([], "C", None,
                                 astPrimaryConstructorDefinition([],
                                                                 [astParameter([], None, astVariablePattern("x", astI32Type()))]),
                                 None, None, None, None)
        self.checkParse(ast, Parser.defn, "class C(x: i32);")

    def testClassWithUnaryCtorWithVarParam(self):
        ast = astClassDefinition([], "C", None,
                                 astPrimaryConstructorDefinition([],
                                                                 [astParameter([], "var", astVariablePattern("x", astI32Type()))]),
                                 None, None, None, None)
        self.checkParse(ast, Parser.defn, "class C(var x: i32);")

    def testClassWithBinaryCtor(self):
        ast = astClassDefinition([], "C", None,
                                 astPrimaryConstructorDefinition([],
                                                                 [astParameter([], None, astVariablePattern("x", astI32Type())),
                                                                  astParameter([], None, astVariablePattern("y", astI32Type()))]),
                                 None, None, None, None)
        self.checkParse(ast, Parser.defn, "class C(x: i32, y: i32);")

    def testClassWithCtorWithAttribs(self):
        self.checkParse(astClassDefinition([], "C", None,
                                           astPrimaryConstructorDefinition([astAttribute("public")], []),
                                           None, None, None, None),
                        Parser.defn,
                        "class C public ();")

    def testOperatorClassDefn(self):
        self.checkParse(astClassDefinition([], "::", None, None, None, None, None, None),
                        Parser.defn,
                        "class ::;")

    def testTraitDefnSimple(self):
        self.checkParse(astTraitDefinition([], "T", None, None, None),
                        Parser.defn,
                        "trait T;")

    def testTraitWithAttribs(self):
        self.checkParse(astTraitDefinition([astAttribute("public")], "T", None, None, None),
                        Parser.defn,
                        "public trait T;")

    def testTraitWithTypeParameters(self):
        self.checkParse(astTraitDefinition([], "Tr",
                                           [astTypeParameter([], None, "S", None, None),
                                            astTypeParameter([], None, "T", None, None)],
                                           None, None),
                        Parser.defn,
                        "trait Tr[S, T];")

    def testTraitWithSubtypes(self):
        self.checkParse(astTraitDefinition([], "Tr", None,
                                           [astClassType([], "Foo", [], set()),
                                            astClassType([], "Bar", [], set())],
                                           None),
                        Parser.defn,
                        "trait Tr <: Foo, Bar;")

    def testTraitWithMembers(self):
        self.checkParse(astTraitDefinition([], "Tr", None, None,
                                           [astVariableDefinition([], "let", astVariablePattern("x", None), None),
                                            astVariableDefinition([], "let", astVariablePattern("y", None), None)]),
                        Parser.defn,
                        "trait Tr { let x; let y; };")

    def testFullTrait(self):
        self.checkParse(astTraitDefinition([astAttribute("public"),
                                            astAttribute("native")],
                                           "Tr",
                                           [astTypeParameter([], None, "S", None, None),
                                            astTypeParameter([], None, "T", None, None)],
                                           [astClassType([], "Foo", [], set()),
                                            astClassType([], "Bar", [], set())],
                                           [astVariableDefinition([], "let", astVariablePattern("x", None), None),
                                            astVariableDefinition([], "let", astVariablePattern("y", None), None)]),
                        Parser.defn,
                        "public native trait Tr[S, T] <: Foo, Bar { let x; let y; };")

    def testTypeParametersEmpty(self):
        self.checkParse([], Parser.typeParameters, "[]")

    def testTypeParameters(self):
        self.checkParse([astTypeParameter([], None, "S", None, None),
                         astTypeParameter([], None, "T", None, None)],
                        Parser.typeParameters,
                        "[S, T]")

    def testTypeParameter(self):
        self.checkParse(astTypeParameter([], None, "T",
                                         astClassType([], "U", [], set()),
                                         astClassType([], "L", [], set())),
                        Parser.typeParameter,
                        "T <: U >: L")

    def testTypeParameterSimple(self):
        self.checkParse(astTypeParameter([], None, "T", None, None),
                        Parser.typeParameter,
                        "T")

    def testTypeParametersWithFlags(self):
        self.checkParse(astTypeParameter([astAttribute("static")], None, "T", None, None),
                        Parser.typeParameter,
                        "static T")
        self.checkParse(astTypeParameter([astAttribute("public"), astAttribute("private")],
                                         None, "T", None, None),
                        Parser.typeParameter,
                        "public private T")

    def testTypeParametersWithVariance(self):
        self.checkParse(astTypeParameter([], "+", "T", None, None),
                        Parser.typeParameter,
                        "+T")
        self.checkParse(astTypeParameter([], "-", "T", None, None),
                        Parser.typeParameter,
                        "-T")

    def testParametersEmpty(self):
        self.checkParse([], Parser.parameters, "()")

    def testParameters(self):
        self.checkParse([astParameter([], None, astVariablePattern("x", None)),
                         astParameter([], None, astVariablePattern("y", None))],
                        Parser.parameters,
                        "(x, y)")

    def testVarParameter(self):
        self.checkParse(astParameter([], "var", astVariablePattern("x", None)),
                        Parser.parameter,
                        "var x")

    def testArrayElements(self):
        self.checkParse(astArrayElementsStatement([],
                                                  astUnitType(),
                                                  astArrayAccessorDefinition([], "get"),
                                                  astArrayAccessorDefinition([], "set"),
                                                  astArrayAccessorDefinition([], "length")),
                        Parser.defn,
                        "arrayelements unit, get, set, length;")

    def testArrayElementsAttribs(self):
        self.checkParse(astArrayElementsStatement([astAttribute("final")],
                                                  astUnitType(),
                                                  astArrayAccessorDefinition([], "get"),
                                                  astArrayAccessorDefinition([], "set"),
                                                  astArrayAccessorDefinition([], "length")),
                        Parser.defn,
                        "final arrayelements unit, get, set, length;")

    def testArrayElementsOperators(self):
        self.checkParse(astArrayElementsStatement([],
                                                  astUnitType(),
                                                  astArrayAccessorDefinition([], "!"),
                                                  astArrayAccessorDefinition([], "!!"),
                                                  astArrayAccessorDefinition([], "!!!")),
                        Parser.defn,
                        "arrayelements unit, !, !!, !!!;")

    def testArrayElementsAttribs(self):
        self.checkParse(astArrayElementsStatement([],
                                                  astUnitType(),
                                                  astArrayAccessorDefinition([astAttribute("public")], "get"),
                                                  astArrayAccessorDefinition([astAttribute("protected")], "set"),
                                                  astArrayAccessorDefinition([astAttribute("private")], "length")),
                        Parser.defn,
                        "arrayelements unit, public get, protected set, private length;")

    def testClassWithArrayElements(self):
        self.checkParse(astClassDefinition([], "Foo", None, None, None, None, None,
                                           [astArrayElementsStatement([],
                                                                      astUnitType(),
                                                                      astArrayAccessorDefinition([], "get"),
                                                                      astArrayAccessorDefinition([], "set"),
                                                                      astArrayAccessorDefinition([], "length"))]),
                        Parser.defn,
                        "class Foo { arrayelements unit, get, set, length; };")

    def testImportBlank(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None),
                                            astScopePrefixComponent("bar", [astBlankType()])],
                                           None),
                        Parser.importStmt,
                        "import foo.bar[_]._;")

    def testImportSingle(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None)],
                                           [astImportBinding("x", None)]),
                        Parser.importStmt,
                        "import foo.x;")

    def testImportSingleAs(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None)],
                                           [astImportBinding("x", "y")]),
                        Parser.importStmt,
                        "import foo.x as y;")

    def testImportMultiple(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None)],
                                           [astImportBinding("x", None),
                                            astImportBinding("y", None)]),
                        Parser.importStmt,
                        "import foo.x, y;")

    def testImportMultipleAs(self):
        self.checkParse(astImportStatement([astScopePrefixComponent("foo", None)],
                                           [astImportBinding("a", "b"),
                                            astImportBinding("c", "d")]),
                        Parser.importStmt,
                        "import foo.a as b, c as d;")

    def testImportNoPrefix(self):
        self.checkParseError(Parser.importStmt, "import foo;")

    def testImportWithTypeArgs(self):
        self.checkParseError(Parser.importStmt, "import foo.bar[_];")

    def testImportInClass(self):
        self.checkParse(astClassDefinition([], "C", None, None, None, None, None,
                                           [astImportStatement([astScopePrefixComponent("foo", None)],
                                                               None)]),
                        Parser.defn,
                        "class C { import foo._; };")

    def testImportInBlockExpr(self):
        self.checkParse(astBlockExpression([astImportStatement([astScopePrefixComponent("foo", None)],
                                                               None)]),
                        Parser.expr,
                        "{ import foo._; }")

    def testImportInModule(self):
        self.checkParse(astModule([astImportStatement([astScopePrefixComponent("foo", None)],
                                                      None)]),
                        Parser.module,
                        "import foo._;")

    # Patterns
    def testVarPatternNoType(self):
        self.checkParse(astVariablePattern("x", None), Parser.pattern, "x")

    def testVarPatternWithType(self):
        self.checkParse(astVariablePattern("x", astClassType([], "T", [], set())),
                        Parser.pattern, "x: T")

    def testBlankPatternNoType(self):
        self.checkParse(astBlankPattern(None), Parser.pattern, "_")

    def testBlankPatternWithType(self):
        self.checkParse(astBlankPattern(astClassType([], "T", [], set())), Parser.pattern, "_: T")

    def testLiteralPatterns(self):
        self.checkParse(astLiteralPattern(astIntegerLiteral("12", 12, 64)), Parser.pattern, "12")
        self.checkParse(astLiteralPattern(astBooleanLiteral(True)), Parser.pattern, "true")
        self.checkParse(astLiteralPattern(astNullLiteral()), Parser.pattern, "null")

    def testGroupPattern(self):
        self.checkParse(astGroupPattern(astBlankPattern(None)), Parser.pattern, "(_)")

    def testTuplePattern(self):
        self.checkParse(
            astGroupPattern(
                astTuplePattern([astBlankPattern(None), astBlankPattern(None)])),
                Parser.pattern, "(_, _)")

    def testTuplePatternWithoutParenthesis(self):
        self.checkParse(astTuplePattern([astBlankPattern(None), astBlankPattern(None)]),
                        Parser.pattern, "_, _")

    def testValuePattern(self):
        self.checkParse(astValuePattern([astScopePrefixComponent("foo", None)], "bar"),
                        Parser.pattern, "foo.bar")

    def testDestructurePatternSimple(self):
        self.checkParse(astDestructurePattern([astScopePrefixComponent("Foo", None)],
                                              [astVariablePattern("x", None)]),
                        Parser.pattern, "Foo(x)")

    def testDestructurePatternAdvanced(self):
        self.checkParse(astDestructurePattern([astScopePrefixComponent("foo", None),
                                               astScopePrefixComponent("Bar", [astBlankType()])],
                                              [astVariablePattern("x", None),
                                               astVariablePattern("y", None)]),
                        Parser.pattern, "foo.Bar[_](x, y)")

    def testUnaryPattern(self):
        self.checkParse(astUnaryPattern("~", astBlankPattern(None)),
                        Parser.pattern, "~_")

    def testUnaryTuplePattern(self):
        self.checkParse(astTuplePattern([astUnaryPattern("-", astVariablePattern("x", None)),
                                         astUnaryPattern("-", astVariablePattern("y", None))]),
                        Parser.pattern, "-x, -y")

    def testDestructureUnaryPatterns(self):
        self.checkParse(astDestructurePattern([astScopePrefixComponent("Foo", None)],
                                              [astUnaryPattern("-", astVariablePattern("x", None)),
                                               astUnaryPattern("-", astVariablePattern("y", None))]),
                        Parser.pattern, "Foo(-x, -y)")

    def testUnaryDestructurePattern(self):
        self.checkParse(astUnaryPattern("~",
                                        astDestructurePattern([astScopePrefixComponent("Foo", None)],
                                                              [astVariablePattern("x", None)])),
                        Parser.pattern, "~Foo(x)")

    def testBinaryPatternSimple(self):
        self.checkParse(astBinaryPattern("+", astBlankPattern(None), astBlankPattern(None)),
                        Parser.pattern, "_ + _")

    def testBinaryPatternPrecedence(self):
        self.checkParse(astBinaryPattern("+", astBlankPattern(None),
                                         astBinaryPattern("*", astBlankPattern(None),
                                                          astBlankPattern(None))),
                        Parser.pattern, "_ + _ * _")

    def testBinaryPatternAssociativityLeft(self):
        self.checkParse(astBinaryPattern("+",
                                         astBinaryPattern("+",
                                                          astBlankPattern(None),
                                                          astBlankPattern(None)),
                                         astBlankPattern(None)),
                        Parser.pattern, "_ + _ + _")

    def testBinaryPatternAssociativityRight(self):
        self.checkParse(astBinaryPattern("::",
                                         astBlankPattern(None),
                                         astBinaryPattern("::",
                                                          astBlankPattern(None),
                                                          astBlankPattern(None))),
                        Parser.pattern, "_ :: _ :: _")

    def testBinaryPatternAssociativityMixed(self):
        self.checkParseError(Parser.pattern, "_ +: _ + _")

    # Scope prefix
    def testScopePrefixSimple(self):
        self.checkParse([astScopePrefixComponent("A", None)],
                        Parser.scopePrefix, "A")

    def testScopePrefixCompound(self):
        self.checkParse([astScopePrefixComponent("A", None),
                         astScopePrefixComponent("B", None)],
                        Parser.scopePrefix, "A.B")

    def testScopePrefixCompoundWithTypeArgs(self):
        self.checkParse([astScopePrefixComponent("A", [astI8Type()]),
                         astScopePrefixComponent("B", [astI32Type()])],
                        Parser.scopePrefix, "A[i8].B[i32]")

    def testScopePrefixComponent(self):
        self.checkParse(astScopePrefixComponent("A", None),
                        Parser.scopePrefixComponent, "A")

    def testScopePrefixComponentWithTypeArgs(self):
        self.checkParse(astScopePrefixComponent("A", [astI8Type(), astI16Type()]),
                        Parser.scopePrefixComponent, "A[i8, i16]")

    # Types
    def testSimpleTypes(self):
        self.checkParse(astUnitType(), Parser.ty, "unit")
        self.checkParse(astBooleanType(), Parser.ty, "boolean")
        self.checkParse(astI8Type(), Parser.ty, "i8")
        self.checkParse(astI16Type(), Parser.ty, "i16")
        self.checkParse(astI32Type(), Parser.ty, "i32")
        self.checkParse(astI64Type(), Parser.ty, "i64")
        self.checkParse(astF32Type(), Parser.ty, "f32")
        self.checkParse(astF64Type(), Parser.ty, "f64")

    def testClassTypeSimple(self):
        self.checkParse(astClassType([], "C", [], set()), Parser.ty, "C")

    def testClassTypeArgs(self):
        self.checkParse(astClassType([], "A", [astClassType([], "B", [astClassType([], "C", [], set())], set()),
                                               astClassType([], "D", [], set())], set()),
                        Parser.ty,
                        "A[B[C], D]")

    def testNullableClassType(self):
        self.checkParse(astClassType([], "C", [], set(["?"])), Parser.ty, "C?")

    def testSimpleScopedType(self):
        self.checkParse(astClassType([astScopePrefixComponent("A", None)],
                                     "B", [], set()),
                        Parser.ty,
                        "A.B")

    def testCompoundScopedType(self):
        self.checkParse(astClassType([astScopePrefixComponent("A", None),
                                      astScopePrefixComponent("B", None)],
                                     "C", [], set()),
                        Parser.ty,
                        "A.B.C")

    def testScopedTypeWithArgs(self):
        self.checkParse(astClassType([astScopePrefixComponent("A", [astClassType([], "B", [], set())])],
                                     "C", [astClassType([], "D", [], set())], set()),
                        Parser.ty,
                        "A[B].C[D]")

    def testScopeFromPrimitive(self):
        self.assertRaises(ParseException, self.parseFromSource, Parser.ty,
                          "i64.A")

    def testPrimitiveInScope(self):
        self.assertRaises(ParseException, self.parseFromSource, Parser.ty,
                          "A.i64")

    def testTupleType(self):
        self.checkParse(astTupleType([astClassType([], "A", [], set()),
                                      astClassType([], "B", [astClassType([], "C", [], set())], set())],
                                     set(["?"])),
                        Parser.ty,
                        "(A, B[C])?")

    def testTupleTypeEmpty(self):
        self.assertRaises(ParseException, self.parseFromSource, Parser.ty, "()")

    def testTupleTypeOne(self):
        self.checkParse(astI32Type(), Parser.ty, "(i32)")

    def testTupleTypeOneNullable(self):
        self.assertRaises(ParseException, self.parseFromSource, Parser.ty, "(i32)?")

    def testBlankType(self):
        self.checkParse(astBlankType(), Parser.ty, "_")

    def testExistentialType(self):
        self.checkParse(astExistentialType([astTypeParameter([], None, "T1",
                                                             astClassType([], "U", [], set()),
                                                             astClassType([], "L", [], set())),
                                            astTypeParameter([], None, "T2", None, None)],
                                           astClassType([], "T1", [], set())),
                        Parser.ty,
                        "forsome [T1 <: U >: L, T2] T1")

    def testFunctionTypeNullary(self):
        self.checkParse(
            astFunctionType([], astI32Type()),
            Parser.ty,
            "() -> i32")

    def testFunctionTypeUnary(self):
        self.checkParse(
            astFunctionType([astI8Type()], astI32Type()),
            Parser.ty,
            "i8 -> i32")

    def testFunctionTypeUnaryGrouped(self):
        self.checkParse(
            astFunctionType([astI8Type()], astI32Type()),
            Parser.ty,
            "(i8) -> i32")

    def testFunctionTypeBinary(self):
        self.checkParse(
            astFunctionType([astI8Type(), astI16Type()], astI32Type()),
            Parser.ty,
            "(i8, i16) -> i32")

    def testFunctionTypeBinaryNullable(self):
        self.assertRaises(ParseException, self.parseFromSource, Parser.ty, "(i8, i16)? -> i32")

    def testFunctionTypeCurried(self):
        self.checkParse(
            astFunctionType(
                [astI8Type()],
                astFunctionType([astI16Type()], astI32Type())),
            Parser.ty,
            "i8 -> i16 -> i32")

    def testFunctionTypeCurriedMultiArgs(self):
        self.checkParse(
            astFunctionType(
                [astI8Type(), astI16Type()],
                astFunctionType(
                    [astI32Type(), astI64Type()],
                    astTupleType([astBooleanType(), astUnitType()], set()))),
            Parser.ty,
            "(i8, i16) -> (i32, i64) -> (boolean, unit)")

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
            self.checkParse(astLiteralExpression(astIntegerLiteral(source, value, width)),
                            Parser.expr, source)

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
            self.checkParse(astLiteralExpression(astFloatLiteral(source, value, width)),
                            Parser.expr, source)

    def testStringExpr(self):
        self.checkParse(astLiteralExpression(astStringLiteral("foo\nbar")),
                        Parser.expr, r'"foo\nbar"')

    def testVarExpr(self):
        self.checkParse(astVariableExpression("x"), Parser.expr, "x")
        self.checkParse(astVariableExpression("fo`o"), Parser.expr, r"`fo\`o`")

    def testThisExpr(self):
        self.checkParse(astThisExpression(), Parser.expr, "this")

    def testSuperExpr(self):
        self.checkParse(astSuperExpression(), Parser.expr, "super")

    def testBlockExpr(self):
        self.checkParse(astBlockExpression([astVariableExpression("x"),
                                            astVariableExpression("y")]),
                        Parser.expr, "{x;y;}")

    def testBlockWithUnitExpr(self):
        self.checkParse(
            astBlockExpression([
                astAssignExpression(astVariableExpression("x"), astVariableExpression("y")),
                astBlockExpression([])]),
            Parser.expr,
            "{x = y; {};}")

    def testProp(self):
        self.checkParse(astPropertyExpression(astVariableExpression("o"), "x"),
                        Parser.expr, "o.x")

    def testPropChain(self):
        self.checkParse(astPropertyExpression(astPropertyExpression(astVariableExpression("a"),
                                                                    "b"),
                                              "c"),
                        Parser.expr, "a.b.c")

    def testCallExpr1(self):
        self.checkParse(astCallExpression(astVariableExpression("f"),
                                          None,
                                          []),
                        Parser.expr, "f()")

    def testCallExpr2(self):
        self.checkParse(astCallExpression(astVariableExpression("f"),
                                          None,
                                          [astVariableExpression("a"),
                                           astVariableExpression("b")]),
                        Parser.expr, "f(a, b)")

    def testCallExpr3(self):
        self.checkParse(astCallExpression(astVariableExpression("f"),
                                          [astClassType([], "T", [], set())],
                                          None),
                        Parser.expr, "f[T]")

    def testCallExpr4(self):
        self.checkParse(astCallExpression(astVariableExpression("f"),
                                          [astClassType([], "T", [], set())],
                                          [astVariableExpression("a")]),
                        Parser.expr, "f[T](a)")

    def testCallMethod1(self):
        self.checkParse(astCallExpression(astPropertyExpression(astVariableExpression("o"),
                                                                "f"),
                                          [astClassType([], "T", [], set())],
                                          None),
                        Parser.expr, "o.f[T]")

    def testCallMethod2(self):
        self.checkParse(astCallExpression(astPropertyExpression(astVariableExpression("o"),
                                                                "f"),
                                          None,
                                          [astVariableExpression("a")]),
                        Parser.expr, "o.f(a)")

    def testCallMethod3(self):
        self.checkParse(astCallExpression(astPropertyExpression(astVariableExpression("o"),
                                                                "f"),
                                          [astClassType([], "T", [], set())],
                                          [astVariableExpression("a")]),
                        Parser.expr, "o.f[T](a)")

    def testCallMethod4(self):
        self.checkParse(astCallExpression(astPropertyExpression(astVariableExpression("o"),
                                                                "f"),
                                          None,
                                          []),
                        Parser.expr, "o.f()")

    def testCallLambda1(self):
        self.checkParse(
            astLambdaExpression(None, astCallExpression(astVariableExpression("f"), None, [])),
            Parser.expr,
            "lambda f()")

    def testCallLambda2(self):
        self.checkParse(
            astCallExpression(
                astGroupExpression(astLambdaExpression(None, astVariableExpression("x"))),
                None,
                []),
            Parser.expr,
            "(lambda x)()")

    def testTupleExprs(self):
        self.checkParse(
            astTupleExpression([
                astVariableExpression("a"),
                astVariableExpression("b")]),
            Parser.expr,
            "a, b")
        self.checkParse(
            astTupleExpression([
                astVariableExpression("a"),
                astGroupExpression(
                    astTupleExpression([
                        astVariableExpression("b"),
                        astVariableExpression("c")])),
                astVariableExpression("d")]),
            Parser.expr,
            "a, (b, c), d")
        self.checkParse(
            astCallExpression(
                astVariableExpression("a"),
                None,
                [astGroupExpression(
                    astTupleExpression([
                        astVariableExpression("b"),
                        astVariableExpression("c")]))]),
            Parser.expr,
            "a((b, c))")

    def testUnaryExpr(self):
        self.checkParse(astUnaryExpression("-", astVariableExpression("x")),
                        Parser.expr, "-x")

    def testUnaryUnaryExpr(self):
        self.checkParse(astUnaryExpression("!",
                                           astUnaryExpression("-",
                                                              astVariableExpression("x"))),
                        Parser.expr, "! -x")

    def testUnaryArgsExpr(self):
        self.checkParse(astCallExpression(astVariableExpression("Vector"),
                                          None,
                                          [astUnaryExpression("-", astVariableExpression("x")),
                                           astUnaryExpression("-", astVariableExpression("y"))]),
                        Parser.expr, "Vector(-x, -y)")

    def testUnaryTupleExpr(self):
        self.checkParse(astTupleExpression([astUnaryExpression("-", astVariableExpression("x")),
                                            astUnaryExpression("-", astVariableExpression("y"))]),
                        Parser.expr, "-x, -y")

    def testUnaryPropertyExpr(self):
        self.checkParse(astUnaryExpression("~",
                                           astPropertyExpression(astVariableExpression("x"),
                                                                 "y")),
                        Parser.expr, "~x.y")

    def testBinaryExpr(self):
        self.checkParse(astBinaryExpression("+",
                                            astVariableExpression("x"),
                                            astVariableExpression("y")),
                        Parser.expr, "x + y")

    def testNestedBinaryExpr(self):
        self.checkParse(astBinaryExpression("+",
                                            astVariableExpression("x"),
                                            astBinaryExpression("*",
                                                                astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        Parser.expr, "x + y * z")

    def testNestedBinaryExpr2(self):
        self.checkParse(astBinaryExpression("+",
                                            astBinaryExpression("*",
                                                                astVariableExpression("x"),
                                                                astVariableExpression("y")),
                                            astVariableExpression("z")),
                        Parser.expr, "x * y + z")

    def testBinaryMultipleOfExpr(self):
        self.checkParse(astBinaryExpression("==",
                                            astBinaryExpression("%",
                                                                astVariableExpression("x"),
                                                                astLiteralExpression(astIntegerLiteral("3", 3, 64))),
                                            astLiteralExpression(astIntegerLiteral("0", 0, 64))),
                        Parser.expr, "x % 3 == 0")

    def testLeftAssociativeBinaryExpr(self):
        self.checkParse(astBinaryExpression("+",
                                            astBinaryExpression("+",
                                                                astVariableExpression("x"),
                                                                astVariableExpression("y")),
                                            astVariableExpression("z")),
                        Parser.expr, "x + y + z")

    def testRightAssociativeBinaryExpr(self):
        self.checkParse(astBinaryExpression("::",
                                            astVariableExpression("x"),
                                            astBinaryExpression("::",
                                                                astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        Parser.expr, "x :: y :: z")

    def testLogicExpr(self):
        self.checkParse(astBinaryExpression("||",
                                            astBinaryExpression("&&",
                                                                astVariableExpression("x"),
                                                                astVariableExpression("y")),
                                            astVariableExpression("z")),
                        Parser.expr, "x && y || z")

    def testOtherBinaryExpr(self):
        self.checkParse(
            astBinaryExpression(
                "+",
                astBinaryExpression(
                    "@", astVariableExpression("x"), astVariableExpression("y")),
                astVariableExpression("z")),
            Parser.expr,
            "x @ y + z")

    def testAssignExpr(self):
        self.checkParse(astAssignExpression(astVariableExpression("x"),
                                            astBinaryExpression("+",
                                                                astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        Parser.expr, "x = y + z")

    def testAssignLowPrecedence(self):
        self.checkParse(astAssignExpression(astVariableExpression("x"),
                                            astBinaryExpression("|",
                                                                astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        Parser.expr, "x = y | z")

    def testAssignRightAssociative(self):
        self.checkParse(astAssignExpression(astVariableExpression("x"),
                                            astAssignExpression(astVariableExpression("y"),
                                                                astVariableExpression("z"))),
                        Parser.expr, "x = y = z")

    def testAssignBinop(self):
        self.checkParse(astBinaryExpression("+=",
                                            astVariableExpression("x"),
                                            astVariableExpression("y")),
                        Parser.expr, "x += y")

    def testEqLogicBinopPrecedence(self):
        self.checkParse(astBinaryExpression("&&",
                                            astBinaryExpression("!==",
                                                                astVariableExpression("a"),
                                                                astVariableExpression("b")),
                                            astBinaryExpression("===",
                                                                astVariableExpression("c"),
                                                                astVariableExpression("d"))),
                        Parser.expr, "a !== b && c === d")

    def testIfExpr(self):
        self.checkParse(astIfExpression(astVariableExpression("x"),
                                        astVariableExpression("y"),
                                        astVariableExpression("z")),
                        Parser.expr, "if (x) y else z")

    def testIfExprNoElse(self):
        self.checkParse(astIfExpression(astVariableExpression("x"),
                                        astVariableExpression("y"),
                                        None),
                        Parser.expr, "if (x) y")

    def testIfExprWithBinop(self):
        self.checkParse(astIfExpression(astVariableExpression("x"),
                                        astBinaryExpression("+",
                                                            astVariableExpression("y"),
                                                            astVariableExpression("z")),
                                        None),
                        Parser.expr, "if (x) y + z")

    def testWhileExpr(self):
        self.checkParse(astWhileExpression(astVariableExpression("x"),
                                           astVariableExpression("y")),
                        Parser.expr, "while (x) y")

    def testWhileBlockEmptyExpr(self):
        self.checkParse(astWhileExpression(astVariableExpression("c"),
                                           astBlockExpression([])),
                        Parser.expr, "while (c) {}")

    def testWhileBlockExpr(self):
        self.checkParse(astWhileExpression(astBinaryExpression(">",
                                                               astVariableExpression("n"),
                                                               astLiteralExpression(astIntegerLiteral("0", 0, 64))),
                                           astAssignExpression(astVariableExpression("n"),
                                                               astBinaryExpression("-",
                                                                                   astVariableExpression("n"),
                                                                                   astLiteralExpression(astIntegerLiteral("1", 1, 64))))),
                        Parser.expr, "while (n > 0)\n" + \
                                      "  n = n - 1")

    def testBreakExpr(self):
        self.checkParse(astBreakExpression(),
                        Parser.expr, "break")

    def testContinueExpr(self):
        self.checkParse(astContinueExpression(),
                        Parser.expr, "continue")

    def testPartialFunctionExpr(self):
        self.checkParse(astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                             astVariableExpression("b"),
                                                                             astVariableExpression("x")),
                                                      astPartialFunctionCase(astVariablePattern("y", None),
                                                                             None,
                                                                             astVariableExpression("y"))]),
                        Parser.partialFnExpr,
                        "{ case x if b => x; case y => y; }")

    def testMatchExpr(self):
        self.checkParse(astMatchExpression(astVariableExpression("x"),
                                           astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                                                None,
                                                                                                astVariableExpression("x"))])),
                        Parser.expr,
                        "match (x) { case x => x; }")

    def testThrowExpr(self):
        self.checkParse(astThrowExpression(astVariableExpression("x")),
                        Parser.expr, "throw x")

    def testTryCatchExpr(self):
        self.checkParse(astTryCatchExpression(astVariableExpression("x"),
                                              astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                                                   None,
                                                                                                   astVariableExpression("x"))]),
                                              None),
                        Parser.expr,
                        "try x catch { case x => x; }")

    def testTryCatchFinallyExpr(self):
        self.checkParse(astTryCatchExpression(astVariableExpression("x"),
                                              astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                                                   None,
                                                                                                   astVariableExpression("x"))]),
                                              astVariableExpression("x")),
                        Parser.expr,
                        "try x catch { case x => x; } finally x")

    def testTryFinallyExpr(self):
        self.checkParse(astTryCatchExpression(astVariableExpression("x"),
                                              None,
                                              astVariableExpression("x")),
                        Parser.expr,
                        "try x finally x")

    def testTryCatchSimpleExpr(self):
        self.checkParse(astTryCatchExpression(astVariableExpression("x"),
                                              astPartialFunctionExpression([astPartialFunctionCase(astVariablePattern("x", None),
                                                                                                   None,
                                                                                                   astVariableExpression("x"))]),
                                              None),
                        Parser.expr,
                        "try x catch (x) x")

    def testTryExprError(self):
        self.assertRaises(ParseException, self.parseFromSource, Parser.expr,
                          "try x")

    def testLambdaExpr(self):
        self.checkParse(
            astLambdaExpression([astParameter([], None, astVariablePattern("x", None)),
                                 astParameter([], None, astVariablePattern("y", None))],
                                astVariableExpression("x")),
            Parser.expr,
            "lambda (x, y) x")

    def testLambdaNoArgs(self):
        self.checkParse(astLambdaExpression(None, astVariableExpression("x")),
                        Parser.expr,
                        "lambda x")

    def testReturnEmpty(self):
        self.checkParse(astReturnExpression(None),
                        Parser.expr, "return")

    def testReturnExpr(self):
        self.checkParse(astReturnExpression(astVariableExpression("x")),
                        Parser.expr, "return x")

    def testGroupExpr(self):
        self.checkParse(
            astBinaryExpression(
                "*",
                astGroupExpression(
                    astBinaryExpression(
                        "+",
                        astVariableExpression("x"),
                        astVariableExpression("y"))),
                astVariableExpression("z")),
            Parser.expr,
            "(x + y) * z")

    def testNewArrayExpr(self):
        self.checkParse(astNewArrayExpression(astLiteralExpression(astIntegerLiteral("123", 123, 64)),
                                              astClassType([], "Foo", [], set()),
                                              None),
                        Parser.expr, "new(123) Foo")

    def testNewArrayExprWithTypeArgs(self):
        self.checkParse(astNewArrayExpression(astLiteralExpression(astIntegerLiteral("123", 123, 64)),
                                              astClassType([], "Foo", [astClassType([], "Bar", [], set())], set()),
                                              None),
                        Parser.expr, "new(123) Foo[Bar]")

    def testNewArrayExprWithPrefix(self):
        self.checkParse(astNewArrayExpression(astLiteralExpression(astIntegerLiteral("123", 123, 64)),
                                              astClassType([astScopePrefixComponent("Foo", [astClassType([], "Bar", [], set())])],
                                                           "Baz", [], set()),
                                              None),
                        Parser.expr, "new(123) Foo[Bar].Baz")

    def testNewArrayExprWithArgs(self):
        self.checkParse(astNewArrayExpression(astLiteralExpression(astIntegerLiteral("123", 123, 64)),
                                              astClassType([], "Foo", [], set()),
                                              [astVariableExpression("x"),
                                               astVariableExpression("y")]),
                        Parser.expr, "new(123) Foo(x, y)")

    # Literals
    def testBooleanLits(self):
        self.checkParse(astBooleanLiteral(True), Parser.literal, "true")
        self.checkParse(astBooleanLiteral(False), Parser.literal, "false")

    def testIntLits(self):
        self.checkParse(astIntegerLiteral("123", 123, 64), Parser.literal, "123")
        self.checkParse(astIntegerLiteral("-123i32", -123, 32), Parser.literal, "-123i32")

    def testFloatLits(self):
        self.checkParse(astFloatLiteral("1.5", 1.5, 64), Parser.literal, "1.5")
        self.checkParse(astFloatLiteral("-1.5f32", -1.5, 32), Parser.literal, "-1.5f32")

    def testNullLit(self):
        self.checkParse(astNullLiteral(), Parser.literal, "null")

    def testStringLit(self):
        self.checkParse(astStringLiteral("foo\nbar"), Parser.literal, r'"foo\nbar"')

if __name__ == "__main__":
    unittest.main()

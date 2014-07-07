# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest
import sys

from builtins import *
from lexer import *
from layout import *
from parser import *
from ast import *
from compile_info import *
from scope_analysis import *
from type_analysis import *
from compiler import *
from ir import *
from ir_types import *
import ir_instructions
from ir_instructions import *
from bytecode import *


class TestCompile(unittest.TestCase):
    def compileFromSource(self, source):
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        info = CompileInfo(ast)
        analyzeDeclarations(info)
        analyzeInheritance(info)
        analyzeTypes(info)
        convertClosures(info)
        flattenClasses(info)
        compile(info)
        return info.package

    def makePackage(self, input):
        if isinstance(input, Package):
            return input
        elif isinstance(input, AstModule):
            scopeAnalysis(input)
            return compile(input)
        elif type(input) is str:
            return self.compileFromSource(input)

    def makeSimpleFunction(self, name, retTy, blocks,
                           parameterTypes=None, variables=None, flags=frozenset()):
        if variables is None:
            variables = []
        if parameterTypes is None:
            parameterTypes = [v.type for v in variables if v.kind is PARAMETER]
        blockList = [BasicBlock(i, insts) for i, insts in enumerate(blocks)]
        function = Function(name, retTy, None, parameterTypes, variables, blockList, flags)
        return function

    def checkFunction(self, input, expected):
        package = self.makePackage(input)
        function = package.findFunction(name=expected.name)
        self.assertEquals(expected, function)

    def testBasicFunction(self):
        self.checkFunction("def f = 12",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               ret()]]))

    def testFloatLiterals(self):
        self.checkFunction("def f = 1f32",
                           self.makeSimpleFunction("f", F32Type, [[
                               f32(1.),
                               ret()]]))
        self.checkFunction("def f = 1f64",
                           self.makeSimpleFunction("f", F64Type, [[
                               f64(1.),
                               ret()]]))

    def testEmptyBlock(self):
        self.checkFunction("def f = {}",
                           self.makeSimpleFunction("f", UnitType, [[
                               unit(),
                               ret()]]))

    def testSimpleBlock(self):
        self.checkFunction("def f = { 12; 34; }",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               drop(),
                               i64(34),
                               ret()]]))

    def testBlockWithinBlock(self):
        self.checkFunction("def f = { { 12; }; 34; };",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               drop(),
                               i64(34),
                               ret()]]))

    def testBlockEndingWithStmt(self):
        self.checkFunction("def f = { var x = 12; };",
                           self.makeSimpleFunction("f", UnitType, [[
                               i64(12),
                               stlocal(-8),
                               unit(),
                               ret()]],
                             variables=[Variable("x", I64Type, LOCAL, frozenset())]))

    def testSimpleVar(self):
        self.checkFunction("def f = { var x = 12; x; }",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               stlocal(-8),
                               ldlocal(-8),
                               ret()]],
                             variables=[Variable("x", I64Type, LOCAL, frozenset())]))

    def testSimpleParameter(self):
        self.checkFunction("def f(x: i64) = x",
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               ret()]],
                             variables=[Variable("x", I64Type, PARAMETER, frozenset())]))

    def testSeveralParameters(self):
        self.checkFunction("def f(a: i32, b: boolean, c: boolean, d: i32) =\n" + \
                           "  a = 1i32\n" + \
                           "  b = true\n" + \
                           "  c = false\n" + \
                           "  d\n",
                           self.makeSimpleFunction("f", I32Type, [[
                               i32(1),
                               stlocal(24),
                               true(),
                               stlocal(16),
                               false(),
                               stlocal(8),
                               ldlocal(0),
                               ret()]],
                             variables=[
                               Variable("a", I32Type, PARAMETER, frozenset()),
                               Variable("b", BooleanType, PARAMETER, frozenset()),
                               Variable("c", BooleanType, PARAMETER, frozenset()),
                               Variable("d", I32Type, PARAMETER, frozenset())]))

    def testAssign(self):
        self.checkFunction("def f =\n" + \
                           "  var x = 12\n" + \
                           "  x = 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               stlocal(-8),
                               i64(34),
                               dup(),
                               stlocal(-8),
                               ret()]],
                             variables=[Variable("x", I64Type, LOCAL, frozenset())]))

    def testAssignShortProps(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var x: boolean\n" +
                                         "  var y: i64\n" +
                                         "def f(foo: Foo) =\n" +
                                         "  foo.x = false\n" +
                                         "  foo.y = 12\n")
        clasTy = ClassType(package.classes[0], ())
        expected = Function("f", I64Type, None, [clasTy], [
                                Variable("foo", clasTy, PARAMETER, frozenset())],
                              [BasicBlock(0, [
                                ldlocal(0),
                                false(),
                                swap(),
                                st8(WORDSIZE),
                                ldlocal(0),
                                i64(12),
                                dup(),
                                swap2(),
                                st64(WORDSIZE + 8),
                                ret()])],
                              frozenset())
        self.assertEquals(expected, package.findFunction(name="f"))

    def testAssignPtrField(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var self: Foo\n" +
                                         "  def this =\n" +
                                         "    this.self = this")
        clas = package.findClass(name="Foo")
        clasTy = ClassType(clas)
        init = package.findFunction(name="$initializer")
        expected = self.makeSimpleFunction("$constructor", UnitType, [[
                       ldlocal(0),
                       callg(1, getRootClass().constructors[0].id),
                       drop(),
                       ldlocal(0),
                       callg(1, init.id),
                       drop(),
                       ldlocal(0),
                       ldlocal(0),
                       dup(),
                       swap2(),
                       stp(WORDSIZE),
                       ret()]],
                     parameterTypes=[clasTy],
                     variables=[Variable("$this", clasTy, PARAMETER, frozenset())])
        self.assertEquals(expected, clas.constructors[0])

    def testLoadPtrField(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var self: Foo\n" +
                                         "def f(foo: Foo) =\n" +
                                         "  foo.self\n")
        clasTy = ClassType(package.classes[0], ())
        expected = self.makeSimpleFunction("f", clasTy, [[
                       ldlocal(0),
                       ldp(WORDSIZE),
                       ret()]],
                     parameterTypes=[clasTy],
                     variables=[Variable("foo", clasTy, PARAMETER, frozenset())])

    def testAccumShortPropForEffect(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var x: i64\n" +
                                         "def f(foo: Foo) =\n" +
                                         "  foo.x += 12\n" +
                                         "  34")
        clasTy = ClassType(package.classes[0], ())
        expected = Function("f", I64Type, None, [clasTy], [
                              Variable("foo", clasTy, PARAMETER, frozenset())],
                            [BasicBlock(0, [
                              ldlocal(0),
                              dup(),
                              ld64(WORDSIZE),
                              i64(12),
                              addi64(),
                              swap(),
                              st64(WORDSIZE),
                              i64(34),
                              ret()])],
                            frozenset())
        self.assertEquals(expected, package.findFunction(name="f"))

    def testAccumShortPropForValue(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var x: i64\n" +
                                         "def f(foo: Foo) =\n" +
                                         "  foo.x += 12\n")
        clasTy = ClassType(package.classes[0], ())
        expected = Function("f", I64Type, None, [clasTy], [
                              Variable("foo", clasTy, PARAMETER, frozenset())],
                            [BasicBlock(0, [
                              ldlocal(0),
                              dup(),
                              ld64(WORDSIZE),
                              i64(12),
                              addi64(),
                              dup(),
                              swap2(),
                              st64(WORDSIZE),
                              ret()])],
                            frozenset())
        self.assertEquals(expected, package.findFunction(name="f"))

    def testLoadNonNullableObject(self):
        ty = getRootClassType()
        self.checkFunction("class Foo\n" + \
                           "  def this = { this.x = this; }\n" + \
                           "  var x: Object\n" + \
                           "def f = Foo.x",
                           self.makeSimpleFunction("f", ty, [[
                               allocobj(0),
                               dup(),
                               callg(1, 1),
                               drop(),
                               ldpc(WORDSIZE),
                               ret()]]))

    def testLoadNullableObject(self):
        ty = ClassType(getRootClass(), (), NULLABLE_TYPE_FLAG)
        self.checkFunction("class Foo\n" + \
                           "  def this = {}\n" + \
                           "  var x: Object?\n" + \
                           " def f = Foo.x",
                           self.makeSimpleFunction("f", ty, [[
                               allocobj(0),
                               dup(),
                               callg(1, 1),
                               drop(),
                               ldp(WORDSIZE),
                               ret()]]))

    def testNullableEq(self):
        ty = ClassType(getRootClass(), (), NULLABLE_TYPE_FLAG)
        self.checkFunction("def f(foo: Object?, bar: Object?) = foo === bar",
                           self.makeSimpleFunction("f", BooleanType, [[
                               ldlocal(WORDSIZE),
                               ldlocal(0),
                               eqp(),
                               ret()]],
                             variables=[Variable("foo", ty, PARAMETER, frozenset()),
                                        Variable("bar", ty, PARAMETER, frozenset())]))

    def testNullableNe(self):
        ty = ClassType(getRootClass(), (), NULLABLE_TYPE_FLAG)
        self.checkFunction("def f(foo: Object?, bar: Object?) = foo !== bar",
                           self.makeSimpleFunction("f", BooleanType, [[
                               ldlocal(WORDSIZE),
                               ldlocal(0),
                               nep(),
                               ret()]],
                             variables=[Variable("foo", ty, PARAMETER, frozenset()),
                                        Variable("bar", ty, PARAMETER, frozenset())]))

    def testTruncI32(self):
        self.checkFunction("def f(n: i64) = n.to-i32",
                           self.makeSimpleFunction("f", I32Type, [[
                               ldlocal(0),
                               trunci32(),
                               ret()]],
                             variables=[Variable("n", I64Type, PARAMETER, frozenset())]))

    def testSext16(self):
        self.checkFunction("def f(n: i8) = n.to-i16",
                           self.makeSimpleFunction("f", I16Type, [[
                               ldlocal(0),
                               sexti16_8(),
                               ret()]],
                             variables=[Variable("n", I8Type, PARAMETER, frozenset())]))

    def testF32ToI64(self):
        self.checkFunction("def f(n: f32) = n.to-i64",
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               fcvti32(),
                               sexti64_32(),
                               ret()]],
                             variables=[Variable("n", F32Type, PARAMETER, frozenset())]))

    def testNeg(self):
        self.checkFunction("def f = - 12",
                           self.makeSimpleFunction("f", I64Type, [[
                             i64(12),
                             negi64(),
                             ret()]]))

    def testAdd(self):
        self.checkFunction("def f = 12 + 34",
                           self.makeSimpleFunction("f", I64Type, [[
                             i64(12),
                             i64(34),
                             addi64(),
                             ret()
                           ]]))

    def testAddAssign(self):
        self.checkFunction("def f = {\n" + \
                           "  var x = 12;\n" + \
                           "  x += 34;\n" + \
                           "};",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               stlocal(-8),
                               ldlocal(-8),
                               i64(34),
                               addi64(),
                               dup(),
                               stlocal(-8),
                               ret()]],
                             variables=[Variable("x", I64Type, LOCAL, frozenset())]))

    def testAddFloat(self):
        self.checkFunction("def f = 1.2 + 3.4",
                           self.makeSimpleFunction("f", F64Type, [[
                               f64(1.2),
                               f64(3.4),
                               addf64(),
                               ret()]]))

    def testConcatStrings(self):
        stringClass = getStringClass()
        concatMethod = stringClass.getMethod("+")
        concatMethodIndex = stringClass.getMethodIndex(concatMethod)
        self.checkFunction("def f = \"foo\" + \"bar\"",
                           self.makeSimpleFunction("f", getStringType(), [[
                               ir_instructions.string(0),
                               ir_instructions.string(1),
                               callv(2, concatMethodIndex),
                               ret()]]))

    def testCompareStrings(self):
        stringClass = getStringClass()
        eqMethod = stringClass.getMethod("==")
        eqMethodIndex = stringClass.getMethodIndex(eqMethod)
        self.checkFunction("def f = \"foo\" == \"bar\"",
                           self.makeSimpleFunction("f", BooleanType, [[
                               ir_instructions.string(0),
                               ir_instructions.string(1),
                               callv(2, eqMethodIndex),
                               ret()]]))

    def testLessThan(self):
        self.checkFunction("def f = 12 < 34",
                           self.makeSimpleFunction("f", BooleanType, [[
                               i64(12),
                               i64(34),
                               lti64(),
                               ret()]]))

    def testAndExpr(self):
        self.checkFunction("def f = true && false",
                           self.makeSimpleFunction("f", BooleanType, [[
                               true(),
                               dup(),
                               branchif(1, 2),
                             ], [
                               drop(),
                               false(),
                               branch(2),
                             ], [
                               ret()
                             ]]))

    def testOrExpr(self):
        self.checkFunction("def f = true || false",
                           self.makeSimpleFunction("f", BooleanType, [[
                               true(),
                               dup(),
                               branchif(2, 1),
                             ], [
                               drop(),
                               false(),
                               branch(2),
                             ], [
                               ret(),
                             ]]))

    def testIfExpr(self):
        self.checkFunction("def f = if (true) 12 else 34",
                           self.makeSimpleFunction("f", I64Type, [[
                             true(),
                             branchif(1, 2),
                           ], [
                             i64(12),
                             branch(3),
                           ], [
                             i64(34),
                             branch(3),
                           ], [
                             ret(),
                           ]]))

    def testIfStmt(self):
        self.checkFunction("def f = { if (true) 12 else 34; 56; }",
                           self.makeSimpleFunction("f", I64Type, [[
                               true(),
                               branchif(1, 2),
                             ], [
                               i64(12),
                               drop(),
                               branch(3),
                             ], [
                               i64(34),
                               drop(),
                               branch(3),
                             ], [
                               i64(56),
                               ret(),
                             ]]))

    def testIfExprWithoutElse(self):
        self.checkFunction("def f = if (true) 12",
                           self.makeSimpleFunction("f", UnitType, [[
                               true(),
                               branchif(1, 2),
                             ], [
                               i64(12),
                               drop(),
                               branch(2),
                             ], [
                               unit(),
                               ret(),
                             ]]))

    def testIfStmtWithoutElse(self):
        self.checkFunction("def f = { if (true) 12; 34; };",
                           self.makeSimpleFunction("f", I64Type, [[
                               true(),
                               branchif(1, 2),
                             ], [
                               i64(12),
                               drop(),
                               branch(2),
                             ], [
                               i64(34),
                               ret(),
                             ]]))

    def testWhileExpr(self):
        self.checkFunction("def f = while (true) 1",
                           self.makeSimpleFunction("f", UnitType, [[
                             branch(1),
                           ], [
                             true(),
                             branchif(2, 3),
                           ], [
                             i64(1),
                             drop(),
                             branch(1),
                           ], [
                             unit(),
                             ret(),
                           ]]))

    def testWhileBlock(self):
        self.checkFunction("def f = while (true) { 1; };",
                           self.makeSimpleFunction("f", UnitType, [[
                             branch(1),
                           ], [
                             true(),
                             branchif(2, 3),
                           ], [
                             i64(1),
                             drop(),
                             branch(1),
                           ], [
                             unit(),
                             ret(),
                           ]]))

    def testTryWithMatch(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        typeofMethod = exnClass.getMethod("typeof")
        typeofMethodIndex = exnClass.getMethodIndex(typeofMethod)
        typeClass = getTypeClass()
        typeTy = ClassType(typeClass)
        isSubtypeOfMethod = typeClass.getMethod("is-subtype-of")
        isSubtypeOfMethodIndex = typeClass.getMethodIndex(isSubtypeOfMethod)
        self.checkFunction("def f = try 12 catch\n" +
                           "    case exn => 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               pushtry(1, 2),
                             ], [
                               i64(12),
                               poptry(4),
                             ], [
                               dup(),
                               callv(1, typeofMethodIndex),
                               dup(),
                               allocarri(typeClass.id, 1),
                               dup(),
                               cls(exnClass.id),
                               callg(2, BUILTIN_TYPE_CTOR_ID),
                               drop(),
                               callv(2, isSubtypeOfMethodIndex),
                               branchif(3, 5),
                             ], [
                               stlocal(-WORDSIZE),
                               drop(),
                               drop(),
                               i64(34),
                               branch(4),
                             ], [
                               ret(),
                             ], [
                               drop(),
                               throw(),
                             ]],
                             variables=[Variable("exn", exnTy, LOCAL, frozenset())]))

    def testTryWithMatchAndFinally(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        typeofMethod = exnClass.getMethod("typeof", [])
        typeofMethodIndex = exnClass.getMethodIndex(typeofMethod)
        typeClass = getTypeClass()
        typeTy = ClassType(typeClass)
        isSubtypeOfMethod = typeClass.getMethod("is-subtype-of")
        isSubtypeOfMethodIndex = typeClass.getMethodIndex(isSubtypeOfMethod)
        self.checkFunction("def f = try 12 catch\n" +
                           "    case exn => 34\n" +
                           "  finally 56",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0
                               pushtry(1, 2),
                             ], [
                               # block 1
                               i64(12),
                               poptry(4),
                             ], [
                               # block 2
                               dup(),
                               callv(1, typeofMethodIndex),
                               dup(),
                               allocarri(BUILTIN_TYPE_CLASS_ID, 1),
                               dup(),
                               cls(exnClass.id),
                               callg(2, BUILTIN_TYPE_CTOR_ID),
                               drop(),
                               callv(2, isSubtypeOfMethodIndex),
                               branchif(3, 5),
                             ], [
                               # block 3
                               stlocal(-WORDSIZE),
                               drop(),
                               drop(),
                               i64(34),
                               branch(4),
                             ], [
                               # block 4
                               null(),
                               branch(6),
                             ], [
                               # block 5
                               drop(),
                               uninitialized(),
                               swap(),
                               branch(6),
                             ], [
                               # block 6
                               i64(56),
                               drop(),
                               dup(),
                               null(),
                               eqp(),
                               branchif(7, 8),
                             ], [
                               # block 7
                               drop(),
                               ret(),
                             ], [
                               # block 8
                               throw(),
                             ]],
                             variables=[Variable("exn", exnTy, LOCAL, frozenset())]))

    def testThrow(self):
        exnTy = ClassType(getExceptionClass())
        self.checkFunction("def f(exn: Exception) = throw exn",
                           self.makeSimpleFunction("f", getNothingClassType(), [[
                               ldlocal(0),
                               throw(),
                             ]],
                             variables=[Variable("exn", exnTy, PARAMETER, frozenset())]))

    def testThrowInIfBody(self):
        exnTy = ClassType(getExceptionClass())
        self.checkFunction("def f(exn: Exception) = if (true) throw exn else 12",
                           self.makeSimpleFunction("f", I64Type, [[
                               true(),
                               branchif(1, 2),
                             ], [
                               ldlocal(0),
                               throw(),
                             ], [
                               i64(12),
                               branch(3),
                             ], [
                               ret(),
                             ]],
                             variables=[Variable("exn", exnTy, PARAMETER, frozenset())]))

    def testThrowInIfCondition(self):
        exnTy = ClassType(getExceptionClass())
        self.checkFunction("def f(exn: Exception) = if (throw exn) 12 else 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               throw(),
                             ]],
                             variables=[Variable("exn", exnTy, PARAMETER, frozenset())]))

    def testThrowInWhileBody(self):
        exnTy = ClassType(getExceptionClass())
        self.checkFunction("def f(exn: Exception) = while (false) throw exn",
                           self.makeSimpleFunction("f", UnitType, [[
                               branch(1),
                             ], [
                               false(),
                               branchif(2, 3),
                             ], [
                               ldlocal(0),
                               throw(),
                             ], [
                               unit(),
                               ret(),
                             ]],
                             variables=[Variable("exn", exnTy, PARAMETER, frozenset())]))

    def testThrowInTry(self):
        exnTy = ClassType(getExceptionClass())
        self.checkFunction("def f = try throw Exception catch\n" +
                           "  case exn => 1",
                           self.makeSimpleFunction("f", I64Type, [[
                               pushtry(1, 2),
                             ], [
                               allocobj(BUILTIN_EXCEPTION_CLASS_ID),
                               dup(),
                               callg(1, BUILTIN_EXCEPTION_CTOR_ID),
                               drop(),
                               throw(),
                             ], [
                               dup(),
                               callv(1, 2),
                               dup(),
                               allocarri(BUILTIN_TYPE_CLASS_ID, 1),
                               dup(),
                               cls(BUILTIN_EXCEPTION_CLASS_ID),
                               callg(2, BUILTIN_TYPE_CTOR_ID),
                               drop(),
                               callv(2, 3),
                               branchif(3, 5),
                             ], [
                               stlocal(-WORDSIZE),
                               drop(),
                               drop(),
                               i64(1),
                               branch(4),
                             ], [
                               ret(),
                             ], [
                               drop(),
                               throw(),
                             ]],
                             variables=[Variable("exn", exnTy, LOCAL, frozenset())]))

    def testTryFinallyExpr(self):
        self.checkFunction("def f = try 12 finally 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               pushtry(1, 3),
                             ], [
                               i64(12),
                               poptry(2),
                             ], [
                               null(),
                               branch(4),
                             ], [
                               uninitialized(),
                               swap(),
                               branch(4),
                             ], [
                               i64(34),
                               drop(),
                               dup(),
                               null(),
                               eqp(),
                               branchif(5, 6),
                             ], [
                               drop(),
                               ret(),
                             ], [
                               throw(),
                             ]]))

    def testReturn(self):
        self.checkFunction("def f = return 12",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               ret(),
                             ]]))

    def testReturnInWhileBody(self):
        self.checkFunction("def f = while (false) return {}",
                           self.makeSimpleFunction("f", UnitType, [[
                               branch(1),
                             ], [
                               false(),
                               branchif(2, 3),
                             ], [
                               unit(),
                               ret(),
                             ], [
                               unit(),
                               ret(),
                             ]]))

    def testReturnBeforeJoin(self):
        self.checkFunction("def f = if (true) return 1 else return 2",
                           self.makeSimpleFunction("f", I64Type, [[
                               true(),
                               branchif(1, 2),
                             ], [
                               i64(1),
                               ret(),
                             ], [
                               i64(2),
                               ret(),
                             ]]))

    def testUnreachableTry(self):
        exnTy = ClassType(getExceptionClass())
        self.checkFunction("def f =\n" +
                           "  return 1\n" +
                           "  try throw Exception catch\n" +
                           "    case exn => 2\n",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(1),
                               ret(),
                             ]],
                             variables=[Variable("exn", exnTy, LOCAL, frozenset())]))

    def testFactorial(self):
        self.checkFunction("def f(n: i64): i64 = {\n" +
                           "  var p = 1;\n" +
                           "  while (n > 1) {\n" +
                           "    p = p * n;\n" +
                           "    n = n - 1;\n" +
                           "  };\n" +
                           "  p;\n" +
                           "};",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(1),
                               stlocal(-8),
                               branch(1),
                             ], [
                               ldlocal(0),
                               i64(1),
                               gti64(),
                               branchif(2, 3),
                             ], [
                               ldlocal(-8),
                               ldlocal(0),
                               muli64(),
                               stlocal(-8),
                               ldlocal(0),
                               i64(1),
                               subi64(),
                               stlocal(0),
                               branch(1),
                             ], [
                               ldlocal(-8),
                               ret(),
                             ]],
                           variables=[
                             Variable("n", I64Type, PARAMETER, frozenset()),
                             Variable("p", I64Type, LOCAL, frozenset())]))

    def testRequireParameterTypes(self):
        self.assertRaises(TypeException, self.compileFromSource, "def f(x) = 12")

    def testTypeRequiresBody(self):
        self.assertRaises(CompileException, self.compileFromSource, "def f: i64")

    def testNullaryCall(self):
        source = "def f: i64 = 12\n" + \
                 "def g: i64 = f\n"
        self.checkFunction(source,
                           self.makeSimpleFunction("g", I64Type, [[
                               callg(0, 0),
                               ret(),
                             ]]))

    def testFunctionCall(self):
        source = "def f(x: i64, y: i64): i64 = x\n" + \
                 "def g: i64 = f(12, 34)\n"
        self.checkFunction(source,
                           self.makeSimpleFunction("g", I64Type, [[
                               i64(12),
                               i64(34),
                               callg(2, 0),
                               ret(),
                             ]]))

    def testInitializer(self):
        source = "class Foo\n" + \
                 "  var x: Foo = this\n" + \
                 "  var y: Foo"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        thisType = ClassType(clas)
        self.checkFunction(package,
                           self.makeSimpleFunction("$initializer", UnitType, [[
                               ldlocal(0),
                               ldlocal(0),
                               stp(WORDSIZE),
                               uninitialized(),
                               ldlocal(0),
                               stp(2 * WORDSIZE),
                               unit(),
                               ret()]],
                             parameterTypes=[thisType],
                             variables=[Variable("$this", thisType, PARAMETER, frozenset())]))

    def testDefaultCtorCallsInitializer(self):
        source = "class Foo"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        thisType = ClassType(clas)
        ctor = clas.constructors[0]
        init = package.findFunction(name="$initializer")
        self.assertEquals(self.makeSimpleFunction("$constructor", UnitType, [[
                              ldlocal(0),
                              callg(1, getRootClass().constructors[0].id),
                              drop(),
                              ldlocal(0),
                              callg(1, init.id),
                              drop(),
                              unit(),
                              ret()]],
                            parameterTypes=[thisType],
                            variables=[Variable("$this", thisType, PARAMETER, frozenset())]),
                          ctor)

    def testPrimaryAndSecondaryCtorsCallInitializer(self):
        source = "class Foo(x: i32)\n" + \
                 "  def this(y: f32) = {}"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        thisType = ClassType(clas)
        init = package.findFunction(name="$initializer")
        for ctor in clas.constructors:
            self.assertEquals(callg(1, init.id), ctor.blocks[0].instructions[4])

    def testNullaryCtor(self):
        source = "class Foo\n" + \
                 "  def this = {}\n" + \
                 "def f = Foo\n"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        objType = ClassType(clas, ())
        self.checkFunction(package,
                           self.makeSimpleFunction("f", objType, [[
                               allocobj(clas.id),
                               dup(),
                               callg(1, clas.constructors[0].id),
                               drop(),
                               ret(),
                             ]]))

    def testNullaryCtorForEffect(self):
        source = "class Foo\n" + \
                 "  def this = {}\n" + \
                 "def f =\n" + \
                 "  Foo\n" + \
                 "  12"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        objType = ClassType(clas, ())
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               allocobj(clas.id),
                               callg(1, clas.constructors[0].id),
                               drop(),
                               i64(12),
                               ret()]]))

    def testPrimaryUnaryCtor(self):
        source = "class Foo(x: i32)"
        package = self.compileFromSource(source)
        clas = package.findClass(name="Foo")
        init = package.findFunction(name="$initializer")
        ctor = clas.constructors[0]
        thisType = ClassType(clas)
        self.assertEquals(self.makeSimpleFunction("$constructor", UnitType, [[
                              ldlocal(WORDSIZE),
                              callg(1, getRootClass().constructors[0].id),
                              drop(),
                              ldlocal(WORDSIZE),
                              callg(1, init.id),
                              drop(),
                              ldlocal(0),
                              ldlocal(WORDSIZE),
                              st32(WORDSIZE),
                              unit(),
                              ret()]],
                            parameterTypes=[thisType, I32Type],
                            variables=[Variable("$this", thisType, PARAMETER, frozenset())]),
                          ctor)

    def testCtorWithArgs(self):
        source = "class Foo\n" + \
                 "  def this(x: i64, y: i64) = {}\n" + \
                 "def f = Foo(1, 2)\n"
        package = self.compileFromSource(source)
        clas = package.classes[0]
        objType = ClassType(clas, ())
        self.checkFunction(package,
                           self.makeSimpleFunction("f", objType, [[
                               allocobj(clas.id),
                               dup(),
                               i64(1),
                               i64(2),
                               callg(3, clas.constructors[0].id),
                               drop(),
                               ret(),
                             ]]))

    def testNullaryMethod(self):
        source = "class Foo\n" + \
                 "  def get = 12\n" + \
                 "def f(foo: Foo) = foo.get"
        package = self.compileFromSource(source)
        clas = package.classes[0]
        method = clas.methods[0]
        objType = ClassType(clas, ())
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               callv(1, 3),
                               ret()
                             ]],
                             variables=[Variable("foo", objType, PARAMETER, frozenset())]))

    def testCaptureThis(self):
        source = "class Foo\n" + \
                 "  var x: i64\n" + \
                 "  def f =\n" + \
                 "    def g = this.x"
        package = self.compileFromSource(source)
        closureClass = package.findClass(name="$closure")
        closureType = ClassType(closureClass)
        self.checkFunction(package,
                           self.makeSimpleFunction("g", I64Type, [[
                               ldlocal(0),
                               ldpc(WORDSIZE),
                               ldpc(WORDSIZE),
                               ld64(WORDSIZE),
                               ret()
                             ]],
                             variables=[Variable("$this", closureType, PARAMETER, frozenset())],
                             parameterTypes=[closureType]))

    def testCallAlternateCtor(self):
        source = "class Foo(a: i64)\n" + \
                 "  def this = this(12)"
        package = self.compileFromSource(source)
        clas = package.findClass(name="Foo")
        ty = ClassType(clas)
        expected = self.makeSimpleFunction("$constructor", UnitType, [[
                                               ldlocal(0),
                                               i64(12),
                                               callg(2, clas.constructors[0].id),
                                               drop(),
                                               unit(),
                                               ret()]],
                                             variables=[Variable("$this", ty, PARAMETER, frozenset())],
                                             parameterTypes=[ty])
        self.assertEquals(expected, clas.constructors[1])

    def testCallAlternateCtorLater(self):
        source = "class Foo(a: i64)\n" + \
                 "  def this =\n" + \
                 "    var x = 12\n" + \
                 "    this(x)"
        self.assertRaises(CompileException, self.compileFromSource, source)

    def testCallSuperCtor(self):
        source = "class Foo(a: i64)\n" + \
                 "class Bar <: Foo\n" + \
                 "  def this = super(12)"
        package = self.compileFromSource(source)
        foo = package.findClass(name="Foo")
        bar = package.findClass(name="Bar")
        barTy = ClassType(bar)
        expected = self.makeSimpleFunction("$constructor", UnitType, [[
                                               ldlocal(0),
                                               i64(12),
                                               callg(2, foo.constructors[0].id),
                                               drop(),
                                               ldlocal(0),
                                               callg(1, bar.initializer.id),
                                               drop(),
                                               unit(),
                                               ret()]],
                                             variables=[Variable("$this", barTy, PARAMETER, frozenset())],
                                             parameterTypes=[barTy])
        self.assertEquals(expected, bar.constructors[0])

    def testCallDefaultSuperCtor(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo"
        package = self.compileFromSource(source)
        foo = package.findClass(name="Foo")
        bar = package.findClass(name="Bar")
        barTy = ClassType(bar)
        expected = self.makeSimpleFunction("$constructor", UnitType, [[
                                               ldlocal(0),
                                               callg(1, foo.constructors[0].id),
                                               drop(),
                                               ldlocal(0),
                                               callg(1, bar.initializer.id),
                                               drop(),
                                               unit(),
                                               ret()]],
                                             variables=[Variable("$this", barTy, PARAMETER, frozenset())],
                                             parameterTypes=[barTy])

    def testNoDefaultSuperCtor(self):
        source = "class Foo(x: i64)\n" + \
                 "class Bar <: Foo"
        self.assertRaises(CompileException, self.compileFromSource, source)

    def testCallBuiltinFunction(self):
        source = "def f = print(\"foo\")"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", UnitType, [[
                               ir_instructions.string(0),
                               callg(1, BUILTIN_PRINT_FUNCTION_ID),
                               ret()]]))

    def testCallBuiltinPrimitiveMethod(self):
        source = "def f = {}.to-string"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", getStringType(), [[
                               unit(),
                               callg(1, BUILTIN_UNIT_TO_STRING_ID),
                               ret()]]))

    def testBlockOrdering(self):
        sys.setrecursionlimit(2000)
        source = "def f =\n" + \
                 "  if (true)\n" + \
                 "    if (false)\n" + \
                 "      1\n" + \
                 "    else\n" + \
                 "      2\n" + \
                 "  else\n" + \
                 "    if (false)\n" + \
                 "      3\n" + \
                 "    else\n" + \
                 "      4\n"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               true(),
                               branchif(1, 5),
                             ], [
                               false(),
                               branchif(2, 3),
                             ], [
                               i64(1),
                               branch(4),
                             ], [
                               i64(2),
                               branch(4),
                             ], [
                               branch(9),
                             ], [
                               false(),
                               branchif(6, 7),
                             ], [
                               i64(3),
                               branch(8),
                             ], [
                               i64(4),
                               branch(8),
                             ], [
                               branch(9),
                             ], [
                               ret()]]))

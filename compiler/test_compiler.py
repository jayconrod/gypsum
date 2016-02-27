# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest
import sys

import ast
from lexer import *
from layout import layout
from parser import *
from compile_info import *
from scope_analysis import *
from type_analysis import *
from externalization import externalize
from compiler import *
from ids import *
from ir import *
from ir_types import *
import ir_instructions
from ir_instructions import *
from bytecode import *
from flags import LET, PUBLIC, METHOD
from compile_info import CompileInfo
from builtins import *
from errors import *
from utils_test import FakePackageLoader, OPTION_SOURCE, TestCaseWithDefinitions, TUPLE_SOURCE


class TestCompiler(TestCaseWithDefinitions):
    def __init__(self, *args):
        super(TestCompiler, self).__init__(*args)
        sys.setrecursionlimit(10000)

    def compileFromSource(self, source, name=None, packageNames=None, packageLoader=None):
        assert packageNames is None or packageLoader is None
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        if name is None:
            name = Name(["test"])
        if packageNames is None:
            packageNames = []
        if packageLoader is None:
            packageNameFromString = lambda s: Name.fromString(s, isPackageName=True)
            packageLoader = FakePackageLoader(map(packageNameFromString, packageNames))
        package = Package(id=TARGET_PACKAGE_ID, name=name)
        info = CompileInfo(ast, package, packageLoader, isUsingStd=False)
        analyzeDeclarations(info)
        analyzeInheritance(info)
        analyzeTypes(info)
        convertClosures(info)
        flattenClasses(info)
        externalize(info)
        compile(info)
        return info.package

    def makePackage(self, input):
        if isinstance(input, Package):
            return input
        elif isinstance(input, ast.Module):
            scopeAnalysis(input)
            return compile(input)
        elif type(input) is str:
            return self.compileFromSource(input)

    def makeSimpleFunction(self, name, retTy, blocks,
                           typeParameters=None, parameterTypes=None,
                           variables=None, flags=None):
        blocks = [BasicBlock(i, insts) for i, insts in enumerate(blocks)]
        params = {"returnType": retTy, "blocks": blocks}
        if typeParameters is not None:
            params["typeParameters"] = typeParameters
        if parameterTypes is not None:
            params["parameterTypes"] = parameterTypes
        if variables is not None:
            params["variables"] = variables
        if flags is not None:
            params["flags"] = flags
        return self.makeFunction(name, **params)

    def checkFunction(self, input, expected):
        package = self.makePackage(input)
        function = package.findFunction(name=expected.name)
        self.assertEquals(expected, function)

    def testEmptyGlobalInit(self):
        self.checkFunction("",
                           self.makeSimpleFunction(PACKAGE_INIT_NAME, UnitType, [[
                               unit(),
                               ret()]]))

    def testDontInitGlobalsWithoutValue(self):
        source = "var o: Object\n" + \
                 "var i: i32"
        self.checkFunction(source,
                           self.makeSimpleFunction(PACKAGE_INIT_NAME, UnitType, [[
                               unit(),
                               ret()]]))

    def testInitGlobal(self):
        package = self.compileFromSource("let x = 42")
        x = package.findGlobal(name="x")
        self.checkFunction(package,
                           self.makeSimpleFunction(PACKAGE_INIT_NAME, UnitType, [[
                               i64(42),
                               stg(x),
                               unit(),
                               ret()]]))

    def testBasicFunction(self):
        self.checkFunction("def f = 12",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               ret()]]))

    def testUnsignedIntLiteral(self):
        self.checkFunction("def f = 0x80i8",
                           self.makeSimpleFunction("f", I8Type, [[
                               i8(-128),
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
                               stlocal(-1),
                               unit(),
                               ret()]],
                             variables=[self.makeVariable("f.x", type=I64Type)]))

    def testSimpleVar(self):
        self.checkFunction("def f = { var x = 12; x; }",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               stlocal(-1),
                               ldlocal(-1),
                               ret()]],
                             variables=[self.makeVariable("f.x", type=I64Type)]))

    def testVarWithSimpleCast(self):
        package = self.compileFromSource("def f = { var x: Object = \"foo\"; x; }")
        fooIndex = package.findString("foo")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", getRootClassType(), [[
                               string(fooIndex),
                               tycs(getRootClass()),
                               cast(),
                               stlocal(-1),
                               ldlocal(-1),
                               ret()]],
                            variables=[self.makeVariable("f.x", type=getRootClassType())]))

    def testVarWithTypeParameterCast(self):
        source = "class Foo[static +T]\n" + \
                 "def f(x: Foo[String]) = { var y: Foo[Object] = x; y; }"
        package = self.compileFromSource(source)
        Foo = package.findClass(name="Foo")
        xType = ClassType(Foo, (getStringType(),))
        yType = ClassType(Foo, (getRootClassType(),))
        expected = self.makeSimpleFunction("f", yType, [[
            ldlocal(0),
            tycs(getRootClass()),
            tycs(Foo),
            cast(),
            stlocal(-1),
            ldlocal(-1),
            ret()]],
          parameterTypes=[xType],
          variables=[self.makeVariable("f.x", type=xType, kind=PARAMETER, flags=frozenset([LET])),
                     self.makeVariable("f.y", type=yType)])
        self.assertEquals(expected, package.findFunction(name="f"))

    def testBlankVar(self):
        self.checkFunction("def f = { var _ = 12; {}; }",
                           self.makeSimpleFunction("f", UnitType, [[
                               i64(12),
                               drop(),
                               unit(),
                               ret(),
                             ]]))

    def testLoadVariablePackage(self):
        source = "def f = foo"
        foo = Package(name=Name(["foo"]))
        package = self.compileFromSource(source, packageLoader=FakePackageLoader([foo]))
        packageType = getPackageType()
        self.checkFunction(package,
                           self.makeSimpleFunction("f", packageType, [[
                               pkg(foo),
                               ret()]]))

    def testLoadPropertyPackage(self):
        source = "def f = foo.bar"
        fooBar = Package(name=Name(["foo", "bar"]))
        package = self.compileFromSource(source, packageLoader=FakePackageLoader([fooBar]))
        packageType = getPackageType()
        self.checkFunction(package,
                           self.makeSimpleFunction("f", packageType, [[
                               pkg(fooBar),
                               ret()]]))

    def testLoadGlobal(self):
        source = "let x = 42\n" + \
                 "def f = x"
        package = self.compileFromSource(source)
        x = package.findGlobal(name="x")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldg(x),
                               ret()]]))

    def testStoreGlobal(self):
        source = "var x: i64\n" + \
                 "def f = { x = 12; {}; }"
        package = self.compileFromSource(source)
        x = package.findGlobal(name="x")
        f = package.findFunction(name="f")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", UnitType, [[
                               i64(12),
                               stg(x),
                               unit(),
                               ret()]]))

    def testLoadForeignGlobal(self):
        foo = Package(name=Name(["foo"]))
        self.assertIsNot(foo.id, TARGET_PACKAGE_ID)
        self.assertIsNone(foo.id.index)
        x = foo.addGlobal(Name(["x"]), type=I64Type, flags=frozenset([PUBLIC]))
        y = foo.addGlobal(Name(["y"]), type=I64Type, flags=frozenset([PUBLIC]))
        source = "def f = foo.y"
        package = self.compileFromSource(source, packageLoader=FakePackageLoader([foo]))
        depIndex = foo.id.index
        self.assertIsNotNone(depIndex)
        dep = package.dependencies[depIndex]
        self.assertIs(foo, dep.package)
        self.assertIs(y.id.packageId, foo.id)
        self.assertNotIn(EXTERN, y.flags)
        self.assertIsNot(dep.externGlobals[y.id.externIndex], y)
        externY = dep.externGlobals[y.id.externIndex]
        self.assertIs(externY.id, y.id)
        self.assertIn(EXTERN, externY.flags)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldgf(y),
                               ret()]]))

    def testStoreForeignGlobal(self):
        foo = Package(name=Name(["foo"]))
        x = foo.addGlobal(Name(["x"]), type=I64Type, flags=frozenset([PUBLIC]))
        loader = FakePackageLoader([foo])
        source = "def f =\n" + \
                 "  foo.x = 12\n" + \
                 "  {}"
        package = self.compileFromSource(source, packageLoader=loader)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", UnitType, [[
                               i64(12),
                               stgf(x),
                               unit(),
                               ret()]]))

    def testConstMustBeAssigned(self):
        source = "def f = { let x: i64; }"
        self.assertRaises(SemanticException, self.compileFromSource, source)

    def testSimpleParameter(self):
        self.checkFunction("def f(x: i64) = x",
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               ret()]],
                             variables=[self.makeVariable("f.x", type=I64Type,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testBlankParameter(self):
        self.checkFunction("def f(_: i64) = 0",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(0),
                               ret()]],
                             variables=[self.makeVariable("f.$parameter", type=I64Type,
                                                          kind=PARAMETER, flags=frozenset([]))]))

    def testTupleParameter(self):
        source = "public class Tuple2[static +T1, static +T2](public _1: T1, public _2: T2)\n" + \
                 "def f((x: String, _: String)) = x"
        package = self.compileFromSource(source, name=STD_NAME)
        tupleClass = package.findClass(name="Tuple2")
        tupleType = ClassType(tupleClass, (getStringType(), getStringType()))
        self.checkFunction(package,
                           self.makeSimpleFunction("f", getStringType(), [[
                               ldlocal(0),
                               ldf(0),
                               stlocal(-1),
                               ldlocal(-1),
                               ret()
                             ]],
                             variables=[self.makeVariable(Name(["f", ANON_PARAMETER_SUFFIX]),
                                                          type=tupleType, kind=PARAMETER),
                                        self.makeVariable("f.x", type=getStringType(),
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testSeveralParameters(self):
        self.checkFunction("def f(var a: i32, var b: boolean, var c: boolean, var d: i32) =\n" + \
                           "  a = 1i32\n" + \
                           "  b = true\n" + \
                           "  c = false\n" + \
                           "  d\n",
                           self.makeSimpleFunction("f", I32Type, [[
                               i32(1),
                               stlocal(0),
                               true(),
                               stlocal(1),
                               false(),
                               stlocal(2),
                               ldlocal(3),
                               ret()]],
                             variables=[
                               self.makeVariable("f.a", type=I32Type, kind=PARAMETER),
                               self.makeVariable("f.b", type=BooleanType, kind=PARAMETER),
                               self.makeVariable("f.c", type=BooleanType, kind=PARAMETER),
                               self.makeVariable("f.d", type=I32Type, kind=PARAMETER)]))

    def testAssign(self):
        self.checkFunction("def f =\n" + \
                           "  var x = 12\n" + \
                           "  x = 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               stlocal(-1),
                               i64(34),
                               dup(),
                               stlocal(-1),
                               ret()]],
                             variables=[self.makeVariable("f.x", type=I64Type)]))

    def testAssignConst(self):
        source = "def f =\n" + \
                 "  let x = 12\n" + \
                 "  x = 34"
        self.assertRaises(SemanticException, self.compileFromSource, source)

    def testAssignCapturedConst(self):
        source = "def f(x: i64) =\n" + \
                 "  def g = x = 12"
        self.assertRaises(SemanticException, self.compileFromSource, source)

    def testAssignParam(self):
        source = "def f(x: i64) = {x = 12; {};}"
        self.assertRaises(SemanticException, self.compileFromSource, source)

    def testAssignVarParam(self):
        self.checkFunction("def f(var x: i64) = {x = 12; {};}",
                           self.makeSimpleFunction("f", UnitType, [[
                               i64(12),
                               stlocal(0),
                               unit(),
                               ret()]],
                             parameterTypes=[I64Type],
                             variables=[self.makeVariable("f.x",
                                                          type=I64Type, kind=PARAMETER)]))

    def testAssignVarWithSimpleCast(self):
        package = self.compileFromSource("def f(s: String, var o: Object) = { o = s; {}; }")
        expected = self.makeSimpleFunction("f", UnitType, [[
            ldlocal(0),
            tycs(getRootClass()),
            cast(),
            stlocal(1),
            unit(),
            ret()]],
          parameterTypes=[getStringType(), getRootClassType()],
          variables=[self.makeVariable("f.s", type=getStringType(),
                                       kind=PARAMETER, flags=frozenset([LET])),
                     self.makeVariable("f.o", type=getRootClassType(), kind=PARAMETER)])
        self.assertEquals(expected, package.findFunction(name="f"))

    def testAssignVarWithTypeParameterCast(self):
        source = "class Foo[static +T]\n" + \
                 "def f(x: Foo[String], var y: Foo[Object]) = { y = x; {}; }"
        package = self.compileFromSource(source)
        Foo = package.findClass(name="Foo")
        xType = ClassType(Foo, (getStringType(),))
        yType = ClassType(Foo, (getRootClassType(),))
        expected = self.makeSimpleFunction("f", UnitType, [[
            ldlocal(0),
            tycs(getRootClass()),
            tycs(Foo),
            cast(),
            stlocal(1),
            unit(),
            ret()]],
          parameterTypes=[xType, yType],
          variables=[self.makeVariable("f.x", type=xType, kind=PARAMETER,
                                       flags=frozenset([LET])),
                     self.makeVariable("f.y", type=yType, kind=PARAMETER)])
        self.assertEquals(expected, package.findFunction(name="f"))

    def testAssignShortProps(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var x: boolean\n" +
                                         "  var y: i64\n" +
                                         "def f(foo: Foo) =\n" +
                                         "  foo.x = false\n" +
                                         "  foo.y = 12\n")
        clasTy = ClassType(package.classes[0], ())
        expected = self.makeSimpleFunction("f", I64Type, [[
                       ldlocal(0),
                       false(),
                       swap(),
                       stf(0),
                       ldlocal(0),
                       i64(12),
                       dup(),
                       swap2(),
                       stf(1),
                       ret()]],
                     parameterTypes=[clasTy],
                     variables=[self.makeVariable("f.foo", type=clasTy,
                                                  kind=PARAMETER, flags=frozenset([LET]))])
        self.assertEquals(expected, package.findFunction(name="f"))

    def testAssignPtrField(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var self: Foo\n" +
                                         "  def this =\n" +
                                         "    this.self = this")
        clas = package.findClass(name="Foo")
        clasTy = ClassType(clas)
        init = package.findFunction(name=Name(["Foo", CLASS_INIT_SUFFIX]))
        expected = self.makeSimpleFunction(Name(["Foo", CONSTRUCTOR_SUFFIX]), UnitType, [[
                       ldlocal(0),
                       callg(getRootClass().constructors[0]),
                       drop(),
                       ldlocal(0),
                       callg(init),
                       drop(),
                       ldlocal(0),
                       ldlocal(0),
                       swap(),
                       stf(0),
                       unit(),
                       ret()]],
                     parameterTypes=[clasTy],
                     variables=[self.makeVariable(Name(["Foo", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                                  type=clasTy,
                                                  kind=PARAMETER, flags=frozenset([LET]))],
                     flags=frozenset([METHOD, CONSTRUCTOR]))
        self.assertEquals(expected, clas.constructors[0])

    def testAssignConstField(self):
        source = "class Foo\n" + \
                 "  let x = 12\n" + \
                 "def f(obj: Foo) = obj.x = 34"
        self.assertRaises(SemanticException, self.compileFromSource, source)

    def testAssignLocalConstField(self):
        source = "class Foo\n" + \
                 "  let x = 12\n" + \
                 "  def set = x = 34"
        self.assertRaises(SemanticException, self.compileFromSource, source)

    def testConstFieldMustBeAssigned(self):
        source = "class Foo\n" + \
                 "  let x: i64"
        self.assertRaises(SemanticException, self.compileFromSource, source)

    def testLoadPtrField(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var self: Foo\n" +
                                         "def f(foo: Foo) =\n" +
                                         "  foo.self\n")
        clasTy = ClassType(package.classes[0], ())
        expected = self.makeSimpleFunction("f", clasTy, [[
                       ldlocal(0),
                       ldf(0),
                       ret()]],
                     parameterTypes=[clasTy],
                     variables=[self.makeVariable("f.foo", type=clasTy,
                                                  kind=PARAMETER, flags=frozenset([LET]))])
        self.assertEquals(expected, package.findFunction(name="f"))

    def testLoadForeignPtrField(self):
        fooPackage = Package(name=Name(["foo"]))
        clas = fooPackage.addClass(Name(["Bar"]), typeParameters=[],
                                   supertypes=[getRootClassType()],
                                   constructors=[], fields=[],
                                   methods=[], flags=frozenset([PUBLIC]))
        field = fooPackage.newField(Name(["Bar", "x"]),
                                    type=getRootClassType(), flags=frozenset([LET, PUBLIC]))
        field.index = 0
        clas.fields.append(field)
        ty = ClassType(clas)
        loader = FakePackageLoader([fooPackage])

        source = "def f(o: foo.Bar) = o.x"
        package = self.compileFromSource(source, packageLoader=loader)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", getRootClassType(), [[
                               ldlocal(0),
                               ldf(0),
                               ret()]],
                             parameterTypes=[ty],
                             variables=[self.makeVariable("f.o", type=ty,
                                        kind=PARAMETER, flags=frozenset([LET]))]))

    def testAccumShortPropForEffect(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var x: i64\n" +
                                         "def f(foo: Foo) =\n" +
                                         "  foo.x += 12\n" +
                                         "  34")
        clasTy = ClassType(package.classes[0], ())
        expected = self.makeSimpleFunction("f", I64Type, [[
                       ldlocal(0),
                       dup(),
                       ldf(0),
                       i64(12),
                       addi64(),
                       swap(),
                       stf(0),
                       i64(34),
                       ret()]],
                     parameterTypes=[clasTy],
                     variables=[self.makeVariable("f.foo", type=clasTy,
                                                  kind=PARAMETER, flags=frozenset([LET]))])
        self.assertEquals(expected, package.findFunction(name="f"))

    def testAccumShortPropForValue(self):
        package = self.compileFromSource("class Foo\n" +
                                         "  var x: i64\n" +
                                         "def f(foo: Foo) =\n" +
                                         "  foo.x += 12\n")
        clasTy = ClassType(package.classes[0], ())
        expected = self.makeSimpleFunction("f", I64Type, [[
                       ldlocal(0),
                       dup(),
                       ldf(0),
                       i64(12),
                       addi64(),
                       dup(),
                       swap2(),
                       stf(0),
                       ret()]],
                     parameterTypes=[clasTy],
                     variables=[self.makeVariable("f.foo", type=clasTy,
                                                  kind=PARAMETER, flags=frozenset([LET]))])
        self.assertEquals(expected, package.findFunction(name="f"))

    def testLoadNonNullableObject(self):
        source = "class Foo\n" + \
                 "  def this = { this.x = this; }\n" + \
                 "  var x: Object\n" + \
                 "def f = Foo().x"
        package = self.compileFromSource(source)
        Foo = package.findClass(name="Foo")
        ty = getRootClassType()
        self.checkFunction(package,
                           self.makeSimpleFunction("f", ty, [[
                               allocobj(Foo),
                               dup(),
                               callg(Foo.constructors[0]),
                               drop(),
                               ldf(0),
                               ret()]]))

    def testLoadNullableObject(self):
        source = "class Foo\n" + \
                 "  def this = {}\n" + \
                 "  var x: Object?\n" + \
                 " def f = Foo().x"
        package = self.compileFromSource(source)
        Foo = package.findClass(name="Foo")
        ty = ClassType(getRootClass(), (), NULLABLE_TYPE_FLAG)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", ty, [[
                               allocobj(Foo),
                               dup(),
                               callg(Foo.constructors[0]),
                               drop(),
                               ldf(0),
                               ret()]]))

    def testNullableEq(self):
        ty = ClassType(getRootClass(), (), NULLABLE_TYPE_FLAG)
        self.checkFunction("def f(foo: Object?, bar: Object?) = foo === bar",
                           self.makeSimpleFunction("f", BooleanType, [[
                               ldlocal(0),
                               ldlocal(1),
                               eqp(),
                               ret()]],
                             variables=[self.makeVariable("f.foo", type=ty,
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("f.bar", type=ty,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testNullableNe(self):
        ty = ClassType(getRootClass(), (), NULLABLE_TYPE_FLAG)
        self.checkFunction("def f(foo: Object?, bar: Object?) = foo !== bar",
                           self.makeSimpleFunction("f", BooleanType, [[
                               ldlocal(0),
                               ldlocal(1),
                               nep(),
                               ret()]],
                             variables=[self.makeVariable("f.foo", type=ty,
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("f.bar", type=ty,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testTruncI32(self):
        self.checkFunction("def f(n: i64) = n.to-i32",
                           self.makeSimpleFunction("f", I32Type, [[
                               ldlocal(0),
                               trunci32(),
                               ret()]],
                             variables=[self.makeVariable("f.n", type=I64Type,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testSext16(self):
        self.checkFunction("def f(n: i8) = n.to-i16",
                           self.makeSimpleFunction("f", I16Type, [[
                               ldlocal(0),
                               sexti16_8(),
                               ret()]],
                             variables=[self.makeVariable("f.n", type=I8Type,
                                                          kind=PARAMETER,
                                                          flags=frozenset([LET]))]))

    def testF32ToI64(self):
        self.checkFunction("def f(n: f32) = n.to-i64",
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               fcvti32(),
                               sexti64_32(),
                               ret()]],
                             variables=[self.makeVariable("f.n", type=F32Type,
                                                          kind=PARAMETER,
                                                          flags=frozenset([LET]))]))

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
                               stlocal(-1),
                               ldlocal(-1),
                               i64(34),
                               addi64(),
                               dup(),
                               stlocal(-1),
                               ret()]],
                             variables=[self.makeVariable("f.x", type=I64Type)]))

    def testAddFloat(self):
        self.checkFunction("def f = 1.2 + 3.4",
                           self.makeSimpleFunction("f", F64Type, [[
                               f64(1.2),
                               f64(3.4),
                               addf64(),
                               ret()]]))

    def testConcatStrings(self):
        package = self.compileFromSource("def f = \"foo\" + \"bar\"")
        stringClass = getStringClass()
        concatMethod = stringClass.findMethodByShortName("+")
        concatMethodIndex = stringClass.getMethodIndex(concatMethod)
        fooIndex = package.findString("foo")
        barIndex = package.findString("bar")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", getStringType(), [[
                               string(fooIndex),
                               string(barIndex),
                               callv(2, concatMethodIndex),
                               ret()]]))

    def testCompareStrings(self):
        package = self.compileFromSource("def f = \"foo\" == \"bar\"")
        stringClass = getStringClass()
        eqMethod = stringClass.findMethodByShortName("==")
        eqMethodIndex = stringClass.getMethodIndex(eqMethod)
        fooIndex = package.findString("foo")
        barIndex = package.findString("bar")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", BooleanType, [[
                               string(fooIndex),
                               string(barIndex),
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

    def testOverloadedUnaryOperatorFunction(self):
        source = "def ~ (x: String) = \"foo\"\n" + \
                 "def f = ~\"bar\""
        package = self.compileFromSource(source)
        Tilde = package.findFunction(name="~")
        barIndex = package.findString("bar")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", getStringType(), [[
                               string(barIndex),
                               callg(Tilde),
                               ret(),
                             ]]))

    def testOverloadedUnaryOperatorConstructor(self):
        source = "class ~ (x: String)\n" + \
                 "def f = ~\"foo\""
        package = self.compileFromSource(source)
        Tilde = package.findClass(name="~")
        fooIndex = package.findString("foo")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", ClassType(Tilde), [[
                               allocobj(Tilde),
                               dup(),
                               string(fooIndex),
                               callg(Tilde.constructors[0]),
                               drop(),
                               ret(),
                             ]]))

    def testOverloadedBinaryOperatorFunction(self):
        source = "def @ (x: i64, y: String) = x.to-string + \"@\" + y\n" + \
                 "def f = 12 @ \"foo\""
        package = self.compileFromSource(source)
        At = package.findFunction(name="@")
        fooIndex = package.findString("foo")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", getStringType(), [[
                               i64(12),
                               string(fooIndex),
                               callg(At),
                               ret(),
                           ]]))

    def testOverloadedBinaryOperatorConstructor(self):
        source = "class @ (x: i64, y: String)\n" + \
                 "def f = 12 @ \"foo\""
        package = self.compileFromSource(source)
        At = package.findClass(name="@")
        fooIndex = package.findString("foo")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", ClassType(At), [[
                               allocobj(At),
                               dup(),
                               i64(12),
                               string(fooIndex),
                               callg(At.constructors[0]),
                               drop(),
                               ret(),
                             ]]))

    def testTupleExpr(self):
        source = "public class Tuple2[static +T1, static +T2](public _1: T1, public _2: T2)\n" + \
                 "def f = (\"foo\", \"bar\")._1"
        package = self.compileFromSource(source, name=STD_NAME)
        stringClass = getStringClass()
        tupleClass = package.findClass(name="Tuple2")
        fooIndex = package.findString("foo")
        barIndex = package.findString("bar")
        expected = self.makeSimpleFunction("f", getStringType(), [[
                       tycs(stringClass),
                       tycs(stringClass),
                       allocobj(tupleClass),
                       dup(),
                       string(fooIndex),
                       string(barIndex),
                       tycs(getStringClass()),
                       tycs(getStringClass()),
                       callg(tupleClass.constructors[0]),
                       drop(),
                       ldf(0),
                       ret()]])
        self.assertEquals(expected, package.findFunction(name="f"))

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

    def testMatchExprWithIntVar(self):
        self.checkFunction("def f = match (12) { case y => y; }",
                           self.makeSimpleFunction("f", I64Type, [[
                             i64(12),
                             stlocal(-1),
                             ldlocal(-1),
                             branch(1),
                           ], [
                             ret(),
                           ]],
                           variables=[self.makeVariable("f.y", type=I64Type,
                                                        kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchExprWithVarWithBlankType(self):
        source = "class Foo[static T]\n" + \
                 "def f(x: Object) =\n" + \
                 "  match (x)\n" + \
                 "    case y: Foo[_] => 12\n" + \
                 "    case _ => 34"
        package = self.compileFromSource(source)
        Foo = package.findClass(name="Foo")
        X = package.findTypeParameter(pred=lambda tp: STATIC not in tp.flags)
        yType = ExistentialType((X,), ClassType(Foo, (VariableType(X),)))
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               tyvd(X),
                               tyvd(X),
                               tycd(Foo),
                               tyxd(1),
                               castcbr(1, 2),
                             ], [
                               stlocal(-1),
                               i64(12),
                               branch(3),
                             ], [
                               drop(),
                               i64(34),
                               branch(3),
                             ], [
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.x", type=getRootClassType(),
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("f.y", type=yType,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchExprWithVarWithExistentialType(self):
        source = "class Foo[static T]\n" + \
                 "def f(x: Object) =\n" + \
                 "  match (x)\n" + \
                 "    case y: forsome [X] Foo[X] => 12\n" + \
                 "    case _ => 34"
        package = self.compileFromSource(source)
        Foo = package.findClass(name="Foo")
        X = package.findTypeParameter(name=Name(["f", EXISTENTIAL_SUFFIX, "X"]))
        yType = ExistentialType((X,), ClassType(Foo, (VariableType(X),)))
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               tyvd(X),
                               tyvd(X),
                               tycd(Foo),
                               tyxd(1),
                               castcbr(1, 2),
                             ], [
                               stlocal(-1),
                               i64(12),
                               branch(3),
                             ], [
                               drop(),
                               i64(34),
                               branch(3),
                             ], [
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.x", type=getRootClassType(),
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("f.y", type=yType,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchExprWithVarWithExistentialForeignTypeArg(self):
        foo = Package(name=Name(["foo"]))
        T = foo.addTypeParameter(Name(["Foo", "T"]), upperBound=getRootClassType(),
                                 lowerBound=getNothingClassType(), flags=frozenset([PUBLIC]))
        Foo = foo.addClass(Name(["Foo"]), typeParameters=[T], supertypes=[getRootClassType()],
                           constructors=[], fields=[],
                           methods=[], flags=frozenset([PUBLIC]))
        loader = FakePackageLoader([foo])

        source = "def f(x: Object) =\n" + \
                 "  match (x)\n" + \
                 "    case y: forsome [X] foo.Foo[X] => 12\n" + \
                 "    case _ => 34"
        package = self.compileFromSource(source, packageLoader=loader)
        X = package.findTypeParameter(name=Name(["f", EXISTENTIAL_SUFFIX, "X"]))
        yType = ExistentialType((X,), ClassType(Foo, (VariableType(X),)))
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               tyvd(X),
                               tyvd(X),
                               tycdf(Foo),
                               tyxd(1),
                               castcbr(1, 2),
                             ], [
                               stlocal(-1),
                               i64(12),
                               branch(3),
                             ], [
                               drop(),
                               i64(34),
                               branch(3),
                             ], [
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.x", type=getRootClassType(),
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("f.y", type=yType,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchExprWithIntShadow(self):
        source = "def f(x: i64) =\n" + \
                 "  let y = 12\n" + \
                 "  match (x)\n" + \
                 "    case y => y\n" + \
                 "    case z => z"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               stlocal(-1),
                               ldlocal(0),
                               ldlocal(-1),
                               dupi(1),
                               eqi64(),
                               branchif(1, 2),
                             ], [
                               drop(),
                               ldlocal(-1),
                               branch(3),
                             ], [
                               stlocal(-2),
                               ldlocal(-2),
                               branch(3),
                             ], [
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.x", type=I64Type,
                                                          kind=PARAMETER,
                                                          flags=frozenset([LET])),
                                        self.makeVariable("f.y", type=I64Type,
                                                          kind=LOCAL, flags=frozenset([LET])),
                                        self.makeVariable("f.z", type=I64Type,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchExprWithObjectShadow(self):
        source = "def f(x: Object) =\n" + \
                 "  let y = \"foo\"\n" + \
                 "  match (x)\n" + \
                 "    case y => y\n" + \
                 "    case z => z"
        package = self.compileFromSource(source)
        fooIndex = package.findString("foo")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", getRootClassType(), [[
                               string(fooIndex),
                               stlocal(-1),
                               ldlocal(0),
                               ldlocal(-1),
                               dupi(1),
                               eqp(),
                               branchif(1, 2),
                             ], [
                               drop(),
                               ldlocal(-1),
                               branch(3),
                             ], [
                               stlocal(-2),
                               ldlocal(-2),
                               branch(3),
                             ], [
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.x", type=getRootClassType(),
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("f.y", type=getStringType(),
                                                          kind=LOCAL, flags=frozenset([LET])),
                                        self.makeVariable("f.z", type=getRootClassType(),
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchExprWithBlankPattern(self):
        self.checkFunction("def f = match (12) { case _ => 34; }",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               drop(),
                               i64(34),
                               branch(1),
                             ], [
                               ret(),
                             ]]))

    def testMatchExprWithBlankPatternSubtype(self):
        source = "def f(o: Object) =\n" + \
                 "  match (o)\n" + \
                 "    case _: String => 1\n" + \
                 "    case _ => 2"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               tycd(getStringClass()),
                               castcbr(1, 2),
                             ], [
                               drop(),
                               i64(1),
                               branch(3),
                             ], [
                               drop(),
                               i64(2),
                               branch(3)
                             ], [
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.o", type=getRootClassType(),
                                                          kind=PARAMETER,
                                                          flags=frozenset([LET]))]))

    def testMatchExprWithBlankPatternSupertype(self):
        source = "def f(s: String) =\n" + \
                 "  match (s)\n" + \
                 "    case _: Object => 1"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               drop(),
                               i64(1),
                               branch(1),
                             ], [
                               ret()
                             ]],
                             variables=[self.makeVariable("f.s", type=getStringType(),
                                                          kind=PARAMETER,
                                                          flags=frozenset([LET]))]))
    def testMatchExprWithIntLiteral(self):
        self.checkFunction("def f = match (12) { case 34 => 56; case _ => 78; }",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               dup(),
                               i64(34),
                               eqi64(),
                               branchif(1, 2),
                             ], [
                               drop(),
                               i64(56),
                               branch(3),
                             ], [
                               drop(),
                               i64(78),
                               branch(3),
                             ], [
                               ret(),
                             ]]))

    def testMatchExprWithStringLiteral(self):
        source = "def f = match (\"foo\") { case \"bar\" => 12; case _ => 34; }"
        package = self.compileFromSource(source)
        fooIndex = package.findString("foo")
        barIndex = package.findString("bar")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               string(fooIndex),
                               string(barIndex),
                               dupi(1),
                               callg(getBuiltinFunctionById(BUILTIN_STRING_EQ_OP_ID)),
                               branchif(1, 2),
                             ], [
                               drop(),
                               i64(12),
                               branch(3),
                             ], [
                               drop(),
                               i64(34),
                               branch(3),
                             ], [
                               ret(),
                             ]]))

    def testMatchExprWithNullLiteral(self):
        xType = ClassType(getRootClass(), (), NULLABLE_TYPE_FLAG)
        self.checkFunction("def f(x: Object?) = match (x) { case null => 0; case _ => 1; }",
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               dup(),
                               null(),
                               eqp(),
                               branchif(1, 2),
                             ], [
                               drop(),
                               i64(0),
                               branch(3),
                             ], [
                               drop(),
                               i64(1),
                               branch(3),
                             ], [
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.x", type=xType,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testMatchExprWithTuple(self):
        pass
        # source = "public class Tuple2[static +T1, static +T2](public _1: T1, public _2: T2)\n" + \
        #          "def f(x: Object) =\n" + \
        #          "  match (x)\n" + \
        #          "    case (y: String, _) => 12\n" + \
        #          "    case _ => 34"
        # package = self.compileFromSource(source, name=STD_NAME)
        # tupleClass = package.findClass(name="Tuple2")
        # T1 = tupleClass.typeParameters[0]
        # T2 = tupleClass.typeParameters[0]
        # self.checkFunction(package,
        #                    self.makeSimpleFunction("f", I64Type, [[
        #                        ldlocal(0),
        #                        tyvd(T1),
        #                        tyvd(T2),
        #                        tycd(tupleClass),
        #                        castcbr(1, 2),
        #                      ], [
        #                        ldf(0),
        #                        tycd(BUILTIN_STRING_CLASS_ID.index),


    def testMatchExprWithValuePrimitive(self):
        foo = Package(name=Name(["foo"]))
        bar = foo.addGlobal(Name(["bar"]), type=I64Type, flags=frozenset([PUBLIC, LET]))
        loader = FakePackageLoader([foo])

        source = "def f(x: i64) =\n" + \
                 "  match (x)\n" + \
                 "    case foo.bar => 12\n" + \
                 "    case _ => 34"
        package = self.compileFromSource(source, packageLoader=loader)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               dup(),
                               ldgf(bar),
                               eqi64(),
                               branchif(1, 2),
                             ], [
                               drop(),
                               i64(12),
                               branch(3),
                             ], [
                               drop(),
                               i64(34),
                               branch(3),
                             ], [
                               ret()
                             ]],
                             variables=[self.makeVariable("f.x", type=I64Type, kind=PARAMETER,
                                                          flags=frozenset([LET]))]))

    def testMatchExprWithValueObject(self):
        stringType = getStringType()
        stringClass = stringType.clas
        foo = Package(name=Name(["foo"]))
        bar = foo.addGlobal(Name(["bar"]), type=stringType, flags=frozenset([PUBLIC, LET]))
        loader = FakePackageLoader([foo])

        source = "def f(x: String) =\n" + \
                 "  match (x)\n" + \
                 "    case foo.bar => 12\n" + \
                 "    case _ => 34"
        package = self.compileFromSource(source, packageLoader=loader)
        eqMethod = stringClass.getMethod("==")
        eqMethodIndex = stringClass.getMethodIndex(eqMethod)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               ldgf(bar),
                               dupi(1),
                               callv(2, eqMethodIndex),
                               branchif(1, 2),
                             ], [
                               drop(),
                               i64(12),
                               branch(3),
                             ], [
                               drop(),
                               i64(34),
                               branch(3),
                             ], [
                               ret()
                             ]],
                             variables=[self.makeVariable("f.x", type=stringType,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testMatchExprDestructureSome(self):
        source = OPTION_SOURCE + \
                 "def f(obj: Object) =\n" + \
                 "  match (obj)\n" + \
                 "    case Some[Object](x: String) => x\n" + \
                 "    case _ => \"no\""
        package = self.compileFromSource(source, name=STD_NAME)
        stringType = getStringType()
        tryMatch = package.findFunction(name="Some.try-match")
        some = package.findClass(name="Some")
        isDefined = some.findMethodByShortName("is-defined")
        isDefinedIndex = some.getMethodIndex(isDefined)
        get = some.findMethodByShortName("get")
        getIndex = some.getMethodIndex(get)
        noIndex = package.findString("no")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", stringType, [[
                               # block 0 []
                               ldlocal(0),
                               dup(),
                               tycs(getRootClass()),
                               callg(tryMatch),
                               dup(),
                               tycs(getRootClass()),
                               callv(1, isDefinedIndex),
                               branchif(1, 3),
                             ], [
                               # block 1 [some value]
                               tycs(getRootClass()),
                               callv(1, getIndex),
                               tycd(getStringClass()),
                               castcbr(2, 3),
                             ], [
                               # block 2 [string value]
                               stlocal(-1),
                               drop(),
                               ldlocal(-1),
                               branch(5),
                             ], [
                               # block 3 [obj value]
                               drop(),
                               branch(4),
                             ], [
                               # block 4 [value]
                               drop(),
                               string(noIndex),
                               branch(5),
                             ], [
                               # block 5 [result]
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.obj", type=getRootClassType(),
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("f.x", type=stringType, kind=LOCAL,
                                                          flags=frozenset([LET]))]))

    def testMatchDestructureSomeTupleFromFunction(self):
        source = TUPLE_SOURCE + \
                 OPTION_SOURCE + \
                 "def Matcher(obj: Object) = Some[(String, String)]((\"foo\", \"bar\"))\n" + \
                 "def f(obj: Object) =\n" + \
                 "  match (obj)\n" + \
                 "    case Matcher(x, y) => 12\n" + \
                 "    case _ => 34"
        package = self.compileFromSource(source, name=STD_NAME)
        Matcher = package.findFunction(name="Matcher")
        Option = package.findClass(name="Option")
        isDefined = Option.findMethodByShortName("is-defined")
        isDefinedIndex = Option.getMethodIndex(isDefined)
        get = Option.findMethodByShortName("get")
        getIndex = Option.getMethodIndex(get)
        Tuple2 = package.findClass(name="Tuple2")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 []
                               ldlocal(0),
                               dup(),
                               callg(Matcher),
                               dup(),
                               tycs(getStringClass()),
                               tycs(getStringClass()),
                               tycs(Tuple2),
                               callv(1, isDefinedIndex),
                               branchif(1, 2),
                             ], [
                               # block 1 [some value]
                               tycs(getStringClass()),
                               tycs(getStringClass()),
                               tycs(Tuple2),
                               callv(1, getIndex),
                               dup(),
                               ldf(0),
                               stlocal(-1),
                               ldf(1),
                               stlocal(-2),
                               drop(),
                               i64(12),
                               branch(4),
                             ], [
                               # block 2 [none value]
                               drop(),
                               branch(3),
                             ], [
                               # block 3 [value]
                               drop(),
                               i64(34),
                               branch(4),
                             ], [
                               # block 4 [result]
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.obj", type=getRootClassType(),
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("f.x", type=getStringType(),
                                                          kind=LOCAL, flags=frozenset([LET])),
                                        self.makeVariable("f.y", type=getStringType(),
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchExprDestructureFromMethod(self):
        source = OPTION_SOURCE + \
                 "class Foo\n" + \
                 "  def Matcher(obj: Object): Option[String] = None\n" + \
                 "  def f(obj: Object) =\n" + \
                 "    match (obj)\n" + \
                 "      case Matcher(x) => 12\n" + \
                 "      case _ => 34"
        package = self.compileFromSource(source, name=STD_NAME)
        Foo = package.findClass(name="Foo")
        Matcher = Foo.findMethodByShortName("Matcher")
        MatcherIndex = Foo.getMethodIndex(Matcher)
        Some = package.findClass(name="Some")
        isDefined = Some.findMethodByShortName("is-defined")
        isDefinedIndex = Some.getMethodIndex(isDefined)
        get = Some.findMethodByShortName("get")
        getIndex = Some.getMethodIndex(get)
        self.checkFunction(package,
                           self.makeSimpleFunction("Foo.f", I64Type, [[
                               # block 0 []
                               ldlocal(1),
                               ldlocal(0),
                               dupi(1),
                               callv(2, MatcherIndex),
                               dup(),
                               tycs(getStringClass()),
                               callv(1, isDefinedIndex),
                               branchif(1, 2),
                             ], [
                               # block 1 [some value]
                               tycs(getStringClass()),
                               callv(1, getIndex),
                               stlocal(-1),
                               drop(),
                               i64(12),
                               branch(4),
                             ], [
                               # block 2 [none value]
                               drop(),
                               branch(3),
                             ], [
                               # block 3 [value]
                               drop(),
                               i64(34),
                               branch(4),
                             ], [
                               # block 4 [result]
                               ret(),
                             ]],
                             variables=[self.makeVariable(Name(["Foo", "f", RECEIVER_SUFFIX]),
                                                          type=ClassType(Foo),
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("Foo.f.obj", type=getRootClassType(),
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable("Foo.f.x", type=getStringType(),
                                                          kind=LOCAL, flags=frozenset([LET]))],
                             flags=frozenset([METHOD])))

    def testMatchExprUnaryWithFunction(self):
        source = OPTION_SOURCE + \
                 "def ~ (obj: Object) = Some[String](\"foo\")\n" + \
                 "def f(obj: Object) =\n" + \
                 "  match (obj)\n" + \
                 "    case ~s => 12\n" + \
                 "    case _ => 34"
        package = self.compileFromSource(source, name=STD_NAME)
        matcher = package.findFunction(name="~")
        Some = package.findClass(name="Some")
        isDefined = Some.findMethodByShortName("is-defined")
        isDefinedIndex = Some.getMethodIndex(isDefined)
        get = Some.findMethodByShortName("get")
        getIndex = Some.getMethodIndex(get)
        objectType = getRootClassType()
        stringType = getStringType()
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 []
                               ldlocal(0),
                               dup(),
                               callg(matcher),
                               dup(),
                               tycs(getStringClass()),
                               callv(1, isDefinedIndex),
                               branchif(1, 2),
                             ], [
                               # block 1 [some value]
                               tycs(getStringClass()),
                               callv(1, getIndex),
                               stlocal(-1),
                               drop(),
                               i64(12),
                               branch(4),
                             ], [
                               # block 2 [none value]
                               drop(),
                               branch(3),
                             ], [
                               # block 3 [value]
                               drop(),
                               i64(34),
                               branch(4),
                             ], [
                               # block 4 [result]
                               ret(),
                             ]],
                             variables=[self.makeVariable(Name(["f", "obj"]), type=objectType,
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable(Name(["f", "s"]), type=stringType,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchExprBinaryWithStaticMethod(self):
        source = OPTION_SOURCE + \
                 TUPLE_SOURCE + \
                 "class Foo\n" + \
                 "class Bar\n" + \
                 "class ::\n" + \
                 "  static def try-match(obj: Object) =\n" + \
                 "    Some[(Foo, Bar)]((Foo(), Bar()))\n" + \
                 "def f(obj: Object) =\n" + \
                 "  match (obj)\n" + \
                 "    case a :: b => 12\n" + \
                 "    case _ => 34"
        package = self.compileFromSource(source, name=STD_NAME)
        Foo = package.findClass(name="Foo")
        Bar = package.findClass(name="Bar")
        matcher = package.findFunction(name="::.try-match")
        Some = package.findClass(name="Some")
        isDefined = Some.findMethodByShortName("is-defined")
        isDefinedIndex = Some.getMethodIndex(isDefined)
        get = Some.findMethodByShortName("get")
        getIndex = Some.getMethodIndex(get)
        Tuple = package.findClass(name="Tuple2")
        objectType = getRootClassType()
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 []
                               ldlocal(0),
                               dup(),
                               callg(matcher),
                               dup(),
                               tycs(Foo),
                               tycs(Bar),
                               tycs(Tuple),
                               callv(1, isDefinedIndex),
                               branchif(1, 2),
                             ], [
                               # block 1 [some value]
                               tycs(Foo),
                               tycs(Bar),
                               tycs(Tuple),
                               callv(1, getIndex),
                               dup(),
                               ldf(0),
                               stlocal(-1),
                               ldf(1),
                               stlocal(-2),
                               drop(),
                               i64(12),
                               branch(4),
                             ], [
                               # block 2 [none value]
                               drop(),
                               branch(3),
                             ], [
                               # block 3 [value]
                               drop(),
                               i64(34),
                               branch(4),
                             ], [
                               # block 4 [result]
                               ret(),
                             ]],
                             variables=[self.makeVariable(Name(["f", "obj"]), type=objectType,
                                                          kind=PARAMETER, flags=frozenset([LET])),
                                        self.makeVariable(Name(["f", "a"]), type=ClassType(Foo),
                                                          kind=LOCAL, flags=frozenset([LET])),
                                        self.makeVariable(Name(["f", "b"]), type=ClassType(Bar),
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchAllCasesTerminate(self):
        source = "def f =\n" + \
                 "  match (12)\n" + \
                 "    case x if false => return 1\n" + \
                 "    case y if true => return 2\n" + \
                 "  return 3"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               dup(),
                               stlocal(-1),
                               false(),
                               branchif(1, 2),
                             ], [
                               drop(),
                               i64(1),
                               ret(),
                             ], [
                               dup(),
                               stlocal(-2),
                               true(),
                               branchif(3, 4),
                             ], [
                               drop(),
                               i64(2),
                               ret(),
                             ], [
                               allocobj(getBuiltinClassById(BUILTIN_MATCH_EXCEPTION_CLASS_ID)),
                               dup(),
                               callg(getBuiltinFunctionById(BUILTIN_MATCH_EXCEPTION_CTOR_ID)),
                               drop(),
                               throw(),
                             ]],
                             variables=[self.makeVariable("f.x", type=I64Type,
                                                          kind=LOCAL, flags=frozenset([LET])),
                                        self.makeVariable("f.y", type=I64Type,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchUnreachableAfterMustMatchCase(self):
        source = "def f =\n" + \
                 "  match (12)\n" + \
                 "    case x => return 1\n" + \
                 "    case y if false => return 2\n" + \
                 "  return 3"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               stlocal(-1),
                               i64(1),
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.x", type=I64Type,
                                                          kind=LOCAL, flags=frozenset([LET])),
                                        self.makeVariable("f.y", type=I64Type,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testMatchNotUnreachableAfterSomeTermination(self):
        source = "def f =\n" + \
                 "  match (12)\n" + \
                 "    case x if false => return 1\n" + \
                 "    case y => 2\n" + \
                 "  3"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               dup(),
                               stlocal(-1),
                               false(),
                               branchif(1, 2),
                             ], [
                               drop(),
                               i64(1),
                               ret(),
                             ], [
                               stlocal(-2),
                               i64(2),
                               drop(),
                               branch(3),
                             ], [
                               i64(3),
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.x", type=I64Type,
                                                          kind=LOCAL, flags=frozenset([LET])),
                                        self.makeVariable("f.y", type=I64Type,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryValueMustCatch(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = try 12 catch\n" +
                           "    case exn => 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               pushtry(1, 2),
                             ], [
                               i64(12),
                               poptry(3),
                             ], [
                               stlocal(-1),
                               i64(34),
                               branch(3),
                             ], [
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryValueMayCatch(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = try 12 catch { case exn if false => 34; }",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0
                               pushtry(1, 2),
                             ], [
                               # block 1 (try)
                               i64(12),
                               poptry(4),
                             ], [
                               # block 2 (catch)
                               dup(),
                               stlocal(-1),
                               false(),
                               branchif(3, 5),
                             ], [
                               # block 3 (catch case)
                               drop(),
                               i64(34),
                               branch(4),
                             ], [
                               # block 4 (done)
                               ret(),
                             ], [
                               # block 5 (rethrow)
                               throw(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryEffectMustCatch(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = { try 12 catch { case exn => 34; }; {}; }",
                           self.makeSimpleFunction("f", UnitType, [[
                               # block 0
                               pushtry(1, 2),
                             ], [
                               # block 1 (try)
                               i64(12),
                               drop(),
                               poptry(3),
                             ], [
                               # block 2 (catch)
                               stlocal(-1),
                               i64(34),
                               drop(),
                               branch(3),
                             ], [
                               # block 3 (done)
                               unit(),
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryEffectMayCatch(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = { try 12 catch { case exn if false => 34; }; {}; }",
                           self.makeSimpleFunction("f", UnitType, [[
                               # block 0
                               pushtry(1, 2),
                             ], [
                               # block 1 (try)
                               i64(12),
                               drop(),
                               poptry(4),
                             ], [
                               # block 2 (catch)
                               dup(),
                               stlocal(-1),
                               false(),
                               branchif(3, 5),
                             ], [
                               # block 3 (catch case)
                               drop(),
                               i64(34),
                               drop(),
                               branch(4),
                             ], [
                               # block 4 (done)
                               unit(),
                               ret(),
                             ], [
                               # block 5 (rethrow)
                               throw(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryValueFinally(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = try 12 finally 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 3),
                             ], [
                               # block 1 (try) [...]
                               i64(12),
                               poptry(2),
                             ], [
                               # block 2 (try-finally) [value ...]
                               uninitialized(),
                               label(6),
                               branch(4),
                             ], [
                               # block 3 (throw-finally) [exception ...]
                               i64(0),
                               swap(),
                               label(5),
                               branch(4),
                             ], [
                               # block 4 (finally) [continuation exception value ...]
                               i64(34),
                               drop(),
                               branchl(5, 6),
                             ], [
                               # block 5 (finally-rethrow) [exception value ...]
                               throw(),
                             ], [
                               # block 6 (done) [exception value ...]
                               drop(),
                               ret(),
                             ]]))

    def testTryEffectFinally(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = { try 12 finally 34; {}; }",
                           self.makeSimpleFunction("f", UnitType, [[
                               # block 0
                               pushtry(1, 3),
                             ], [
                               # block 1 (try)
                               i64(12),
                               drop(),
                               poptry(2),
                             ], [
                               # block 2 (try-finally)
                               uninitialized(),
                               label(6),
                               branch(4),
                             ], [
                               # block 3 (throw-finally)
                               label(5),
                               branch(4),
                             ], [
                               # block 4 (finally)
                               i64(34),
                               drop(),
                               branchl(5, 6),
                             ], [
                               # block 5 (finally-rethrow)
                               throw(),
                             ], [
                               # block 6 (done)
                               drop(),
                               unit(),
                               ret(),
                             ]]))

    def testTryValueMustMatchFinally(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = try 12 catch\n" +
                           "    case exn => 34\n" +
                           "  finally 56",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 3),
                             ], [
                               # block 1 (try) [...]
                               i64(12),
                               poptry(2),
                             ], [
                               # block 2 (try-finally) [value ...]
                               uninitialized(),
                               label(10),
                               branch(8),
                             ], [
                               # block 3 (catch-try) [exception ...]
                               pushtry(4, 7),
                             ], [
                               # block 4 (catch) [exception ...]
                               dup(),
                               stlocal(-1),
                               i64(34),
                               branch(5),
                             ], [
                               # block 5 (catch-normal) [value exception ...]
                               poptry(6),
                             ], [
                               # block 6 (catch-normal-finally) [value exception ...]
                               swap(),
                               drop(),
                               uninitialized(),
                               label(10),
                               branch(8),
                             ], [
                               # block 7 (catch-throw-finally) [exception exception ...]
                               swap(),
                               drop(),
                               i64(0),
                               swap(),
                               label(9),
                               branch(8),
                             ], [
                               # block 8 (finally) [continuation exception value ...]
                               i64(56),
                               drop(),
                               branchl(9, 10),
                             ], [
                               # block 9 (finally-rethrow) [exception value ...]
                               throw(),
                             ], [
                               # block 10 (done) [exception value ...]
                               drop(),
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryValueMayCatchFinally(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = try 12 catch { case exn if false => 34; } finally 56",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 3),
                             ], [
                               # block 1 (try) [...]
                               i64(12),
                               poptry(2),
                             ], [
                               # block 2 (try-finally) [value ...]
                               uninitialized(),
                               label(12),
                               branch(10),
                             ], [
                               # block 3 (catch-try) [exception ...]
                               pushtry(4, 9),
                             ], [
                               # block 4 (catch) [exception ...]
                               dup(),
                               dup(),
                               stlocal(-1),
                               false(),
                               branchif(5, 8),
                             ], [
                               # block 5 (catch case) [exception exception ...]
                               drop(),
                               i64(34),
                               branch(6),
                             ], [
                               # block 6 (catch-normal) [value exception ...]
                               poptry(7),
                             ], [
                               # block 7 (catch-normal-finally) [value exception ...]
                               swap(),
                               drop(),
                               uninitialized(),
                               label(12),
                               branch(10),
                             ], [
                               # block 8 (catch-miss) [exception exception ...]
                               poptry(9),
                             ], [
                               # block 9 (catch-throw-finally) [exception exception ...]
                               swap(),
                               drop(),
                               i64(0),
                               swap(),
                               label(11),
                               branch(10),
                             ], [
                               # block 10 (finally) [continuation exception value ...]
                               i64(56),
                               drop(),
                               branchl(11, 12),
                             ], [
                               # block 11 (finally-rethrow) [exception value ...]
                               throw(),
                             ], [
                               # block 12 (done) [exception value ...]
                               drop(),
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryEffectMustCatchFinally(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = { try 12 catch { case exn => 34; } finally 56; {}; }",
                           self.makeSimpleFunction("f", UnitType, [[
                               # block 0 [...]
                               pushtry(1, 3),
                             ], [
                               # block 1 (try) [...]
                               i64(12),
                               drop(),
                               poptry(2),
                             ], [
                               # block 2 (try-finally) [...]
                               uninitialized(),
                               label(10),
                               branch(8),
                             ], [
                               # block 3 (catch-try) [exception ...]
                               pushtry(4, 7),
                             ], [
                               # block 4 (catch) [exception ...]
                               dup(),
                               stlocal(-1),
                               i64(34),
                               drop(),
                               branch(5),
                             ], [
                               # block 5 (catch-normal) [exception ...]
                               poptry(6),
                             ], [
                               # block 6 (catch-normal-finally) [exception ...]
                               drop(),
                               uninitialized(),
                               label(10),
                               branch(8),
                             ], [
                               # block 7 (catch-throw-finally) [exception exception ...]
                               swap(),
                               drop(),
                               label(9),
                               branch(8),
                             ], [
                               # block 8 (finally) [continuation exception ...]
                               i64(56),
                               drop(),
                               branchl(9, 10),
                             ], [
                               # block 9 (finally-rethrow) [exception ...]
                               throw(),
                             ], [
                               # block 10 (done) [exception ...]
                               drop(),
                               unit(),
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryEffectMayCatchFinally(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = { try 12 catch { case exn if false => 34; } finally 56; {}; }",
                           self.makeSimpleFunction("f", UnitType, [[
                               # block 0 [...]
                               pushtry(1, 3),
                             ], [
                               # block 1 (try) [...]
                               i64(12),
                               drop(),
                               poptry(2),
                             ], [
                               # block 2 (try-finally) [...]
                               uninitialized(),
                               label(12),
                               branch(10),
                             ], [
                               # block 3 (try-catch) [exception ...]
                               pushtry(4, 9),
                             ], [
                               # block 4 (catch) [exception ...]
                               dup(),
                               dup(),
                               stlocal(-1),
                               false(),
                               branchif(5, 8),
                             ], [
                               # block 5 [exception exception ...]
                               drop(),
                               i64(34),
                               drop(),
                               branch(6),
                             ], [
                               # block 6 (catch-normal) [exception ...]
                               poptry(7),
                             ], [
                               # block 7 (catch-normal-finally) [exception ...]
                               drop(),
                               uninitialized(),
                               label(12),
                               branch(10),
                             ], [
                               # block 8 (catch-miss) [exception exception ...]
                               poptry(9),
                             ], [
                               # block 9 (catch-throw-finally)
                               swap(),
                               drop(),
                               label(11),
                               branch(10),
                             ], [
                               # block 10 (finally)
                               i64(56),
                               drop(),
                               branchl(11, 12),
                             ], [
                               # block 11 (finally-rethrow)
                               throw(),
                             ], [
                               # block 12 (done)
                               drop(),
                               unit(),
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testAssignVarDefinedInCatch(self):
        source = "def f =\n" + \
                 "  try\n" + \
                 "    {}\n" + \
                 "  catch\n" + \
                 "    case x =>\n" + \
                 "      x = Exception()\n" + \
                 "      {}"
        self.assertRaises(SemanticException, self.compileFromSource, source)

    def testThrow(self):
        exnTy = ClassType(getExceptionClass())
        self.checkFunction("def f(exn: Exception) = throw exn",
                           self.makeSimpleFunction("f", getNothingClassType(), [[
                               ldlocal(0),
                               throw(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

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
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testThrowInIfCondition(self):
        exnTy = ClassType(getExceptionClass())
        self.checkFunction("def f(exn: Exception) = if (throw exn) 12 else 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               throw(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

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
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testThrowInTry(self):
        exnClass = getExceptionClass()
        exnTy = ClassType(exnClass)
        self.checkFunction("def f = try throw Exception() catch\n" +
                           "  case exn => 1",
                           self.makeSimpleFunction("f", I64Type, [[
                               pushtry(1, 2),
                             ], [
                               allocobj(getExceptionClass()),
                               dup(),
                               callg(getBuiltinFunctionById(BUILTIN_EXCEPTION_CTOR_ID)),
                               drop(),
                               throw(),
                             ], [
                               stlocal(-1),
                               i64(1),
                               branch(3),
                             ], [
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryFinallyExpr(self):
        self.checkFunction("def f = try 12 finally 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 3),
                             ], [
                               # block 1 (try) [...]
                               i64(12),
                               poptry(2),
                             ], [
                               # block 2 (try-finally) [value ...]
                               uninitialized(),
                               label(6),
                               branch(4),
                             ], [
                               # block 3 (throw-finally) [exception ...]
                               i64(0),
                               swap(),
                               label(5),
                               branch(4),
                             ], [
                               # block 4 (finally) [continuation exception value ...]
                               i64(34),
                               drop(),
                               branchl(5, 6),
                             ], [
                               # block 5 (finally-rethrow) [exception value ...]
                               throw(),
                             ], [
                               # block 6 (done) [exception value ...]
                               drop(),
                               ret(),
                             ]]))

    def testTryFinallyReturn(self):
        source = "var g = 0\n" + \
                 "def f =\n" + \
                 "  try\n" + \
                 "    100 + (200 + return 12)\n" + \
                 "  finally\n" + \
                 "    g = 34\n"
        package = self.compileFromSource(source)
        g = package.findGlobal(name="g")
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 (entry)
                               pushtry(1, 3),
                             ], [
                               # block 1 (try)
                               i64(100),
                               i64(200),
                               dropi(2),
                               i64(12),
                               poptry(2),
                             ], [
                               # block 2 (return finally)
                               i64(0),  # value
                               swap(),
                               uninitialized(),  # exception
                               swap(),
                               label(6),
                               branch(4),
                             ], [
                               # block 3 (catch finally)
                               i64(0),  # value
                               swap(),
                               i64(0),  # return
                               label(5),
                               branch(4),
                             ], [
                               # block 4 (finally)
                               i64(34),
                               stg(g),
                               branchl(5, 6),
                             ], [
                               # block 5 (finally rethrow)
                               drop(),
                               throw(),
                             ], [
                               # block 6 (finally return)
                               ret(),
                             ]]))

    def testTryReturn(self):
        exnTy = ClassType(getExceptionClass())
        self.checkFunction("def f = try return 12 catch { case exn => 34; }",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 2),
                             ], [
                               # block 1 (try) [...]
                               i64(12),
                               ret(),
                             ], [
                               # block 2 (catch) [exception ...]
                               stlocal(-1),
                               i64(34),
                               branch(3),
                             ], [
                               # block 3 (done) [value ...]
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testTryFinallyReturnTwice(self):
        self.checkFunction("def f = try if (true) return 1 else return 2 finally 3",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 5),
                             ], [
                               # block 1 (try) [...]
                               true(),
                               branchif(2, 3),
                             ], [
                               # block 2 [...]
                               i64(1),
                               poptry(4),
                             ], [
                               # block 3 [...]
                               i64(2),
                               poptry(4),
                             ], [
                               # block 4 [return ...]
                               unit(),
                               swap(),
                               uninitialized(),
                               swap(),
                               label(8),
                               branch(6),
                             ], [
                               # block 5 (throw-finally) [exception ...]
                               unit(),
                               swap(),
                               i64(0),
                               label(7),
                               branch(6),
                             ], [
                               # block 6 (finally) [continuation return exception value ...]
                               i64(3),
                               drop(),
                               branchl(7, 8),
                             ], [
                               # block 7 (finally-rethrow) [return exception value ...]
                               drop(),
                               throw(),
                             ], [
                               # block 8 (return) [return exception value ...]
                               ret(),
                             ]]))

    def testTryCatchFinallyReturns(self):
        self.checkFunction("def f = try return 1 catch { case _ => return 2; } finally 3",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 3),
                             ], [
                               # block 1 (try) [...]
                               i64(1),
                               poptry(2),
                             ], [
                               # block 2 (try-return-finally) [return ...]
                               unit(),
                               swap(),
                               uninitialized(),
                               swap(),
                               label(9),
                               branch(7),
                             ], [
                               # block 3 (try-catch) [exception ...]
                               pushtry(4, 6),
                             ], [
                               # block 4 (catch) [exception ...]
                               dup(),
                               drop(),
                               i64(2),
                               poptry(5),
                             ], [
                               # block 5 (catch-return-finally) [return exception ...]
                               swap(),
                               drop(),
                               unit(),
                               swap(),
                               uninitialized(),
                               swap(),
                               label(9),
                               branch(7),
                             ], [
                               # block 6 (catch-throw-finally) [exception exception ...]
                               swap(),
                               drop(),
                               unit(),
                               swap(),
                               i64(0),
                               label(8),
                               branch(7),
                             ], [
                               # block 7 (finally) [continuation return exception value ...]
                               i64(3),
                               drop(),
                               branchl(8, 9),
                             ], [
                               # block 8 (finally-rethrow) [return exception value ...]
                               drop(),
                               throw(),
                             ], [
                               # block 9 (finally-return) [return exception value ...]
                               ret(),
                             ]]))

    def testReturnFromFinally(self):
        self.checkFunction("def f = try 12 finally return 34",
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 3),
                             ], [
                               # block 1 (try) [...]
                               i64(12),
                               poptry(2),
                             ], [
                               # block 2 (try-finally) [value ...]
                               uninitialized(),
                               label(-1),
                               branch(4),
                             ], [
                               # block 3 (throw-finally) [exception ...]
                               i64(0),
                               swap(),
                               label(-1),
                               branch(4),
                             ], [
                               # block 4 (finally) [continuation exception value ...]
                               i64(34),
                               ret(),
                             ]]))

    def testReturnFromNestedTry(self):
        source = "def f =\n" + \
                 "  try\n" + \
                 "    try\n" + \
                 "      return 12\n" + \
                 "      {}\n" + \
                 "    finally 34\n" + \
                 "    56\n" + \
                 "  catch\n" + \
                 "    case _ => 78"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 8),
                             ], [
                               # block 1 (outer try) [...]
                               pushtry(2, 4),
                             ], [
                               # block 2 (inner try) [...]
                               i64(12),
                               poptry(3),
                             ], [
                               # block 3 (inner try-return-finally) [return ...]
                               uninitialized(),
                               swap(),
                               label(7),
                               branch(5),
                             ], [
                               # block 4 (inner throw-finally) [exception ...]
                               i64(0),
                               label(6),
                               branch(5),
                             ], [
                               # block 5 (inner finally) [continuation return exception ...]
                               i64(34),
                               drop(),
                               branchl(6, 7),
                             ], [
                               # block 6 (inner finally-rethrow) [return exception ...]
                               drop(),
                               throw(),
                             ], [
                               # block 7 (inner finally-return) [return exception ...]
                               ret(),
                             ], [
                               # block 8 (outer catch) [exception ...]
                               drop(),
                               i64(78),
                               branch(9),
                             ], [
                               # block 9 (outer done) [value ...]
                               ret(),
                             ]]))

    def testReturnFromNestedTry2(self):
        source = "def f =\n" + \
                 "  try\n" + \
                 "    try\n" + \
                 "      return 1\n" + \
                 "    catch\n" + \
                 "      case _ => 2\n" + \
                 "  finally\n" + \
                 "    3"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 7),
                             ], [
                               # block 1 (outer try) [...]
                               pushtry(2, 4),
                             ], [
                               # block 2 (inner try) [...]
                               i64(1),
                               poptry(3),
                             ], [
                               # block 3 (inner try-return-finally) [return ...]
                               i64(0),
                               swap(),
                               uninitialized(),
                               swap(),
                               label(11),
                               branch(8),
                             ], [
                               # block 4 (inner catch) [exception ...]
                               drop(),
                               i64(2),
                               branch(5),
                             ], [
                               # block 5 (inner done) [value ...]
                               poptry(6),
                             ], [
                               # block 6 (outer try-finally) [value ...]
                               uninitialized(),
                               i64(0),
                               label(10),
                               branch(8),
                             ], [
                               # block 7 (outer throw-finally) [exception ...]
                               i64(0),
                               swap(),
                               i64(0),
                               label(9),
                               branch(8),
                             ], [
                               # block 8 (outer finally)
                               # [continuation return exception value ...]
                               i64(3),
                               drop(),
                               branchl(9, 10, 11),
                             ], [
                               # block 9 (outer finally-rethrow)
                               drop(),
                               throw(),
                             ], [
                               # block 10 (outer done)
                               dropi(2),
                               ret(),
                             ], [
                               # block 11 (outer finally-return)
                               ret(),
                             ]]))

    def testReturnFromTripleNestedTryFinally(self):
        source = "def f =\n" + \
                 "  try\n" + \
                 "    try\n" + \
                 "      try\n" + \
                 "        return 1\n" + \
                 "      finally\n" + \
                 "        3\n" + \
                 "    catch\n" + \
                 "      case _ => return 4\n" + \
                 "  finally\n" + \
                 "    5"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 11),
                             ], [
                               # block 1 (outer try) [...]
                               pushtry(2, 9),
                             ], [
                               # block 2 (middle try) [...]
                               pushtry(3, 5),
                             ], [
                               # block 3 (inner try) [...]
                               i64(1),
                               poptry(4),
                             ], [
                               # block 4 (inner try-finally-return) [return ...]
                               unit(),
                               swap(),
                               uninitialized(),
                               swap(),
                               label(8),
                               branch(6),
                             ], [
                               # block 5 (inner throw-finally) [exception ...]
                               unit(),
                               swap(),
                               i64(0),
                               label(7),
                               branch(6),
                             ], [
                               # block 6 (inner finally)
                               # [continuation return exception value ...]
                               i64(3),
                               drop(),
                               branchl(7, 8),
                             ], [
                               # block 7 (inner finally-rethrow) [return exception value ...]
                               drop(),
                               throw(),
                             ], [
                               # block 8 (inner finally-return) [return exception value ...]
                               label(14),
                               branch(12),
                             ], [
                               # block 9 (middle catch) [exception ...]
                               drop(),
                               i64(4),
                               poptry(10),
                             ], [
                               # block 10 (middle catch-return-finally) [return ...]
                               unit(),
                               swap(),
                               uninitialized(),
                               swap(),
                               label(14),
                               branch(12),
                             ], [
                               # block 11 (outer throw-finally) [exception ...]
                               unit(),
                               swap(),
                               i64(0),
                               label(13),
                               branch(12),
                             ], [
                               # block 12 (outer finally)
                               # [continuation return exception value ...]
                               i64(5),
                               drop(),
                               branchl(13, 14),
                             ], [
                               # block 13 (outer finally-rethrow) [return exception value ...]
                               drop(),
                               throw(),
                             ], [
                               # block 14 (outer finally-return) [return exception value ...]
                               ret(),
                             ]]))

    def testReturnFromNestedFinally(self):
        source = "def f =\n" + \
                 "  try\n" + \
                 "    try\n" + \
                 "      1\n" + \
                 "    finally\n" + \
                 "      return 2\n" + \
                 "  finally\n" + \
                 "    3"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", I64Type, [[
                               # block 0 [...]
                               pushtry(1, 7),
                             ], [
                               # block 1 (outer try) [...]
                               pushtry(2, 4),
                             ], [
                               # block 2 (inner try) [...]
                               i64(1),
                               poptry(3),
                             ], [
                               # block 3 (inner try-finally) [value ...]
                               uninitialized(),
                               label(-1),
                               branch(5),
                             ], [
                               # block 4 (inner throw-finally) [exception ...]
                               i64(0),
                               swap(),
                               label(-1),
                               branch(5),
                             ], [
                               # block 5 (inner finally) [continuation exception value ...]
                               dropi(3),
                               i64(2),
                               poptry(6),
                             ], [
                               # block 6 (outer try-return-finally) [return ...]
                               i64(0),
                               swap(),
                               uninitialized(),
                               swap(),
                               label(10),
                               branch(8),
                             ], [
                               # block 7 (outer throw-finally) [exception ...]
                               i64(0),
                               swap(),
                               i64(0),
                               label(9),
                               branch(8),
                             ], [
                               # block 8 (outer finally)
                               # [continuation return exception value ...]
                               i64(3),
                               drop(),
                               branchl(9, 10),
                             ], [
                               # block 9 (outer finally-rethrow) [return exception value ...]
                               drop(),
                               throw(),
                             ], [
                               # block 10 (outer finally-return) [return exception value ...]
                               ret(),
                             ]]))

    def testReturnFromNestedTryDifferentTypes(self):
        source = "def f =\n" + \
                 "  try\n" + \
                 "    if (true)\n" + \
                 "      let x = try\n" + \
                 "        if (true) return true else 2\n" + \
                 "      finally\n" + \
                 "        3\n" + \
                 "      true\n" + \
                 "    else\n" + \
                 "      false\n" + \
                 "  finally\n" + \
                 "    4"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", BooleanType, [[
                               # block 0 [...]
                               pushtry(1, 17),
                             ], [
                               # block 1 (outer try) [...]
                               true(),
                               branchif(2, 14),
                             ], [
                               # block 2 [...]
                               pushtry(3, 9),
                             ], [
                               # block 3 (inner try) [...]
                               true(),
                               branchif(4, 6),
                             ], [
                               # block 4 [...]
                               true(),
                               poptry(5),
                             ], [
                               # block 5 (inner try-return-finally) [return ...]
                               i64(0),
                               swap(),
                               uninitialized(),
                               swap(),
                               label(13),
                               branch(10),
                             ], [
                               # block 6 [...]
                               i64(2),
                               branch(7),
                             ], [
                               # block 7 [value ...]
                               poptry(8),
                             ], [
                               # block 8 (inner try-finally) [value ...]
                               uninitialized(),
                               false(),
                               label(12),
                               branch(10),
                             ], [
                               # block 9 (inner throw-finally) [exception ...]
                               i64(0),
                               swap(),
                               false(),
                               label(11),
                               branch(10),
                             ], [
                               # block 10 (inner finally)
                               # [continuation return exception value ...]
                               i64(3),
                               drop(),
                               branchl(11, 12, 13),
                             ], [
                               # block 11 (inner finally-rethrow) [return exception value ...]
                               drop(),
                               throw(),
                             ], [
                               # block 12 (inner done) [return exception value ...]
                               dropi(2),
                               stlocal(-1),
                               true(),
                               branch(15),
                             ], [
                               # block 13 (inner finally-return) [return exception value ...]
                               swap2(),
                               drop(),
                               false(),
                               swap2(),
                               label(21),
                               branch(18),
                             ], [
                               # block 14 [...]
                               false(),
                               branch(15),
                             ], [
                               # block 15 [value ...]
                               poptry(16),
                             ], [
                               # block 16 (outer try-finally) [value ...]
                               uninitialized(),
                               false(),
                               label(20),
                               branch(18),
                             ], [
                               # block 17 (outer throw-finally) [exception ...]
                               false(),
                               swap(),
                               false(),
                               label(19),
                               branch(18),
                             ], [
                               # block 18 (outer finally)
                               # [continuation return exception value ...]
                               i64(4),
                               drop(),
                               branchl(19, 20, 21),
                             ], [
                               # block 19 (outer finally-rethrow) [return exception value ...]
                               drop(),
                               throw(),
                             ], [
                               # block 20 (outer done) [return exception value ...]
                               dropi(2),
                               ret(),
                             ], [
                               # block 21 (outer finally-return) [return exception value ...]
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.x", type=I64Type,
                                                          kind=LOCAL, flags=frozenset([LET]))]))


    def testReturn(self):
        self.checkFunction("def f = return 12",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               ret(),
                             ]]))

    def testReturnEmpty(self):
        self.checkFunction("def f = return",
                           self.makeSimpleFunction("f", UnitType, [[
                               unit(),
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
                           "  try throw Exception() catch\n" +
                           "    case exn => 2\n",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(1),
                               ret(),
                             ]],
                             variables=[self.makeVariable("f.exn", type=exnTy,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testFactorial(self):
        self.checkFunction("def f(var n: i64): i64 = {\n" +
                           "  var p = 1;\n" +
                           "  while (n > 1) {\n" +
                           "    p = p * n;\n" +
                           "    n = n - 1;\n" +
                           "  };\n" +
                           "  p;\n" +
                           "};",
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(1),
                               stlocal(-1),
                               branch(1),
                             ], [
                               ldlocal(0),
                               i64(1),
                               gti64(),
                               branchif(2, 3),
                             ], [
                               ldlocal(-1),
                               ldlocal(0),
                               muli64(),
                               stlocal(-1),
                               ldlocal(0),
                               i64(1),
                               subi64(),
                               stlocal(0),
                               branch(1),
                             ], [
                               ldlocal(-1),
                               ret(),
                             ]],
                           variables=[
                             self.makeVariable("f.n", type=I64Type, kind=PARAMETER),
                             self.makeVariable("f.p", type=I64Type)]))

    def testRequireParameterTypes(self):
        self.assertRaises(TypeException, self.compileFromSource, "def f(x) = 12")

    def testNullaryCall(self):
        source = "def f: i64 = 12\n" + \
                 "def g: i64 = f\n"
        package = self.compileFromSource(source)
        f = package.findFunction(name="f")
        g = package.findFunction(name="g")
        self.assertEquals(g,
                          self.makeSimpleFunction("g", I64Type, [[
                              callg(f),
                              ret(),
                            ]]))

    def testFunctionCall(self):
        source = "def f(x: i64, y: i64): i64 = x\n" + \
                 "def g: i64 = f(12, 34)\n"
        package = self.compileFromSource(source)
        f = package.findFunction(name="f")
        g = package.findFunction(name="g")
        self.assertEquals(g,
                          self.makeSimpleFunction("g", I64Type, [[
                              i64(12),
                              i64(34),
                              callg(f),
                              ret(),
                            ]]))

    def testForeignFunctionCall(self):
        foo = Package(name=Name(["foo"]))
        bar = foo.addFunction(Name(["bar"]), returnType=I64Type,
                              typeParameters=[], parameterTypes=[I64Type],
                              flags=frozenset([PUBLIC]))
        loader = FakePackageLoader([foo])
        source = "def f = foo.bar(12)"
        package = self.compileFromSource(source, packageLoader=loader)
        self.assertIs(foo.id, bar.id.packageId)
        dep = package.dependencies[foo.id.index]
        externBar = dep.externFunctions[bar.id.externIndex]
        self.assertIn(EXTERN, externBar.flags)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               i64(12),
                               callgf(bar),
                               ret()]]))

    def testForeignFunctionCallWithTypeArg(self):
        foo = Package(name=Name(["foo"]))
        T = foo.addTypeParameter(Name(["foo", "T"]), upperBound=getRootClassType(),
                                 lowerBound=getNothingClassType(), flags=frozenset([STATIC]))
        Tty = VariableType(T)
        bar = foo.addFunction(Name(["bar"]), returnType=Tty, typeParameters=[T],
                              parameterTypes=[Tty], flags=frozenset([PUBLIC]))
        loader = FakePackageLoader([foo])
        source = "def f(s: String) = foo.bar[String](s)"
        package = self.compileFromSource(source, packageLoader=loader)
        stringTy = getStringType()
        self.checkFunction(package,
                           self.makeSimpleFunction("f", stringTy, [[
                               ldlocal(0),
                               tycs(getStringClass()),
                               callgf(bar),
                               ret()]],
                             parameterTypes=[stringTy],
                             variables=[self.makeVariable("f.s", type=stringTy,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testFunctionCallWithForeignTypeArg(self):
        fooPackage = Package(name=Name(["foo"]))
        barClass = fooPackage.addClass(Name(["Bar"]), typeParameters=[],
                                       supertypes=[getRootClassType()],
                                       constructors=[], fields=[],
                                       methods=[], flags=frozenset([PUBLIC]))
        barType = ClassType(barClass)
        loader = FakePackageLoader([fooPackage])

        source = "def id[static T](o: T) = o\n" + \
                 "def f(o: foo.Bar) = id[foo.Bar](o)"
        package = self.compileFromSource(source, packageLoader=loader)
        idFunction = package.findFunction(name="id")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", barType, [[
                               ldlocal(0),
                               tycsf(barClass),
                               callg(idFunction),
                               ret(),
                             ]],
                             parameterTypes=[barType],
                             variables=[self.makeVariable("f.o", type=barType,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testMatchForeignType(self):
        # try-catch is used for now, since full pattern matching hasn't been implemented yet.
        fooPackage = Package(name=Name(["foo"]))
        exceptionClass = getExceptionClass()
        clas = fooPackage.addClass(Name(["Bar"]), typeParameters=[],
                                   supertypes=[ClassType(exceptionClass)],
                                   constructors=[], fields=[], methods=[],
                                   flags=frozenset([PUBLIC]))
        loader = FakePackageLoader([fooPackage])

        source = "def f =\n" + \
                 "  try {} catch\n" + \
                 "    case x: foo.Bar => {}"
        package = self.compileFromSource(source, packageLoader=loader)

        barType = ClassType(clas)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", UnitType, [[
                               pushtry(1, 2),
                             ], [
                               unit(),
                               poptry(4),
                             ], [
                               tycdf(clas),
                               castcbr(3, 5),
                             ], [
                               stlocal(-1),
                               unit(),
                               branch(4),
                             ], [
                               ret(),
                             ], [
                               throw(),
                             ]],
                             variables=[self.makeVariable("f.x", type=barType,
                                                          kind=LOCAL, flags=frozenset([LET]))]))

    def testInitializer(self):
        source = "class Foo\n" + \
                 "  var x: Foo = this\n" + \
                 "  var y: Foo"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        thisType = ClassType(clas)
        self.checkFunction(package,
                           self.makeSimpleFunction(Name(["Foo", CLASS_INIT_SUFFIX]), UnitType, [[
                               ldlocal(0),
                               ldlocal(0),
                               stf(0),
                               uninitialized(),
                               ldlocal(0),
                               stf(1),
                               unit(),
                               ret()]],
                             parameterTypes=[thisType],
                             variables=[self.makeVariable(Name(["Foo", CLASS_INIT_SUFFIX, RECEIVER_SUFFIX]),
                                                          type=thisType,
                                                          kind=PARAMETER, flags=frozenset([LET]))],
                             flags=frozenset([METHOD, INITIALIZER])))

    def testDefaultCtorCallsInitializer(self):
        source = "class Foo"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        thisType = ClassType(clas)
        ctor = clas.constructors[0]
        init = package.findFunction(name=Name(["Foo", CLASS_INIT_SUFFIX]))
        self.assertEquals(self.makeSimpleFunction(Name(["Foo", CONSTRUCTOR_SUFFIX]), UnitType, [[
                              ldlocal(0),
                              callg(getRootClass().constructors[0]),
                              drop(),
                              ldlocal(0),
                              callg(init),
                              drop(),
                              unit(),
                              ret()]],
                            parameterTypes=[thisType],
                            variables=[self.makeVariable(Name(["Foo", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                                         type=thisType,
                                                         kind=PARAMETER, flags=frozenset([LET]))],
                            flags=frozenset([METHOD, CONSTRUCTOR])),
                          ctor)

    def testPrimaryCtorCallsInitializer(self):
        source = "class Foo(x: i32)\n" + \
                 "  var y = x"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        thisType = ClassType(clas)
        ctor = clas.constructors[0]
        init = clas.initializer
        expected = self.makeSimpleFunction(Name(["Foo", CONSTRUCTOR_SUFFIX]), UnitType, [[
            ldlocal(1),
            ldlocal(0),
            stf(0),
            ldlocal(0),
            callg(getRootClass().constructors[0]),
            drop(),
            ldlocal(0),
            callg(init),
            drop(),
            unit(),
            ret()]],
          parameterTypes=[thisType, I32Type],
          variables=[self.makeVariable(Name(["Foo", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                       type=thisType,
                                       kind=PARAMETER, flags=frozenset([LET])),
                     self.makeVariable(Name(["Foo", "x"]), type=I32Type,
                                       kind=PARAMETER, flags=frozenset([LET]))],
          flags=frozenset([METHOD, CONSTRUCTOR]))
        self.assertEquals(expected, ctor)

    def testSecondaryCtorCallsInitializer(self):
        source = "class Foo\n" + \
                 "  def this(x: i32) =\n" + \
                 "    this.x = x\n" + \
                 "  var x: i32\n" + \
                 "  var y: i32"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        thisType = ClassType(clas)
        ctor = clas.constructors[0]
        init = clas.initializer
        expected = self.makeSimpleFunction(Name(["Foo", CONSTRUCTOR_SUFFIX]), UnitType, [[
            ldlocal(0),
            callg(getRootClass().constructors[0]),
            drop(),
            ldlocal(0),
            callg(init),
            drop(),
            ldlocal(0),
            ldlocal(1),
            swap(),
            stf(0),
            unit(),
            ret()]],
          parameterTypes=[thisType, I32Type],
          variables=[self.makeVariable(Name(["Foo", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                       type=thisType,
                                       kind=PARAMETER, flags=frozenset([LET])),
                     self.makeVariable(Name(["Foo", CONSTRUCTOR_SUFFIX, "x"]), type=I32Type,
                                       kind=PARAMETER, flags=frozenset([LET]))],
          flags=frozenset([METHOD, CONSTRUCTOR]))
        self.assertEquals(expected, ctor)

    def testNullaryCtor(self):
        source = "class Foo\n" + \
                 "  def this = {}\n" + \
                 "def f = Foo()\n"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        objType = ClassType(clas, ())
        self.checkFunction(package,
                           self.makeSimpleFunction("f", objType, [[
                               allocobj(clas),
                               dup(),
                               callg(clas.constructors[0]),
                               drop(),
                               ret(),
                             ]]))

    def testNullaryCtorForEffect(self):
        source = "class Foo\n" + \
                 "  def this = {}\n" + \
                 "def f =\n" + \
                 "  Foo()\n" + \
                 "  12"
        package = self.makePackage(source)
        clas = package.findClass(name="Foo")
        objType = ClassType(clas, ())
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               allocobj(clas),
                               callg(clas.constructors[0]),
                               drop(),
                               i64(12),
                               ret()]]))

    def testPrimaryUnaryCtor(self):
        source = "class Foo(x: i32)"
        package = self.compileFromSource(source)
        clas = package.findClass(name="Foo")
        init = package.findFunction(name=Name(["Foo", CLASS_INIT_SUFFIX]))
        ctor = clas.constructors[0]
        thisType = ClassType(clas)
        self.assertEquals(self.makeSimpleFunction(Name(["Foo", CONSTRUCTOR_SUFFIX]), UnitType, [[
                              ldlocal(1),
                              ldlocal(0),
                              stf(0),
                              ldlocal(0),
                              callg(getRootClass().constructors[0]),
                              drop(),
                              ldlocal(0),
                              callg(init),
                              drop(),
                              unit(),
                              ret()]],
                            parameterTypes=[thisType, I32Type],
                            variables=[self.makeVariable(Name(["Foo", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                                         type=thisType,
                                                         kind=PARAMETER, flags=frozenset([LET])),
                                       self.makeVariable(Name(["Foo", "x"]), type=I32Type,
                                                         kind=PARAMETER, flags=frozenset([LET]))],
                            flags=frozenset([METHOD, CONSTRUCTOR])),
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
                               allocobj(clas),
                               dup(),
                               i64(1),
                               i64(2),
                               callg(clas.constructors[0]),
                               drop(),
                               ret(),
                             ]]))

    def testForeignCtor(self):
        fooPackage = Package(name=Name(["foo"]))
        barClass = fooPackage.addClass(Name(["Bar"]), typeParameters=[],
                                       supertypes=[getRootClassType()],
                                       constructors=[], fields=[], methods=[],
                                       flags=frozenset([PUBLIC]))
        barTy = ClassType(barClass)
        ctor = fooPackage.addFunction(Name(["Bar", CONSTRUCTOR_SUFFIX]),
                                      returnType=UnitType, typeParameters=[],
                                      parameterTypes=[barTy],
                                      flags=frozenset([PUBLIC, METHOD, CONSTRUCTOR]),
                                      definingClass=barClass)
        barClass.constructors.append(ctor)
        loader = FakePackageLoader([fooPackage])

        source = "def f = foo.Bar()"
        package = self.compileFromSource(source, packageLoader=loader)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", barTy, [[
                               allocobjf(barClass),
                               dup(),
                               callgf(ctor),
                               drop(),
                               ret()]]))

    def testNullaryMethod(self):
        source = "class Foo\n" + \
                 "  def get = 12\n" + \
                 "def f(foo: Foo) = foo.get"
        package = self.compileFromSource(source)
        clas = package.findClass(name="Foo")
        method = package.findFunction(name="Foo.get")
        index = clas.getMethodIndex(method)
        objType = ClassType(clas, ())
        self.checkFunction(package,
                           self.makeSimpleFunction("f", I64Type, [[
                               ldlocal(0),
                               callv(1, index),
                               ret()
                             ]],
                             variables=[self.makeVariable("f.foo", type=objType,
                                                          kind=PARAMETER, flags=frozenset([LET]))]))

    def testCaptureThis(self):
        source = "class Foo\n" + \
                 "  var x: i64\n" + \
                 "  def f =\n" + \
                 "    def g = this.x"
        package = self.compileFromSource(source)
        closureClass = package.findClass(name=Name(["Foo", "f", "g", CLOSURE_SUFFIX]))
        closureType = ClassType(closureClass)
        self.checkFunction(package,
                           self.makeSimpleFunction("Foo.f.g", I64Type, [[
                               ldlocal(0),
                               ldf(0),
                               ldf(0),
                               ldf(0),
                               ret()
                             ]],
                             variables=[self.makeVariable(Name(["Foo", "f", "g", RECEIVER_SUFFIX]),
                                                          type=closureType,
                                                          kind=PARAMETER, flags=frozenset([LET]))],
                             parameterTypes=[closureType],
                             flags=frozenset([METHOD])))

    def testCallClosure(self):
        source = "def foo(x: i64) =\n" + \
                 "  def bar = x\n" + \
                 "  bar"
        package = self.compileFromSource(source)
        contextClass = package.findClass(name=Name(["foo", CONTEXT_SUFFIX]))
        contextType = ClassType(contextClass)
        closureClass = package.findClass(name=Name(["foo", "bar", CLOSURE_SUFFIX]))
        closureType = ClassType(closureClass)
        self.checkFunction(package,
                           self.makeSimpleFunction("foo", I64Type, [[
                               allocobj(contextClass),
                               dup(),
                               callg(contextClass.constructors[0]),
                               drop(),
                               stlocal(-1),
                               ldlocal(0),
                               ldlocal(-1),
                               stf(0),
                               allocobj(closureClass),
                               dup(),
                               ldlocal(-1),
                               callg(closureClass.constructors[0]),
                               drop(),
                               stlocal(-2),
                               ldlocal(-2),
                               callv(1, len(closureClass.methods) - 1),
                               ret()]],
                             variables=[self.makeVariable(Name(["foo", CONTEXT_SUFFIX]), type=contextType),
                                        self.makeVariable("foo.bar", type=closureType)],
                             parameterTypes=[I64Type]))

    def testCallAlternateCtor(self):
        source = "class Foo(a: i64)\n" + \
                 "  def this = this(12)"
        package = self.compileFromSource(source)
        clas = package.findClass(name="Foo")
        ty = ClassType(clas)
        expected = self.makeSimpleFunction(Name(["Foo", CONSTRUCTOR_SUFFIX]), UnitType, [[
                                               ldlocal(0),
                                               i64(12),
                                               callg(clas.constructors[0]),
                                               drop(),
                                               unit(),
                                               ret()]],
                                             variables=[self.makeVariable(Name(["Foo", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                                                          type=ty,
                                                                          kind=PARAMETER,
                                                                          flags=frozenset([LET]))],
                                             parameterTypes=[ty],
                                             flags=frozenset([METHOD, CONSTRUCTOR]))
        self.assertEquals(expected, clas.constructors[1])

    def testCallAlternateCtorLater(self):
        source = "class Foo(a: i64)\n" + \
                 "  def this =\n" + \
                 "    var x = 12\n" + \
                 "    this(x)"
        self.assertRaises(SemanticException, self.compileFromSource, source)

    def testCallSuperCtorFromPrimary(self):
        source = "class Foo(x: i64)\n" + \
                 "class Bar(y: i64) <: Foo(y)"
        package = self.compileFromSource(source)
        foo = package.findClass(name="Foo")
        bar = package.findClass(name="Bar")
        barTy = ClassType(bar)
        self.checkFunction(package,
                           self.makeSimpleFunction(Name(["Bar", CONSTRUCTOR_SUFFIX]),
                               UnitType,
                               [[
                                   ldlocal(1),
                                   ldlocal(0),
                                   stf(bar.fields[1].index),
                                   ldlocal(0),
                                   ldlocal(0),
                                   ldf(bar.fields[1].index),
                                   callg(foo.constructors[0]),
                                   drop(),
                                   ldlocal(0),
                                   callg(bar.initializer),
                                   drop(),
                                   unit(),
                                   ret(),
                               ]],
                               variables=[self.makeVariable(Name(["Bar", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                                            type=barTy,
                                                            kind=PARAMETER,
                                                            flags=frozenset([LET])),
                                          self.makeVariable(Name(["Bar", "y"]),
                                                            type=I64Type, kind=PARAMETER,
                                                            flags=frozenset([LET]))],
                               flags=frozenset([METHOD, CONSTRUCTOR])))

    def testCallSuperCtorFromSecondary(self):
        source = "class Foo(a: i64)\n" + \
                 "class Bar <: Foo\n" + \
                 "  def this = super(12)"
        package = self.compileFromSource(source)
        foo = package.findClass(name="Foo")
        bar = package.findClass(name="Bar")
        barTy = ClassType(bar)
        expected = self.makeSimpleFunction(Name(["Bar", CONSTRUCTOR_SUFFIX]), UnitType, [[
                                               ldlocal(0),
                                               i64(12),
                                               callg(foo.constructors[0]),
                                               drop(),
                                               ldlocal(0),
                                               callg(bar.initializer),
                                               drop(),
                                               unit(),
                                               ret()]],
                                             variables=[self.makeVariable(Name(["Bar", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]), type=barTy,
                                                                          kind=PARAMETER, flags=frozenset([LET]))],
                                             parameterTypes=[barTy],
                                             flags=frozenset([METHOD, CONSTRUCTOR]))
        self.assertEquals(expected, bar.constructors[0])

    def testCallDefaultSuperCtor(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo"
        package = self.compileFromSource(source)
        foo = package.findClass(name="Foo")
        bar = package.findClass(name="Bar")
        barTy = ClassType(bar)
        expected = self.makeSimpleFunction(Name(["Bar", CONSTRUCTOR_SUFFIX]), UnitType, [[
                                               ldlocal(0),
                                               callg(foo.constructors[0]),
                                               drop(),
                                               ldlocal(0),
                                               callg(bar.initializer),
                                               drop(),
                                               unit(),
                                               ret()]],
                                             variables=[self.makeVariable(Name(["Bar", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                                                          type=barTy,
                                                                          kind=PARAMETER,
                                                                          flags=frozenset([LET]))],
                                             parameterTypes=[barTy])
        # TODO: I feel like there was supposed to be an assertion here.

    def testCallBuiltinFunction(self):
        package = self.compileFromSource("def f = print(\"foo\")")
        fooIndex = package.findString("foo")
        self.checkFunction(package,
                           self.makeSimpleFunction("f", UnitType, [[
                               string(fooIndex),
                               callg(getBuiltinFunctionById(BUILTIN_PRINT_FUNCTION_ID)),
                               ret()]]))

    def testCallBuiltinPrimitiveMethod(self):
        source = "def f = {}.to-string"
        self.checkFunction(source,
                           self.makeSimpleFunction("f", getStringType(), [[
                               unit(),
                               callg(getBuiltinFunctionById(BUILTIN_UNIT_TO_STRING_ID)),
                               ret()]]))

    def testCallWithStaticClassTypeArgument(self):
        source = "class C\n" + \
                 "def id[static T](x: T) = x\n" + \
                 "def f(o: C) = id[C](o)"
        package = self.compileFromSource(source)
        C = package.findClass(name="C")
        Cty = ClassType(C)
        id = package.findFunction(name="id")
        f = package.findFunction(name="f")
        expected = self.makeSimpleFunction("f", Cty, [[
                       ldlocal(0),
                       tycs(C),
                       callg(id),
                       ret()]],
                     variables=[self.makeVariable("f.o", type=Cty,
                                                  kind=PARAMETER, flags=frozenset([LET]))],
                     parameterTypes=[Cty])
        self.assertEquals(expected, f)

    def testCallWithStaticVariableTypeArgument(self):
        source = "def id-outer[static TO](x: TO) = id-inner[TO](x)\n" + \
                 "def id-inner[static TI](x: TI) = x"
        package = self.compileFromSource(source)
        T = package.findTypeParameter(name="id-outer.TO")
        Tty = VariableType(T)
        inner = package.findFunction(name="id-inner")
        outer = package.findFunction(name="id-outer")
        expected = self.makeSimpleFunction("id-outer", Tty, [[
            ldlocal(0),
            tyvs(T),
            callg(inner),
            ret()]],
          variables=[self.makeVariable("id-outer.x", type=Tty,
                                       kind=PARAMETER, flags=frozenset([LET]))],
          typeParameters=[T],
          parameterTypes=[Tty])
        self.assertEquals(expected, outer)

    def testCallWithExplicitTypeArgument(self):
        source = "def id-outer[static T](x: T) =\n" + \
                 "  def id-inner(x: T) = x\n" + \
                 "  id-inner(x)"
        package = self.compileFromSource(source)
        T = package.findTypeParameter(name="id-outer.T")
        Tty = VariableType(T)
        idOuter = package.findFunction(name="id-outer")
        idInner = package.findFunction(name="id-outer.id-inner")
        expected = self.makeSimpleFunction("id-outer", Tty, [[
            ldlocal(0),
            tyvs(T),
            callg(idInner),
            ret()]],
          variables=[self.makeVariable("id-outer.x", type=Tty,
                                       kind=PARAMETER, flags=frozenset([LET]))],
          typeParameters=[T],
          parameterTypes=[Tty])
        self.assertEquals(expected, idOuter)

    def testCallWithImplicitTypeArgument(self):
        source = "def id-outer[static T](x: T) =\n" + \
                 "  def id-inner = x\n" + \
                 "  id-inner"
        package = self.compileFromSource(source)
        T = package.findTypeParameter(name="id-outer.T")
        Tty = VariableType(T)
        idOuter = package.findFunction(name="id-outer")
        contextClass = package.findClass(name=Name(["id-outer", CONTEXT_SUFFIX]))
        closureClass = package.findClass(name=Name(["id-outer", "id-inner", CLOSURE_SUFFIX]))
        idInner = package.findFunction(name="id-outer.id-inner")
        idInnerMethodIndex = closureClass.getMethodIndex(idInner)
        expected = self.makeSimpleFunction("id-outer", Tty, [[
            tyvs(T),
            allocobj(contextClass),
            dup(),
            tyvs(T),
            callg(contextClass.constructors[0]),
            drop(),
            stlocal(-1),
            ldlocal(0),
            ldlocal(-1),
            stf(0),
            tyvs(T),
            allocobj(closureClass),
            dup(),
            ldlocal(-1),
            tyvs(T),
            callg(closureClass.constructors[0]),
            drop(),
            stlocal(-2),
            ldlocal(-2),
            tyvs(T),
            callv(1, idInnerMethodIndex),
            ret()]],
          variables=[self.makeVariable(Name(["id-outer", CONTEXT_SUFFIX]),
                                       type=ClassType(contextClass)),
                     self.makeVariable("id-outer.id-inner", type=ClassType(closureClass))],
          typeParameters=[T],
          parameterTypes=[Tty])
        self.assertEquals(expected, idOuter)

    def testCallInheritedMethodWithoutTypeArgument(self):
        source = "class Foo[static T]\n" + \
                 "def f(foo: Foo[Object]) =\n" + \
                 "  foo.to-string\n" + \
                 "  {}"
        package = self.compileFromSource(source)
        f = package.findFunction(name="f")
        Foo = package.findClass(name="Foo")
        FooType = ClassType(Foo, (getRootClassType(),))
        toString = Foo.findMethodByShortName("to-string")
        toStringIndex = Foo.getMethodIndex(toString)
        expected = self.makeSimpleFunction("f", UnitType, [[
            ldlocal(0),
            callv(1, toStringIndex),
            drop(),
            unit(),
            ret()]],
          variables=[self.makeVariable("f.foo", type=FooType,
                                       kind=PARAMETER, flags=frozenset([LET]))],
          parameterTypes=[FooType])
        self.assertEquals(expected, f)

    def testCallOverridenMethodWithTypeArgument(self):
        source = "class Foo[static T]\n" + \
                 "  override def to-string = \"Foo\"\n" + \
                 "def f(foo: Foo[Object]) =\n" + \
                 "  foo.to-string\n" + \
                 "  {}"
        package = self.compileFromSource(source)
        f = package.findFunction(name="f")
        Foo = package.findClass(name="Foo")
        FooType = ClassType(Foo, (getRootClassType(),))
        toString = Foo.findMethodByShortName("to-string")
        toStringIndex = Foo.getMethodIndex(toString)
        expected = self.makeSimpleFunction("f", UnitType, [[
            ldlocal(0),
            tycs(getRootClass()),
            callv(1, toStringIndex),
            drop(),
            unit(),
            ret()]],
          variables=[self.makeVariable("f.foo", type=FooType,
                                       kind=PARAMETER, flags=frozenset([LET]))],
          parameterTypes=[FooType])
        self.assertEquals(expected, f)

    def testConstructorCallInitializerInClassWithStaticTypeArgs(self):
        source = "class C[static T]"
        package = self.compileFromSource(source)
        C = package.findClass(name="C")
        T = package.findTypeParameter(name="C.T")
        Ctype = ClassType(C, (VariableType(T),))
        expectedCtor = self.makeSimpleFunction(Name(["C", CONSTRUCTOR_SUFFIX]), UnitType, [[
            ldlocal(0),
            callg(getBuiltinFunctionById(BUILTIN_ROOT_CLASS_CTOR_ID)),
            drop(),
            ldlocal(0),
            tyvs(T),
            callg(C.initializer),
            drop(),
            unit(),
            ret()]],
          variables=[self.makeVariable(Name(["C", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                       type=Ctype,
                                       kind=PARAMETER, flags=frozenset([LET]))],
          typeParameters=[T],
          parameterTypes=[Ctype],
          flags=frozenset([METHOD, CONSTRUCTOR]))
        self.assertEquals(expectedCtor, C.constructors[0])

    def testCallClassMethodsWithStaticTypeArgs(self):
        source = "class C\n" + \
                 "class Box[static T](var val: T)\n" + \
                 "  def get = val\n" + \
                 "  def set(val: T) =\n" + \
                 "    this.val = val\n" + \
                 "    {}\n" + \
                 "def f(box: Box[C]) = box.set(box.get)"
        package = self.compileFromSource(source)
        C = package.findClass(name="C")
        Box = package.findClass(name="Box")
        boxType = ClassType(Box, (ClassType(C),))
        get = package.findFunction(name="Box.get")
        set = package.findFunction(name="Box.set")
        f = package.findFunction(name="f")
        expectedF = self.makeSimpleFunction("f", UnitType, [[
            ldlocal(0),
            ldlocal(0),
            tycs(C),
            callv(1, Box.getMethodIndex(get)),
            tycs(C),
            callv(2, Box.getMethodIndex(set)),
            ret()]],
          variables=[self.makeVariable("f.box", type=boxType,
                                       kind=PARAMETER, flags=frozenset([LET]))],
          parameterTypes=[boxType])
        self.assertEquals(expectedF, f)

    def testCallStaticMethodFromMethod(self):
        source = "class Foo\n" + \
                 "  static def f = 12\n" + \
                 "  def g = f"
        package = self.compileFromSource(source)
        foo = package.findClass(name="Foo")
        fooType = ClassType(foo)
        f = package.findFunction(name="Foo.f")
        self.checkFunction(package,
                           self.makeSimpleFunction("Foo.f", I64Type, [[
                               i64(12),
                               ret(),
                             ]],
                             flags=frozenset([STATIC, METHOD])))
        self.checkFunction(package,
                           self.makeSimpleFunction("Foo.g", I64Type, [[
                               callg(f),
                               ret(),
                             ]],
                             variables=[self.makeVariable(Name(["Foo", "g", RECEIVER_SUFFIX]),
                                                          type=fooType, kind=PARAMETER,
                                                          flags=frozenset([LET]))],
                             flags=frozenset([METHOD])))

    def testClassWithArrayElements(self):
        source = "final class Foo\n" + \
                 "  arrayelements String, get, set, length"
        package = self.compileFromSource(source)
        Foo = package.findClass(name="Foo")
        FooType = ClassType(Foo)
        self.checkFunction(package,
                           self.makeSimpleFunction("Foo.get", getStringType(), [[
                               ldlocal(1),
                               ldlocal(0),
                               lde(),
                               ret(),
                             ]],
                             variables=[self.makeVariable(Name(["Foo", "get", RECEIVER_SUFFIX]),
                                                          type=FooType, kind=PARAMETER,
                                                          flags=frozenset([LET]))],
                             flags=frozenset([METHOD, ARRAY])))
        self.checkFunction(package,
                           self.makeSimpleFunction("Foo.set", UnitType, [[
                               ldlocal(2),
                               ldlocal(1),
                               ldlocal(0),
                               ste(),
                               unit(),
                               ret(),
                             ]],
                             variables=[self.makeVariable(Name(["Foo", "set", RECEIVER_SUFFIX]),
                                                          type=FooType, kind=PARAMETER,
                                                          flags=frozenset([LET]))],
                             flags=frozenset([METHOD, ARRAY])))
        length = next(f for f in Foo.fields if ARRAY in f.flags)
        self.checkFunction(package,
                           self.makeSimpleFunction("Foo.length", I32Type, [[
                               ldlocal(0),
                               ldf(length.index),
                               ret(),
                             ]],
                             variables=[self.makeVariable(Name(["Foo", "set", RECEIVER_SUFFIX]),
                                                          type=FooType, kind=PARAMETER,
                                                          flags=frozenset([LET]))],
                             flags=frozenset([METHOD, ARRAY])))

    def testAllocateLocalArrayForEffect(self):
        source = "final class Foo[static T](x: i64)\n" + \
                 "  arrayelements T, get, set, length\n" + \
                 "def f = { new(3i32) Foo[String](12); {}; }"
        package = self.compileFromSource(source)
        Foo = package.findClass(name="Foo")
        ctor = Foo.constructors[0]
        self.checkFunction(package,
                           self.makeSimpleFunction("f", UnitType, [[
                               i32(3),
                               tycs(getStringClass()),
                               allocarr(Foo),
                               i64(12),
                               tycs(getStringClass()),
                               callg(ctor),
                               drop(),
                               unit(),
                               ret(),
                             ]]))

    def testAllocateForeignArrayForValue(self):
        fooPackage = Package(name=Name(["foo"]))
        arrayClass = fooPackage.addClass(Name(["Array"]), typeParameters=[],
                                         supertypes=[getRootClassType()],
                                         fields=[], methods=[],
                                         flags=frozenset([PUBLIC, FINAL, ARRAY]),
                                         elementType=I32Type)
        arrayType = ClassType(arrayClass)
        ctor = fooPackage.addFunction(Name(["Array", CONSTRUCTOR_SUFFIX]), returnType=UnitType,
                                      typeParameters=[], parameterTypes=[arrayType],
                                      flags=frozenset([PUBLIC, METHOD, CONSTRUCTOR]),
                                      definingClass=arrayClass)
        arrayClass.constructors = [ctor]
        loader = FakePackageLoader([fooPackage])

        source = "def f = new(3i32) foo.Array()"
        package = self.compileFromSource(source, packageLoader=loader)
        self.checkFunction(package,
                           self.makeSimpleFunction("f", arrayType, [[
                               i32(3),
                               allocarrf(arrayClass),
                               dup(),
                               callgf(ctor),
                               drop(),
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

    # Regression tests
    def testPrimaryCtorCallsSuperCtorWithTypeArgs(self):
        source = "class Foo[static T]\n" + \
                 "class Bar <: Foo[Nothing]"
        package = self.compileFromSource(source)
        foo = package.findClass(name="Foo")
        bar = package.findClass(name="Bar")
        self.checkFunction(package,
                           self.makeSimpleFunction(Name(["Bar", CONSTRUCTOR_SUFFIX]),
                                                   UnitType,
                                                   [[
                                                       ldlocal(0),
                                                       tycs(getNothingClass()),
                                                       callg(foo.constructors[0]),
                                                       drop(),
                                                       ldlocal(0),
                                                       callg(bar.initializer),
                                                       drop(),
                                                       unit(),
                                                       ret(),
                                                   ]],
                                                   variables=[self.makeVariable(Name(["Bar", CONSTRUCTOR_SUFFIX, RECEIVER_SUFFIX]),
                                                                                type=ClassType(bar),
                                                                                kind=PARAMETER,
                                                                                flags=frozenset([LET]))],
                                                   flags=frozenset([METHOD, CONSTRUCTOR])))

    def testStringLengthCantBeWritten(self):
        source = "def f =\n" + \
                 "  let s = \"foobar\"\n" + \
                 "  s.length = 42i32"
        self.assertRaises(SemanticException, self.compileFromSource, source)

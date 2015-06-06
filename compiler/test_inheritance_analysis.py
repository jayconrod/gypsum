# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from ast import *
from compile_info import *
import ids
from ir import *
from ir_types import *
from errors import *
from flags import *
from layout import layout
from lexer import *
from parser import *
from scope_analysis import *
from builtins import getRootClass, getExceptionClass
from bytecode import BUILTIN_ROOT_CLASS_ID
from utils_test import FakePackageLoader

class TestInheritanceAnalysis(unittest.TestCase):
    def parseFromSource(self, source):
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        return ast

    def analyzeFromSource(self, source, packageLoader=None):
        if packageLoader is None:
            packageLoader = FakePackageLoader([])
        ast = self.parseFromSource(source)
        info = CompileInfo(ast, package=Package(ids.TARGET_PACKAGE_ID),
                           packageLoader=packageLoader, isUsingStd=False)
        analyzeDeclarations(info)
        analyzeInheritance(info)
        return info

    def testClassInfoForBuiltin(self):
        info = self.analyzeFromSource("")
        rootClassInfo = info.getClassInfo(BUILTIN_ROOT_CLASS_ID)
        self.assertIs(getRootClass(), rootClassInfo.irDefn)
        self.assertIs(None, rootClassInfo.superclassInfo)

    def testNoBaseClass(self):
        info = self.analyzeFromSource("class Foo")
        clas = info.package.findClass(name="Foo")
        classInfo = info.getClassInfo(clas)
        self.assertEquals(BUILTIN_ROOT_CLASS_ID, classInfo.superclassInfo.irDefn.id)

    def testWithBaseClass(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo"
        info = self.analyzeFromSource(source)
        ast = info.ast
        fooClass = info.package.findClass(name="Foo")
        barClass = info.package.findClass(name="Bar")
        fooClassInfo = info.getClassInfo(fooClass)
        barClassInfo = info.getClassInfo(barClass)
        self.assertIs(fooClassInfo, barClassInfo.superclassInfo)

    def testInheritFromForeignType(self):
        package = Package(name=Name(["foo"]))
        foreignClass = package.addClass(Name(["Bar"]), None, [], [getRootClassType()],
                                        None, [], [], [], frozenset([PUBLIC]))
        field = package.newField(Name(["x"]), None, None, frozenset([PUBLIC]))
        foreignClass.fields = [field]
        loader = FakePackageLoader([package])
        source = "class Baz <: foo.Bar"
        info = self.analyzeFromSource(source, packageLoader=loader)
        bazClass = info.package.findClass(name="Baz")
        bazClassInfo = info.getClassInfo(bazClass)
        foreignClassInfo = info.getClassInfo(foreignClass)
        self.assertIs(foreignClassInfo, bazClassInfo.superclassInfo)
        bazScope = info.getScope(bazClass)
        self.assertTrue(bazScope.isBound("x"))

    @unittest.skip("not implemented")
    def testInheritFromLocalTypeInForeignClass(self):
        # TODO: test foreign type inheriting from local class
        # when we support other packages depending on package being compiled
        self.fail()

    def testInheritForeignTypeInForeignTypeInSamePackage(self):
        package = Package(name=Name(["foo"]))
        barClass = package.addClass(Name(["Bar"]), None, [], [getRootClassType()],
                                    None, [], [], [], frozenset([PUBLIC]))
        bazClass = package.addClass(Name(["Baz"]), None, [], [ClassType(barClass)],
                                    None, [], [], [], frozenset())
        loader = FakePackageLoader([package])
        info = self.analyzeFromSource("class Quux <: foo.Bar", packageLoader=loader)
        barClassInfo = info.getClassInfo(barClass)
        bazClassInfo = info.getClassInfo(bazClass)
        self.assertIs(barClassInfo, bazClassInfo.superclassInfo)

    def testInheritForeignTypeInForeignTypeInDifferentPackage(self):
        fooPackage = Package(name=Name(["foo"]))
        barClass = fooPackage.addClass(Name(["Bar"]), None, [], [getRootClassType()],
                                       None, [], [], [], frozenset([PUBLIC]))
        bazPackage = Package(name=Name(["baz"]))
        loader = FakePackageLoader([fooPackage, bazPackage])
        bazPackage.dependencies.append(PackageDependency.fromPackage(fooPackage))
        quuxClass = bazPackage.addClass(Name(["Quux"]), None, [], [ClassType(barClass)],
                                        None, [], [], [], frozenset([PUBLIC]))
        info = self.analyzeFromSource("class Zzyzx <: baz.Quux", packageLoader=loader)
        barClassInfo = info.getClassInfo(barClass)
        quuxClassInfo = info.getClassInfo(quuxClass)
        self.assertIs(barClassInfo, quuxClassInfo.superclassInfo)

    def testInheritFromException(self):
        info = self.analyzeFromSource("class Foo <: Exception")
        ast = info.ast
        clas = info.package.findClass(name="Foo")
        classInfo = info.getClassInfo(clas)
        self.assertIs(getExceptionClass(), classInfo.superclassInfo.irDefn)

    def testInheritFromSelf(self):
        self.assertRaises(ScopeException, self.analyzeFromSource, "class Foo <: Foo")

    def testInheritCycle(self):
        source = "class Foo <: Bar\n" + \
                 "class Bar <: Foo"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    @unittest.skip("not implemented")
    def testInheritCycleForeign(self):
        # TODO: test an inheritance cycle with a foreign class and a local class.
        # when we support other packages depeneding on the one being compiled.
        self.fail()

    def testTypeParameterCycle(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo\n" + \
                 "def f[static T <: Bar >: Foo] = 12"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testTypeParameterCycleForeign(self):
        package = Package(name=Name(["foo"]))
        barClass = package.addClass(Name(["Bar"]), None, [], [getRootClassType()],
                                    None, [], [], [], frozenset([PUBLIC]))
        loader = FakePackageLoader([package])
        source = "class Baz <: foo.Bar\n" + \
                 "def f[static T <: Baz >: foo.Bar]"
        self.assertRaises(ScopeException, self.analyzeFromSource, source, packageLoader=loader)

    def testInheritedDefinitionsAreBound(self):
        source = "class Foo\n" + \
                 "  var x = 12\n" + \
                 "class Bar <: Foo"
        info = self.analyzeFromSource(source)
        ast = info.ast
        barScope = info.getScope(ast.modules[0].definitions[1])
        self.assertTrue(barScope.isBound("x"))

    def testInheritedBuiltinDefinitionsAreBound(self):
        info = self.analyzeFromSource("class Foo")
        scope = info.getScope(info.ast.modules[0].definitions[0])
        self.assertTrue(scope.isBound("typeof"))

    def testFieldNamesConflict(self):
        source = "class Foo\n" + \
                 "  var x\n" + \
                 "class Bar <: Foo\n" + \
                 "  var x"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testFieldsAndMethodsConflict(self):
        source = "class Foo\n" + \
                 "  var x\n" + \
                 "class Bar <: Foo\n" + \
                 "  def x = 12"
        self.assertRaises(ScopeException, self.analyzeFromSource, source)

    def testMethodsDoNotConflict(self):
        source = "class Foo\n" + \
                 "  def f = 12\n" + \
                 "class Bar <: Foo\n" + \
                 "  def f = 34"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[1])
        self.assertEquals(2, len(scope.getDefinition("f").overloads))

    def testOverrideBuiltin(self):
        source = "class Foo\n" + \
                 "  def typeof = 12"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[0])
        self.assertEquals(2, len(scope.getDefinition("typeof").overloads))

    def testConstructorsNotHeritable(self):
        source = "class A[static S](value: S)\n" + \
                 "class B[static T] <: A[T]\n" + \
                 "  def this(value: T) = super(value)\n" + \
                 "class C[static U] <: B[U]\n" + \
                 "  def this(value: U) = super(value)\n"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[0])
        self.assertEquals(1, len(scope.getDefinition(CONSTRUCTOR_SUFFIX).overloads))

    def testTypeParametersNotHeritable(self):
        source = "class A[static T]\n" + \
                 "class B <: A[String]"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[1])
        self.assertIs(None, scope.getDefinition("T"))

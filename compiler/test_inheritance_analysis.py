# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from builtins import getRootClass
from compile_info import CompileInfo
import ids
from ir import Package, PackageDependency
from ir_types import ClassType, getRootClassType, getExceptionClassType
from errors import InheritanceException
from flags import *
from layout import layout
from lexer import lex
from parser import parse
from scope_analysis import analyzeDeclarations
from type_analysis import analyzeTypeDeclarations
from inheritance_analysis import analyzeInheritance
from utils_test import FakePackageLoader
from name import CONSTRUCTOR_SUFFIX, Name


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
        analyzeTypeDeclarations(info)
        analyzeInheritance(info)
        return info

    def testNoBaseClass(self):
        info = self.analyzeFromSource("class Foo")
        clas = info.package.findClass(name="Foo")
        self.assertEquals([getRootClassType()], clas.supertypes)

    def testWithBaseClass(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo"
        info = self.analyzeFromSource(source)
        ast = info.ast
        fooClass = info.package.findClass(name="Foo")
        barClass = info.package.findClass(name="Bar")
        self.assertEquals([ClassType(fooClass), getRootClassType()], barClass.supertypes)

    def testInheritFromForeignType(self):
        package = Package(name=Name(["foo"]))
        foreignClass = package.addClass(Name(["Bar"]), sourceName="Bar", typeParameters=[],
                                        supertypes=[getRootClassType()],
                                        constructors=[], fields=[],
                                        methods=[], flags=frozenset([PUBLIC]))
        field = package.newField(Name(["Bar", "x"]), sourceName="x", flags=frozenset([PUBLIC]))
        foreignClass.fields = [field]
        loader = FakePackageLoader([package])
        source = "class Baz <: foo.Bar"
        info = self.analyzeFromSource(source, packageLoader=loader)
        bazClass = info.package.findClass(name="Baz")
        self.assertEquals([ClassType(foreignClass), getRootClassType()], bazClass.supertypes)
        bazScope = info.getScope(bazClass)
        self.assertTrue(bazScope.isBound("x"))

    def testInheritForeignTypeInForeignTypeInSamePackage(self):
        package = Package(name=Name(["foo"]))
        barClass = package.addClass(Name(["Bar"]), sourceName="Bar", typeParameters=[],
                                    supertypes=[getRootClassType()],
                                    constructors=[], fields=[],
                                    methods=[], flags=frozenset([PUBLIC]))
        bazClass = package.addClass(Name(["Baz"]), sourceName="Baz", typeParameters=[],
                                    supertypes=[ClassType(barClass), getRootClassType()],
                                    constructors=[], fields=[],
                                    methods=[], flags=frozenset([PUBLIC]))
        loader = FakePackageLoader([package])
        info = self.analyzeFromSource("class Quux <: foo.Baz", packageLoader=loader)
        quuxClass = info.package.findClass(name="Quux")
        self.assertEquals([ClassType(bazClass), ClassType(barClass), getRootClassType()],
                          quuxClass.supertypes)

    def testInheritForeignTypeInForeignTypeInDifferentPackage(self):
        fooPackage = Package(name=Name(["foo"]))
        barClass = fooPackage.addClass(Name(["Bar"]), sourceName="Bar", typeParameters=[],
                                       supertypes=[getRootClassType()],
                                       constructors=[], fields=[],
                                       methods=[], flags=frozenset([PUBLIC]))
        bazPackage = Package(name=Name(["baz"]))
        loader = FakePackageLoader([fooPackage, bazPackage])
        bazPackage.dependencies.append(PackageDependency.fromPackage(fooPackage))
        quuxClass = bazPackage.addClass(Name(["Quux"]), sourceName="Quux", typeParameters=[],
                                        supertypes=[ClassType(barClass), getRootClassType()],
                                        constructors=[], fields=[],
                                        methods=[], flags=frozenset([PUBLIC]))
        info = self.analyzeFromSource("class Zzyzx <: baz.Quux", packageLoader=loader)
        zzyzxClass = info.package.findClass(name="Zzyzx")
        self.assertEquals([ClassType(quuxClass), ClassType(barClass), getRootClassType()],
                          zzyzxClass.supertypes)

    def testInheritFromException(self):
        info = self.analyzeFromSource("class Foo <: Exception")
        clas = info.package.findClass(name="Foo")
        self.assertEquals([getExceptionClassType(), getRootClassType()], clas.supertypes)

    def testClassInheritFromTraitWithHigherClass(self):
        source = "trait Tr\n" + \
                 "class Foo <: Tr"
        info = self.analyzeFromSource(source)
        Tr = info.package.findTrait(name="Tr")
        Foo = info.package.findClass(name="Foo")
        self.assertEquals([getRootClassType(), ClassType(Tr)], Foo.supertypes)

    def testClassInheritFromTraitWithSameClass(self):
        source = "trait Tr <: Foo\n" + \
                 "class Foo <: Tr"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testClassInheritFromTraitWithUnrelatedClass(self):
        source = "class Bar\n" + \
                 "trait Tr <: Bar\n" + \
                 "class Foo <: Tr"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testClassInheritTraitMultipleTimesDirectly(self):
        source = "trait Tr\n" + \
                 "class Foo <: Tr, Tr"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testClassInheritTraitMultipleTimesIndirectlySameType(self):
        source = "trait Foo\n" + \
                 "trait Bar <: Foo\n" + \
                 "class Baz <: Foo, Bar"
        info = self.analyzeFromSource(source)
        Foo = info.package.findTrait(name="Foo")
        Bar = info.package.findTrait(name="Bar")
        Baz = info.package.findClass(name="Baz")
        self.assertEquals([getRootClassType(), ClassType(Foo), ClassType(Bar)], Baz.supertypes)

    def testClassInheritTraitMultipleTimesIndirectlyConflictingTypes(self):
        source = "trait Foo[static T]\n" + \
                 "trait Bar <: Foo[String]\n" + \
                 "class Baz <: Foo[Object], Bar"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testClassInheritNullableTrait(self):
        source = "trait Tr\n" + \
                 "class C <: Tr?"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testClassInheritNothingTrait(self):
        source = "class C <: Object, Nothing"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTraitNoSupertypes(self):
        info = self.analyzeFromSource("trait Foo")
        trait = info.package.findTrait(name="Foo")
        self.assertEquals([getRootClassType()], trait.supertypes)

    def testTraitInheritFromClass(self):
        source = "class Foo\n" + \
                 "trait Bar <: Foo"
        info = self.analyzeFromSource(source)
        Foo = info.package.findClass(name="Foo")
        Bar = info.package.findTrait(name="Bar")
        self.assertEquals([ClassType(Foo), getRootClassType()], Bar.supertypes)

    def testTraitInheritFromTrait(self):
        source = "trait Foo\n" + \
                 "trait Bar <: Foo"
        info = self.analyzeFromSource(source)
        Foo = info.package.findTrait(name="Foo")
        Bar = info.package.findTrait(name="Bar")
        self.assertEquals([getRootClassType(), ClassType(Foo)], Bar.supertypes)

    def testTraitInheritFromClassAndHigherTrait(self):
        source = "class Foo\n" + \
                 "trait Bar\n" + \
                 "trait Baz <: Foo, Bar"
        info = self.analyzeFromSource(source)
        Foo = info.package.findClass(name="Foo")
        Bar = info.package.findTrait(name="Bar")
        Baz = info.package.findTrait(name="Baz")
        self.assertEquals([ClassType(Foo), getRootClassType(), ClassType(Bar)], Baz.supertypes)

    def testTraitInheritFromClassAndSameTrait(self):
        source = "class Foo\n" + \
                 "trait Bar <: Foo\n" + \
                 "trait Baz <: Foo, Bar"
        info = self.analyzeFromSource(source)
        Foo = info.package.findClass(name="Foo")
        Bar = info.package.findTrait(name="Bar")
        Baz = info.package.findTrait(name="Baz")
        self.assertEquals([ClassType(Foo), getRootClassType(), ClassType(Bar)], Baz.supertypes)

    def testTraitInheritFromClassAndLowerTrait(self):
        source = "class Foo\n" + \
                 "class Quux <: Foo\n" + \
                 "trait Bar <: Quux\n" + \
                 "trait Baz <: Foo, Bar"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTraitInheritFromTraitAndClass(self):
        source = "class Foo\n" + \
                 "trait Bar\n" + \
                 "trait Baz <: Bar, Foo"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTraitInheritClassWithConflictingTypes(self):
        source = "class Foo[static T]\n" + \
                 "trait Bar <: Foo[Object]\n" + \
                 "trait Baz <: Foo[String], Bar"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTraitInheritTraitMultipleTimesDirectlySameTypes(self):
        source = "trait Foo[static T]\n" + \
                 "class Bar <: Foo[Object]\n" + \
                 "trait Baz <: Bar, Foo[Object]"
        info = self.analyzeFromSource(source)
        Foo = info.package.findTrait(name="Foo")
        Bar = info.package.findClass(name="Bar")
        Baz = info.package.findTrait(name="Baz")
        fooType = ClassType(Foo, (getRootClassType(),))
        self.assertEquals([ClassType(Bar), getRootClassType(), fooType], Baz.supertypes)

    def testTraitInheritTraitMultipleTimesDirectlyDifferentTypes(self):
        source = "trait Foo[static T]\n" + \
                 "class Bar <: Foo[Object]\n" + \
                 "trait Baz <: Bar, Foo[String]"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTraitInheritTraitMultipleTimesIndirectlySameTypes(self):
        source = "trait Foo[static T]\n" + \
                 "trait Bar <: Foo[Object]\n" + \
                 "class Baz <: Foo[Object]\n" + \
                 "trait Quux <: Baz, Bar"
        info = self.analyzeFromSource(source)
        Foo = info.package.findTrait(name="Foo")
        Bar = info.package.findTrait(name="Bar")
        Baz = info.package.findClass(name="Baz")
        Quux = info.package.findTrait(name="Quux")
        fooType = ClassType(Foo, (getRootClassType(),))
        self.assertEquals([ClassType(Baz), getRootClassType(), fooType, ClassType(Bar)],
                          Quux.supertypes)

    def testTraitInheritTraitMultipleTimesIndirectlyDifferentTypes(self):
        source = "trait Foo[static T]\n" + \
                 "trait Bar <: Foo[Object]\n" + \
                 "class Baz <: Foo[String]\n" + \
                 "trait Quux <: Baz, Bar"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTraitInheritRedundantTraits(self):
        source = "trait Foo\n" + \
                 "trait Bar <: Foo, Foo"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTraitInheritNullable(self):
        source = "trait Foo\n" + \
                 "trait Bar <: Foo?"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTraitInheritNothing(self):
        source = "trait Foo <: Nothing"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testInheritFromSelf(self):
        self.assertRaises(InheritanceException, self.analyzeFromSource, "class Foo <: Foo")

    def testInheritCycle(self):
        source = "class Foo <: Bar\n" + \
                 "class Bar <: Foo"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testInheritFromFinalClass(self):
        source = "final class Foo\n" + \
                 "class Bar <: Foo"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTypeParameterCycle(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo\n" + \
                 "def f[static T <: Bar >: Foo] = 12"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testTypeParameterCycleForeign(self):
        package = Package(name=Name(["foo"]))
        barClass = package.addClass(Name(["Bar"]), sourceName="Bar", typeParameters=[],
                                    supertypes=[getRootClassType()],
                                    constructors=[], fields=[],
                                    methods=[], flags=frozenset([PUBLIC]))
        loader = FakePackageLoader([package])
        source = "class Baz <: foo.Bar\n" + \
                 "def f[static T <: Baz >: foo.Bar] = {}"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source, packageLoader=loader)

    def testTypeParameterCycleExistential(self):
        source = "class Foo\n" + \
                 "class Bar <: Foo\n" + \
                 "let g: forsome [X <: Bar >: Foo] X"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

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
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testFieldsAndMethodsConflict(self):
        source = "class Foo\n" + \
                 "  var x\n" + \
                 "class Bar <: Foo\n" + \
                 "  def x = 12"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testMethodsDoNotConflict(self):
        source = "class Foo\n" + \
                 "  def f = 12\n" + \
                 "class Bar <: Foo\n" + \
                 "  override def f = 34"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[1])
        self.assertEquals(1, len(scope.getDefinition("f").overloads))

    def testOverrideBuiltinWithoutAttrib(self):
        source = "class Foo\n" + \
                 "  def typeof = 12"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testOverrideBuiltin(self):
        source = "class Foo\n" + \
                 "  override def to-string = \"Foo\""
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[0])
        self.assertEquals(1, len(scope.getDefinition("to-string").overloads))
        overrides = scope.getDefinition("to-string").overloads[0].irDefn.overrides
        self.assertEquals([getRootClass().findMethodBySourceName("to-string").id],
                          [o.id for o in overrides])

    def testMultipleMethodsOverrideSameMethod(self):
        source = "class A\n" + \
                 "class B <: A\n" + \
                 "abstract class Foo\n" + \
                 "  abstract def f(b: B): unit\n" + \
                 "class Bar <: Foo\n" + \
                 "  override def f(a: A) = {}\n" + \
                 "  override def f(o: Object) = {}"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testSameMethodOverridesMultipleMethodsInSameClass(self):
        source = "class A\n" + \
                 "class B\n" + \
                 "abstract class Foo\n" + \
                 "  abstract def f(a: A): unit\n" + \
                 "  abstract def f(b: B): unit\n" + \
                 "class Bar <: Foo\n" + \
                 "  override def f(o: Object) = {}"
        info = self.analyzeFromSource(source)
        FoofA = info.package.findFunction(name="Foo.f",
                                          pred=lambda f: f.variables[-1].sourceName == "a")
        FoofB = info.package.findFunction(name="Foo.f",
                                          pred=lambda f: f.variables[-1].sourceName == "b")
        Bar = info.package.findClass(name="Bar")
        Barf = info.package.findFunction(name="Bar.f")
        self.assertEquals([FoofA.id, FoofB.id], [o.id for o in Barf.overrides])
        self.assertEquals(Barf.id, FoofA.overriddenBy[Bar.id].id)
        self.assertEquals(Barf.id, FoofB.overriddenBy[Bar.id].id)

    def testSameMethodOverridesMultipleMethodsInDifferentClasses(self):
        source = "class A\n" + \
                 "class B <: A\n" + \
                 "abstract class Foo\n" + \
                 "  abstract def f(a: A): unit\n" + \
                 "abstract class Bar <: Foo\n" + \
                 "  abstract def f(b: B): unit\n" + \
                 "class Baz <: Bar\n" + \
                 "  override def f(o: Object) = {}"
        info = self.analyzeFromSource(source)
        Foof = info.package.findFunction(name="Foo.f")
        Barf = info.package.findFunction(name="Bar.f")
        Bazf = info.package.findFunction(name="Baz.f")
        Baz = info.package.findClass(name="Baz")
        self.assertEquals([Barf.id, Foof.id], [o.id for o in Bazf.overrides])
        self.assertEquals(Bazf.id, Foof.overriddenBy[Baz.id].id)
        self.assertEquals(Bazf.id, Barf.overriddenBy[Baz.id].id)

    def testSameMethodOverridesMultipleMethodsInDifferentTraits(self):
        source = "trait Tr1\n" + \
                 "  def f: unit\n" + \
                 "trait Tr2\n" + \
                 "  def f: unit\n" + \
                 "class Foo <: Tr1, Tr2\n" + \
                 "  override def f = {}"
        info = self.analyzeFromSource(source)
        Tr1f = info.package.findFunction(name="Tr1.f")
        Tr2f = info.package.findFunction(name="Tr2.f")
        Foof = info.package.findFunction(name="Foo.f")
        Foo = info.package.findClass(name="Foo")
        self.assertEquals([Tr1f.id, Tr2f.id], [o.id for o in Foof.overrides])
        self.assertEquals(Foof.id, Tr1f.overriddenBy[Foo.id].id)
        self.assertEquals(Foof.id, Tr2f.overriddenBy[Foo.id].id)

    def testMethodsCannotOverrideStaticMethodsWithOverride(self):
        source = "class Foo\n" + \
                 "  static def f = {}\n" + \
                 "class Bar <: Foo\n" + \
                 "  override def f = {}"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testMethodsCannotOverrideStaticMethodsWithoutOverride(self):
        source = "class Foo\n" + \
                 "  static def f = {}\n" + \
                 "class Bar <: Foo\n" + \
                 "  def f = {}"
        info = self.analyzeFromSource(source)
        Barf = info.package.findFunction(name="Bar.f")
        self.assertIsNone(Barf.overrides)

    def testMethodCannotOverrideFinalMethod(self):
        source = "class Foo\n" + \
                 "  final def f = {}\n" + \
                 "class Bar <: Foo\n" + \
                 "  override def f = {}"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testOverrideGrandParent(self):
        source = "abstract class Foo\n" + \
                 "  abstract def f: unit\n" + \
                 "abstract class Bar <: Foo\n" + \
                 "class Baz <: Bar\n" + \
                 "  override def f = {}"
        info = self.analyzeFromSource(source)
        Foof = info.package.findFunction(name="Foo.f")
        Bazf = info.package.findFunction(name="Baz.f")
        Baz = info.package.findClass(name="Baz")
        self.assertEquals([Foof.id], [o.id for o in Bazf.overrides])
        self.assertEquals(Bazf.id, Foof.overriddenBy[Baz.id].id)

    def testMethodOverridesNothing(self):
        source = "class Foo\n" + \
                 "  override def f = {}"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testMethodOverridesIncompatible(self):
        source = "class Foo\n" + \
                 "  def f = {}\n" + \
                 "class Bar <: Foo\n" + \
                 "  override def f(o: Object) = o"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testMethodOverridesInSameClass(self):
        source = "class Foo\n" + \
                 "  def f(s: String) = {}\n" + \
                 "  override def f(o: Object) = {}"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testOverrideMethodWithOverloadInBaseClass(self):
        source = "class Foo\n" + \
                 "  def f = {}\n" + \
                 "  def f(o: Object) = {}\n" + \
                 "class Bar <: Foo\n" + \
                 "  override def f = {}"
        info = self.analyzeFromSource(source)
        Foof = info.package.findFunction(name="Foo.f",
                                         pred=lambda f: len(f.parameterTypes) == 1)
        Barf = info.package.findFunction(name="Bar.f")
        Bar = info.package.findClass(name="Bar")
        self.assertEquals([Foof.id], [o.id for o in Barf.overrides])
        self.assertEquals(Barf.id, Foof.overriddenBy[Bar.id].id)

    def testStaticMethodDoesNotOverrideStatic(self):
        source = "class Foo\n" + \
                 "  static def f = 12\n" + \
                 "class Bar <: Foo\n" + \
                 "  static def f = 34"
        info = self.analyzeFromSource(source)
        Foof = info.package.findFunction(name="Foo.f")
        Barf = info.package.findFunction(name="Bar.f")
        self.assertIsNone(Barf.overrides)
        self.assertIsNone(Foof.overriddenBy)

    def testStaticMethodDoesNotOverrideNormal(self):
        source = "class Foo\n" + \
                 "  def f = 12\n" + \
                 "class Bar <: Foo\n" + \
                 "  static def f = 12"
        info = self.analyzeFromSource(source)
        Foof = info.package.findFunction(name="Foo.f")
        Barf = info.package.findFunction(name="Bar.f")
        self.assertIsNone(Barf.overrides)
        self.assertEquals({}, Foof.overriddenBy)

    def testDerivedArrayClassHasArrayFlag(self):
        source = "class Array\n" + \
                 "  arrayelements Object, get, set, length\n" + \
                 "class Derived <: Array"
        info = self.analyzeFromSource(source)
        Derived = info.package.findClass(name="Derived")
        self.assertIn(ARRAY, Derived.flags)

    def testDerivedFinalArrayClassHasFinalArrayFlag(self):
        source = "class Array\n" + \
                 "  final arrayelements Object, get, set, length\n" + \
                 "class Derived <: Array"
        info = self.analyzeFromSource(source)
        Derived = info.package.findClass(name="Derived")
        self.assertTrue(ARRAY_FINAL in Derived.flags)

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

    def testInheritStaticMethod(self):
        source = "class A\n" + \
                 "  static def f = 12\n" + \
                 "class B <: A"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[1])
        self.assertTrue(scope.isBound("f"))

    def testInheritedStaticMethodDoesntConflictWithStaticMethod(self):
        source = "class A\n" + \
                 "  static def f = 12\n" + \
                 "class B <: A\n" + \
                 "  static def f = 34"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[1])
        self.assertEquals(2, len(scope.getDefinition("f").overloads))

    def testInheritedStaticMethodDoesntConflictWithNormalMethod(self):
        source = "class A\n" + \
                 "  static def f = 12\n" + \
                 "class B <: A\n" + \
                 "  def f = 34"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[1])
        self.assertEquals(2, len(scope.getDefinition("f").overloads))

    def testInheritFromImportedClass(self):
        foo = Package(name=Name(["foo"]))
        Bar = foo.addClass(Name(["Bar"]), sourceName="Bar", typeParameters=[],
                           supertypes=[getRootClassType()],
                           constructors=[], fields=[],
                           methods=[], flags=frozenset([PUBLIC]))
        x = foo.newField(Name(["Bar", "x"]), sourceName="x", flags=frozenset([PUBLIC, LET]))
        Bar.fields.append(x)

        source = "import foo.Bar\n" + \
                 "class Baz <: Bar"
        info = self.analyzeFromSource(source, packageLoader=FakePackageLoader([foo]))
        bazScope = info.getScope(info.ast.modules[0].definitions[1])
        self.assertTrue(bazScope.isBound("x"))

    def testMethodInheritedOnceAlongMultiplePaths(self):
        source = "trait Foo\n" + \
                 "class Bar <: Object, Foo"
        info = self.analyzeFromSource(source)
        scope = info.getScope(info.ast.modules[0].definitions[1])
        self.assertEquals(1, len([b for b in scope.iterBindings() if b[0] == "to-string"]))

    def testMethodOverriddenOnceAlongMultiplePaths(self):
        source = "trait Foo\n" + \
                 "class Bar <: Object, Foo\n" + \
                 "  public override def to-string = \"Bar\""
        info = self.analyzeFromSource(source)
        ObjectToString = getRootClass().findMethodBySourceName("to-string")
        Bar = info.package.findClass(name="Bar")
        BarToString = info.package.findFunction(name="Bar.to-string")
        self.assertEquals(1, len(BarToString.overrides))
        self.assertIs(ObjectToString, BarToString.overrides[0])
        self.assertIs(BarToString, ObjectToString.overriddenBy[Bar.id])

    def testAbstractClassMethodNotOverriddenInConcreteClass(self):
        source = "abstract class Foo\n" + \
                 "  abstract def f: unit\n" + \
                 "class Bar <: Foo"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

    def testAbstractTraitMethodNotOverriddenInConcreteClass(self):
        source = "trait Foo\n" + \
                 "  def f: unit\n" + \
                 "class Bar <: Foo"
        self.assertRaises(InheritanceException, self.analyzeFromSource, source)

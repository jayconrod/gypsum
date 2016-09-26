# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from builtins import *
from compile_info import CompileInfo
from errors import *
from flatten import flattenTypeDefinitions
from ids import *
from inheritance_analysis import *
from ir import *
from ir_types import *
from layout import layout
from lexer import *
from parser import *
from scope_analysis import *
from type_analysis import *
from utils_test import FakePackageLoader

class TestFlatten(unittest.TestCase):
    def setUp(self):
        self.rootMethodNames = [m.name.short() for m in getRootClass().methods]

    def analyzeFromSource(self, source):
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        package = Package(TARGET_PACKAGE_ID)
        packageLoader = FakePackageLoader([])
        info = CompileInfo(ast, package, packageLoader, isUsingStd=False)
        analyzeDeclarations(info)
        analyzeTypeDeclarations(info)
        analyzeInheritance(info)
        analyzeTypes(info)
        convertClosures(info)
        flattenTypeDefinitions(info)
        return info

    def checkMethodNames(self, expected, irDefn):
        self.assertEquals(expected, [m.sourceName for m in irDefn.methods])

    def checkTraitMethodNames(self, expected, irClass, package):
        traitMethodNames = {}
        for traitId, methods in irClass.traits.iteritems():
            traitName = package.getDefn(traitId).sourceName
            methodNames = [m.sourceName for m in methods]
            traitMethodNames[traitName] = methodNames
        self.assertEquals(expected, traitMethodNames)

    def testSimpleClass(self):
        info = self.analyzeFromSource("class C")
        C = info.package.findClass(name="C")
        self.checkMethodNames(self.rootMethodNames, C)

    def testDerivedClassWithMethods(self):
        source = "class A\n" + \
                 "  def f = 12\n" + \
                 "class B <: A\n" + \
                 "  def g = 12\n"
        info = self.analyzeFromSource(source)
        B = info.package.findClass(name="B")
        self.checkMethodNames(self.rootMethodNames + ["f", "g"], B)

    def testDerivedClassWithOverloads(self):
        source = "class A\n" + \
                 "  def f(x: i32) = x\n" + \
                 "class B <: A\n" + \
                 "  def f(x: i64) = x\n"
        info = self.analyzeFromSource(source)
        B = info.package.findClass(name="B")
        self.checkMethodNames(self.rootMethodNames + ["f", "f"], B)

    def testDerivedClassWithOverride(self):
        source = "class A\n" + \
                 "  def f(x: i32) = x\n" + \
                 "class B <: A\n" + \
                 "  override def f(x: i32) = 12i32\n"
        info = self.analyzeFromSource(source)
        B = info.package.findClass(name="B")
        self.checkMethodNames(self.rootMethodNames + ["f"], B)

    def testSimpleTrait(self):
        source = "trait Tr\n" + \
                 "  def f = 12\n" + \
                 "class C <: Tr\n" + \
                 "  def g = 34"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.checkMethodNames(self.rootMethodNames + ["f", "g"], C)
        self.checkTraitMethodNames({"Tr": self.rootMethodNames + ["f"]}, C, info.package)

    def testTraitOverrideBaseClassMethod(self):
        source = "trait Tr\n" + \
                 "  override def to-string = \"Tr\"\n" + \
                 "class C <: Tr"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.checkMethodNames(self.rootMethodNames, C)
        self.checkTraitMethodNames({"Tr": self.rootMethodNames}, C, info.package)
        Tr = info.package.findTrait(name="Tr")
        toString = info.package.findFunction(name="Tr.to-string")
        CTrToString = next(m for m in C.traits[Tr.id] if m.sourceName == "to-string")
        self.assertIs(toString, CTrToString)

    def testTraitAndClassOverrideBaseClassMethod(self):
        source = "trait Tr\n" + \
                 "  override def to-string = \"Tr\"\n" + \
                 "class C <: Tr\n" + \
                 "  override def to-string = \"C\""
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.checkMethodNames(self.rootMethodNames, C)
        self.checkTraitMethodNames({"Tr": self.rootMethodNames}, C, info.package)
        Tr = info.package.findTrait(name="Tr")
        toString = info.package.findFunction(name="C.to-string")
        self.assertTrue(any(m.id is toString.id for m in C.methods))
        CTrToString = next(m for m in C.traits[Tr.id] if m.sourceName == "to-string")
        self.assertIs(toString, CTrToString)

    def testTraitAndBaseClassOverrideHigherBaseClassMethod(self):
        source = "class Base\n" + \
                 "  override def to-string = \"Base\"\n" + \
                 "trait Tr\n" + \
                 "  override def to-string = \"Tr\"\n" + \
                 "class C <: Base, Tr"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        Tr = info.package.findTrait(name="Tr")
        toString = info.package.findFunction(name="Tr.to-string")
        self.assertTrue(any(m.id is toString.id for m in C.methods))
        CTrToString = next(m for m in C.traits[Tr.id] if m.sourceName == "to-string")
        self.assertIs(toString, CTrToString)

    def testTraitInheritedThroughMultiplePaths(self):
        source = "trait Tr1\n" + \
                 "  def f = {}\n" + \
                 "trait Tr2 <: Tr1\n" + \
                 "  override def f = {}\n" + \
                 "class C <: Tr2, Tr1"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.checkMethodNames(self.rootMethodNames + ["f"], C)
        Tr1 = info.package.findTrait(name="Tr1")
        Tr2 = info.package.findTrait(name="Tr2")
        f = info.package.findFunction(name="Tr2.f")
        self.assertIs(f, C.methods[-1])
        self.assertIs(f, C.traits[Tr1.id][-1])
        self.assertIs(f, C.traits[Tr2.id][-1])

    def testMultipleTraitsOverrideSameMethodInBaseTrait(self):
        source = "trait Base\n" + \
                 "  def f = {}\n" + \
                 "trait Tr1 <: Base\n" + \
                 "  override def f = {}\n" + \
                 "trait Tr2 <: Base\n" + \
                 "  override def f = {}\n" + \
                 "class C <: Tr1, Tr2"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.checkMethodNames(self.rootMethodNames + ["f"], C)
        Base = info.package.findTrait(name="Base")
        Tr1 = info.package.findTrait(name="Tr1")
        Tr2 = info.package.findTrait(name="Tr2")
        f = info.package.findFunction(name="Tr2.f")
        self.assertIs(f, C.methods[-1])
        self.assertIs(f, C.traits[Base.id][-1])
        self.assertIs(f, C.traits[Tr1.id][-1])
        self.assertIs(f, C.traits[Tr2.id][-1])

    def testRedundantTraitOverrideNotEffective(self):
        source = "trait Base\n" + \
                 "  def f = {}\n" + \
                 "trait Tr1 <: Base\n" + \
                 "  override def f = {}\n" + \
                 "trait Tr2 <: Base\n" + \
                 "  override def f = {}\n" + \
                 "trait Tr1Proxy <: Tr1\n" + \
                 "class C <: Tr1Proxy, Tr2, Tr1"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.checkMethodNames(self.rootMethodNames + ["f"], C)
        f = info.package.findFunction(name="Tr2.f")
        self.assertIs(f, C.methods[-1])
        for traitName in ("Base", "Tr1", "Tr2"):
            trait = info.package.findTrait(name=traitName)
            self.assertIs(f, C.traits[trait.id][-1])

    def testMethodIndependentlyOverridesClassAndTrait(self):
        source = "class Base\n" + \
                 "  def f = {}\n" + \
                 "trait Tr\n" + \
                 "  def f = {}\n" + \
                 "class C <: Base, Tr\n" + \
                 "  override def f = {}"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.checkMethodNames(self.rootMethodNames + ["f", "f"], C)
        f = info.package.findFunction(name="C.f")
        self.assertIs(f, C.methods[-1])
        self.assertIs(f, C.methods[-2])
        Tr = info.package.findTrait(name="Tr")
        self.assertIs(f, C.traits[Tr.id][-1])

    def testMethodIndependentlyOverridesTraits(self):
        source = "trait Tr1\n" + \
                 "  def f = {}\n" + \
                 "trait Tr2\n" + \
                 "  def f = {}\n" + \
                 "class C <: Tr1, Tr2\n" + \
                 "  override def f = {}"
        info = self.analyzeFromSource(source)
        C = info.package.findClass(name="C")
        self.checkMethodNames(self.rootMethodNames + ["f", "f"], C)
        f = info.package.findFunction(name="C.f")
        self.assertIs(f, C.methods[-1])
        self.assertIs(f, C.methods[-2])
        Tr1 = info.package.findTrait(name="Tr1")
        self.assertIs(f, C.traits[Tr1.id][-1])
        Tr2 = info.package.findTrait(name="Tr2")
        self.assertIs(f, C.traits[Tr2.id][-1])

    def testClassInheritedFieldsAreSubstituted(self):
        source = "class Box[static T]\n" + \
                 "  var value: T\n" + \
                 "class StringBox <: Box[String]"
        info = self.analyzeFromSource(source)
        field = info.package.findClass(name="StringBox").fields[-1]
        self.assertEquals(getStringType(), field.type)

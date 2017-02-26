# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from builtins import *
from compile_info import CompileInfo, STD_NAME
from errors import *
from flags import LET
from ids import *
from inheritance_analysis import analyzeInheritance
from ir import *
from ir_types import *
from layout import layout
from lexer import *
from parser import *
from scope_analysis import *
from type_analysis import *
from utils_test import (
    FUNCTION_SOURCE,
    FakePackageLoader,
    TestCaseWithDefinitions,
)
from name import (
    CLOSURE_SUFFIX,
    CONTEXT_SUFFIX,
    Name,
    RECEIVER_SUFFIX,
)


class TestClosureConversion(TestCaseWithDefinitions):
    def analyzeFromSource(self, source, name=None):
        filename = "(test)"
        rawTokens = lex(filename, source)
        layoutTokens = layout(rawTokens)
        ast = parse(filename, layoutTokens)
        if name is None:
            name = Name(["test"])
        package = Package(TARGET_PACKAGE_ID, name=name)
        packageLoader = FakePackageLoader([])
        info = CompileInfo(ast, package, packageLoader, isUsingStd=False)
        analyzeDeclarations(info)
        analyzeTypeDeclarations(info)
        analyzeInheritance(info)
        analyzeTypes(info)
        convertClosures(info)
        return info

    def testUseFunctionVarInFunction(self):
        source = "def f =\n" + \
                 "  var x = 12\n" + \
                 "  def g = x\n"
        info = self.analyzeFromSource(source)
        fAst = info.ast.modules[0].definitions[0]
        fScopeId = info.getScope(info.getDefnInfo(fAst).irDefn).scopeId
        gAst = fAst.body.statements[1]
        gScopeId = info.getScope(info.getDefnInfo(gAst).irDefn).scopeId

        fContextInfo = info.getContextInfo(fScopeId)
        fContextClass = fContextInfo.irContextClass
        self.assertEquals([self.makeField("f.x", type=I64Type)], fContextClass.fields)
        xDefnInfo = info.getDefnInfo(fAst.body.statements[0].pattern)
        self.assertEquals(self.makeField("f.x", type=I64Type), xDefnInfo.irDefn)
        self.assertIs(xDefnInfo, info.getUseInfo(gAst.body).defnInfo)
        gClosureInfo = info.getClosureInfo(gScopeId)
        gClosureClass = gClosureInfo.irClosureClass
        self.assertEquals([self.makeField(Name(["f", "g", CLOSURE_SUFFIX, CONTEXT_SUFFIX]),
                                          type=ClassType(fContextClass))],
                          gClosureClass.fields)

        self.assertEquals(self.makeVariable(Name(["f", "g"]), type=ClassType(gClosureClass)),
                          gClosureInfo.irClosureVar)

    def testUseFieldInMethod(self):
        source = "class C\n" + \
                 "  var x = 12\n" + \
                 "  def f = x"
        info = self.analyzeFromSource(source)
        cAst = info.ast.modules[0].definitions[0]
        C = info.package.findClass(name="C")
        cScopeId = info.getScope(C).scopeId
        cContextInfo = info.getContextInfo(cScopeId)
        self.assertIs(C, cContextInfo.irContextClass)
        xDefnInfo = info.getDefnInfo(cAst.members[0].pattern)
        xUseInfo = info.getUseInfo(cAst.members[1].body)
        self.assertIs(xDefnInfo, xUseInfo.defnInfo)

    def testUseMethodInMethod(self):
        source = "class C\n" + \
                 "  def f = 12\n" + \
                 "  def g = f\n"
        info = self.analyzeFromSource(source)
        cAst = info.ast.modules[0].definitions[0]
        C = info.package.findClass(name="C")
        cScopeId = info.getScope(C).scopeId
        cContextInfo = info.getContextInfo(cScopeId)
        self.assertIs(C, cContextInfo.irContextClass)
        fDefnInfo = info.getDefnInfo(cAst.members[0])
        fUseInfo = info.getUseInfo(cAst.members[1].body)
        self.assertIs(fDefnInfo, fUseInfo.defnInfo)

    def testCaptureThis(self):
        source = "class C\n" + \
                 "  def f =\n" + \
                 "    def g = this"
        info = self.analyzeFromSource(source)
        cAst = info.ast.modules[0].definitions[0]
        C = info.package.findClass(name="C")
        CType = ClassType(C)
        f = info.package.findFunction(name="C.f")
        fScopeId = info.getScope(f).scopeId
        fContextInfo = info.getContextInfo(fScopeId)
        fContextClass = info.package.findClass(name=Name(["C", "f", CONTEXT_SUFFIX]))
        self.assertIs(fContextClass, fContextInfo.irContextClass)
        self.assertEquals(1, len(fContextClass.constructors))
        self.assertEquals([self.makeField(Name(["C", "f", RECEIVER_SUFFIX]),
                                          type=CType, flags=frozenset([LET]))],
                          fContextClass.fields)
        g = info.package.findFunction(name="C.f.g")
        gScopeId = info.getScope(g).scopeId
        gClosureInfo = info.getClosureInfo(gScopeId)
        gClosureClass = info.package.findClass(name=Name(["C", "f", "g", CLOSURE_SUFFIX]))
        self.assertIs(gClosureClass, gClosureInfo.irClosureClass)
        self.assertEquals({fScopeId: gClosureClass.fields[0]}, gClosureInfo.irClosureContexts)
        self.assertTrue(gClosureInfo.irClosureVar in f.variables)
        self.assertEquals(1, len(gClosureClass.constructors))
        self.assertEquals([ClassType(gClosureClass), ClassType(fContextClass)],
                          gClosureClass.constructors[0].parameterTypes)
        self.assertEquals([self.makeField(Name(["C", "f", "g", CLOSURE_SUFFIX, CONTEXT_SUFFIX]),
                                          type=ClassType(fContextClass))],
                          gClosureClass.fields)

    def testFunctionTraitNotUsedWithoutStd(self):
        source = "def f(x: i64) =\n" + \
                 "  def g = {x; Object();}\n" + \
                 "  {}"
        info = self.analyzeFromSource(source)
        gClosureClass = info.package.findClass(name=Name(["f", "g", CLOSURE_SUFFIX]))
        self.assertEquals([getRootClassType()], gClosureClass.supertypes)

    def testFunctionTraitUsedWithFewObjectParams(self):
        source = FUNCTION_SOURCE + \
                 "def f(x: i64) =\n" + \
                 "  def g(a: Object, b: Object) = {x; Object();}\n" + \
                 "  {}"
        info = self.analyzeFromSource(source, name=STD_NAME)
        gClosureClass = info.package.findClass(name=Name(["f", "g", CLOSURE_SUFFIX]))
        functionTrait = info.package.findTrait(name="Function2")
        rootType = getRootClassType()
        functionTraitType = ClassType(functionTrait, (rootType, rootType, rootType))
        self.assertEquals([getRootClassType(), functionTraitType], gClosureClass.supertypes)

    def testFunctionTraitNotUsedWithPrimitiveParams(self):
        source = FUNCTION_SOURCE + \
                 "def f(x: i64) =\n" + \
                 "  def g(a: i64) = {x; Object();}\n" + \
                 "  {}"
        info = self.analyzeFromSource(source, name=STD_NAME)
        gClosureClass = info.package.findClass(name=Name(["f", "g", CLOSURE_SUFFIX]))
        self.assertEquals([getRootClassType()], gClosureClass.supertypes)

    def testFunctionTraitNotUsedWithPrimitiveReturn(self):
        source = FUNCTION_SOURCE + \
                 "def f(x: i64) =\n" + \
                 "  def g = x\n" + \
                 "  {}"
        info = self.analyzeFromSource(source, name=STD_NAME)
        gClosureClass = info.package.findClass(name=Name(["f", "g", CLOSURE_SUFFIX]))
        self.assertEquals([getRootClassType()], gClosureClass.supertypes)

    def testFunctionTraitNotUsedWithManyParams(self):
        source = FUNCTION_SOURCE + \
                 "def f(x: i64) =\n" + \
                 "  def g(a: Object, b: Object, c: Object) = {x; Object();}\n" + \
                 "  {}"
        info = self.analyzeFromSource(source, name=STD_NAME)
        gClosureClass = info.package.findClass(name=Name(["f", "g", CLOSURE_SUFFIX]))
        self.assertEquals([getRootClassType()], gClosureClass.supertypes)

    def testClosureMethodFlagsPrimitive(self):
        source = "def f(x: i64) =\n" + \
                 "  def g = x\n" + \
                 "  {}"
        info = self.analyzeFromSource(source)
        g = info.package.findFunction(name="f.g")
        self.assertEquals(frozenset([PUBLIC, METHOD, FINAL]), g.flags)

    def testClosureMethodFlagsFunction(self):
        source = FUNCTION_SOURCE + \
                 "def f(s: String) =\n" + \
                 "  def g = s\n" + \
                 "  {}"
        info = self.analyzeFromSource(source, name=STD_NAME)
        g = info.package.findFunction(name="f.g")
        call = info.package.findFunction(name="Function0.call")
        self.assertEquals(frozenset([PUBLIC, METHOD, FINAL, OVERRIDE]), g.flags)
        self.assertEquals(1, len(g.overrides))
        self.assertIs(call, g.overrides[0])

    def testClosureWithTypeParametersNotFunction(self):
        source = FUNCTION_SOURCE + \
                 "def f(s: String) =\n" + \
                 "  def g[static T] = s\n" + \
                 "  {}"
        info = self.analyzeFromSource(source, name=STD_NAME)
        gClosureClass = info.package.findClass(name=Name(["f", "g", CLOSURE_SUFFIX]))
        self.assertEquals([getRootClassType()], gClosureClass.supertypes)


if __name__ == "__main__":
    unittest.main()

# Copyright 2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from ir import Global, Function, Class, Package, TypeParameter, Variable, Field, LOCAL
from ir_types import getNothingClassType, getRootClassType
from utils import Counter


class TestCaseWithDefinitions(unittest.TestCase):
    def setUp(self):
        self.globalCounter = Counter()
        self.functionCounter = Counter()
        self.classCounter = Counter()
        self.typeParameterCounter = Counter()

    def tearDown(self):
        self.globalCounter = None
        self.functionCounter = None
        self.classCounter = None
        self.typeParameterCounter = None

    def makeGlobal(self, name, **args):
        defaultValues = {"astDefn": None,
                         "type": None,
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        args["id"] = self.globalCounter()
        return Global(name, **args)

    def makeFunction(self, name, **args):
        defaultValues = {"astDefn": None,
                         "returnType": None,
                         "typeParameters": [],
                         "parameterTypes": None,
                         "variables": [],
                         "blocks": None,
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        args["id"] = self.functionCounter()
        return Function(name, **args)

    def makeClass(self, name, **args):
        defaultValues = {"astDefn": None,
                         "typeParameters": [],
                         "supertypes": None,
                         "initializer": None,
                         "constructors": [],
                         "fields": [],
                         "methods": [],
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        args["id"] = self.classCounter()
        return Class(name, **args)

    def makeTypeParameter(self, name, **args):
        import builtins
        defaultValues = {"astDefn": None,
                         "upperBound": getRootClassType(),
                         "lowerBound": getNothingClassType(),
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        args["id"] = self.typeParameterCounter()
        return TypeParameter(name, **args)

    def makeVariable(self, name, **args):
        defaultValues = {"astDefn": None,
                         "type": None,
                         "kind": LOCAL,
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        return Variable(name, **args)

    def makeField(self, name, **args):
        defaultValues = {"astDefn": None,
                         "type": None,
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        return Field(name, **args)

    def fillDefaultValues(self, args, values):
        for k, v in values.iteritems():
            if k not in args:
                args[k] = v


class MockPackageLoader(object):
    def __init__(self, packageNames):
        self.packageNames = packageNames

    def getPackageNames(self):
        return self.packageNames

    def loadPackage(self, name):
        return Package()

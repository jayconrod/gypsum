# Copyright 2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from ir import Global, Function, Class, Package, Name, PackagePrefix, TypeParameter, Variable, Field, LOCAL
from ir_types import getNothingClassType, getRootClassType
from package_loader import BasePackageLoader
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
        name = self.makeName(name)
        defaultValues = {"astDefn": None,
                         "type": None,
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        args["id"] = self.globalCounter()
        return Global(name, **args)

    def makeFunction(self, name, **args):
        name = self.makeName(name)
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
        name = self.makeName(name)
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
        name = self.makeName(name)
        defaultValues = {"astDefn": None,
                         "upperBound": getRootClassType(),
                         "lowerBound": getNothingClassType(),
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        args["id"] = self.typeParameterCounter()
        return TypeParameter(name, **args)

    def makeVariable(self, name, **args):
        name = self.makeName(name)
        defaultValues = {"astDefn": None,
                         "type": None,
                         "kind": LOCAL,
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        return Variable(name, **args)

    def makeField(self, name, **args):
        name = self.makeName(name)
        defaultValues = {"astDefn": None,
                         "type": None,
                         "flags": frozenset()}
        self.fillDefaultValues(args, defaultValues)
        return Field(name, **args)

    def fillDefaultValues(self, args, values):
        for k, v in values.iteritems():
            if k not in args:
                args[k] = v

    def makeName(self, name):
        if isinstance(name, str):
            name = Name.fromString(name)
        assert isinstance(name, Name)
        return name


class FakePackageLoader(BasePackageLoader):
    def __init__(self, packagesOrPackageNames):
        super(FakePackageLoader, self).__init__()
        if len(packagesOrPackageNames) == 0:
            self.packageNames = []
            self.packages = {}
        elif isinstance(packagesOrPackageNames[0], Package):
            self.packageNames = [p.name for p in packagesOrPackageNames]
            self.packages = {p.name: p for p in packagesOrPackageNames}
        else:
            assert isinstance(packagesOrPackageNames[0], Name)
            self.packageNames = packagesOrPackageNames
            self.packages = {name: Package(name=name) for name in packagesOrPackageNames}
        self.loadedIds = set()

    def getPackageNames(self):
        return self.packageNames

    def isPackage(self, name):
        return name in self.packageNames

    def loadPackage(self, name, loc):
        assert name in self.packageNames
        if name not in self.packages:
            self.packages[name] = Package(name=name)
        package = self.packages[name]

        if package.id not in self.loadedIds:
            self.loadedIds.add(package.id)
            for dep in package.dependencies:
                dep.package = self.loadPackage(dep.name, loc)
            self.runLoadHooks(package)
        return package

    def getLoadedPackages(self):
        return map(self.getPackageById, self.loadedIds)

    def getPackageById(self, id):
        return next(package for package in self.packages.itervalues() if package.id is id)

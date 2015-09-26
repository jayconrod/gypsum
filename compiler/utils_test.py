# Copyright 2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from ir import Class, Global, Field, Function, IrTopDefn, LOCAL, Name, Package, PackagePrefix, TypeParameter, Variable
from ir_types import getNothingClassType, getRootClassType
from package_loader import BasePackageLoader
from utils import Counter, reprFormat


OPTION_SOURCE = "public class Option[static +T]\n" + \
                "public class Some[static +T](value: T) <: Option[T]\n" + \
                "  public def get = value\n" + \
                "  public static def try-match(obj: Object): Option[Object] =\n" + \
                "    match (obj)\n" + \
                "      case some: Some[_] => some\n" + \
                "      case _ => None\n" + \
                "class None-class <: Option[Nothing]\n" + \
                "public let None: Option[Nothing] = None-class()\n"

TUPLE_SOURCE = "class Tuple2[static +T1, static +T2](public _1: T1, public _2: T2)\n"

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
        return TestDefn(self, Global, name, **args)

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
        return TestDefn(self, TypeParameter, name, **args)

    def makeVariable(self, name, **args):
        name = self.makeName(name)
        return TestDefn(self, Variable, name, **args)

    def makeField(self, name, **args):
        name = self.makeName(name)
        return TestDefn(self, Field, name, **args)

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


class TestDefn(object):
    """A stand-in for definitions in comparisons.

    Very frequently in unit tests, we need to compare an expected definition to an actual
    definition. Instances of this class can take the place of expected definitions. Objects
    are constructed with a name and a list of key-value pairs. These pairs are compared with
    attributes of the "actual" object. Other attributes are ignored.

    Attributes:
        test (unittest.TestCase): the test that uses this definition. Used to call
            `assertEqual` and `assertNotEqual`.
        clas (class): the expected class.
        name (Name): the expected name
        properties (dict[str, *]): the properties to check during comparisons.
    """

    def __init__(self, test, clas, name, **kwargs):
        self.test = test
        self.clas = clas
        if issubclass(clas, IrTopDefn) and "id" not in kwargs:
            id = None
            self.fake = clas(name, id, **kwargs)
        else:
            self.fake = clas(name, **kwargs)
        self.propNames = frozenset(kwargs.keys()) | frozenset(["name"])

    def __repr__(self):
        pairStrs = ("%s=%s" % (key, getattr(self.fake, key)) for key in self.propNames)
        return "Test%s(%s)" % (self.clas.__name__, ", ".join(pairStrs))

    def __eq__(self, other):
        if self.clas is not other.__class__:
            # This assertion should throw.
            self.test.assertEquals(self.clas, other.__class__)
            return False
        for key in self.propNames:
            value = getattr(self.fake, key)
            otherValue = getattr(other, key)
            if value != otherValue:
                # This assertion should throw.
                self.test.assertEqual(value, otherValue,
                                      "for key '%s': %s != %s" % (key, value, otherValue))
                return False
        return True

    def __ne__(self, other):
        return any(getattr(self.fake, k) != getattr(other, k) for k in self.propNames)

    def __getattr__(self, name):
        return getattr(self.fake, name)

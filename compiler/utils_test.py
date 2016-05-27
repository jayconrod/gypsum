# Copyright 2015-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from ids import DefnId, TARGET_PACKAGE_ID
from ir import Class, Global, Field, Function, IrTopDefn, LOCAL, Name, Package, PackagePrefix, Trait, TypeParameter, Variable
from ir_types import getNothingClassType, getRootClassType
from location import NoLoc
from package_loader import BasePackageLoader
from utils import Counter, reprFormat


OPTION_SOURCE = "public abstract class Option[static +T]\n" + \
                "  public abstract def is-defined: boolean\n" + \
                "  public abstract def get: T\n" + \
                "public class Some[static +T](value: T) <: Option[T]\n" + \
                "  public override def is-defined = true\n" + \
                "  public override def get = value\n" + \
                "  public static def try-match(obj: Object): Option[Object] =\n" + \
                "    match (obj)\n" + \
                "      case some: Some[_] => some\n" + \
                "      case _ => None\n" + \
                "class None-class <: Option[Nothing]\n" + \
                "  public override def is-defined = false\n" + \
                "  public override def get = throw Exception()\n" + \
                "public let None: Option[Nothing] = None-class()\n"

TUPLE_SOURCE = "class Tuple2[static +T1, static +T2](public _1: T1, public _2: T2)\n"

class TestCaseWithDefinitions(unittest.TestCase):
    def setUp(self):
        self.globalCounter = Counter()
        self.functionCounter = Counter()
        self.classCounter = Counter()
        self.traitCounter = Counter()
        self.typeParameterCounter = Counter()

    def tearDown(self):
        self.globalCounter = None
        self.functionCounter = None
        self.classCounter = None
        self.traitCounter = None
        self.typeParameterCounter = None

    def makeGlobal(self, name, **args):
        name = self.makeName(name)
        id = DefnId(TARGET_PACKAGE_ID, DefnId.GLOBAL, self.globalCounter())
        return TestGlobal(self, name, id, **args)

    def makeFunction(self, name, **args):
        name = self.makeName(name)
        id = DefnId(TARGET_PACKAGE_ID, DefnId.FUNCTION, self.functionCounter())
        return TestFunction(self, name, id, **args)

    def makeClass(self, name, **args):
        name = self.makeName(name)
        id = DefnId(TARGET_PACKAGE_ID, DefnId.CLASS, self.classCounter())
        return TestClass(self, name, id, **args)

    def makeTrait(self, name, **args):
        name = self.makeName(name)
        id = DefnId(TARGET_PACKAGE_ID, DefnId.TRAIT, self.traitCounter())
        return TestTrait(self, name, id, **args)

    def makeTypeParameter(self, name, **args):
        name = self.makeName(name)
        id = DefnId(TARGET_PACKAGE_ID, DefnId.TYPE_PARAMETER, self.typeParameterCounter())
        return TestTypeParameter(self, name, id, **args)

    def makeVariable(self, name, **args):
        name = self.makeName(name)
        return TestVariable(self, name, **args)

    def makeField(self, name, **args):
        name = self.makeName(name)
        return TestField(self, name, **args)

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

    def loadPackage(self, name, loc=NoLoc):
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
    """A mix-in for fake definitions in comparisons.

    Very frequently in unit tests, we need to compare an expected definition to an actual
    definition. Instances of this class can take the place of expected definitions. Objects
    are constructed with a name and a list of key-value pairs. These pairs are compared with
    attributes of the "actual" object. Other attributes are ignored.

    Attributes:
        test (unittest.TestCase): the test that uses this definition. Used to call
            `assertEqual` and `assertNotEqual`.
        propNames (frozenset[str]): a set of property names specified when creating this
            definition. Only these properties are checked for equality.

        name (Name): the expected name
        properties (dict[str, *]): the properties to check during comparisons.
    """

    def __init__(self, test, name, _id=None, **kwargs):
        """The actual init function intended to be called when a test definition is constructed.

        This will call the real init function, and will also set some internal properties."""
        if isinstance(self, IrTopDefn) and "id" not in kwargs:
            super(TestDefn, self).__init__(name, _id, **kwargs)
        else:
            super(TestDefn, self).__init__(name, **kwargs)
        self.test = test
        self.propNames = frozenset(kwargs.keys())

    def __repr__(self):
        pairStrs = ("%s=%s" % (key, getattr(self, key)) for key in self.propNames)
        return "%s(%s)" % (self.__class__.__name__, ", ".join(pairStrs))

    def __eq__(self, other):
        if self.__class__.__bases__[1] is not other.__class__:
            # This assertion should throw.
            self.test.assertEquals(self.__class__.__bases__[1], other.__class__)
            return False
        for key in self.propNames:
            value = getattr(self, key)
            otherValue = getattr(other, key)
            if value != otherValue:
                # This assertion should throw.
                self.test.assertEqual(value, otherValue,
                                      "for key '%s': %s != %s" % (key, value, otherValue))
                return False
        return True

    def __ne__(self, other):
        return any(getattr(self, k) != getattr(other, k) for k in self.propNames)


# The classes below are test definitions. They inherit from `TestDefn` first, so methods from
# that class will override methods in the real definition class.

class TestGlobal(TestDefn, Global):
    pass

class TestFunction(TestDefn, Function):
    pass

class TestClass(TestDefn, Class):
    pass

class TestTrait(TestDefn, Trait):
    pass

class TestTypeParameter(TestDefn, TypeParameter):
    pass

class TestVariable(TestDefn, Variable):
    pass

class TestField(TestDefn, Field):
    pass

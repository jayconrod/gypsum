# Copyright 2015-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import os
import os.path
import re

import errors
import ir
from location import NoLoc
import serialize


class BasePackageLoader(object):
    def __init__(self):
        self.loadHooks = []

    def addLoadHook(self, hook):
        self.loadHooks.append(hook)

    def removeLoadHook(self, hook):
        self.loadHooks.remove(hook)

    def runLoadHooks(self, package):
        for hook in self.loadHooks:
            hook(package)


class PackageLoader(BasePackageLoader):
    class Info(object):
        def __init__(self, name, version, fileName):
            self.name = name
            self.version = version
            self.fileName = fileName
            self.package = None

    def __init__(self, paths=None):
        super(PackageLoader, self).__init__()
        if paths is not None:
            self.paths = paths
        elif os.environ.get("CS_PACKAGE_PATH") is not None:
            self.paths = os.environ["CS_PACKAGE_PATH"].split(":")
        else:
            self.paths = []

        self.packageInfoByName = None
        self.packageInfoById = None
        self.loadHooks = []

    def ensurePackageInfo(self):
        if self.packageInfoByName is not None:
            return
        self.packageInfoByName = {}
        self.packageInfoById = {}
        packageFileNameRex = re.compile(r"\A(%s)-(%s).csp\Z" %
                                        (ir.Name.packageSrc, ir.PackageVersion.versionSrc))
        for dirName in self.paths:
            try:
                for baseName in os.listdir(dirName):
                    m = packageFileNameRex.match(baseName)
                    if m is None:
                        continue
                    name = ir.Name.fromString(m.group(1), isPackageName=True)
                    version = ir.PackageVersion.fromString(m.group(2))
                    fileName = os.path.join(dirName, baseName)
                    if name not in self.packageInfoByName or \
                       version > self.packageInfoByName[name].version:
                        self.packageInfoByName[name] = \
                          PackageLoader.Info(name, version, fileName)
            except OSError:
                continue

    def getPackageNames(self):
        self.ensurePackageInfo()
        return self.packageInfoByName.keys()

    def isPackage(self, name):
        self.ensurePackageInfo()
        return name in self.packageInfoByName

    def loadPackage(self, name, loc=NoLoc):
        self.ensurePackageInfo()
        if name not in self.packageInfoByName:
            raise errors.PackageException(loc, "%s: could not find package" % str(name))

        info = self.packageInfoByName[name]
        if info.package is not None:
            return info.package
        fileName = self.packageInfoByName[name].fileName
        package = serialize.deserialize(fileName)
        info.package = package
        self.packageInfoById[package.id] = info

        for dep in package.dependencies:
            dep.package = self.loadPackage(dep.name, loc)

        self.runLoadHooks(package)
        return package

    def getLoadedPackages(self):
        return [packageInfo.package for packageInfo in self.packageInfoById.itervalues()]

    def getPackageById(self, id):
        package = self.packageInfoById[id].package
        assert package is not None
        return package

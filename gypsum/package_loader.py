# Copyright Jay Conrod. All rights reserved.
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
        """Registers a function that should be called when a package is loaded from disk.

        Args:
            hook (function(Package)): the function to be called.
        """
        self.loadHooks.append(hook)

    def removeLoadHook(self, hook):
        """Removes a previously registered hook function."""
        self.loadHooks.remove(hook)

    def _runLoadHooks(self, package):
        for hook in self.loadHooks:
            hook(package)


class PackageLoader(BasePackageLoader):
    """Loads packages from files, making them available to be used as dependencies. Also
    indexes available packages, used for package prefixes in the top-level scope.
    """

    class Info(object):
        def __init__(self, name, version, fileName, package=None):
            self.name = name
            self.version = version
            self.fileName = fileName
            self.package = package

    def __init__(self, paths=None):
        super(PackageLoader, self).__init__()
        if paths is not None:
            self.paths = paths
        elif os.environ.get("CS_PACKAGE_PATH") is not None:
            self.paths = os.environ["CS_PACKAGE_PATH"].split(os.pathsep)
        else:
            self.paths = []

        self.packageInfoByName = None
        self.packageInfoById = None
        self.packageInfoByFile = None
        self.loadHooks = []

    def ensurePackageInfo(self):
        """Scans packages directories and builds an index of available packages.

        Package directories can be set by passing in `paths` to the constructor of this class
        or by setting the `CS_PACKAGE_PATH` environment variable.

        If there are multiple versions of a package with the same name, the newest version will
        be indexed. If the same version is present in multiple directories, the first one will
        be indexed.

        Packages will not actually be loaded during this process. Name and version information
        are extracted from file names in the format <name>-<version>.csp. Files not in this
        format will not be indexed. If a package is loaded later, and the name and version
        from its metadata don't match the name and version from its file name, an error will
        be reported.

        This method may be called multiple times. The index is only built once. Most other
        methods call this internally.
        """
        if self.packageInfoByName is not None:
            return
        self.packageInfoByName = {}
        self.packageInfoById = {}
        self.packageInfoByFile = {}
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
                    if (name not in self.packageInfoByName or
                        (self.packageInfoByName[name].package is None and
                         version > self.packageInfoByName[name].version)):
                        info = PackageLoader.Info(name, version, fileName)
                        self.packageInfoByName[name] = info
                        self.packageInfoByFile[fileName] = info
            except OSError:
                continue

    def getPackageNames(self):
        """Returns a selection of all known package names.

        Calls `ensurePackageInfo` internally.
        """
        self.ensurePackageInfo()
        return self.packageInfoByName.keys()

    def isPackage(self, name):
        """Returns whether the given name refers to a known package.

        Calls `ensurePackageInfo` internally.
        """
        self.ensurePackageInfo()
        return name in self.packageInfoByName

    def loadPackage(self, name, loc=NoLoc):
        """Loads a package by name.

        Calls `ensurePackageInfo` internally. If the package is already loaded, it will be
        returned instead of being loaded again. Dependencies of the loaded package will be
        loaded recursively. Load hooks will be called for each newly loaded package.

        Args:
            name (Name): the name of the package.
            loc (Location): used for error messages.

        Returns:
            (Package): the loaded package.

        Raises:
            PackageException: if the package cannot be located or its metadata does not match
                its filename.
            OSError: if there is an I/O error loading the package.
        """
        self.ensurePackageInfo()
        if name not in self.packageInfoByName:
            raise errors.PackageException(loc, "%s: could not find package" % str(name))

        info = self.packageInfoByName[name]
        if info.package is not None:
            return info.package
        fileName = self.packageInfoByName[name].fileName
        package = serialize.deserialize(fileName, self)
        info.package = package
        self.packageInfoById[package.id] = info

        for dep in package.dependencies:
            dep.package = self.loadPackage(dep.name, loc)

        self._runLoadHooks(package)
        return package

    def loadPackageFiles(self, fileNames):
        """Loads a set of packages by file name.

        Does not call `ensurePackageInfo` internally. After this method is called,
        `ensurePackageInfo` will have no effect, so if you want the package index built, call
        that first.

        A package is loaded for each file name given. If a package from this set is already
        loaded, it will not be loaded again.

        When all packages are loaded, the loader will check that their dependencies are
        loaded (dependencies may be among the loaded set). If any dependencies are unsatisfied,
        an error is reported.

        Load hooks will be called on each newly loaded package.

        Args:
            fileNames (list(str)): list of file names of packages to load.

        Raises:
            PackageException: if a package cannot be located or if a different package with
                the same name as one of the loaded packages is already loaded.
            OSError, IOError: if there is an I/O error loading the package.
        """
        # Make a list of package names we will load and create info where necessary. We may
        # have already loaded some of these packages.
        names = []
        for fileName in fileNames:
            if fileName in self.packageInfoByFile:
                # We've either indexed or loaded this package already.
                info = self.packageInfoByFile[fileName]
                assert self.packageInfoByName[info.name] is info
                names.append(info.name)
            else:
                # We have never heard of this package before. We need to load some metadata
                # to see what it is.
                name, version = serialize.deserializeNameAndVersion(fileName)
                names.append(name)
                if name in self.packageInfoByName:
                    # We've indexed a package of the same name.
                    info = self.packageInfoByName[name]
                    if info.package is not None:
                        # We already loaded a package of the same name.
                        raise errors.PackageException(
                            NoLoc, "%s: already loaded a different package" % name)
                    else:
                        # We didn't load it yet. Update the info to point to the new file.
                        info.version = version
                        del self.packageInfoByFile[info.fileName]
                        info.fileName = fileName
                        self.packageInfoByFile[fileName] = info
                else:
                    # We've never heard of this package. Create new info.
                    info = PackageLoader.Info(name, version, fileName)
                    self.packageInfoByName[name] = info
                    self.packageInfoByFile[fileName] = info
                info = PackageLoader.Info(name, version, fileName)

        # Load the packages, if they're not loaded already.
        return map(self.loadPackage, names)

    def getLoadedPackages(self):
        return [packageInfo.package for packageInfo in self.packageInfoById.itervalues()]

    def getPackageById(self, id):
        package = self.packageInfoById[id].package
        assert package is not None
        return package

    def _checkPackageMatchesInfo(self, info, package, loc):
        if package.name != info.name:
            raise errors.PackageException(
                loc, "package name in metadata (%s) differs from file name (%s)" % (
                    package.name, info.name))
        if package.version != info.version:
            raise errors.PackageException(
                loc, "package version in metadata (%s) differs from file version (%s)" % (
                    package.version, info.version))

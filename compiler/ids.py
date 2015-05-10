# Copyright 2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


class Id(object):
    """Base class of other identifier classes. It should not be instantiated directly.
    Identifiers are used as keys to look up different kinds of things. For example, a
    ScopeId identifies a particular scope and is associated with definitions, so that,
    for any given definition, we can find its scope without referencing it directly.
    Identifiers are symbolic objects: they never compare equal with anything except
    themselves. Some identifiers carry extra information useful for debugging or locating
    things, but this information isn't used to check equality."""
    pass


class AstId(Id):
    """Identifies a node in an abstract syntax tree. Every node has one."""
    def __init__(self, id):
        self.id = id

    def __repr__(self):
        return "AstId(%d)" % self.id

    def __str__(self):
        return "#%d" % self.id


class ScopeId(Id):
    """Identifies a scope. If the scope comes from source code, an AstId may be specified
    to aid debugging. If it comes from somewhere else (for example, the builtin scope or an
    imported package), a string comment may be given."""
    def __init__(self, astIdOrComment):
        self.astIdOrComment = astIdOrComment

    def __repr__(self):
        return "ScopeId(%s)" % repr(self.astIdOrComment)

    def __str__(self):
        return "#%s" % repr(self.astIdOrComment)

BUILTIN_SCOPE_ID = ScopeId("builtin")
GLOBAL_SCOPE_ID = ScopeId("global")
PACKAGE_SCOPE_ID = ScopeId("package-list")


class PackageId(Id):
    """Identifies a package. If an index is specified, this identifies an imported package.
    If not, it identifies the package being compiled."""
    def __init__(self, index=None):
        self.index = index

    def __repr__(self):
        if self is TARGET_PACKAGE_ID:
            return "PackageId(TARGET)"
        else:
            return "PackageId(%s)" % str(self.index)

    def __str__(self):
        if self is TARGET_PACKAGE_ID:
            return "#TARGET"
        else:
            return "#%s" % str(self.index)

TARGET_PACKAGE_ID = PackageId()
BUILTIN_PACKAGE_INDEX = -2
LOCAL_PACKAGE_INDEX = -1


class DefnId(Id):
    """Identifies a definition in a package. Includes the package id, definition kind, and
    definition index to help locate the definition."""
    GLOBAL = "global"
    FUNCTION = "function"
    CLASS = "class"
    TYPE_PARAMETER = "type-parameter"

    def __init__(self, packageId, kind, index, externIndex=None):
        if packageId is None:
            assert index < 0
        else:
            assert index >= 0 and isinstance(packageId, PackageId)
        self.packageId = packageId
        self.kind = kind
        self.index = index
        self.externIndex = externIndex

    def __repr__(self):
        packageStr = str(self.packageId) if self.packageId else "BUILTIN"
        return "DefnId(%s, %s, %s, %s)" % (packageStr, self.kind, self.index, self.externIndex)

    def __str__(self):
        packageStr = "" \
                     if self.packageId is TARGET_PACKAGE_ID or self.packageId is None \
                     else "%d." % self.packageId.index
        indexStr = "%d" % self.index
        externIndexStr = ":%d" % self.externIndex if self.externIndex is not None else ""
        return "#%s%s%s" % (packageStr, indexStr, externIndexStr)

    def isBuiltin(self):
        return self.packageId is None

    def isLocal(self):
        return self.packageId is TARGET_PACKAGE_ID

    def isForeign(self):
        return not self.isBuiltin() and not self.isLocal()

    def getPackageIndex(self):
        if self.packageId is None:
            return BUILTIN_PACKAGE_INDEX
        elif self.packageId is TARGET_PACKAGE_ID:
            return LOCAL_PACKAGE_INDEX
        else:
            assert self.packageId.index is not None
            return self.packageId.index

    def getDefnIndex(self):
        return self.externIndex if self.isForeign() else self.index

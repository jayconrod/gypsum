# Copyright 2015-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import re

from utils import hashList


class Name(object):
    """The name of a package or a definition within a package.

    Names consist of a list between 1 and 100 (inclusive) components. Each component is a
    string of between 1 and 1000 unicode characters. The only invalid character is '.'; this
    acts as a separator when names are printed.

    Package name components are restricted to upper and lower case Roman letters, numbers,
    and '_' (although '_' cannot be the first character).

    Names are not guaranteed to be unique among all definitions in the same package. They
    should be unique among public and protected definitions though."""

    nameComponentSrc = "[^.]+"
    nameSrc = r"%s(?:\.%s)*" % (nameComponentSrc, nameComponentSrc)
    nameRex = re.compile(r"\A%s\Z" % nameSrc)

    packageComponentSrc = "[A-Za-z][A-Za-z0-9_]*"
    packageSrc = r"%s(?:\.%s)*" % (packageComponentSrc, packageComponentSrc)
    packageRex = re.compile(r"\A%s\Z" % packageSrc)

    def __init__(self, components):
        self.components = list(components)

    @staticmethod
    def fromString(s, isPackageName=False):
        m = Name.packageRex.match(s) if isPackageName else Name.nameRex.match(s)
        if not m or m.end() != len(s):
            raise ValueError("invalid package name: " + s)
        return Name(s.split("."))

    def __cmp__(self, other):
        return cmp(self.components, other.components)

    def __hash__(self):
        return hashList(self.components)

    def __repr__(self):
        return "Name(%s)" % ".".join(self.components)

    def __str__(self):
        return ".".join(self.components)

    def __add__(self, other):
        return Name(self.components + other.components)

    def withSuffix(self, suffix):
        return Name(self.components + [suffix])

    def hasPrefix(self, components):
        return len(components) < len(self.components) and \
               all(a == b for a, b in zip(self.components, components))

    def short(self):
        s = self.components[-1]
        if isinstance(s, unicode):
            s = str(s)
        return s


# Strings used in internal names for generated definitions.
# Use CompileInfo.makeUniqueName to generate internal names that won't conflict with each other.
CLOSURE_SUFFIX = "$closure"
CONSTRUCTOR_SUFFIX = "$constructor"
CONTEXT_SUFFIX = "$context"
PACKAGE_INIT_NAME = Name(["$pkginit"])
CLASS_INIT_SUFFIX = "$init"
ANON_PARAMETER_SUFFIX = "$parameter"
RECEIVER_SUFFIX = "$this"
ARRAY_LENGTH_SUFFIX = "$length"
EXISTENTIAL_SUFFIX = "$forsome"
BLANK_SUFFIX = "$blank"
LOCAL_SUFFIX = "$local"

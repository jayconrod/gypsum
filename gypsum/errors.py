# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from location import NoLoc


class CompileException(Exception):
    def __init__(self, location, message):
        self.location = location
        self.message = message

    def __str__(self):
        locStr = str(self.location) if self.location is not None else "<unknown>"
        return "%s: %s error: %s" % (self.location, self.kind, self.message)

    @classmethod
    def fromDefn(cls, defn, message):
        loc = defn.getLocation()
        sourceName = defn.getSourceName()
        messageWithName = sourceName + ": " + message
        return cls(loc, messageWithName)

    @classmethod
    def fromUse(cls, loc, defn, message):
        sourceName = defn.getSourceName()
        messageWithName = sourceName + ": " + message
        return cls(loc, messageWithName)


class PackageException(CompileException):
    kind = "package"


class LexException(CompileException):
    kind = "lexical"


class ParseException(CompileException):
    kind = "syntax"


class ScopeException(CompileException):
    kind = "scope"


class InheritanceException(CompileException):
    kind = "inheritance"


class TypeException(CompileException):
    kind = "type"


class SemanticException(CompileException):
    kind = "semantic"

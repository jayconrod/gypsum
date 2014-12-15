# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


class CompileException(Exception):
    def __init__(self, location, message):
        self.location = location
        self.message = message

    def __str__(self):
        locStr = str(self.location) if self.location is not None else "<unknown>"
        return "%s: %s" % (self.location, self.message)


class LexException(CompileException):
    pass


class LayoutException(CompileException):
    pass


class ParseException(CompileException):
    pass


class ScopeException(CompileException):
    pass


class TypeException(CompileException):
    pass


class SemanticException(CompileException):
    pass

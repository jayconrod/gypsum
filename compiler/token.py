# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


RESERVED = "reserved"
SYMBOL = "symbol"
OPERATOR = "operator"
INTEGER = "integer"
FLOAT = "float"
STRING = "string"

NEWLINE = "newline"
SPACE = "space"
COMMENT = "comment"

INTERNAL = "internal"

class Token:
    def __init__(self, text, tag, location):
        self.text = text
        self.tag = tag
        self.location = location

    def __str__(self):
        return '("%s", %s) @ %s' % (self.text, self.tag, self.__locationStr())

    def __repr__(self):
        return 'Token("%s", %s, %s)' % (self.text, self.tag, repr(self.location))

    def __locationStr(self):
        return str(self.location) if self.location else "<unknown>"

    def isPrintable(self):
        return self.tag not in [NEWLINE, SPACE, COMMENT]


class Location:
    def __init__(self, filename, beginLine, beginColumn, endLine, endColumn):
        self.filename = filename
        self.beginLine = beginLine
        self.beginColumn = beginColumn
        self.endLine = endLine
        self.endColumn = endColumn

    def __str__(self):
        return "%s:%d.%d-%d.%d" % \
          (self.filename, self.beginLine, self.beginColumn, self.endLine, self.endColumn)

    def __repr__(self):
        return 'Location("%s", %d, %d, %d, %d)' % \
          (self.filename, self.beginLine, self.beginColumn, self.endLine, self.endColumn)

    def __cmp__(self, other):
        l = (self.filename, self.beginLine, self.beginColumn, self.endLine, self.endColumn)
        r = (other.filename, other.beginLine, other.beginColumn, other.endLine, other.endColumn)
        return cmp(l, r)

    def __hash__(self, other):
        h = hash(self.filename)
        h = 33 * h + hash(self.beginLine)
        h = 33 * h + hash(self.endLine)
        h = 33 * h + hash(self.beginColumn)
        h = 33 * h + hash(self.endColumn)
        return h

    def combine(self, other):
        assert(self.filename is other.filename)
        return Location(self.filename, \
                        self.beginLine, \
                        self.beginColumn, \
                        other.endLine, \
                        other.endColumn)

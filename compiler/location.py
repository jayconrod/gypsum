# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from data import Data

class Location(Data):
    propertyNames = ["fileName", "beginRow", "beginColumn", "endRow", "endColumn"]

    def __str__(self):
        if self is NoLoc:
            return "<unknown>"
        else:
            return "%s:%d.%d-%d.%d" % \
                (self.fileName, self.beginRow, self.beginColumn,
                 self.endRow, self.endColumn)

    def __cmp__(self, other):
        l = (self.filename, self.beginLine, self.beginColumn, self.endLine, self.endColumn)
        r = (other.filename, other.beginLine, other.beginColumn, other.endLine, other.endColumn)
        return cmp(l, r)

    def combine(self, other):
        assert self.fileName == other.fileName
        beginRow, beginColumn = min((self.beginRow, self.beginColumn),
                                    (other.beginRow, other.beginColumn))
        endRow, endColumn = max((self.endRow, self.endColumn),
                                (other.endRow, other.endColumn))
        return Location(self.fileName, beginRow, beginColumn, endRow, endColumn)


NoLoc = Location("<unknown>", 1, 1, 1, 1)

__all__ = ["Location", "NoLoc"]

# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import os.path
import string
import sys
from StringIO import StringIO


def each(f, iterable):
    for elem in iterable:
        f(elem)

def indexSame(haystack, needle):
    for index, elem in enumerate(haystack):
        if elem is needle:
            return index
    return -1


def align(n, alignment):
    return (n + alignment - 1) & ~(alignment - 1)


def isAligned(n, alignment):
    return (n & ~(alignment - 1)) == n


def isPowerOf2(n):
    return (n & (n - 1)) == 0


def bit(n, offset):
    return (n >> offset) & 1


def bitExtract(n, offset, width):
    mask = (1 << width) - 1
    bits = (n >> offset) & mask
    return bits


def bitInsert(n, offset, width, value):
    mask = ((1 << width) - 1) << offset
    n = (n & ~mask) | (value << offset)
    return n


def hashMix(n):
    n = (n ^ 61) ^ (n >> 16)
    n = n + (n << 3)
    n = n ^ (n >> 4)
    n = n * 0x27d4eb2d
    n = n ^ (n >> 15)
    return n


def hashList(elems):
    return reduce(lambda a, b: hashMix(a ^ b), map(hash, elems), 0)


def reprFormat(obj, *keys):
    """Used to implement __repr__ in some classes.

    Args:
        obj: the object to generate a repr string for.
        keys ([str]): names of attributes that should be looked up and included in the string.
    """
    nameStr = obj.__class__.__name__
    itemsStr = ", ".join(repr(getattr(obj, key)) for key in keys)
    return "%s(%s)" % (nameStr, itemsStr)


_decodeEscapeChars = {'n': '\n',
                      'a': '\a',
                      'b': '\b',
                      'f': '\f',
                      'n': '\n',
                      'r': '\r',
                      't': '\t',
                      'v': '\v'}
_encodeEscapeChars = {v: k for k, v in _decodeEscapeChars.iteritems()}

def tryDecodeString(inStr):
    assert len(inStr) >= 2 and inStr[0] in '"`' and inStr[-1] in '"`' and inStr[0] == inStr[-1]
    outBuf = StringIO()
    NORMAL = "normal"
    ESCAPE = "escape"
    UNICODE = "unicode"
    state = NORMAL
    hexValue = 0
    hexCharsLeft = 0
    for i in xrange(1, len(inStr) - 1):
        ch = inStr[i]
        if state is NORMAL:
            if ch == '\\':
                state = ESCAPE
            else:
                outBuf.write(ch)
        elif state is ESCAPE:
            if ch in _decodeEscapeChars:
                outBuf.write(_decodeEscapeChars[ch])
                state = NORMAL
            elif ch == 'x':
                state = UNICODE
                hexCharsLeft = 2
            elif ch == 'u':
                state = UNICODE
                hexCharsLeft = 4
            elif ch == 'U':
                state = UNICODE
                hexCharsLeft = 8
            else:
                outBuf.write(ch)
                state = NORMAL
        else:
            assert state is UNICODE
            assert hexCharsLeft > 0
            hexCharsLeft -= 1
            if ch not in string.hexdigits:
                return None
            hexValue = 16 * hexValue + int(ch, base=16)
            if hexCharsLeft == 0:
                if hexValue > 0x10ffff:
                    return None
                outBuf.write(unichr(hexValue))
                state = NORMAL
                hexValue = 0
    if state is not NORMAL:
        return None
    outStr = outBuf.getvalue()
    if type(outStr) == str:
        outStr = unicode(outStr)
    return outStr


def encodeString(inStr):
    outBuf = StringIO()
    outBuf.write('"')
    for ch in inStr:
        if ch == '"':
            outBuf.write(r'\"')
        elif 32 <= ord(ch) and ord(ch) < 128:
            outBuf.write(ch)
        elif ch in _encodeEscapeChars:
            outBuf.write('\\' + _encodeEscapeChars[ch])
        elif ord(ch) <= 0xff:
            outBuf.write(r'\x%02x' % ord(ch))
        elif ord(ch) <= 0xffff:
            outBuf.write(r'\x%04x' % ord(ch))
        else:
            outBuf.write(r'\x%08x' % ord(ch))
    outBuf.write('"')
    return outBuf.getvalue()


def openCommonFile(name):
    """The compiler may be packaged using pyinstaller. We need to be able to open resource files
    both in that configuration and in the test configuration (regular Python files). pyinstaller
    unpacks scripts and resources to a temp folder whose name is in sys._MEIPASS2."""
    if hasattr(sys, "_MEIPASS"):
        common = sys._MEIPASS
    else:
        common = os.path.join(os.path.dirname(__file__), "..", "common")
    return open(os.path.join(common, name))


class Counter(object):
    def __init__(self, start=0, inc=1):
        self.n = start
        self.inc = inc

    def __call__(self):
        n = self.n
        self.n += self.inc
        return n

    def value(self):
        return self.n


COMPILE_FOR_VALUE = "compile-for-value"
COMPILE_FOR_EFFECT = "compile-for-effect"
COMPILE_FOR_MATCH = "compile-for-match"
COMPILE_FOR_UNINITIALIZED = "compile-for-uninitialized"


__all__ = ["each", "encodeString", "tryDecodeString", "openCommonFile", "Counter", "hashList",
           "COMPILE_FOR_VALUE", "COMPILE_FOR_EFFECT", "COMPILE_FOR_MATCH",
           "COMPILE_FOR_UNINITIALIZED"]

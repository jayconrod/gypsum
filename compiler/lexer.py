# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import re

from errors import LexException
from location import Location
from tok import *

__opchars = r"[!#%&*+\-/:<=>?@\\^|~]"

__expressions = [
  (r"\r?\n", NEWLINE),
  (r"[\t ]+", SPACE),
  (r"//[^\r\n]*", COMMENT),

  (r"\[",  RESERVED),
  (r"\]",  RESERVED),
  (r"\(",  RESERVED),
  (r"\)",  RESERVED),
  (r"\{",  RESERVED),
  (r"\}",  RESERVED),
  (r"<:",  RESERVED),
  (r">:",  RESERVED),
  (r"=>",  RESERVED),
  (r"_",   RESERVED),
  (r"\.",  RESERVED),
  (r",",   RESERVED),
  (r":",   RESERVED),
  (r";",   RESERVED),
  (r"=",   RESERVED),

  (r"var", RESERVED),
  (r"let", RESERVED),
  (r"def", RESERVED),
  (r"class", RESERVED),
  (r"trait", RESERVED),
  (r"if", RESERVED),
  (r"else", RESERVED),
  (r"while", RESERVED),
  (r"break", RESERVED),
  (r"continue", RESERVED),
  (r"case", RESERVED),
  (r"match", RESERVED),
  (r"throw", RESERVED),
  (r"try", RESERVED),
  (r"catch", RESERVED),
  (r"finally", RESERVED),
  (r"lambda", RESERVED),
  (r"return", RESERVED),
  (r"unit", RESERVED),
  (r"i8", RESERVED),
  (r"i16", RESERVED),
  (r"i32", RESERVED),
  (r"i64", RESERVED),
  (r"f32", RESERVED),
  (r"f64", RESERVED),
  (r"boolean", RESERVED),
  (r"true", RESERVED),
  (r"false", RESERVED),
  (r"this", RESERVED),
  (r"super", RESERVED),
  (r"null", RESERVED),

  (r"abstract", ATTRIB),
  (r"public", ATTRIB),
  (r"protected", ATTRIB),
  (r"private", ATTRIB),
  (r"static", ATTRIB),

  (r"[+-]?[0-9]+(?:i[0-9]+)?", INTEGER),
  (r"[+-]?0[xX][0-9A-Fa-f]+(?:i[0-9]+)?", INTEGER),
  (r"[+-]?0[bB][01]+(?:i[0-9]+)?", INTEGER),

  (r"[+-]?[0-9]+\.[0-9]*(?:[Ee][+-]?[0-9]+)?(?:f[0-9]+)?", FLOAT),
  (r"[+-]?\.[0-9]+(?:[Ee][+-]?[0-9]+)?(?:f[0-9]+)?", FLOAT),
  (r"[+-]?[0-9]+[Ee][+-]?[0-9]+(?:f[0-9]+)?", FLOAT),
  (r"[+-]?[0-9]+f[0-9]+", FLOAT),

  (r'"(?:\\"|[^"])*"', STRING),

  (r"[!#%&*+\-/:<=>?@\\^|~]+", OPERATOR),
  (r"[A-Za-z_][A-Za-z0-9_-]*", SYMBOL),
]
__expressions = [(re.compile(expr[0]), expr[1]) for expr in __expressions]


def lex(filename, source):
    tokens = []
    pos = 0
    end = len(source)
    line = 1
    column = 1
    while pos < end:
        token = None
        for expr in __expressions:
            (rx, tag) = expr
            m = rx.match(source, pos)
            if m:
                text = m.group(0)
                if token and len(text) <= len(token.text):
                    continue

                location = Location(filename, line, column, line, column + len(text))
                token = Token(m.group(0), tag, location)
        if not token:
            location = Location(filename, line, column, line, column + 1)
            raise LexException(location, "illegal character: %s" % source[pos:pos+1])
        tokens.append(token)
        if token.tag is NEWLINE:
            line += 1
            column = 1
        else:
            column += len(token.text)
        pos += len(token.text)

    return tokens

__all__ = ["lex"]

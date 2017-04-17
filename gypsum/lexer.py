# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import re

from errors import LexException
from location import Location
from tok import *

_expressions = [
  (r"\r?\n", NEWLINE),
  (r"[\t ]+", SPACE),
  (r"//[^\r\n]*", COMMENT),

  (r"\[",  LBRACK),
  (r"\]",  RBRACK),
  (r"\(",  LPAREN),
  (r"\)",  RPAREN),
  (r"\{",  LBRACE),
  (r"\}",  RBRACE),
  (r"<:",  SUBTYPE),
  (r">:",  SUPERTYPE),
  (r"=>",  BIG_ARROW),
  (r"->",  SMALL_ARROW),
  (r"_",   UNDERSCORE),
  (r"\.",  DOT),
  (r",",   COMMA),
  (r":",   COLON),
  (r";",   SEMI),
  (r"=",   EQ),

  (r"var", VAR),
  (r"let", LET),
  (r"def", DEF),
  (r"class", CLASS),
  (r"trait", TRAIT),
  (r"arrayelements", ARRAYELEMENTS),
  (r"import", IMPORT),
  (r"as", AS),
  (r"if", IF),
  (r"else", ELSE),
  (r"while", WHILE),
  (r"break", BREAK),
  (r"continue", CONTINUE),
  (r"case", CASE),
  (r"match", MATCH),
  (r"throw", THROW),
  (r"try", TRY),
  (r"catch", CATCH),
  (r"finally", FINALLY),
  (r"new", NEW),
  (r"lambda", LAMBDA),
  (r"return", RETURN),
  (r"unit", UNIT),
  (r"i8", I8),
  (r"i16", I16),
  (r"i32", I32),
  (r"i64", I64),
  (r"f32", F32),
  (r"f64", F64),
  (r"boolean", BOOLEAN),
  (r"forsome", FORSOME),
  (r"true", TRUE),
  (r"false", FALSE),
  (r"this", THIS),
  (r"super", SUPER),
  (r"null", NULL),

  (r"abstract", ATTRIB),
  (r"final", ATTRIB),
  (r"public", ATTRIB),
  (r"protected", ATTRIB),
  (r"private", ATTRIB),
  (r"static", ATTRIB),
  (r"override", ATTRIB),
  (r"native", ATTRIB),

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
  (r"`(?:\\`|[^`])*`", SYMBOL),
]
_expressions = [(re.compile(expr[0]), expr[1]) for expr in _expressions]


def lex(filename, source):
    tokens = []
    pos = 0
    end = len(source)
    line = 1
    column = 1
    while pos < end:
        token = None
        for expr in _expressions:
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

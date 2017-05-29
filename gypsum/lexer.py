# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import re

from errors import LexException
from location import Location
from tok import *

_SPACE = "space"
_ESCAPED_NEWLINE = "escaped newline"

_expressions = [
    (r"[\t ]+", _SPACE),
    (r"\r\n?|\n", NEWLINE),
    (r"\\(?:\r\n?|\n)", _ESCAPED_NEWLINE),
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
_spaceRx = _expressions[0][0]
_newlineRx = _expressions[1][0]


def lex(filename, source):
    tokens = []
    pos = 0
    end = len(source)
    line = 1
    column = 1
    indents = [""]

    def nextToken():
        bestText = None
        bestTag = None
        for (rx, tag) in _expressions:
            (rx, tag) = expr
            m = rx.match(source, pos)
            if m:
                text = m.group(0)
                if bestText is None or len(text) > len(bestText):
                    bestText = text
                    bestTag = tag
        if bestText is None:
            loc = Location(filename, line, column, line, column + 1)
            raise LexException(loc, "illegal character: %s" % source[pos:pos+1])
        loc = Location(filename, line, column, line, column + len(bestText))
        column += len(bestText)
        pos += len(bestText)
        return Token(bestText, bestTag, loc)

    # Outer loop: lex each line in file
    while pos < end:
        # Read space at beginning of line.
        m = _spaceRx.match(source, pos)
        space = m.group(0) if m else ""
        column += len(space)
        pos += len(space)

        # If the next token is a newline, this is a blank line. If we're not inside brackets,
        # emit a newline token without checking indentation.
        m = _newlineRx.match(source, pos)
        if m:
            if nesting == 0:
                loc = Location(filename, line, column, line + 1, 1)
                tokens.append(Token(NEWLINE, NEWLINE, loc))
            line += 1
            column = 1
            pos += len(m.group(0))
            continue

        # Check indentation.
        # If space is the same as the current indentation level, do nothing.
        # If the current indentation level is a prefix of space, add an indentation level.
        # If space is a prefix of the current indentation level, remove indentation levels
        # until they match.
        # We always match prefixes here. '\t ' is NOT the same as ' \t'.
        # Outdents are inserted before the last newline, indents are inserted after.
        loc = Location(filename, line, column, line, column)
        indent = indents[-1]
        if len(space) < len(indent):
            outdents = 0
            while len(space) < len(indent):
                outdents += 1
                indents.pop()
                indent = indents[-1]
            if space != indent:
                raise LexException(loc, "indentation does not match any outer indentation level")
            tok = Token(OUTDENT, OUTDENT, loc)
            if len(tokens) > 0 and tokens[-1].tag is NEWLINE:
                for _ in xrange(outdents):
                    tokens.insert(-1, tok)
            else:
                for _ in xrange(outdents):
                    tokens.append(tok)
        else:
            if not space.startswith(indent):
                raise LexException(loc, "indentation does not match indentation above")
            if len(space) > len(indent):
                indents.append(space)
                tokens.append(Token(INDENT, INDENT, loc))

        # Inner loop: read all the tokens on the logical line. A logical line is different
        # than a physical line. We ignore newlines inside nested parentheses, brackets, and
        # braces. Newlines may also be escaped.
        nesting = 0
        while pos < end:
            # Use the table to find the next token.
            bestText = None
            bestTag = None
            for (rx, tag) in _expressions:
                m = rx.match(source, pos)
                if m:
                    text = m.group(0)
                    if bestText is None or len(text) > len(bestText):
                        bestText = text
                        bestTag = tag
            if bestText is None:
                loc = Location(filename, line, column, line, column + 1)
                raise LexException(loc, "illegal character: %s" % source[pos:pos+1])
            loc = Location(filename, line, column, line, column + len(bestText))
            column += len(bestText)
            pos += len(bestText)

            # Ignore whitespace.
            if bestTag is _SPACE:
                continue

            # Advance position for escaped or nested newline.
            if bestTag is _ESCAPED_NEWLINE or (bestTag is NEWLINE and nesting > 0):
                line += 1
                column = 1
                continue

            tok = Token(bestText, bestTag, loc)

            # Check nesting.
            # TODO: report mismatched nesting.
            if bestTag in (LPAREN, LBRACK, LBRACE):
                nesting += 1
            elif bestTag in (RPAREN, RBRACK, RBRACE):
                nesting -= 1

            # If we found an unnested, unescaped newline, we have reached the end
            # of the logical line.
            if bestTag is NEWLINE:
                line += 1
                column = 1
                tok.text = NEWLINE
                tokens.append(tok)
                break

            tokens.append(tok)

    # Add final tokens at the end of the file.
    loc = Location(filename, line, column, line, column)
    for _ in xrange(1, len(indents)):
        tokens.append(Token(OUTDENT, OUTDENT, loc))
    tokens.append(Token(EOF, EOF, loc))

    return tokens

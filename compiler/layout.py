# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


# This module provides automatic insertion of semicolons (;) and braces ({, }) based on
# indentation. It operates on the output of the lexer, returning a new list of tokens.
#
# Internally, a stack of indentation is maintained. Each element is the number of tabs
# and spaces used to indent a particular block. Tabs are almost more significant than
# spaces, and tabs must come before spaces in indentation, or an exception will be
# thrown.
#
# An opening brace ({) is inserted if the current line has greater indentation than the
# block on top of the stack AND one of the patterns below is matched AND the next printable
# token is not an opening brace ({). The indentation of the current line is also pushed onto
# the stack.
# var ... =
#
# A semicolon (;) is inserted if the current line has the same indentation as the block on
# top of the stack, and the previous line did not end with a semicolon.
#
# If the end of the file is reached, or the current line has less indentation than the
# block on top of the stack, closing braces (}) and semicolons (;) may be inserted. Until
# the current indentation ((0, 0) in case of end of file) is greater than or equal to the
# block on top of the stack, the stack is popped and a closing brace (}) is inserted. Unless
# the next token is the keyword "else" or "catch", a semicolon is also inserted.

import re

from tok import *

class LayoutException(Exception):
    def __init__(self, location, message):
        self.location = location
        self.message = message

    def __str__(self):
        return "%s: error: %s\n" % (self.location, self.message)


def layout(tokensIn, skipAnalysis=False):
    if skipAnalysis:
        tokensOut = filter(lambda t: t.isPrintable(), tokensIn)
        return tokensOut

    tokensOut = []
    indentStack = [IndentLevel(0, 0)]
    patterns = PatternManager([
      ["var", ANY_PATTERN, "="],
      ["def", ANY_PATTERN, "="],
      ["class", ANY_PATTERN],
      ["if", "(", ANY_PATTERN, ")"],
      ["else"],
      ["else", "if", "(", ANY_PATTERN, ")"],
      ["while", "(", ANY_PATTERN, ")"],
      ["match", "(", ANY_PATTERN, ")"],
      ["case", ANY_PATTERN, "=>"],
      ["try"],
      ["catch"],
      ["catch", "(", ANY_PATTERN, ")"],
      ["finally"],
      ["lambda", "(", ANY_PATTERN, ")"]
    ])

    BEGIN = "begin"
    PRENORMAL = "pre-normal"
    NORMAL = "normal"
    state = BEGIN
    indent = None

    def dedent(indent, text, loc):
        while indent < indentStack[-1]:
            indentStack.pop()
            patterns.pop()
            if tokensOut[-1].text != ";":
                tokensOut.append(Token(";", INTERNAL, loc))
            if indent < indentStack[-1] or text != "}":
                tokensOut.append(Token("}", INTERNAL, loc))

    for pos in range(0, len(tokensIn)):
        token = tokensIn[pos]
        if state is BEGIN:
            if token.tag is SPACE:
                m = re.match("^(\t*)( *)$", token.text)
                if m:
                    indent = IndentLevel(len(m.group(1)), len(m.group(2)))
                    state = PRENORMAL
                    continue
                else:
                    raise LayoutException(token.location, "mixed tabs and spaces used for indentation")
            elif token.tag in [NEWLINE, COMMENT]:
                continue   # Blank line
            elif token.isPrintable():
                indent = IndentLevel(0, 0)
                state = PRENORMAL
                # fall through

        if state is PRENORMAL:
            assert token.tag is not SPACE
            if token.tag is COMMENT:
                continue   # next should be newline
            elif token.tag is NEWLINE:
                state = BEGIN   # turned out to be a blank line
                continue
            else:
                assert token.isPrintable()
                if indent < indentStack[-1]:
                    dedent(indent, token.text, token.location)
                    if tokensOut[-1].text != ";" and token.text not in ["else", "catch", "finally"]:
                        tokensOut.append(Token(";", INTERNAL, token.location))
                elif indent == indentStack[-1]:
                    if len(tokensOut) > 1 and \
                       tokensOut[-1].text != ";" and \
                       (token.text != "{" or not patterns.match()):
                        tokensOut.append(Token(";", INTERNAL, token.location))
                        patterns.reset()
                else:
                    assert indent > indentStack[-1]
                    if patterns.match() or (len(tokensOut) > 0 and tokensOut[-1].text == "{"):
                        if tokensOut[-1].text != "{":
                            tokensOut.append(Token("{", INTERNAL, token.location))
                            patterns.reset()
                        patterns.push()
                        indentStack.append(indent)
                    else:
                        # this is a continuation of a previous line
                        pass

                state = NORMAL
                # fall through

        if state is NORMAL:
            if token.tag is NEWLINE:
                state = BEGIN
            elif not token.isPrintable():
                # discard stuff that doesn't matter
                pass
            else:
                assert token.isPrintable()
                patterns.next(token)
                tokensOut.append(token)

    if len(tokensOut) > 0 and tokensOut[-1].text != ";":
        tokensOut.append(Token(";", INTERNAL, tokensOut[-1].location))
    if len(indentStack) > 1:
        dedent(indentStack[0], None, tokensOut[-1].location)
    if len(tokensOut) > 0 and tokensOut[-1].text != ";":
        tokensOut.append(Token(";", INTERNAL, tokensOut[-1].location))

    return tokensOut


class IndentLevel(object):
    def __init__(self, tabs, spaces):
        self.tabs = tabs
        self.spaces = spaces

    def __cmp__(self, other):
        d = self.tabs - other.tabs
        return d if d != 0 else self.spaces - other.spaces

    def __repr__(self):
        return "IndentLevel(%d, %d)" % (self.tabs, self.spaces)


ID_PATTERN = "id"
ANY_PATTERN = "..."
class PatternMatcher(object):
    def __init__(self, pattern):
        self.pattern = pattern
        self.pos = 0
        self.delimiters = []

    def isMatch(self):
        return self.pos == len(self.pattern) or \
               (len(self.pattern) > 0 and \
                self.pos == len(self.pattern) - 1 and \
                self.pattern[-1] is ANY_PATTERN and \
                len(self.delimiters) == 0)

    def __nonzero__(self):
        return self.isMatch()

    def reset(self):
        self.pos = 0
        self.delimiters = []

    def next(self, token):
        if self.pos == len(self.pattern):
            self.pos = 0

        def retry():
            if self.pos > 0:
                self.pos = 0
                self.next(token)

        if self.pattern[self.pos] is ANY_PATTERN:
            if self.pos + 1 < len(self.pattern) and \
               len(self.delimiters) == 0 and \
               token.text == self.pattern[self.pos + 1]:
                self.pos += 2
            else:
                delimiterMap = { "}": "{", "]": "[", ")": "(" }
                if token.text in delimiterMap.values():
                    self.delimiters.append(token.text)
                elif len(self.delimiters) > 0 and \
                     delimiterMap.get(token.text) == self.delimiters[-1]:
                    self.delimiters.pop()
        elif self.pattern[self.pos] is ID_PATTERN:
            if token.tag in [SYMBOL, OPERATOR]:
                self.pos += 1
            else:
                retry()
        else:
            if token.text == self.pattern[self.pos]:
                self.pos += 1
            else:
                retry()


class PatternManager(object):
    def __init__(self, patterns):
        self.patterns = patterns
        self.stack = []
        self.push()

    def push(self):
        self.stack.append([PatternMatcher(pattern) for pattern in self.patterns])

    def pop(self):
        self.stack.pop()

    def next(self, token):
        for matcher in self.stack[-1]:
            matcher.next(token)

    def reset(self):
        for matcher in self.stack[-1]:
            matcher.reset()

    def match(self):
        return any(self.stack[-1])

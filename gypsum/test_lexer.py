# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from lexer import *
from errors import LexException
from location import Location
from tok import *

class TestLexer(unittest.TestCase):
    def checkTags(self, expected, text):
        tokens = lex("test", text)
        tags = [t.tag for t in tokens]
        self.assertEquals(expected, tags)

    def checkText(self, expected, text):
        tokens = lex("test", text)
        texts = [t.text for t in tokens]
        self.assertEqual(expected, texts)

    def checkTag(self, expected, text):
        tokens = lex("test", text)
        tags = [t.tag for t in tokens]
        self.assertIs(expected, tags[0])

    def testEmpty(self):
        self.checkTags([EOF], "")

    def testBasic(self):
        text = "var x"
        self.checkTags([VAR, SYMBOL, EOF], text)
        self.checkText(["var", "x", EOF], text)

    def testOperators(self):
        for op in ["|", "&&", "<=", "::", "=="]:
            self.checkTag(OPERATOR, op)

    def testPunctuation(self):
        cases = [
            (LBRACK, "["),
            (RBRACK, "]"),
            (LPAREN, "("),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (RBRACE, "}"),
            (SUBTYPE, "<:"),
            (SUPERTYPE, ">:"),
            (BIG_ARROW, "=>"),
            (SMALL_ARROW, "->"),
            (UNDERSCORE, "_"),
            (DOT, "."),
            (COMMA, ","),
            (COLON, ":"),
            (SEMI, ";"),
            (EQ, "="),
        ]
        for tag, text in cases:
            self.checkTag(tag, text)

    def testSymbols(self):
        for sym in ["a", "a0_", "_a", "__", "_0", "A_0", "A-0"]:
            self.checkTag(SYMBOL, sym)

    def testQuotedSymbol(self):
        self.checkTag(SYMBOL, r"`fo\`o`")

    def testIntegers(self):
        for i in ["0", "1", "123", "-1", "-123", "-0"]:
            self.checkTag(INTEGER, i)

    def testSizedIntegers(self):
        for i in ["0i8", "12i16", "123i64", "-1i32", "-123i8", "-0i32"]:
            self.checkTag(INTEGER, i)

    def testHexIntegers(self):
        for i in ["0x0", "0xabc", "0xABC", "0XAb0", "-0xAb1", "0xAb1i32", "-0xAb1i32"]:
            self.checkTag(INTEGER, i)

    def testBinaryIntegers(self):
        for i in ["0b0", "0b1000101", "-0b1", "0b1i32", "-0b1i16"]:
            self.checkTag(INTEGER, i)

    def testFloats(self):
        for i in ["1.", ".1", "123.45", "-1.", "-.1", "+.1",
                  "1.2e3", "-1.2E34", "9e8", "-9e+8", "-9e-8", "1.2f32", "-1e2f64", "-1f64"]:
            self.checkTag(FLOAT, i)

    def testStrings(self):
        self.checkTag(STRING, '""')
        self.checkTag(STRING, '"foo"')
        self.checkTags([STRING, INTEGER, STRING, EOF], '"foo"123"bar"')
        self.checkTag(STRING, r'"foo\"123\"bar"')

    def testNewline(self):
        text = "a \nb"
        self.checkTags([SYMBOL, NEWLINE, SYMBOL, EOF], text)

    def testComment(self):
        text = "a//this is a comment\nb"
        self.checkTags([SYMBOL, NEWLINE, SYMBOL, EOF], text)

    def testLocation(self):
        text = "a\n b"
        tokens = lex("test", text)
        self.assertEquals("b", tokens[3].text)
        self.assertEqual(Location("test", 2, 2, 2, 3), tokens[3].location)

    def testError(self):
        with self.assertRaises(LexException):
            lex("test", "`")

    def testNoNewlineForEmptyLine(self):
        self.checkTags([SYMBOL, NEWLINE, SYMBOL, EOF], "a\n\nb")
        self.checkTags([SYMBOL, EOF], "//comment\na")

    def testEscapeNewline(self):
        self.checkTags([SYMBOL, SYMBOL, EOF], "a\\\nb")

    def testNestedNewline(self):
        self.checkTags([LPAREN, RPAREN, EOF], "(\n)")
        self.checkTags([LBRACK, RBRACK, EOF], "[\n]")
        self.checkTags([LBRACE, RBRACE, EOF], "{\n}")

    def testNoIndentBlankLine(self):
        self.checkTags([SYMBOL, NEWLINE, SYMBOL, EOF], "a\n    \nb")

    def testNoOutdentBlankLine(self):
        self.checkTags([INDENT, SYMBOL, NEWLINE, SYMBOL, NEWLINE, OUTDENT, EOF],
                       "  a\n\n  b\n")

    def testIndentCommentLine(self):
        self.checkTags([SYMBOL, NEWLINE, INDENT, OUTDENT, SYMBOL, EOF],
                       "a\n  // com\nb")

    def testOutdentCommentLine(self):
        self.checkTags([INDENT, SYMBOL, OUTDENT, NEWLINE, INDENT, SYMBOL, OUTDENT, EOF],
                       "  a\n//c\n  b")

    def testIndentRegularLine(self):
        self.checkTags([SYMBOL, NEWLINE, INDENT, SYMBOL, OUTDENT, NEWLINE, SYMBOL, EOF],
                       "a\n  b\nc")

    def testOutdentRegularLine(self):
        self.checkTags([INDENT, SYMBOL, OUTDENT, NEWLINE, SYMBOL, NEWLINE, INDENT, SYMBOL, OUTDENT, EOF],
                       "  a\nb\n  c")

    def testMultipleOutdent(self):
        self.checkTags([INDENT, SYMBOL, NEWLINE, INDENT, SYMBOL, OUTDENT, OUTDENT, NEWLINE, SYMBOL, EOF],
                       "  a\n    b\nc")

    def testNestedIndent(self):
        self.checkTags([SYMBOL, LPAREN, SYMBOL, RPAREN, EOF], "a(\n  b)")

    def testNestedOutdent(self):
        self.checkTags([INDENT, SYMBOL, LPAREN, SYMBOL, RPAREN, OUTDENT, EOF],
                       "  a(\nb)")

    def testNonMatchingIndentation(self):
        self.assertRaises(LexException, lex, "test", " \ta\n\t b")
        self.assertRaises(LexException, lex, "test", " \ta\n\t  b")
        self.assertRaises(LexException, lex, "test", " \ta\n\tb")

    def testCommentEof(self):
        self.checkTags([SYMBOL, NEWLINE, EOF], "a\n//com")

    def testMultipleOutdentEof(self):
        self.checkTags([INDENT, SYMBOL, NEWLINE, INDENT, SYMBOL, OUTDENT, OUTDENT, EOF],
                       "  a\n    b")

if __name__ == "__main__":
    unittest.main()

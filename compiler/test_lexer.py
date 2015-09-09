# Copyright 2014, Jay Conrod. All rights reserved.
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
        self.assertEquals(len(expected), len(tags))
        for i in range(len(expected)):
            self.assertIs(expected[i], tags[i])

    def checkText(self, expected, text):
        tokens = lex("test", text)
        texts = [t.text for t in tokens]
        self.assertEqual(expected, texts)

    def checkTag(self, expected, text):
        tokens = lex("test", text)
        self.assertEqual(1, len(tokens))
        self.assertIs(expected, tokens[0].tag)

    def testEmpty(self):
        self.checkTags([], "")

    def testBasic(self):
        text = "var x"
        self.checkTags([RESERVED, SPACE, SYMBOL], text)
        self.checkText(["var", " ", "x"], text)

    def testOperators(self):
        for op in ["|", "&&", "<=", "::", "=="]:
            self.checkTags([OPERATOR], op)

    def testReservedOperators(self):
        for op in ["=", "=>", "<:", ">:", ":"]:
            self.checkTags([RESERVED], op)

    def testSymbols(self):
        for sym in ["a", "a0_", "_a", "__", "_0", "A_0", "A-0"]:
            self.checkTags([SYMBOL], sym)
        self.checkTags([RESERVED], "_")

    def testQuotedSymbol(self):
        self.checkTags([SYMBOL], r"`fo\`o`")

    def testIntegers(self):
        for i in ["0", "1", "123", "-1", "-123", "-0"]:
            self.checkTags([INTEGER], i)

    def testSizedIntegers(self):
        for i in ["0i8", "12i16", "123i64", "-1i32", "-123i8", "-0i32"]:
            self.checkTags([INTEGER], i)

    def testHexIntegers(self):
        for i in ["0x0", "0xabc", "0xABC", "0XAb0", "-0xAb1", "0xAb1i32", "-0xAb1i32"]:
            self.checkTags([INTEGER], i)

    def testBinaryIntegers(self):
        for i in ["0b0", "0b1000101", "-0b1", "0b1i32", "-0b1i16"]:
            self.checkTags([INTEGER], i)

    def testFloats(self):
        for i in ["1.", ".1", "123.45", "-1.", "-.1", "+.1",
                  "1.2e3", "-1.2E34", "9e8", "-9e+8", "-9e-8", "1.2f32", "-1e2f64", "-1f64"]:
            self.checkTags([FLOAT], i)

    def testStrings(self):
        self.checkTags([STRING], '""')
        self.checkTags([STRING], '"foo"')
        self.checkTags([STRING, INTEGER, STRING], '"foo"123"bar"')
        self.checkTags([STRING], r'"foo\"123\"bar"')

    def testNewline(self):
        text = "a \n b"
        self.checkTags([SYMBOL, SPACE, NEWLINE, SPACE, SYMBOL], text)

    def testComment(self):
        text = "a//this is a comment\nb"
        self.checkTags([SYMBOL, COMMENT, NEWLINE, SYMBOL], text)

    def testLocation(self):
        text = "a\n b"
        tokens = lex("test", text)
        self.assertEqual(4, len(tokens))
        self.assertEqual(Location("test", 2, 2, 2, 3), tokens[-1].location)

    def testError(self):
        with self.assertRaises(LexException):
            lex("test", "`")

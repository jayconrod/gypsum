# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

execfile("combinators.py")

from tok import *
from lexer import *

symbol = Tag(SYMBOL)


def tokenIsPrintable(token):
    return token.tag in [RESERVED, SYMBOL]


def makeReader(text):
    filename = "test"
    tokens = filter(tokenIsPrintable, lex(filename, text))
    return Reader(filename, tokens)


class TestCombinators(unittest.TestCase):
    def checkParse(self, expected, parser, text):
        reader = makeReader(text)
        result = parser(reader)
        self.assertTrue(result)
        value = result.value
        self.assertEqual(expected, value)

    def testReserved(self):
        parser = Reserved(RESERVED, "var")
        self.checkParse("var", parser, "var")

    def testReservedEmpty(self):
        parser = Reserved(RESERVED, "var")
        reader = makeReader("")
        self.assertTrue(reader.isEmpty())
        result = parser(reader)
        self.assertFalse(result)

    def testTag(self):
        parser = symbol
        self.checkParse("xyz", parser, "xyz")

    def testCommit(self):
        parser = Commit(symbol)
        reader = makeReader("")
        result = parser(reader)
        self.assertFalse(result)
        self.assertEquals(False, result.retry)

    def testPhrase(self):
        parser = Phrase(symbol)
        reader = makeReader("a b")
        result = parser(reader)
        self.assertFalse(result)

    def testProcess(self):
        parser = symbol ^ (lambda p, loc: len(p))
        self.checkParse(3, parser, "abc")

    def testProcessFailure(self):
        message = "starts with a"
        def process(parsed, _):
            if parsed[0] == "a":
                return FailValue(message)
            else:
                return parsed
        parser = symbol ^ process
        reader = makeReader("abc")
        result = parser(reader)
        self.assertTrue(isinstance(result, Failure))
        self.assertEqual(message, result.message)

    def testOptSuccess(self):
        parser = Opt(symbol)
        self.checkParse("a", parser, "a")

    def testOptFail(self):
        parser = Opt(symbol)
        self.checkParse(None, parser, "var")

    def testOptCommit(self):
        parser = Opt(Commit(symbol))
        reader = makeReader("var")
        result = parser(reader)
        self.assertFalse(result)
        self.assertEqual(False, result.retry)

    def testConcat(self):
        parser = symbol + symbol
        self.checkParse(("a", "b"), parser, "a b")

    def testAlternate(self):
        parser = symbol | Reserved(RESERVED, "var")
        self.checkParse("abc", parser, "abc")
        self.checkParse("var", parser, "var")

    def testAlternateCommit(self):
        parser = Commit(symbol) | Reserved(RESERVED, "var")
        reader = makeReader("var")
        result = parser(reader)
        self.assertFalse(result)
        self.assertEquals(False, result.retry)

    def testRepEmpty(self):
        parser = Rep(symbol)
        self.checkParse([], parser, "var")

    def testRep(self):
        parser = Rep(symbol)
        self.checkParse(["a", "b", "c"], parser, "a b c")

    def testRepCommit(self):
        parser = Rep(Commit(symbol))
        reader = makeReader("var")
        result = parser(reader)
        self.assertFalse(result)
        self.assertEqual(False, result.retry)

    def testRep1Empty(self):
        parser = Rep1(symbol)
        reader = makeReader("")
        result = parser(reader)
        self.assertFalse(result)

    def testRep1(self):
        parser = Rep1(symbol)
        self.checkParse(["a", "b"], parser, "a b")

    def testRep1SepOne(self):
        parser = Rep1Sep(symbol, Reserved(RESERVED, ","))
        self.checkParse(["a"], parser, "a")

    def testRep1SepMany(self):
        parser = Rep1Sep(symbol, Reserved(RESERVED, ","))
        self.checkParse(["a", "b", "c"], parser, "a,b,c")

    def testLeftRec(self):
        def combine(sym, next, loc):
            return sym + next[1]
        parser = LeftRec(symbol, Reserved(RESERVED, ",") + symbol, combine)
        self.checkParse("abc", parser, "a,b,c")

    def testLeftRecStopInCombine(self):
        def combine(sym, next, _):
            if next is None:
                return FailValue()
            else:
                return sym + next[1]
        parser = LeftRec(symbol, Opt(Reserved(RESERVED, ",") + symbol), combine)
        self.checkParse("abc", parser, "a,b,c")

    def testUntangle(self):
        tangled = (1, (2, (3, 4), 5))
        self.assertEqual([1, 2, 3, 4, 5], untangle(tangled))

# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from lexer import *
from layout import layout

class TestLayout(unittest.TestCase):
    def checkLayout(self, expectedTexts, text):
        rawTokens = lex("(test)", text)
        layoutTokens = layout(rawTokens)
        layoutTexts = [token.text for token in layoutTokens]
        self.assertEquals(expectedTexts, layoutTexts)

    def testEmpty(self):
        self.checkLayout([], "")

    def testSpace(self):
        self.checkLayout([], "  \n  ")

    def testContinuation(self):
        self.checkLayout(["a", ";"], "  a\n")

    def testIndentDef(self):
        self.checkLayout(["def", "a", "=", "{", "a", ";", "}", ";"],
                         "def a =\n" + \
                         "  a")

    def testNoIndentDef(self):
        self.checkLayout(["def", "a", "=", "{", "a", ";", "}", ";"],
                         "def a = {\n" + \
                         "  a;\n" + \
                         "};")

    def testIndentClass(self):
        self.checkLayout(["class", "C", "{", "var", "x", ":", "int", ";", "}", ";"],
                         "class C\n" + \
                         "  var x: int")

    def testIndentTrait(self):
        self.checkLayout(["trait", "Tr", "{", "var", "x", ":", "int", ";", "}", ";"],
                         "trait Tr\n" + \
                         "  var x: int")

    def testIndentIf(self):
        self.checkLayout(["if", "(", "a", ")", "{", "a", ";", "}", ";"],
                         "if (a)\n" + \
                         "  a")

    def testIndentIfElse(self):
        self.checkLayout(["if", "(", "a", ")", "{", "b", ";", "}", "else", "{", "c", ";", "}", ";"],
                         "if (a)\n" +
                         "  b\n" +
                         "else\n" +
                         "  c")

    def testIndentIfElseNested(self):
        self.checkLayout(["if", "(", "a", ")", "{",
                            "if", "(", "b", ")", "{",
                              "c", ";",
                            "}", ";",
                          "}", "else", "{",
                            "d", ";",
                          "}", ";"],
                         "if (a)\n" +
                         "  if (b)\n" +
                         "    c\n" +
                         "else\n" +
                         "  d")

    def testMultipleUnindent(self):
        self.checkLayout(["if", "(", "a", ")", "{",
                            "if", "(", "b", ")", "{",
                              "if", "(", "c", ")", "{",
                                "d", ";",
                              "}", ";",
                            "}", ";",
                          "}", ";"],
                         "if (a)\n" +
                         "  if (b) {\n" +
                         "    if (c)\n" +
                         "      d\n" +
                         "  }")

    def testIfExpr(self):
        self.checkLayout(["var", "a", "=", "if", "(", "b", ")", "{",
                            "c", ";",
                          "}", "else", "{",
                            "d", ";",
                          "}", ";"],
                         "var a = if (b)\n" + \
                         "  c\n" + \
                         "else\n" + \
                         "  d")

    def testContinuationAndIndent(self):
        self.checkLayout(["if", "(", "a", "&&",
                              "b", ")", "{",
                            "c", ";",
                          "}", ";"],
                         "if (a &&\n" +
                         "    b)\n" +
                         "  c")

    def testWhileExpr(self):
        self.checkLayout(["while", "(", "a", ")", "{",
                            "b", ";",
                          "}", ";"],
                         "while (a)\n" +
                         "  b")

    def testMatchExpr(self):
        self.checkLayout(["match", "(", "a", ")", "{",
                            "case", "b", "=>", "c", ";",
                          "}", ";"],
                         "match (a)\n" +
                         "  case b => c")

    def testPartialFunctionCase(self):
        self.checkLayout(["case", "a", "=>", "{",
                            "b", ";",
                          "}", ";"],
                         "case a =>\n" +
                         "  b")

    def testTryCatchExpr(self):
        self.checkLayout(["try", "{",
                            "a", ";",
                          "}", "catch", "{",
                            "case", "b", "=>", "c", ";",
                          "}", "finally", "{",
                            "d", ";",
                          "}", ";"],
                         "try\n" +
                         "  a\n" +
                         "catch\n" +
                         "  case b => c\n" +
                         "finally\n" +
                         "  d")

    def testTryCatchSimpleExpr(self):
        self.checkLayout(["try", "{",
                            "a", ";",
                          "}", "catch", "(", "b", ")", "{",
                            "c", ";",
                          "}", ";"],
                         "try\n" + \
                         "  a\n" + \
                         "catch (b)\n" + \
                         "  c")

    def testLambdaExpr(self):
        self.checkLayout(["lambda", "(", "a", ")", "{",
                            "b", ";",
                          "}", ";"],
                         "lambda (a)\n" +
                         "  b")

    def testWhileInsideDef(self):
        self.checkLayout(["def", "f", "=", "{",
                            "while", "(", "a", ")", "{",
                              "b", ";",
                            "}", ";",
                            "c", ";",
                          "}", ";"],
                         "def f =\n" +
                         "  while (a)\n" +
                         "    b\n" +
                         "  c")

    def testUnitBlockSameLevel(self):
        self.checkLayout(["def", "f", "=", "{",
                            "g", ";",
                            "{", "}", ";",
                          "}", ";"],
                         "def f =\n" +
                         "  g\n" +
                         "  {}")

    def testUnitBlockDedented(self):
        self.checkLayout(["def", "f", "=", "{",
                            "while", "(", "a", ")", "{",
                              "b", ";",
                            "}", ";",
                            "{", "}", ";",
                          "}", ";"],
                         "def f =\n" +
                         "  while (a)\n" +
                         "    b\n" +
                         "  {}")

    def testUnitBlockAsWhileBody(self):
        self.checkLayout(["def", "f", "=", "{",
                            "while", "(", "a", ")",
                              "{", "}", ";",
                          "}", ";"],
                         "def f =\n" +
                         "  while (a)\n" +
                         "  {}")


if __name__ == "__main__":
    unittest.main()

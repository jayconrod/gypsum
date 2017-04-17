# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.

NEWLINE = "newline"
SPACE = "space"
COMMENT = "comment"
EOF = "end of file"

LBRACK = "["
RBRACK = "]"
LPAREN = "("
RPAREN = ")"
LBRACE = "{"
RBRACE = "}"
SUBTYPE = "<:"
SUPERTYPE = ">:"
BIG_ARROW = "=>"
SMALL_ARROW = "->"
UNDERSCORE = "_"
DOT = "."
COMMA = ","
COLON = ":"
SEMI = ";"
EQ = "="

VAR = "var"
LET = "let"
DEF = "def"
CLASS = "class"
TRAIT = "trait"
ARRAYELEMENTS = "arrayelements"
IMPORT = "import"
AS = "as"
IF = "if"
ELSE = "else"
WHILE = "while"
BREAK = "break"
CONTINUE = "continue"
CASE = "case"
MATCH = "match"
THROW = "throw"
TRY = "try"
CATCH = "catch"
FINALLY = "finally"
NEW = "new"
LAMBDA = "lambda"
RETURN = "return"
UNIT = "unit"
I8 = "i8"
I16 = "i16"
I32 = "i32"
I64 = "i64"
F32 = "f32"
F64 = "f64"
BOOLEAN = "boolean"
FORSOME = "forsome"
TRUE = "true"
FALSE = "false"
THIS = "this"
SUPER = "super"
NULL = "null"

ATTRIB = "attribute"
SYMBOL = "symbol"
OPERATOR = "operator"
INTEGER = "integer"
FLOAT = "float"
STRING = "string"

IMPLICIT_LBRACE = "implicit {"
IMPLICIT_RBRACE = "implicit }"
IMPLICIT_SEMI = "implicit ;"

class Token(object):
    def __init__(self, text, tag, location):
        self.text = text
        self.tag = tag
        self.location = location

    def __str__(self):
        return '("%s", %s) @ %s' % (self.text, self.tag, str(self.location))

    def __repr__(self):
        return "Token(%s, %s, %s)" % (self.text, self.tag, repr(self.location))

    def __eq__(self, other):
        return self.text == other.text and self.tag is other.tag

    def isPrintable(self):
        return self.tag not in [NEWLINE, SPACE, COMMENT]

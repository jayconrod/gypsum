# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import re

from ast import *
from combinators import *
from token import *
from utils import *


# Main function
def parse(filename, tokens):
    reader = Reader(filename, tokens)
    parser = module()
    result = parser(reader)
    if not result:
        raise ParseException(result.location, result.message)
    else:
        addNodeIds(result.value)
        return result.value


class ParseException(Exception):
    def __init__(self, location, message):
        self.location = location
        self.message = message

    def __str__(self):
        return "%s: error: %s\n" % (self.location, self.message)


# Top level
def module():
    def process(parsed):
        return AstModule(parsed)
    return Phrase(Rep(definition())) ^ process


# Definitions
def definition():
    return varDefn() | functionDefn() | classDefn()


def attribs():
    return Rep(attrib())


def attrib():
    return Tag(ATTRIB) ^ AstAttribute


def varDefn():
    def process(parsed):
        [ats, _, pat, expr, _] = untangle(parsed)
        return AstVariableDefinition(ats, pat, expr)
    exprOpt = Opt(keyword("=") + expression() ^ (lambda p: p[1]))
    return attribs() + keyword("var") + Commit(pattern() + exprOpt + semi) ^ process


def functionDefn():
    def process(parsed):
        [ats, _, name, tps, ps, rty, body, _] = untangle(parsed)
        return AstFunctionDefinition(ats, name, tps, ps, rty, body)
    functionName = keyword("this") | symbol
    bodyOpt = Opt(keyword("=") + expression() ^ (lambda p: p[1]))
    return attribs() + keyword("def") + Commit(functionName + typeParameters() + parameters() +
        tyOpt() + bodyOpt + semi) ^ process


def classDefn():
    def process(parsed):
        [ats, _, name, tps, ctor, stys, ms, _] = untangle(parsed)
        return AstClassDefinition(ats, name, tps, ctor, stys, ms)
    classBodyOpt = Opt(layoutBlock(Rep(Lazy(definition)))) ^ \
                   (lambda p: p if p is not None else [])
    return attribs() + keyword("class") + Commit(symbol + typeParameters() +
           Opt(constructor()) + supertypes() + classBodyOpt + semi) ^ process


def constructor():
    def process(parsed):
        [ats, _, params, _] = untangle(parsed)
        return AstPrimaryConstructorDefinition(ats, params)
    return attribs() + keyword("(") + \
           Commit(RepSep(parameter(), keyword(",")) + keyword(")")) ^ process


def supertypes():
    def process(parsed):
        return untangle(parsed)[1] if parsed else []
    return Opt(keyword("<:") + Rep1Sep(classType(), keyword(","))) ^ process


def typeParameters():
    def process(parsed):
        return untangle(parsed)[1] if parsed else []
    return Opt(keyword("[") + Rep1Sep(typeParameter(), keyword(",")) + keyword("]")) ^ process


def typeParameter():
    def process(parsed):
        (((ats, name), upper), lower) = parsed
        upper = upper[1] if upper else None
        lower = lower[1] if lower else None
        return AstTypeParameter(ats, name, upper, lower)
    return attribs() + symbol + Opt(keyword("<:") + ty()) + Opt(keyword(">:") + ty()) ^ process


def parameters():
    def process(parsed):
        return untangle(parsed)[1] if parsed else []
    return Opt(keyword("(") + Rep1Sep(parameter(), keyword(",")) + keyword(")")) ^ process


def parameter():
    def process(parsed):
        [ats, pat] = untangle(parsed)
        return AstParameter(ats, pat)
    return attribs() + pattern() ^ process


# Patterns
def pattern():
    return varPattern()


def varPattern():
    def process(parsed):
        (name, parsedTy) = parsed
        ty = parsedTy[1] if parsedTy else None
        return AstVariablePattern(name, ty)
    return symbol + Opt(keyword(":") + ty()) ^ process


# Types
def ty():
    return simpleType() | classType()


def tyOpt():
    return Opt(keyword(":") + ty() ^ (lambda p: p[1]))


def simpleType():
    return (keyword("unit") ^ (lambda p: AstUnitType())) | \
           (keyword("i8") ^ (lambda p: AstI8Type())) | \
           (keyword("i16") ^ (lambda p: AstI16Type())) | \
           (keyword("i32") ^ (lambda p: AstI32Type())) | \
           (keyword("i64") ^ (lambda p: AstI64Type())) | \
           (keyword("f32") ^ (lambda p: AstF32Type())) | \
           (keyword("f64") ^ (lambda p: AstF64Type())) | \
           (keyword("boolean") ^ (lambda p: AstBooleanType()))


def classType():
    def process(parsed):
        name, typeArgs, nullFlag = untangle(parsed)
        typeArgs = [] if typeArgs is None else typeArgs
        flags = set([nullFlag]) if nullFlag else set()
        return AstClassType(name, typeArgs, flags)
    return symbol + typeArguments() + Opt(Reserved(OPERATOR, "?")) ^ process


def typeArguments():
    def process(parsed):
        return untangle(parsed)[1] if parsed else None
    return Opt(keyword("[") + Rep1Sep(Lazy(ty), keyword(",")) + keyword("]")) ^ process


# Expressions
def expression():
    def combine(left, right):
        return AstAssignExpression(left, right)
    rhs = keyword("=") + Lazy(expression) ^ (lambda p: p[1])
    return LeftRec(maybeBinopExpr(), rhs, combine)


def maybeBinopExpr():
    binopLevels = [[],   # other
                   ["*", "/", "%"],
                   ["+", "-"],
                   [":"],
                   ["=", "!"],
                   ["<", ">"],
                   ["&"],
                   ["^"],
                   ["|"]]
    BINOP_OTHER_LEVEL = 0
    BINOP_LOGIC_AND_LEVEL = len(binopLevels)
    BINOP_LOGIC_OR_LEVEL = BINOP_LOGIC_AND_LEVEL + 1
    BINOP_ASSIGN_LEVEL = BINOP_LOGIC_OR_LEVEL + 1
    NUM_BINOP_LEVELS = BINOP_ASSIGN_LEVEL + 1
    def precedenceOf(op):
        if op[-1] == "=" and \
           not (len(op) > 1 and op[0] == "=") and \
           op not in ["<=", ">=", "!="]:
            return BINOP_ASSIGN_LEVEL
        elif op == "&&":
            return BINOP_LOGIC_AND_LEVEL
        elif op == "||":
            return BINOP_LOGIC_OR_LEVEL
        else:
            for i in range(0, len(binopLevels)):
                if op[0] in binopLevels[i]:
                    return i
            return BINOP_OTHER_LEVEL

    LEFT = "left"
    RIGHT = "right"
    def associativityOf(op):
        return RIGHT if op[-1] == ":" else LEFT

    def buildBinaryParser(term, level):
        return LeftRec(term, binarySuffix(term, level), processBinary) ^ postProcessBinary

    def binarySuffix(term, level):
        return If(operator, lambda op: precedenceOf(op) == level) + Commit(term)

    def processBinary(left, parsed):
        (op, right) = parsed
        if not isinstance(left, list):
            left = [(None, left)]
        return left + [(op, right)]

    def postProcessBinary(expr):
        if not isinstance(expr, list):
            return expr
        else:
            assert len(expr) > 1
            associativity = associativityOf(expr[1][0])
            if associativity is LEFT:
                result = expr[0][1]
                for i in range(1, len(expr)):
                    (op, subexpr) = expr[i]
                    if associativityOf(op) is not LEFT:
                        return FailValue("left and right associativie operators are mixed together")
                    result = AstBinaryExpression(op, result, subexpr)
                return result
            else:
                result = expr[-1][1]
                for i in range(-1, -len(expr), -1):
                    op = expr[i][0]
                    subexpr = expr[i-1][1]
                    if associativityOf(op) is not RIGHT:
                        return FailValue("left and right associativie operators are mixed together")
                    result = AstBinaryExpression(op, subexpr, result)
                return result

    parser = maybeCallExpr()
    for i in xrange(0, NUM_BINOP_LEVELS):
        parser = buildBinaryParser(parser, i)
    return parser


def maybeCallExpr():
    return LeftRec(receiverExpr(), callSuffix(), processCall)

def callSuffix():
    methodNameOpt = Opt(keyword(".") + symbol ^ (lambda p: p[1]))
    argumentsOpt = Opt(keyword("(") + RepSep(Lazy(expression), keyword(",")) + keyword(")")) ^ \
        (lambda p: untangle(p)[1] if p else None)
    getMethodOpt = Opt(keyword("_")) ^ (lambda p: bool(p))
    return methodNameOpt + typeArguments() + argumentsOpt + getMethodOpt

def processCall(receiver, parsed):
    [methodName, typeArguments, arguments, isGetMethod] = untangle(parsed)
    if isGetMethod:
        if methodName is None or \
           typeArguments is not None or \
           arguments is not None:
            return FailValue()
        return AstFunctionValueExpression(AstPropertyExpression(receiver, methodName))
    elif methodName is not None or \
         typeArguments is not None or \
         arguments is not None:
        hasArguments = typeArguments is not None or arguments is not None
        typeArguments = [] if typeArguments is None else typeArguments
        arguments = [] if arguments is None else arguments
        if methodName is not None:
            method = AstPropertyExpression(receiver, methodName)
        else:
            method = receiver
        if hasArguments:
            return AstCallExpression(method, typeArguments, arguments)
        else:
            return method
    else:
        return FailValue("not a call")


def receiverExpr():
    return unaryExpr() | \
           literalExpr() | \
           varExpr() | \
           thisExpr() | \
           superExpr() | \
           groupExpr() | \
           ifExpr() | \
           whileExpr() | \
           breakExpr() | \
           continueExpr() | \
           partialFnExpr() | \
           matchExpr() | \
           throwExpr() | \
           tryCatchExpr() | \
           blockExpr() | \
           lambdaExpr() | \
           returnExpr()


def literalExpr():
    return literal() ^ (lambda p: AstLiteralExpression(p))


def varExpr():
    return symbol ^ AstVariableExpression


def thisExpr():
    return keyword("this") ^ (lambda p: AstThisExpression())


def superExpr():
    return keyword("super") ^ (lambda p: AstSuperExpression())


def groupExpr():
    def process(parsed):
        [_, e, _] = untangle(parsed)
        return e
    return keyword("(") + Lazy(expression) + keyword(")") ^ process


def blockExpr():
    return layoutBlock(Rep(Lazy(statement)) ^ (lambda stmts: AstBlockExpression(stmts)))


def unaryExpr():
    def process(parsed):
        (op, e) = parsed
        return AstUnaryExpression(op, e)
    op = If(operator, lambda op: op in ["!", "-", "+", "~"])
    return op + Commit(Lazy(expression)) ^ process


def ifExpr():
    def process(parsed):
        [_, _, c, _, t, f] = untangle(parsed)
        return AstIfExpression(c, t, f)
    elseClause = Opt(keyword("else") + Commit(Lazy(expression))) ^ (lambda p: p[1] if p else None)
    return keyword("if") + Commit(keyword("(") + Lazy(expression) + keyword(")") + \
        Lazy(expression) + elseClause) ^ process


def whileExpr():
    def process(parsed):
        [_, _, c, _, b] = untangle(parsed)
        return AstWhileExpression(c, b)
    return keyword("while") + Commit(keyword("(") + Lazy(expression) + keyword(")") + \
        Lazy(expression)) ^ process


def breakExpr():
    return keyword("break") ^ (lambda p: AstBreakExpression())


def continueExpr():
    return keyword("continue") ^ (lambda p: AstContinueExpression())


def partialFnExpr():
    def process(cases):
        return AstPartialFunctionExpression(cases)
    return layoutBlock(Rep1(partialFunctionCase()) ^ process)


def partialFunctionCase():
    def process(parsed):
        [_, p, c, _, e, _] = untangle(parsed)
        return AstPartialFunctionCase(p, c, e)
    conditionOpt = Opt(keyword("if") + Lazy(expression)) ^ (lambda p: p[1] if p else None)
    return keyword("case") + Commit(pattern() + conditionOpt + keyword("=>") + \
        Lazy(expression) + semi) ^ process


def matchExpr():
    def process(parsed):
        [_, _, e, _, m] = untangle(parsed)
        return AstMatchExpression(e, m)
    return keyword("match") + Commit(keyword("(") + Lazy(expression) + keyword(")") + \
        partialFnExpr()) ^ process


def throwExpr():
    def process(parsed):
        (_, x) = parsed
        return AstThrowExpression(x)
    return keyword("throw") + Commit(Lazy(expression)) ^ process


def tryCatchExpr():
    def process(parsed):
        [_, e, c, f] = untangle(parsed)
        if c is None and f is None:
            return FailValue("expected 'catch' or 'finally' in 'try'-expression")
        else:
            return AstTryCatchExpression(e, c, f)

    finallyOpt = Opt(keyword("finally") + Lazy(expression)) ^ (lambda p: p[1] if p else None)
    return keyword("try") + Commit(Lazy(expression) + catchHandler() + finallyOpt) ^ process


def catchHandler():
    def processSimple(parsed):
        [_, p, _, e] = untangle(parsed)
        return AstPartialFunctionExpression([AstPartialFunctionCase(p, None, e)])
    simpleHandler = keyword("(") + Commit(pattern() + keyword(")") + \
                    Lazy(expression)) ^ processSimple

    matchHandler = partialFnExpr()

    def process(parsed):
        return parsed[1]
    return Opt(keyword("catch") + Commit(simpleHandler | matchHandler) ^ process)


def lambdaExpr():
    def process(parsed):
        [_, n, tps, _, ps, _, b] = untangle(parsed)
        return AstLambdaExpression(n, tps, ps, b)
    return keyword("lambda") + Commit(Opt(symbol) + typeParameters() + keyword("(") + \
        RepSep(pattern(), keyword(",")) + keyword(")") + Lazy(expression)) ^ process


def returnExpr():
    def process(parsed):
        (_, e) = parsed
        return AstReturnExpression(e)
    return keyword("return") + Opt(Lazy(expression)) ^ process


def statement():
    return definition() | ((expression() + semi) ^ (lambda parsed: parsed[0]))


# Literals
def literal():
    return intLiteral() | floatLiteral() | booleanLiteral() | nullLiteral() | stringLiteral()


def intLiteral():
    def process(t):
        m = re.match("([+-]?)(0[BbXx])?([0-9A-Fa-f]+)(?:i([0-9]+))?", t)
        if m.group(1) == '-':
            sign = -1
        else:
            sign = 1
        if m.group(2) is None:
            base = 10
        elif m.group(2) in ["0B", "0b"]:
            base = 2
        else:
            assert m.group(2) in ["0X", "0x"]
            base = 16
        # Python has automatic big integers, so this conversion will be accurate. We will
        # check if the value and width are valid in type analysis.
        value = sign * int(m.group(3), base)
        if m.group(4) is None:
            width = 64
        else:
            width = int(m.group(4))
        return AstIntegerLiteral(value, width)

    return Tag(INTEGER) ^ process


def floatLiteral():
    def process(t):
        m = re.match("([^f]*)(?:f([0-9]+))?", t)
        value = float(m.group(1))
        width = int(m.group(2)) if m.group(2) is not None else 64
        return AstFloatLiteral(value, width)

    return Tag(FLOAT) ^ process


def booleanLiteral():
    return (keyword("true") ^ (lambda p: AstBooleanLiteral(True))) | \
           (keyword("false") ^ (lambda p: AstBooleanLiteral(False)))


def nullLiteral():
    return keyword("null") ^ (lambda p: AstNullLiteral())


def stringLiteral():
    def process(t):
        value = tryDecodeString(t)
        if value is None:
            return FailValue("invalid string")
        return AstStringLiteral(value)
    return Tag(STRING) ^ process


# Basic parsers
def keyword(kw):
    return Reserved(RESERVED, kw)

def layoutBlock(contents):
    def process(parsed):
        [_, ast, _] = untangle(parsed)
        return ast
    return ((keyword("{") + contents + keyword("}")) |
            (Reserved(INTERNAL, "{") + contents + Reserved(INTERNAL, "}"))) ^ process

symbol = Tag(SYMBOL)
operator = Tag(OPERATOR)
id = symbol | operator

semi = keyword(";") | Reserved(INTERNAL, ";")

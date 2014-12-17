# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import re

from ast import *
from combinators import *
from errors import *
from tok import *
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


# Top level
def module():
    def process(parsed, loc):
        return AstModule(parsed, loc)
    return Phrase(Rep(definition())) ^ process


# Definitions
def definition():
    return varDefn() | functionDefn() | classDefn()


def attribs():
    return Rep(attrib())


def attrib():
    return Tag(ATTRIB) ^ (lambda name, loc: AstAttribute(name, loc))


def varDefn():
    def process(parsed, loc):
        [ats, _, pat, expr, _] = untangle(parsed)
        return AstVariableDefinition(ats, pat, expr, loc)
    exprOpt = Opt(keyword("=") + expression() ^ (lambda p, _: p[1]))
    return attribs() + keyword("var") + Commit(pattern() + exprOpt + semi) ^ process


def functionDefn():
    def process(parsed, loc):
        [ats, _, name, tps, ps, rty, body, _] = untangle(parsed)
        return AstFunctionDefinition(ats, name, tps, ps, rty, body, loc)
    functionName = keyword("this") | symbol
    bodyOpt = Opt(keyword("=") + expression() ^ (lambda p, _: p[1]))
    return attribs() + keyword("def") + Commit(functionName + typeParameters() + parameters() +
        tyOpt() + bodyOpt + semi) ^ process


def classDefn():
    def process(parsed, loc):
        [ats, _, name, tps, ctor, sty, sargs, ms, _] = untangle(parsed)
        return AstClassDefinition(ats, name, tps, ctor, sty, sargs, ms, loc)
    classBodyOpt = Opt(layoutBlock(Rep(Lazy(definition)))) ^ \
                   (lambda p, _: p if p is not None else [])
    return attribs() + keyword("class") + Commit(symbol + typeParameters() +
           constructor() + superclass() + classBodyOpt + semi) ^ process


def constructor():
    def process(parsed, loc):
        [ats, _, params, _] = untangle(parsed)
        return AstPrimaryConstructorDefinition(ats, params, loc)
    return Opt(attribs() + keyword("(") + \
               Commit(RepSep(parameter(), keyword(",")) + keyword(")")) ^ process)


def superclass():
    def processArgs(parsed, loc):
        return untangle(parsed)[1] if parsed is not None else []
    args = Opt(keyword("(") + RepSep(expression(), keyword(",")) + keyword(")")) ^ processArgs
    def process(parsed, loc):
        if parsed is None:
            return None, None
        else:
            [_, supertype, args] = untangle(parsed)
            return supertype, args
    return Opt(keyword("<:") + classType() + args) ^ process


def supertypes():
    def process(parsed, _):
        return untangle(parsed)[1] if parsed else []
    return Opt(keyword("<:") + Rep1Sep(classType(), keyword(","))) ^ process


def typeParameters():
    def process(parsed, _):
        return untangle(parsed)[1] if parsed else []
    return Opt(keyword("[") + Rep1Sep(typeParameter(), keyword(",")) + keyword("]")) ^ process


def typeParameter():
    def process(parsed, loc):
        (((ats, name), upper), lower) = parsed
        upper = upper[1] if upper else None
        lower = lower[1] if lower else None
        return AstTypeParameter(ats, name, upper, lower, loc)
    return attribs() + symbol + Opt(keyword("<:") + ty()) + Opt(keyword(">:") + ty()) ^ process


def parameters():
    def process(parsed, _):
        return untangle(parsed)[1] if parsed else []
    return Opt(keyword("(") + Rep1Sep(parameter(), keyword(",")) + keyword(")")) ^ process


def parameter():
    def process(parsed, loc):
        [ats, pat] = untangle(parsed)
        return AstParameter(ats, pat, loc)
    return attribs() + pattern() ^ process


# Patterns
def pattern():
    return varPattern()


def varPattern():
    def process(parsed, loc):
        (name, parsedTy) = parsed
        ty = parsedTy[1] if parsedTy else None
        return AstVariablePattern(name, ty, loc)
    return symbol + Opt(keyword(":") + ty()) ^ process


# Types
def ty():
    return simpleType() | classType()


def tyOpt():
    return Opt(keyword(":") + ty() ^ (lambda p, _: p[1]))


def simpleType():
    return (keyword("unit") ^ (lambda _, loc: AstUnitType(loc))) | \
           (keyword("i8") ^ (lambda _, loc: AstI8Type(loc))) | \
           (keyword("i16") ^ (lambda _, loc: AstI16Type(loc))) | \
           (keyword("i32") ^ (lambda _, loc: AstI32Type(loc))) | \
           (keyword("i64") ^ (lambda _, loc: AstI64Type(loc))) | \
           (keyword("f32") ^ (lambda _, loc: AstF32Type(loc))) | \
           (keyword("f64") ^ (lambda _, loc: AstF64Type(loc))) | \
           (keyword("boolean") ^ (lambda _, loc: AstBooleanType(loc)))


def classType():
    def process(parsed, loc):
        name, typeArgs, nullFlag = untangle(parsed)
        typeArgs = [] if typeArgs is None else typeArgs
        flags = set([nullFlag]) if nullFlag else set()
        return AstClassType(name, typeArgs, flags, loc)
    return symbol + typeArguments() + Opt(Reserved(OPERATOR, "?")) ^ process


def typeArguments():
    def process(parsed, _):
        return untangle(parsed)[1] if parsed else None
    return Opt(keyword("[") + Rep1Sep(Lazy(ty), keyword(",")) + keyword("]")) ^ process


# Expressions
def expression():
    def combine(left, right, loc):
        return AstAssignExpression(left, right, loc)
    rhs = keyword("=") + Lazy(expression) ^ (lambda p, _: p[1])
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

    def processBinary(left, parsed, _):
        (op, right) = parsed
        if not isinstance(left, list):
            left = [(None, left)]
        return left + [(op, right)]

    def postProcessBinary(expr, loc):
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
                    loc = result.location.combine(subexpr.location)
                    result = AstBinaryExpression(op, result, subexpr, loc)
                return result
            else:
                result = expr[-1][1]
                for i in range(-1, -len(expr), -1):
                    op = expr[i][0]
                    subexpr = expr[i-1][1]
                    if associativityOf(op) is not RIGHT:
                        return FailValue("left and right associativie operators are mixed together")
                    loc = result.location.combine(subexpr.location)
                    result = AstBinaryExpression(op, subexpr, result, loc)
                return result

    parser = maybeCallExpr()
    for i in xrange(0, NUM_BINOP_LEVELS):
        parser = buildBinaryParser(parser, i)
    return parser


def maybeCallExpr():
    return LeftRec(receiverExpr(), callSuffix(), processCall)

def callSuffix():
    methodNameOpt = Opt(keyword(".") + symbol ^ (lambda p, _: p[1]))
    argumentsOpt = Opt(keyword("(") + RepSep(Lazy(expression), keyword(",")) + keyword(")")) ^ \
        (lambda p, _: untangle(p)[1] if p else None)
    getMethodOpt = Opt(keyword("_")) ^ (lambda p, _: bool(p))
    return methodNameOpt + typeArguments() + argumentsOpt + getMethodOpt

def processCall(receiver, parsed, loc):
    [methodName, typeArguments, arguments, isGetMethod] = untangle(parsed)
    if isGetMethod:
        if methodName is None or \
           typeArguments is not None or \
           arguments is not None:
            return FailValue()
        return AstFunctionValueExpression(AstPropertyExpression(receiver, methodName, loc), loc)
    elif methodName is not None or \
         typeArguments is not None or \
         arguments is not None:
        hasArguments = typeArguments is not None or arguments is not None
        typeArguments = [] if typeArguments is None else typeArguments
        arguments = [] if arguments is None else arguments
        if methodName is not None:
            method = AstPropertyExpression(receiver, methodName, loc)
        else:
            method = receiver
        if hasArguments:
            return AstCallExpression(method, typeArguments, arguments, loc)
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
    return literal() ^ (lambda lit, loc: AstLiteralExpression(lit, loc))


def varExpr():
    return symbol ^ (lambda name, loc: AstVariableExpression(name, loc))


def thisExpr():
    return keyword("this") ^ (lambda _, loc: AstThisExpression(loc))


def superExpr():
    return keyword("super") ^ (lambda _, loc: AstSuperExpression(loc))


def groupExpr():
    def process(parsed, _):
        [_, e, _] = untangle(parsed)
        return e
    return keyword("(") + Lazy(expression) + keyword(")") ^ process


def blockExpr():
    def process(stmts, loc):
        return AstBlockExpression(stmts, loc)
    return layoutBlock(Rep(Lazy(statement)) ^ process)


def unaryExpr():
    def process(parsed, loc):
        (op, e) = parsed
        return AstUnaryExpression(op, e, loc)
    op = If(operator, lambda op: op in ["!", "-", "+", "~"])
    return op + Commit(Lazy(expression)) ^ process


def ifExpr():
    def process(parsed, loc):
        [_, _, c, _, t, f] = untangle(parsed)
        return AstIfExpression(c, t, f, loc)
    elseClause = Opt(keyword("else") + Commit(Lazy(expression))) ^ \
        (lambda p, _: p[1] if p else None)
    return keyword("if") + Commit(keyword("(") + Lazy(expression) + keyword(")") + \
        Lazy(expression) + elseClause) ^ process


def whileExpr():
    def process(parsed, loc):
        [_, _, c, _, b] = untangle(parsed)
        return AstWhileExpression(c, b, loc)
    return keyword("while") + Commit(keyword("(") + Lazy(expression) + keyword(")") + \
        Lazy(expression)) ^ process


def breakExpr():
    return keyword("break") ^ (lambda _, loc: AstBreakExpression(loc))


def continueExpr():
    return keyword("continue") ^ (lambda _, loc: AstContinueExpression(loc))


def partialFnExpr():
    def process(cases, loc):
        return AstPartialFunctionExpression(cases, loc)
    return layoutBlock(Rep1(partialFunctionCase()) ^ process)


def partialFunctionCase():
    def process(parsed, loc):
        [_, p, c, _, e, _] = untangle(parsed)
        return AstPartialFunctionCase(p, c, e, loc)
    conditionOpt = Opt(keyword("if") + Lazy(expression)) ^ (lambda p, _: p[1] if p else None)
    return keyword("case") + Commit(pattern() + conditionOpt + keyword("=>") + \
        Lazy(expression) + semi) ^ process


def matchExpr():
    def process(parsed, loc):
        [_, _, e, _, m] = untangle(parsed)
        return AstMatchExpression(e, m, loc)
    return keyword("match") + Commit(keyword("(") + Lazy(expression) + keyword(")") + \
        partialFnExpr()) ^ process


def throwExpr():
    def process(parsed, loc):
        (_, x) = parsed
        return AstThrowExpression(x, loc)
    return keyword("throw") + Commit(Lazy(expression)) ^ process


def tryCatchExpr():
    def process(parsed, loc):
        [_, e, c, f] = untangle(parsed)
        if c is None and f is None:
            return FailValue("expected 'catch' or 'finally' in 'try'-expression")
        else:
            return AstTryCatchExpression(e, c, f, loc)

    finallyOpt = Opt(keyword("finally") + Lazy(expression)) ^ (lambda p, _: p[1] if p else None)
    return keyword("try") + Commit(Lazy(expression) + catchHandler() + finallyOpt) ^ process


def catchHandler():
    def processSimple(parsed, loc):
        [_, p, _, e] = untangle(parsed)
        return AstPartialFunctionExpression([AstPartialFunctionCase(p, None, e, loc)], loc)
    simpleHandler = keyword("(") + Commit(pattern() + keyword(")") + \
                    Lazy(expression)) ^ processSimple

    matchHandler = partialFnExpr()

    def process(parsed, _):
        return parsed[1]
    return Opt(keyword("catch") + Commit(simpleHandler | matchHandler) ^ process)


def lambdaExpr():
    def process(parsed, loc):
        [_, n, tps, _, ps, _, b] = untangle(parsed)
        return AstLambdaExpression(n, tps, ps, b, loc)
    return keyword("lambda") + Commit(Opt(symbol) + typeParameters() + keyword("(") + \
        RepSep(pattern(), keyword(",")) + keyword(")") + Lazy(expression)) ^ process


def returnExpr():
    def process(parsed, loc):
        (_, e) = parsed
        return AstReturnExpression(e, loc)
    return keyword("return") + Opt(Lazy(expression)) ^ process


def statement():
    return definition() | ((expression() + semi) ^ (lambda p, _: p[0]))


# Literals
def literal():
    return intLiteral() | floatLiteral() | booleanLiteral() | nullLiteral() | stringLiteral()


def intLiteral():
    def process(text, loc):
        m = re.match("([+-]?)(0[BbXx])?([0-9A-Fa-f]+)(?:i([0-9]+))?", text)
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
        return AstIntegerLiteral(value, width, loc)

    return Tag(INTEGER) ^ process


def floatLiteral():
    def process(text, loc):
        m = re.match("([^f]*)(?:f([0-9]+))?", text)
        value = float(m.group(1))
        width = int(m.group(2)) if m.group(2) is not None else 64
        return AstFloatLiteral(value, width, loc)

    return Tag(FLOAT) ^ process


def booleanLiteral():
    return (keyword("true") ^ (lambda _, loc: AstBooleanLiteral(True, loc))) | \
           (keyword("false") ^ (lambda _, loc: AstBooleanLiteral(False, loc)))


def nullLiteral():
    return keyword("null") ^ (lambda _, loc: AstNullLiteral(loc))


def stringLiteral():
    def process(text, loc):
        value = tryDecodeString(text)
        if value is None:
            return FailValue("invalid string")
        return AstStringLiteral(value, loc)
    return Tag(STRING) ^ process


# Basic parsers
def keyword(kw):
    return Reserved(RESERVED, kw)

def layoutBlock(contents):
    def process(parsed, loc):
        [_, ast, _] = untangle(parsed)
        return ast
    return ((keyword("{") + contents + keyword("}")) |
            (Reserved(INTERNAL, "{") + contents + Reserved(INTERNAL, "}"))) ^ process

symbol = Tag(SYMBOL)
operator = Tag(OPERATOR)
id = symbol | operator

semi = keyword(";") | Reserved(INTERNAL, ";")

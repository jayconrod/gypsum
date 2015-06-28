# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import re

import ast
import combinators as ct
from errors import ParseException
from tok import *
from utils import tryDecodeString


# Main function
def parse(filename, tokens):
    reader = ct.Reader(filename, tokens)
    parser = module()
    result = parser(reader)
    if not result:
        raise ParseException(result.location, result.message)
    else:
        ast.addNodeIds(result.value)
        return result.value


# Top level
def module():
    def process(parsed, loc):
        return ast.AstModule(parsed, loc)
    return ct.Phrase(ct.Rep(definition())) ^ process


# Definitions
def definition():
    return varDefn() | functionDefn() | classDefn()


def attribs():
    return ct.Rep(attrib())


def attrib():
    return ct.Tag(ATTRIB) ^ (lambda name, loc: ast.AstAttribute(name, loc))


def varDefn():
    def process(parsed, loc):
        [ats, kw, pat, expr, _] = ct.untangle(parsed)
        return ast.AstVariableDefinition(ats, kw, pat, expr, loc)
    exprOpt = ct.Opt(keyword("=") + expression() ^ (lambda p, _: p[1]))
    kw = keyword("var") | keyword("let")
    return attribs() + kw + ct.Commit(pattern() + exprOpt + semi) ^ process


def functionDefn():
    def process(parsed, loc):
        [ats, _, name, tps, ps, rty, body, _] = ct.untangle(parsed)
        return ast.AstFunctionDefinition(ats, name, tps, ps, rty, body, loc)
    functionName = keyword("this") | symbol
    bodyOpt = ct.Opt(keyword("=") + expression() ^ (lambda p, _: p[1]))
    return attribs() + keyword("def") + ct.Commit(functionName + typeParameters() + parameters() +
        tyOpt() + bodyOpt + semi) ^ process


def classDefn():
    def process(parsed, loc):
        [ats, _, name, tps, ctor, sty, sargs, ms, _] = ct.untangle(parsed)
        return ast.AstClassDefinition(ats, name, tps, ctor, sty, sargs, ms, loc)
    classBodyOpt = ct.Opt(layoutBlock(ct.Rep(ct.Lazy(definition)))) ^ \
                   (lambda p, _: p if p is not None else [])
    return attribs() + keyword("class") + ct.Commit(symbol + typeParameters() +
           constructor() + superclass() + classBodyOpt + semi) ^ process


def constructor():
    def process(parsed, loc):
        [ats, _, params, _] = ct.untangle(parsed)
        return ast.AstPrimaryConstructorDefinition(ats, params, loc)
    return ct.Opt(attribs() + keyword("(") + \
               ct.Commit(ct.RepSep(parameter(), keyword(",")) + keyword(")")) ^ process)


def superclass():
    def processArgs(parsed, loc):
        return ct.untangle(parsed)[1] if parsed is not None else []
    args = ct.Opt(keyword("(") + ct.RepSep(maybeBinopExpr(), keyword(",")) + keyword(")")) ^ processArgs
    def process(parsed, loc):
        if parsed is None:
            return None, None
        else:
            [_, supertype, args] = ct.untangle(parsed)
            return supertype, args
    return ct.Opt(keyword("<:") + ty() + args) ^ process


def supertypes():
    def process(parsed, _):
        return ct.untangle(parsed)[1] if parsed else []
    return ct.Opt(keyword("<:") + ct.Rep1Sep(classType(), keyword(","))) ^ process


def typeParameters():
    def process(parsed, _):
        return ct.untangle(parsed)[1] if parsed else []
    return ct.Opt(keyword("[") + ct.Rep1Sep(typeParameter(), keyword(",")) + keyword("]")) ^ process


def typeParameter():
    def process(parsed, loc):
        [ats, var, name, upper, lower] = ct.untangle(parsed)
        return ast.AstTypeParameter(ats, var, name, upper, lower, loc)
    variance = ct.Opt(ct.Reserved(OPERATOR, "+") | ct.Reserved(OPERATOR, "-"))
    upperBound = ct.Opt(keyword("<:") + ty() ^ (lambda p, _: p[1]))
    lowerBound = ct.Opt(keyword(">:") + ty() ^ (lambda p, _: p[1]))
    return attribs() + variance + symbol + upperBound + lowerBound ^ process


def parameters():
    def process(parsed, _):
        return ct.untangle(parsed)[1] if parsed else []
    return ct.Opt(keyword("(") + ct.Rep1Sep(parameter(), keyword(",")) + keyword(")")) ^ process


def parameter():
    def process(parsed, loc):
        [ats, var, pat] = ct.untangle(parsed)
        return ast.AstParameter(ats, var, pat, loc)
    return attribs() + ct.Opt(keyword("var")) + simplePattern() ^ process


# Patterns
def pattern():
    def process(parsed, loc):
        return parsed[0] if len(parsed) == 1 else ast.AstTuplePattern(parsed, loc)
    return ct.Rep1Sep(simplePattern(), keyword(",")) ^ process


def simplePattern():
    return varPattern() | \
           blankPattern() | \
           litPattern() | \
           groupPattern()


def varPattern():
    def process(parsed, loc):
        (name, parsedTy) = parsed
        ty = parsedTy[1] if parsedTy else None
        return ast.AstVariablePattern(name, ty, loc)
    return symbol + ct.Opt(keyword(":") + ty()) ^ process


def blankPattern():
    def process(parsed, loc):
        _, parsedTy = parsed
        ty = parsedTy[1] if parsedTy else None
        return ast.AstBlankPattern(ty, loc)
    return keyword("_") + ct.Opt(keyword(":") + ty()) ^ process


def litPattern():
    return literal() ^ ast.AstLiteralPattern


def groupPattern():
    def process(parsed, loc):
        _, p, _ = ct.untangle(parsed)
        return p
    return keyword("(") + ct.Commit(ct.Lazy(pattern) + keyword(")")) ^ process


# Types
def ty():
    return simpleType() | projectedType()


def tyOpt():
    return ct.Opt(keyword(":") + ty() ^ (lambda p, _: p[1]))


def simpleType():
    return (keyword("unit") ^ (lambda _, loc: ast.AstUnitType(loc))) | \
           (keyword("i8") ^ (lambda _, loc: ast.AstI8Type(loc))) | \
           (keyword("i16") ^ (lambda _, loc: ast.AstI16Type(loc))) | \
           (keyword("i32") ^ (lambda _, loc: ast.AstI32Type(loc))) | \
           (keyword("i64") ^ (lambda _, loc: ast.AstI64Type(loc))) | \
           (keyword("f32") ^ (lambda _, loc: ast.AstF32Type(loc))) | \
           (keyword("f64") ^ (lambda _, loc: ast.AstF64Type(loc))) | \
           (keyword("boolean") ^ (lambda _, loc: ast.AstBooleanType(loc))) | \
           (keyword("_") ^ (lambda _, loc: ast.AstErasedType(loc))) | \
           tupleType()


def tupleType():
    def process(parsed, loc):
        _, first, _, rest, _, nullFlag = ct.untangle(parsed)
        flags = set([nullFlag]) if nullFlag else set()
        return ast.AstTupleType([first] + rest, flags, loc)
    return keyword("(") + ct.Commit(ct.Lazy(ty) + keyword(",") +
        ct.Rep1Sep(ct.Lazy(ty), keyword(",")) + keyword(")") +
        ct.Opt(ct.Reserved(OPERATOR, "?"))) ^ process


def classType():
    def process(parsed, loc):
        name, typeArgs, nullFlag = ct.untangle(parsed)
        typeArgs = [] if typeArgs is None else typeArgs
        flags = set([nullFlag]) if nullFlag else set()
        return ast.AstClassType(name, typeArgs, flags, loc)
    return symbol + typeArguments() + ct.Opt(ct.Reserved(OPERATOR, "?")) ^ process


def projectableType():
    return classType()


def projectedType():
    def process(parsed, loc):
        if len(parsed) == 1:
            return parsed[0]
        else:
            n = len(parsed)
            projected = parsed[n - 1]
            for i in xrange(n - 2, -1, -1):
                projected = ast.AstProjectedType(parsed[i], projected, loc)
            return projected
    return ct.Rep1Sep(projectableType(), keyword(".")) ^ process


def typeArguments():
    def process(parsed, _):
        return ct.untangle(parsed)[1] if parsed else None
    return ct.Opt(keyword("[") + ct.Rep1Sep(ct.Lazy(ty), keyword(",")) + keyword("]")) ^ process


# Expressions
def expression():
    def combine(left, right, loc):
        return ast.AstAssignExpression(left, right, loc)
    rhs = keyword("=") + ct.Lazy(expression) ^ (lambda p, _: p[1])
    return ct.LeftRec(maybeTupleExpr(), rhs, combine)


def maybeTupleExpr():
    def process(parsed, loc):
        return parsed[0] if len(parsed) == 1 else ast.AstTupleExpression(parsed, loc)
    return ct.Rep1Sep(maybeBinopExpr(), keyword(",")) ^ process


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
           op not in ["<=", ">=", "!=", "!=="]:
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
        return ct.LeftRec(term, binarySuffix(term, level), processBinary) ^ postProcessBinary

    def binarySuffix(term, level):
        return ct.If(operator, lambda op: precedenceOf(op) == level) + ct.Commit(term)

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
                        return ct.FailValue("left and right associativie operators are mixed together")
                    loc = result.location.combine(subexpr.location)
                    result = ast.AstBinaryExpression(op, result, subexpr, loc)
                return result
            else:
                result = expr[-1][1]
                for i in range(-1, -len(expr), -1):
                    op = expr[i][0]
                    subexpr = expr[i-1][1]
                    if associativityOf(op) is not RIGHT:
                        return ct.FailValue("left and right associativie operators are mixed together")
                    loc = result.location.combine(subexpr.location)
                    result = ast.AstBinaryExpression(op, subexpr, result, loc)
                return result

    parser = maybeCallExpr()
    for i in xrange(0, NUM_BINOP_LEVELS):
        parser = buildBinaryParser(parser, i)
    return parser


def maybeCallExpr():
    return ct.LeftRec(receiverExpr(), callSuffix(), processCall)

def callSuffix():
    methodNameOpt = ct.Opt(keyword(".") + symbol ^ (lambda p, _: p[1]))
    argumentsOpt = ct.Opt(keyword("(") + ct.RepSep(ct.Lazy(maybeBinopExpr), keyword(",")) + keyword(")")) ^ \
        (lambda p, _: ct.untangle(p)[1] if p else None)
    getMethodOpt = ct.Opt(keyword("_")) ^ (lambda p, _: bool(p))
    return methodNameOpt + typeArguments() + argumentsOpt + getMethodOpt

def processCall(receiver, parsed, loc):
    [methodName, typeArguments, arguments, isGetMethod] = ct.untangle(parsed)
    if isGetMethod:
        if methodName is None or \
           typeArguments is not None or \
           arguments is not None:
            return ct.FailValue()
        return ast.AstFunctionValueExpression(ast.AstPropertyExpression(receiver, methodName, loc), loc)
    elif methodName is not None or \
         typeArguments is not None or \
         arguments is not None:
        hasArguments = typeArguments is not None or arguments is not None
        typeArguments = [] if typeArguments is None else typeArguments
        arguments = [] if arguments is None else arguments
        if methodName is not None:
            method = ast.AstPropertyExpression(receiver, methodName, loc)
        else:
            method = receiver
        if hasArguments:
            return ast.AstCallExpression(method, typeArguments, arguments, loc)
        else:
            return method
    else:
        return ct.FailValue("not a call")


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
    return literal() ^ (lambda lit, loc: ast.AstLiteralExpression(lit, loc))


def varExpr():
    return symbol ^ (lambda name, loc: ast.AstVariableExpression(name, loc))


def thisExpr():
    return keyword("this") ^ (lambda _, loc: ast.AstThisExpression(loc))


def superExpr():
    return keyword("super") ^ (lambda _, loc: ast.AstSuperExpression(loc))


def groupExpr():
    def process(parsed, _):
        [_, e, _] = ct.untangle(parsed)
        return e
    return keyword("(") + ct.Lazy(expression) + keyword(")") ^ process


def blockExpr():
    def process(stmts, loc):
        return ast.AstBlockExpression(stmts, loc)
    return layoutBlock(ct.Rep(ct.Lazy(statement)) ^ process)


def unaryExpr():
    def process(parsed, loc):
        (op, e) = parsed
        return ast.AstUnaryExpression(op, e, loc)
    op = ct.If(operator, lambda op: op in ["!", "-", "+", "~"])
    return op + ct.Commit(ct.Lazy(expression)) ^ process


def ifExpr():
    def process(parsed, loc):
        [_, _, c, _, t, f] = ct.untangle(parsed)
        return ast.AstIfExpression(c, t, f, loc)
    elseClause = ct.Opt(keyword("else") + ct.Commit(ct.Lazy(expression))) ^ \
        (lambda p, _: p[1] if p else None)
    return keyword("if") + ct.Commit(keyword("(") + ct.Lazy(expression) + keyword(")") + \
        ct.Lazy(expression) + elseClause) ^ process


def whileExpr():
    def process(parsed, loc):
        [_, _, c, _, b] = ct.untangle(parsed)
        return ast.AstWhileExpression(c, b, loc)
    return keyword("while") + ct.Commit(keyword("(") + ct.Lazy(expression) + keyword(")") + \
        ct.Lazy(expression)) ^ process


def breakExpr():
    return keyword("break") ^ (lambda _, loc: ast.AstBreakExpression(loc))


def continueExpr():
    return keyword("continue") ^ (lambda _, loc: ast.AstContinueExpression(loc))


def partialFnExpr():
    def process(cases, loc):
        return ast.AstPartialFunctionExpression(cases, loc)
    return layoutBlock(ct.Rep1(partialFunctionCase()) ^ process)


def partialFunctionCase():
    def process(parsed, loc):
        [_, p, c, _, e, _] = ct.untangle(parsed)
        return ast.AstPartialFunctionCase(p, c, e, loc)
    conditionOpt = ct.Opt(keyword("if") + ct.Lazy(expression)) ^ (lambda p, _: p[1] if p else None)
    return keyword("case") + ct.Commit(pattern() + conditionOpt + keyword("=>") + \
        ct.Lazy(expression) + semi) ^ process


def matchExpr():
    def process(parsed, loc):
        [_, _, e, _, m] = ct.untangle(parsed)
        return ast.AstMatchExpression(e, m, loc)
    return keyword("match") + ct.Commit(keyword("(") + ct.Lazy(expression) + keyword(")") + \
        partialFnExpr()) ^ process


def throwExpr():
    def process(parsed, loc):
        (_, x) = parsed
        return ast.AstThrowExpression(x, loc)
    return keyword("throw") + ct.Commit(ct.Lazy(expression)) ^ process


def tryCatchExpr():
    def process(parsed, loc):
        [_, e, c, f] = ct.untangle(parsed)
        if c is None and f is None:
            return ct.FailValue("expected 'catch' or 'finally' in 'try'-expression")
        else:
            return ast.AstTryCatchExpression(e, c, f, loc)

    finallyOpt = ct.Opt(keyword("finally") + ct.Lazy(expression)) ^ (lambda p, _: p[1] if p else None)
    return keyword("try") + ct.Commit(ct.Lazy(expression) + catchHandler() + finallyOpt) ^ process


def catchHandler():
    def processSimple(parsed, loc):
        [_, p, _, e] = ct.untangle(parsed)
        return ast.AstPartialFunctionExpression([ast.AstPartialFunctionCase(p, None, e, loc)], loc)
    simpleHandler = keyword("(") + ct.Commit(pattern() + keyword(")") + \
                    ct.Lazy(expression)) ^ processSimple

    matchHandler = partialFnExpr()

    def process(parsed, _):
        return parsed[1]
    return ct.Opt(keyword("catch") + ct.Commit(simpleHandler | matchHandler) ^ process)


def lambdaExpr():
    def process(parsed, loc):
        [_, n, tps, _, ps, _, b] = ct.untangle(parsed)
        return ast.AstLambdaExpression(n, tps, ps, b, loc)
    return keyword("lambda") + ct.Commit(ct.Opt(symbol) + typeParameters() + keyword("(") + \
        ct.RepSep(simplePattern(), keyword(",")) + keyword(")") + ct.Lazy(expression)) ^ process


def returnExpr():
    def process(parsed, loc):
        (_, e) = parsed
        return ast.AstReturnExpression(e, loc)
    return keyword("return") + ct.Opt(ct.Lazy(expression)) ^ process


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
        return ast.AstIntegerLiteral(value, width, loc)

    return ct.Tag(INTEGER) ^ process


def floatLiteral():
    def process(text, loc):
        m = re.match("([^f]*)(?:f([0-9]+))?", text)
        value = float(m.group(1))
        width = int(m.group(2)) if m.group(2) is not None else 64
        return ast.AstFloatLiteral(value, width, loc)

    return ct.Tag(FLOAT) ^ process


def booleanLiteral():
    return (keyword("true") ^ (lambda _, loc: ast.AstBooleanLiteral(True, loc))) | \
           (keyword("false") ^ (lambda _, loc: ast.AstBooleanLiteral(False, loc)))


def nullLiteral():
    return keyword("null") ^ (lambda _, loc: ast.AstNullLiteral(loc))


def stringLiteral():
    def process(text, loc):
        value = tryDecodeString(text)
        if value is None:
            return ct.FailValue("invalid string")
        return ast.AstStringLiteral(value, loc)
    return ct.Tag(STRING) ^ process


# Basic parsers
def keyword(kw):
    return ct.Reserved(RESERVED, kw)

def layoutBlock(contents):
    def process(parsed, loc):
        [_, ast, _] = ct.untangle(parsed)
        return ast
    return ((keyword("{") + contents + keyword("}")) |
            (ct.Reserved(INTERNAL, "{") + contents + ct.Reserved(INTERNAL, "}"))) ^ process

symbol = ct.Tag(SYMBOL)
operator = ct.Tag(OPERATOR)

semi = keyword(";") | ct.Reserved(INTERNAL, ";")

#expression is exported solely for unittest functionality
__all__ = ["parse", "symbol", "operator", "semi"]

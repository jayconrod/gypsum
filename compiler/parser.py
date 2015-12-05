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
        return ast.Module(parsed, loc)
    return ct.Phrase(ct.Rep(definition() | importStmt())) ^ process


# Definitions
def definition():
    return varDefn() | functionDefn() | classDefn()


def attribs():
    return ct.Rep(attrib())


def attrib():
    return ct.Tag(ATTRIB) ^ (lambda name, loc: ast.Attribute(name, loc))


def varDefn():
    def process(parsed, loc):
        [ats, kw, pat, expr, _] = ct.untangle(parsed)
        return ast.VariableDefinition(ats, kw, pat, expr, loc)
    exprOpt = ct.Opt(keyword("=") + expression() ^ (lambda p, _: p[1]))
    kw = keyword("var") | keyword("let")
    return attribs() + kw + ct.Commit(pattern() + exprOpt + semi) ^ process


def functionDefn():
    def process(parsed, loc):
        [ats, _, name, tps, ps, rty, body, _] = ct.untangle(parsed)
        return ast.FunctionDefinition(ats, name, tps, ps, rty, body, loc)
    functionName = keyword("this") | identifier
    bodyOpt = ct.Opt(keyword("=") + expression() ^ (lambda p, _: p[1]))
    return attribs() + keyword("def") + ct.Commit(functionName + typeParameters() + parameters() +
        tyOpt() + bodyOpt + semi) ^ process


def classDefn():
    def process(parsed, loc):
        [ats, _, name, tps, ctor, sty, sargs, ms, _] = ct.untangle(parsed)
        return ast.ClassDefinition(ats, name, tps, ctor, sty, sargs, ms, loc)
    classBodyOpt = ct.Opt(layoutBlock(ct.Rep(ct.Lazy(classMember)))) ^ \
                   (lambda p, _: p if p is not None else [])
    return attribs() + keyword("class") + ct.Commit(identifier + typeParameters() +
           constructor() + superclass() + classBodyOpt + semi) ^ process


def constructor():
    def process(parsed, loc):
        [ats, _, params, _] = ct.untangle(parsed)
        return ast.PrimaryConstructorDefinition(ats, params, loc)
    return ct.Opt(attribs() + keyword("(") + \
               ct.Commit(ct.RepSep(parameter(), keyword(",")) + keyword(")")) ^ process)


def superclass():
    def processArgs(parsed, loc):
        return ct.untangle(parsed)[1] if parsed is not None else None
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


def classMember():
    return definition() | importStmt() | arrayElementsStmt()


def importStmt():
    def processBinding(parsed, loc):
        name = parsed[0]
        asName = parsed[1][1] if parsed[1] is not None else None
        return ast.ImportBinding(name, asName, loc)
    importBinding = symbol + ct.Opt(keyword("as") + symbol) ^ processBinding

    def processSuffixEmpty(parsed, loc):
        return lambda prefix: None
    importSuffixEmpty = keyword(".") + keyword("_") ^ processSuffixEmpty

    def processSuffixAs(parsed, loc):
        firstAsName = parsed[0][1] if parsed[0] is not None else None
        laterBindings = [p[1] for p in parsed[1]]
        def processFn(prefix):
            if len(prefix) == 1:
                raise ParseException(prefix[0].location,
                                     "import statement requires a prefix before symbol to import")
            lastComponent = prefix.pop()
            if lastComponent.typeArguments is not None:
                raise ParseException(lastComponent.location,
                                     "type arguments can't be at the end of an import statement")
            firstBinding = ast.ImportBinding(lastComponent.name, firstAsName,
                                                lastComponent.location)
            return [firstBinding] + laterBindings
        return processFn
    importSuffixAs = ct.Opt(keyword("as") + symbol) + ct.Rep(keyword(",") + importBinding) ^ \
      processSuffixAs

    importSuffix = (importSuffixEmpty | importSuffixAs) + semi

    def process(parsed, loc):
        _, prefix, suffixFn, _ = ct.untangle(parsed)
        bindings = suffixFn(prefix)
        return ast.ImportStatement(prefix, bindings, loc)
    return keyword("import") + ct.Commit(scopePrefix() + importSuffix) ^ process


def arrayElementsStmt():
    def process(parsed, loc):
        [attrs, _, elementType, _, getName, _, setName, _, lengthName, _] = ct.untangle(parsed)
        return ast.ArrayElementsStatement(attrs, elementType,
                                             getName, setName, lengthName, loc)
    return attribs() + keyword("arrayelements") + ct.Commit(ty() + keyword(",") +
        arrayAccessorDefn() + keyword(",") + arrayAccessorDefn() + keyword(",") +
        arrayAccessorDefn() + semi) ^ process


def arrayAccessorDefn():
    def process(parsed, loc):
        attribs, name = parsed
        return ast.ArrayAccessorDefinition(attribs, name, loc)
    return attribs() + identifier ^ process


def typeParameters():
    def process(parsed, _):
        return ct.untangle(parsed)[1] if parsed else []
    return ct.Opt(keyword("[") + ct.Rep1Sep(typeParameter(), keyword(",")) + keyword("]")) ^ process


def typeParameter():
    def process(parsed, loc):
        [ats, var, name, upper, lower] = ct.untangle(parsed)
        return ast.TypeParameter(ats, var, name, upper, lower, loc)
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
        return ast.Parameter(ats, var, pat, loc)
    return attribs() + ct.Opt(keyword("var")) + simplePattern() ^ process


# Patterns
def pattern():
    def process(parsed, loc):
        return parsed[0] if len(parsed) == 1 else ast.TuplePattern(parsed, loc)
    return ct.Rep1Sep(maybeBinopPattern(), keyword(",")) ^ process


def maybeBinopPattern():
    return buildBinaryParser(simplePattern(), ast.BinaryPattern)


def simplePattern():
    return prefixedPattern() | \
           blankPattern() | \
           litPattern() | \
           unopPattern() | \
           groupPattern()


def prefixedPattern():
    def process(parsed, loc):
        prefix, suffix = parsed
        if len(prefix) == 1 and (suffix is None or isinstance(suffix, ast.Type)):
            mayHaveTypeArgs = False
            result = ast.VariablePattern(prefix[0].name, suffix, loc)
        elif suffix is None:
            mayHaveTypeArgs = False
            result = ast.ValuePattern(prefix[:-1], prefix[-1].name, loc)
        elif isinstance(suffix, list):
            mayHaveTypeArgs = True
            result = ast.DestructurePattern(prefix, suffix, loc)
        else:
            return ct.FailValue("invalid variable, value, or destructure pattern")
        if not mayHaveTypeArgs and prefix[-1].typeArguments is not None:
            return ct.FailValue("can't have type arguments at end of variable or value pattern")
        return result

    def processSuffix(parsed, loc):
        return ct.untangle(parsed)[1]
    argumentSuffix = (keyword("(") + ct.Rep1Sep(ct.Lazy(simplePattern), keyword(",")) +
                      keyword(")") ^ processSuffix)
    typeSuffix = keyword(":") + ty() ^ processSuffix
    suffix = argumentSuffix | typeSuffix

    return scopePrefix() + ct.Opt(suffix) ^ process


def blankPattern():
    def process(parsed, loc):
        _, parsedTy = parsed
        ty = parsedTy[1] if parsedTy else None
        return ast.BlankPattern(ty, loc)
    return keyword("_") + ct.Opt(keyword(":") + ty()) ^ process


def litPattern():
    return literal() ^ ast.LiteralPattern


def unopPattern():
    def process(parsed, loc):
        op, pat = parsed
        return ast.UnaryPattern(op, pat, loc)
    return unaryOperator + ct.Lazy(simplePattern) ^ process


def groupPattern():
    def process(parsed, loc):
        _, p, _ = ct.untangle(parsed)
        return p
    return keyword("(") + ct.Commit(ct.Lazy(pattern) + keyword(")")) ^ process


# Scope prefix
def scopePrefix():
    return ct.Rep1Sep(scopePrefixComponent(), keyword("."))


def scopePrefixComponent():
    def process(parsed, loc):
        name, args = parsed
        return ast.ScopePrefixComponent(name, args, loc)
    return symbol + typeArguments() ^ process


# Types
def ty():
    return (keyword("unit") ^ (lambda _, loc: ast.UnitType(loc))) | \
           (keyword("i8") ^ (lambda _, loc: ast.I8Type(loc))) | \
           (keyword("i16") ^ (lambda _, loc: ast.I16Type(loc))) | \
           (keyword("i32") ^ (lambda _, loc: ast.I32Type(loc))) | \
           (keyword("i64") ^ (lambda _, loc: ast.I64Type(loc))) | \
           (keyword("f32") ^ (lambda _, loc: ast.F32Type(loc))) | \
           (keyword("f64") ^ (lambda _, loc: ast.F64Type(loc))) | \
           (keyword("boolean") ^ (lambda _, loc: ast.BooleanType(loc))) | \
           (keyword("_") ^ (lambda _, loc: ast.BlankType(loc))) | \
           tupleType() | \
           classType()


def tyOpt():
    return ct.Opt(keyword(":") + ty() ^ (lambda p, _: p[1]))


def tupleType():
    def process(parsed, loc):
        _, first, _, rest, _, nullFlag = ct.untangle(parsed)
        flags = set([nullFlag]) if nullFlag else set()
        return ast.TupleType([first] + rest, flags, loc)
    return keyword("(") + ct.Commit(ct.Lazy(ty) + keyword(",") +
        ct.Rep1Sep(ct.Lazy(ty), keyword(",")) + keyword(")") +
        ct.Opt(ct.Reserved(OPERATOR, "?"))) ^ process


def classType():
    def process(parsed, loc):
        prefixComponents, nullFlag = ct.untangle(parsed)
        prefix = prefixComponents[:-1]
        last = prefixComponents[-1]
        name = last.name
        typeArgs = last.typeArguments if last.typeArguments is not None else []
        flags = set([nullFlag]) if nullFlag is not None else set()
        return ast.ClassType(prefix, name, typeArgs, flags, loc)
    return scopePrefix() + ct.Opt(ct.Reserved(OPERATOR, "?")) ^ process


def typeArguments():
    def process(parsed, _):
        return ct.untangle(parsed)[1] if parsed else None
    return ct.Opt(keyword("[") + ct.Rep1Sep(ct.Lazy(ty), keyword(",")) + keyword("]")) ^ process


# Expressions
def expression():
    def combine(left, right, loc):
        return ast.AssignExpression(left, right, loc)
    rhs = keyword("=") + ct.Lazy(expression) ^ (lambda p, _: p[1])
    return ct.LeftRec(maybeTupleExpr(), rhs, combine)


def maybeTupleExpr():
    def process(parsed, loc):
        return parsed[0] if len(parsed) == 1 else ast.TupleExpression(parsed, loc)
    return ct.Rep1Sep(maybeBinopExpr(), keyword(",")) ^ process


def maybeBinopExpr():
    return buildBinaryParser(maybeCallExpr(), ast.BinaryExpression)


def maybeCallExpr():
    return ct.LeftRec(receiverExpr(), callSuffix(), processCall)

def callSuffix():
    methodNameOpt = ct.Opt(keyword(".") + symbol ^ (lambda p, _: p[1]))
    getMethodOpt = ct.Opt(keyword("_")) ^ (lambda p, _: bool(p))
    return methodNameOpt + typeArguments() + argumentsOpt() + getMethodOpt

def argumentsOpt():
    def process(parsed, _):
        return ct.untangle(parsed)[1] if parsed else None
    return ct.Opt(keyword("(") + ct.RepSep(ct.Lazy(maybeBinopExpr), keyword(",")) +
                      keyword(")")) ^ process

def processCall(receiver, parsed, loc):
    [methodName, typeArguments, arguments, isGetMethod] = ct.untangle(parsed)
    if isGetMethod:
        if methodName is None or \
           typeArguments is not None or \
           arguments is not None:
            return ct.FailValue()
        return ast.FunctionValueExpression(ast.PropertyExpression(receiver, methodName, loc), loc)
    elif methodName is not None or \
         typeArguments is not None or \
         arguments is not None:
        hasArguments = typeArguments is not None or arguments is not None
        if methodName is not None:
            method = ast.PropertyExpression(receiver, methodName, loc)
        else:
            method = receiver
        if hasArguments:
            return ast.CallExpression(method, typeArguments, arguments, loc)
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
           newArrayExpr() | \
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
    return literal() ^ (lambda lit, loc: ast.LiteralExpression(lit, loc))


def varExpr():
    return symbol ^ (lambda name, loc: ast.VariableExpression(name, loc))


def thisExpr():
    return keyword("this") ^ (lambda _, loc: ast.ThisExpression(loc))


def superExpr():
    return keyword("super") ^ (lambda _, loc: ast.SuperExpression(loc))


def groupExpr():
    def process(parsed, _):
        [_, e, _] = ct.untangle(parsed)
        return e
    return keyword("(") + ct.Lazy(expression) + keyword(")") ^ process


def newArrayExpr():
    def process(parsed, loc):
        [_, _, length, _, ty, args] = ct.untangle(parsed)
        return ast.NewArrayExpression(length, ty, args, loc)
    return keyword("new") + ct.Commit(keyword("(") + ct.Lazy(expression) + keyword(")") +
        ty() + argumentsOpt()) ^ process


def blockExpr():
    def process(stmts, loc):
        return ast.BlockExpression(stmts, loc)
    return layoutBlock(ct.Rep(ct.Lazy(statement)) ^ process)


def unaryExpr():
    def process(parsed, loc):
        (op, e) = parsed
        return ast.UnaryExpression(op, e, loc)
    return unaryOperator + ct.Commit(ct.Lazy(maybeCallExpr)) ^ process


def ifExpr():
    def process(parsed, loc):
        [_, _, c, _, t, f] = ct.untangle(parsed)
        return ast.IfExpression(c, t, f, loc)
    elseClause = ct.Opt(keyword("else") + ct.Commit(ct.Lazy(expression))) ^ \
        (lambda p, _: p[1] if p else None)
    return keyword("if") + ct.Commit(keyword("(") + ct.Lazy(expression) + keyword(")") + \
        ct.Lazy(expression) + elseClause) ^ process


def whileExpr():
    def process(parsed, loc):
        [_, _, c, _, b] = ct.untangle(parsed)
        return ast.WhileExpression(c, b, loc)
    return keyword("while") + ct.Commit(keyword("(") + ct.Lazy(expression) + keyword(")") + \
        ct.Lazy(expression)) ^ process


def breakExpr():
    return keyword("break") ^ (lambda _, loc: ast.BreakExpression(loc))


def continueExpr():
    return keyword("continue") ^ (lambda _, loc: ast.ContinueExpression(loc))


def partialFnExpr():
    def process(cases, loc):
        return ast.PartialFunctionExpression(cases, loc)
    return layoutBlock(ct.Rep1(partialFunctionCase()) ^ process)


def partialFunctionCase():
    def process(parsed, loc):
        [_, p, c, _, e, _] = ct.untangle(parsed)
        return ast.PartialFunctionCase(p, c, e, loc)
    conditionOpt = ct.Opt(keyword("if") + ct.Lazy(expression)) ^ (lambda p, _: p[1] if p else None)
    return keyword("case") + ct.Commit(pattern() + conditionOpt + keyword("=>") + \
        ct.Lazy(expression) + semi) ^ process


def matchExpr():
    def process(parsed, loc):
        [_, _, e, _, m] = ct.untangle(parsed)
        return ast.MatchExpression(e, m, loc)
    return keyword("match") + ct.Commit(keyword("(") + ct.Lazy(expression) + keyword(")") + \
        partialFnExpr()) ^ process


def throwExpr():
    def process(parsed, loc):
        (_, x) = parsed
        return ast.ThrowExpression(x, loc)
    return keyword("throw") + ct.Commit(ct.Lazy(expression)) ^ process


def tryCatchExpr():
    def process(parsed, loc):
        [_, e, c, f] = ct.untangle(parsed)
        if c is None and f is None:
            return ct.FailValue("expected 'catch' or 'finally' in 'try'-expression")
        else:
            return ast.TryCatchExpression(e, c, f, loc)

    finallyOpt = ct.Opt(keyword("finally") + ct.Lazy(expression)) ^ (lambda p, _: p[1] if p else None)
    return keyword("try") + ct.Commit(ct.Lazy(expression) + catchHandler() + finallyOpt) ^ process


def catchHandler():
    def processSimple(parsed, loc):
        [_, p, _, e] = ct.untangle(parsed)
        return ast.PartialFunctionExpression([ast.PartialFunctionCase(p, None, e, loc)], loc)
    simpleHandler = keyword("(") + ct.Commit(pattern() + keyword(")") + \
                    ct.Lazy(expression)) ^ processSimple

    matchHandler = partialFnExpr()

    def process(parsed, _):
        return parsed[1]
    return ct.Opt(keyword("catch") + ct.Commit(simpleHandler | matchHandler) ^ process)


def lambdaExpr():
    def process(parsed, loc):
        [_, n, tps, _, ps, _, b] = ct.untangle(parsed)
        return ast.LambdaExpression(n, tps, ps, b, loc)
    return keyword("lambda") + ct.Commit(ct.Opt(symbol) + typeParameters() + keyword("(") + \
        ct.RepSep(simplePattern(), keyword(",")) + keyword(")") + ct.Lazy(expression)) ^ process


def returnExpr():
    def process(parsed, loc):
        (_, e) = parsed
        return ast.ReturnExpression(e, loc)
    return keyword("return") + ct.Opt(ct.Lazy(expression)) ^ process


def statement():
    return definition() | importStmt() | ((expression() + semi) ^ (lambda p, _: p[0]))


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
        return ast.IntegerLiteral(value, width, loc)

    return ct.Tag(INTEGER) ^ process


def floatLiteral():
    def process(text, loc):
        m = re.match("([^f]*)(?:f([0-9]+))?", text)
        value = float(m.group(1))
        width = int(m.group(2)) if m.group(2) is not None else 64
        return ast.FloatLiteral(value, width, loc)

    return ct.Tag(FLOAT) ^ process


def booleanLiteral():
    return (keyword("true") ^ (lambda _, loc: ast.BooleanLiteral(True, loc))) | \
           (keyword("false") ^ (lambda _, loc: ast.BooleanLiteral(False, loc)))


def nullLiteral():
    return keyword("null") ^ (lambda _, loc: ast.NullLiteral(loc))


def stringLiteral():
    def process(text, loc):
        value = tryDecodeString(text)
        if value is None:
            return ct.FailValue("invalid string")
        return ast.StringLiteral(value, loc)
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

def processSymbol(sym, loc):
    if sym.startswith("`"):
        sym = tryDecodeString(sym)
        if sym is None:
            return ct.FailValue("invalid symbol")
    return sym

symbol = ct.Tag(SYMBOL) ^ processSymbol
operator = ct.Tag(OPERATOR)
unaryOperator = ct.If(operator, lambda op: op in ["!", "-", "+", "~"])
identifier = symbol | operator

semi = keyword(";") | ct.Reserved(INTERNAL, ";")

# Utilities for building binary expressions and patterns.
BINOP_LEVELS = [
    [],   # other
    ["*", "/", "%"],
    ["+", "-"],
    [":"],
    ["=", "!"],
    ["<", ">"],
    ["&"],
    ["^"],
    ["|"],
]
BINOP_OTHER_LEVEL = 0
BINOP_LOGIC_AND_LEVEL = len(BINOP_LEVELS)
BINOP_LOGIC_OR_LEVEL = BINOP_LOGIC_AND_LEVEL + 1
BINOP_ASSIGN_LEVEL = BINOP_LOGIC_OR_LEVEL + 1
NUM_BINOP_LEVELS = BINOP_ASSIGN_LEVEL + 1

def precedenceOf(op):
    """Returns the precedence level of an operator.

    Precedence is mostly determined by the first character in the operator name, with a few
    exceptions. See BINOP_LEVELS for a list of precedence levels. If the operator name ends
    with '=', and does not start with '=', and the operator is not one of "<=", ">=", "!=",
    or "!==", the operator is considered an assignment and has a low precedence. The operators
    "&&" and "||" have their own special precedence levels, just above assignment.

    Args:
        op: the name of an operator (str).

    Returns:
        An integer indicating the precedence of the operator. Lower numbers indicate higher
        precedence.
    """
    if op[-1] == "=" and \
       not (len(op) > 1 and op[0] == "=") and \
       op not in ["<=", ">=", "!=", "!=="]:
        return BINOP_ASSIGN_LEVEL
    elif op == "&&":
        return BINOP_LOGIC_AND_LEVEL
    elif op == "||":
        return BINOP_LOGIC_OR_LEVEL
    else:
        for i in range(0, len(BINOP_LEVELS)):
            if op[0] in BINOP_LEVELS[i]:
                return i
        return BINOP_OTHER_LEVEL

LEFT_ASSOC = "left"
RIGHT_ASSOC = "right"
def associativityOf(op):
    """Returns the associativity of an operator.

    All operators have right associativity, except those that end with ':', which have left
    associativity.

    Returns:
        LEFT_ASSOC or RIGHT_ASSOC.
    """
    return RIGHT_ASSOC if op[-1] == ":" else LEFT_ASSOC

def buildBinaryParser(term, combine):
    """Builds a parser for binary operators at the same level of precedence.

    Used to build binary expressions and patterns.

    Args:
        term: a parser for the sub-expressions that make up a binary expression. Usually this
            is a parser for simple expressions like literals or variables.
        level: an integer indicating the precedence level to build for.
        combine: a function `(str, ast.Node, ast.Node, Location => ast.Node)` which combines
            two nodes produced by `term` with an operator and a location into a new node.

    Returns:
        A parser for binary operator expressions at this level of precedence.
    """
    postprocess = lambda ast, loc: postProcessBinary(combine, ast, loc)
    parser = term
    for level in xrange(0, NUM_BINOP_LEVELS):
        parser = ct.LeftRec(parser, binarySuffix(parser, level), processBinary) ^ postprocess
    return parser

def binarySuffix(term, level):
    return ct.If(operator, lambda op: precedenceOf(op) == level) + ct.Commit(term)

def processBinary(left, parsed, _):
    (op, right) = parsed
    if not isinstance(left, list):
        left = [(None, left)]
    return left + [(op, right)]

def postProcessBinary(combine, ast, loc):
    if not isinstance(ast, list):
        return ast
    else:
        assert len(ast) > 1
        associativity = associativityOf(ast[1][0])
        if associativity is LEFT_ASSOC:
            result = ast[0][1]
            for i in range(1, len(ast)):
                (op, subast) = ast[i]
                if associativityOf(op) is not LEFT_ASSOC:
                    return ct.FailValue("left and right associativie operators are mixed together")
                loc = result.location.combine(subast.location)
                result = combine(op, result, subast, loc)
            return result
        else:
            result = ast[-1][1]
            for i in range(-1, -len(ast), -1):
                op = ast[i][0]
                subast = ast[i-1][1]
                if associativityOf(op) is not RIGHT_ASSOC:
                    return ct.FailValue("left and right associativie operators are mixed together")
                loc = result.location.combine(subast.location)
                result = combine(op, subast, result, loc)
            return result


__all__ = ["parse", "symbol", "operator", "semi"]

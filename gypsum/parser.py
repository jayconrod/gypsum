# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.

import re

import ast
from errors import ParseException
from location import NoLoc
from tok import (
    ARRAYELEMENTS,
    AS,
    ATTRIB,
    BIG_ARROW,
    BOOLEAN,
    BREAK,
    CASE,
    CATCH,
    CLASS,
    COLON,
    COMMA,
    COMMENT,
    CONTINUE,
    DEF,
    DOT,
    ELSE,
    EOF,
    EQ,
    F32,
    F64,
    FALSE,
    FINALLY,
    FLOAT,
    FORSOME,
    I16,
    I32,
    I64,
    I8,
    IF,
    IMPORT,
    INDENT,
    INTEGER,
    LAMBDA,
    LBRACE,
    LBRACK,
    LET,
    LPAREN,
    MATCH,
    NEW,
    NEWLINE,
    NULL,
    OPERATOR,
    OUTDENT,
    RBRACE,
    RBRACK,
    RETURN,
    RPAREN,
    SMALL_ARROW,
    STRING,
    SUBTYPE,
    SUPER,
    SUPERTYPE,
    SYMBOL,
    THIS,
    THROW,
    TRAIT,
    TRUE,
    TRY,
    Token,
    UNDERSCORE,
    UNIT,
    VAR,
    WHILE,
)
from utils import tryDecodeString


def parse(fileName, tokens):
    parser = Parser(fileName, tokens)
    module = parser.module()
    if not parser.atEnd():
        raise ParseException(parser.location, "garbage at end of input")
    ast.addNodeIds(module)
    return module

class Parser(object):
    def __init__(self, fileName, tokens):
        self.fileName = fileName
        self.tokens = tokens
        self.pos = 0
        self.location = None

    BINOP_LEVELS = [
        "",  # other
        "*/%",
        "+-",
        ":",
        "=!",
        "<>",
        "&",
        "^",
        "|",
    ]
    BINOP_OTHER_LEVEL = 0
    BINOP_LOGIC_AND_LEVEL = len(BINOP_LEVELS) + 1
    BINOP_LOGIC_OR_LEVEL = BINOP_LOGIC_AND_LEVEL + 1
    BINOP_ASSIGN_LEVEL = BINOP_LOGIC_OR_LEVEL + 1
    LAST_BINOP_LEVEL = BINOP_ASSIGN_LEVEL

    LEFT_ASSOC = "left associative"
    RIGHT_ASSOC = "right associative"

    EXPR_START_TOKENS = (
        LBRACK,
        LPAREN,
        LBRACE,
        IF,
        ELSE,
        WHILE,
        BREAK,
        CONTINUE,
        MATCH,
        THROW,
        TRY,
        NEW,
        LAMBDA,
        RETURN,
        TRUE,
        FALSE,
        THIS,
        SUPER,
        NULL,
        SYMBOL,
        OPERATOR,
        INTEGER,
        FLOAT,
        STRING
    )

    # Top level
    def module(self):
        l = self._peek().location
        defns = self._parseList(self.defnOrImport, "definition or import", None, NEWLINE, EOF)
        return ast.Module(defns, self._location(l))

    def defnOrImport(self):
        if self._peekTag() is IMPORT:
            return self.importStmt()
        else:
            return self.defn()

    def importStmt(self):
        l = self._nextTag(IMPORT).location
        prefix = self.scopePrefix()
        if self._peekTag() is DOT:
            self._next()
            self._nextTag(UNDERSCORE)
            return ast.ImportStatement(prefix, None, self._location(l))

        if len(prefix) == 1:
            raise ParseException(
                prefix[0].location,
                "import statement requires a prefix before symbol to import")
        lastComponent = prefix.pop()
        if lastComponent.typeArguments is not None:
            raise ParseException(
                lastComponent.location,
                "type arguments can't be at the end of an import statement")

        bindings = []
        if self._peekTag() is AS:
            self._next()
            asName = self.ident()
            bindings.append(ast.ImportBinding(
                lastComponent.name, asName, self._location(lastComponent.location)))
        else:
            bindings.append(ast.ImportBinding(lastComponent.name, None, lastComponent.location))
        while self._peekTag() is COMMA:
            self._next()
            name = self.ident()
            bl = self.location
            if self._peekTag() is AS:
                self._next()
                asName = self.ident()
            else:
                asName = None
            bindings.append(ast.ImportBinding(name, asName, self._location(bl)))

        return ast.ImportStatement(prefix, bindings, self._location(l))

    # Definitions
    def defn(self):
        l = self._peek().location
        ats = self.attribs()
        tag = self._peek().tag
        if tag in (LET, VAR):
            return self.varDefn(l, ats)
        elif tag is DEF:
            return self.functionDefn(l, ats)
        elif tag is CLASS:
            return self.classDefn(l, ats)
        elif tag is TRAIT:
            return self.traitDefn(l, ats)
        elif tag is ARRAYELEMENTS:
            return self.arrayElementsStmt(l, ats)
        else:
            self._error("definition")

    def varDefn(self, l, ats):
        tok = self._next()
        if tok.tag not in (LET, VAR):
            self._error("let or var")
        var = tok.text
        pat = self.pattern()
        if self._peekTag() is EQ:
            self._next()
            # Use maybeTupleExpr instead of expr to avoid assignments.
            e = self.maybeTupleExpr()
        else:
            e = None
        return ast.VariableDefinition(ats, var, pat, e, self._location(l))

    def functionDefn(self, l, ats):
        self._nextTag(DEF)
        if self._peekTag() not in (SYMBOL, OPERATOR, THIS):
            self._error("function name")
        name = self._next().text
        tps = self._parseOption(LBRACK, self.typeParameters)
        ps = self._parseOption(LPAREN, self.parameters)
        if self._peekTag() is COLON:
            self._next()
            rty = self.ty()
        else:
            rty = None
        if self._peekTag() is EQ:
            self._next()
            body = self.expr()
        else:
            body = None
        return ast.FunctionDefinition(ats, name, tps, ps, rty, body, self._location(l))

    def classDefn(self, l, ats):
        self._nextTag(CLASS)
        name = self.ident()
        tps = self._parseOption(LBRACK, self.typeParameters)
        ctor = self.constructor()
        if self._peekTag() is SUBTYPE:
            self._next()
            scl = self.ty()
            sargs = self._parseOption(LPAREN, self.arguments)
            if self._peekTag() is COMMA:
                self._next()
                strs = self._parseRepSep(self.ty, COMMA)
            else:
                strs = None
        else:
            scl = None
            sargs = None
            strs = None
        ms = self._classBody("class body")
        loc = self._location(l)
        return ast.ClassDefinition(ats, name, tps, ctor, scl, sargs, strs, ms, loc)

    def traitDefn(self, l, ats):
        self._nextTag(TRAIT)
        name = self.ident()
        tps = self._parseOption(LBRACK, self.typeParameters)
        if self._peekTag() is SUBTYPE:
            self._next()
            sts = [self.ty()]
            while self._peekTag() is COMMA:
                self._next()
                sts.append(self.ty())
        else:
            sts = None
        ms = self._classBody("trait body")
        loc = self._location(l)
        return ast.TraitDefinition(ats, name, tps, sts, ms, loc)

    def arrayElementsStmt(self, l, ats):
        l = self._nextTag(ARRAYELEMENTS).location
        ety = self.ty()
        self._nextTag(COMMA)
        getDefn = self.arrayAccessorDefn()
        self._nextTag(COMMA)
        setDefn = self.arrayAccessorDefn()
        self._nextTag(COMMA)
        lengthDefn = self.arrayAccessorDefn()
        loc = self._location(l)
        return ast.ArrayElementsStatement(ats, ety, getDefn, setDefn, lengthDefn, loc)

    def arrayAccessorDefn(self):
        l = self._peek().location
        ats = self.attribs()
        name = self.ident()
        return ast.ArrayAccessorDefinition(ats, name, self._location(l))

    def constructor(self):
        if self._peekTag() not in (ATTRIB, LPAREN):
            return None
        l = self._peek().location
        ats = self.attribs()
        params = self.parameters()
        return ast.PrimaryConstructorDefinition(ats, params, self._location(l))

    def _classBody(self, label):
        if self._peekTag() is NEWLINE and self._peekTag(1) is INDENT:
            return self._parseBlock(self.defnOrImport, label)
        else:
            return None

    # Patterns
    def pattern(self):
        return self.maybeTuplePattern()

    def maybeTuplePattern(self):
        l = self._peek().location
        elems = self._parseRepSep(self.maybeBinopPattern, COMMA)
        return elems[0] if len(elems) == 1 else ast.TuplePattern(elems, self._location(l))

    def maybeBinopPattern(self):
        return self._parseBinop(self.simplePattern, ast.BinaryPattern, self.LAST_BINOP_LEVEL)

    def simplePattern(self):
        tag = self._peekTag()
        if tag is SYMBOL:
            return self.prefixedPattern()
        elif tag is UNDERSCORE:
            return self.blankPattern()
        elif tag in (TRUE, FALSE, NULL, INTEGER, FLOAT, STRING):
            return self.literalPattern()
        elif tag is OPERATOR:
            return self.unopPattern()
        elif tag is LPAREN:
            return self.groupPattern()
        else:
            self._error("pattern")

    def prefixedPattern(self):
        l = self._peek().location
        prefix = self.scopePrefix()
        tag = self._peekTag()
        if tag is LPAREN:
            args = self._parseList(
                self.simplePattern, "pattern arguments", LPAREN, COMMA, RPAREN)
            ty = None
        elif tag is COLON:
            self._next()
            ty = self.ty()
            args = None
        else:
            args = None
            ty = None
        loc = self._location(l)
        if len(prefix) == 1 and args is None:
            mayHaveTypeArgs = False
            result = ast.VariablePattern(prefix[0].name, ty, loc)
        elif args is None and ty is None:
            mayHaveTypeArgs = False
            result = ast.ValuePattern(prefix[:-1], prefix[-1].name, loc)
        elif args is not None:
            mayHaveTypeArgs = True
            result = ast.DestructurePattern(prefix, args, loc)
        else:
            raise ParseException(loc, "invalid variable, value, or destructure pattern")
        if not mayHaveTypeArgs and prefix[-1].typeArguments is not None:
            raise ParseException(
                loc, "can't have type arguments at end of variable or value pattern")
        return result

    def blankPattern(self):
        l = self._nextTag(UNDERSCORE).location
        if self._peekTag() is COLON:
            self._next()
            ty = self.ty()
        else:
            ty = None
        return ast.BlankPattern(ty, self._location(l))

    def unopPattern(self):
        tok = self._nextTag(OPERATOR)
        pat = self.simplePattern()
        return ast.UnaryPattern(tok.text, pat, self._location(tok.location))

    def literalPattern(self):
        lit = self.literal()
        return ast.LiteralPattern(lit, lit.location)

    def groupPattern(self):
        l = self._nextTag(LPAREN)
        pat = self.pattern()
        self._nextTag(RPAREN)
        return ast.GroupPattern(pat, self._location(l.location))

    # Types
    def ty(self):
        l = self._peek().location
        t = self.maybeFunctionType()
        if isinstance(t, ast.TupleType) and len(t.types) == 0:
            raise ParseException(l, "expected type but found ()")
        return t

    def maybeFunctionType(self):
        fnTypes = self._parseRepSep(self.simpleType, SMALL_ARROW)

        ty = fnTypes.pop()
        while len(fnTypes) > 0:
            pty = fnTypes.pop()
            if isinstance(pty, ast.TupleType):
                if len(pty.flags) > 0:
                    raise ParseException(
                        pty.location, "function parameter list can't be nullable")
                ptys = pty.types
            else:
                ptys = [pty]
            loc = pty.location.combine(ty.location)
            ty = ast.FunctionType(ptys, ty, loc)
        return ty

    def simpleType(self):
        tok = self._peek()
        if tok.tag is UNIT:
            t = ast.UnitType(tok.location)
        elif tok.tag is I8:
            t = ast.I8Type(tok.location)
        elif tok.tag is I16:
            t = ast.I16Type(tok.location)
        elif tok.tag is I32:
            t = ast.I32Type(tok.location)
        elif tok.tag is I64:
            t = ast.I64Type(tok.location)
        elif tok.tag is F32:
            t = ast.F32Type(tok.location)
        elif tok.tag is F64:
            t = ast.F64Type(tok.location)
        elif tok.tag is BOOLEAN:
            t = ast.BooleanType(tok.location)
        elif tok.tag is UNDERSCORE:
            t = ast.BlankType(tok.location)
        elif tok.tag is LPAREN:
            return self.tupleType()
        elif tok.tag is SYMBOL:
            return self.classType()
        elif tok.tag is FORSOME:
            return self.existentialType()
        else:
            self._error("type")
        self._next()
        return t

    def tupleType(self):
        l = self._peek().location
        tys = self._parseList(self.ty, "type", LPAREN, COMMA, RPAREN)
        loc = self._location(l)
        # Don't check for empty tuple, since this is needed for nullary function types.
        # We check for empty tuples in ty() instead.
        if len(tys) == 1:
            return tys[0]
        flags = self.typeFlags()
        return ast.TupleType(tys, flags, loc)

    def classType(self):
        l = self._peek().location
        prefixComponents = self.scopePrefix()
        prefix = prefixComponents[:-1]
        last = prefixComponents[-1]
        name = last.name
        typeArgs = last.typeArguments if last.typeArguments is not None else []
        flags = self.typeFlags()
        return ast.ClassType(prefix, name, typeArgs, flags, self._location(l))

    def existentialType(self):
        l = self._nextTag(FORSOME)
        tps = self.typeParameters()
        t = self.ty()
        return ast.ExistentialType(tps, t, self._location(l.location))

    def typeFlags(self):
        tok = self._peek()
        flags = set()
        if tok.tag is OPERATOR and tok.text == "?":
            self._next()
            flags.add("?")
        return flags

    # Expressions
    def expr(self):
        es = self._parseRepSep(self.maybeTupleExpr, EQ)
        e = es.pop()
        while len(es) > 0:
            l = es.pop()
            e = ast.AssignExpression(l, e, l.location.combine(e.location))
        return e

    def maybeTupleExpr(self):
        l = self._peek().location
        elems = self._parseRepSep(self.maybeBinopExpr, COMMA)
        return elems[0] if len(elems) == 1 else ast.TupleExpression(elems, self._location(l))

    def maybeBinopExpr(self):
        return self._parseBinop(self.maybeCallExpr, ast.BinaryExpression, self.LAST_BINOP_LEVEL)

    def _precedence(self, op):
        """Returns the precedence level of an operator.

        Precedence is mostly determined by the first character in the operator name, with a few
        exceptions. See BINOP_LEVELS for a list of precedence levels. If the operator name ends
        with '=', and does not start with '=', and the operator is not one of "<=", ">=", "!=",
        or "!==", the operator is considered an assignment and has a low precedence. The
        operators "&&" and "||" have their own special precedence levels, just above assignment.

        Args:
            op: the name of an operator (str).

        Returns:
            An integer indicating the precedence of the operator. Lower numbers indicate higher
            precedence.
        """
        if (op[-1] == "=" and
            not (len(op) > 1 and op[0] == "=") and
            op not in ("<=", ">=", "!=", "!==")):
            return self.BINOP_ASSIGN_LEVEL
        elif op == "&&":
            return self.BINOP_LOGIC_AND_LEVEL
        elif op == "||":
            return self.BINOP_LOGIC_OR_LEVEL
        else:
            for prec, level in enumerate(self.BINOP_LEVELS):
                if op[0] in level:
                    return prec
            return self.BINOP_OTHER_LEVEL

    def _associativity(self, op):
        """Returns the associativity of an operator.

        All operators have right associativity, except those that end with ':', which have left
        associativity.

        Returns:
            LEFT_ASSOC or RIGHT_ASSOC.
        """
        return self.RIGHT_ASSOC if op[-1] == ":" else self.LEFT_ASSOC

    def maybeCallExpr(self):
        e = self.receiverExpr()
        tag = self._peek().tag
        while tag in (DOT, LBRACK, LPAREN):
            if tag is DOT:
                self._next()
                name = self.symbol()
                callee = ast.PropertyExpression(e, name, e.location.combine(self.location))
            else:
                callee = e
            typeArgs = self._parseOption(LBRACK, self.typeArguments)
            args = self._parseOption(LPAREN, self.arguments)
            if typeArgs is not None or args is not None:
                e = ast.CallExpression(callee, typeArgs, args, self._location(e.location))
            else:
                e = callee
            tag = self._peek().tag
        return e

    def receiverExpr(self):
        tag = self._peek().tag
        if tag is OPERATOR:
            return self.unaryExpr()
        elif tag in (INTEGER, FLOAT, STRING, TRUE, FALSE, NULL, LBRACE):
            return self.literalExpr()
        elif tag is SYMBOL:
            return self.varExpr()
        elif tag is THIS:
            return self.thisExpr()
        elif tag is SUPER:
            return self.superExpr()
        elif tag is LPAREN:
            return self.groupExpr()
        elif tag is NEW:
            return self.newArrayExpr()
        elif tag is IF:
            return self.ifExpr()
        elif tag is WHILE:
            return self.whileExpr()
        elif tag is BREAK:
            return self.breakExpr()
        elif tag is CONTINUE:
            return self.continueExpr()
        elif tag is MATCH:
            return self.matchExpr()
        elif tag is THROW:
            return self.throwExpr()
        elif tag is TRY:
            return self.tryExpr()
        elif tag is NEWLINE:
            return self.blockExpr()
        elif tag is LAMBDA:
            return self.lambdaExpr()
        elif tag is RETURN:
            return self.returnExpr()
        else:
            self._error("expression")

    def unaryExpr(self):
        op = self._nextTag(OPERATOR, "expression")
        e = self.maybeCallExpr()
        return ast.UnaryExpression(op.text, e, op.location.combine(e.location))

    def literalExpr(self):
        lit = self.literal()
        return ast.LiteralExpression(lit, lit.location)

    def varExpr(self):
        sym = self.symbol()
        return ast.VariableExpression(sym, self.location)

    def thisExpr(self):
        self._nextTag(THIS)
        return ast.ThisExpression(self.location)

    def superExpr(self):
        self._nextTag(SUPER)
        return ast.SuperExpression(self.location)

    def groupExpr(self):
        l = self._nextTag(LPAREN)
        e = self.expr()
        r = self._nextTag(RPAREN)
        return ast.GroupExpression(e, l.location.combine(r.location))

    def newArrayExpr(self):
        l = self._nextTag(NEW)
        self._nextTag(LPAREN)
        length = self.expr()
        self._nextTag(RPAREN)
        ty = self.ty()
        args = self._parseOption(LPAREN, self.arguments)
        return ast.NewArrayExpression(length, ty, args, self._location(l.location))

    def ifExpr(self):
        l = self._nextTag(IF)
        self._nextTag(LPAREN)
        condExpr = self.expr()
        self._nextTag(RPAREN)
        trueExpr = self.expr()
        if self._peekHang() is ELSE:
            self._next()  # newline
            self._next()  # else
            falseExpr = self.expr()
        elif self._peekTag() is ELSE:
            self._next()
            falseExpr = self.expr()
        else:
            falseExpr = None
        return ast.IfExpression(condExpr, trueExpr, falseExpr, self._location(l.location))

    def whileExpr(self):
        l = self._nextTag(WHILE)
        self._nextTag(LPAREN)
        condExpr = self.expr()
        self._nextTag(RPAREN)
        bodyExpr = self.expr()
        return ast.WhileExpression(condExpr, bodyExpr, self._location(l.location))

    def breakExpr(self):
        self._nextTag(BREAK)
        return ast.BreakExpression(self.location)

    def continueExpr(self):
        self._nextTag(CONTINUE)
        return ast.ContinueExpression(self.location)

    def matchExpr(self):
        l = self._nextTag(MATCH)
        self._nextTag(LPAREN)
        e = self.expr()
        self._nextTag(RPAREN)
        m = self.partialFnExpr()
        return ast.MatchExpression(e, m, self._location(l.location))

    def partialFnExpr(self):
        l = self._peek().location
        cases = self._parseBlock(self.partialFnCase, "partial function expression")
        return ast.PartialFunctionExpression(cases, self._location(l))

    def partialFnCase(self):
        l = self._nextTag(CASE)
        p = self.pattern()
        if self._peekTag() is IF:
            self._next()
            c = self.expr()
        else:
            c = None
        self._nextTag(BIG_ARROW)
        e = self.expr()
        return ast.PartialFunctionCase(p, c, e, self._location(l.location))

    def throwExpr(self):
        l = self._nextTag(THROW)
        e = self.expr()
        return ast.ThrowExpression(e, self._location(l.location))

    def tryExpr(self):
        l = self._nextTag(TRY)
        e = self.expr()
        if self._peekHang() is CATCH:
            self._next()  # newline
            catchOpt = self.catchHandler()
        elif self._peekTag() is CATCH:
            catchOpt = self.catchHandler()
        else:
            catchOpt = None
        if self._peekHang() is FINALLY:
            self._next()  # newline
            finallyOpt = self.finallyHandler()
        elif self._peekTag() is FINALLY:
            finallyOpt = self.finallyHandler()
        else:
            finallyOpt = None
        if catchOpt is None and finallyOpt is None:
            self._error("catch or finally")
        return ast.TryCatchExpression(e, catchOpt, finallyOpt, self._location(l.location))

    def catchHandler(self):
        l = self._nextTag(CATCH)
        if self._peekTag() is LPAREN:
            self._next()
            p = self.pattern()
            self._nextTag(RPAREN)
            e = self.expr()
            loc = self._location(l.location)
            case = ast.PartialFunctionCase(p, None, e, loc)
            return ast.PartialFunctionExpression([case], loc)
        else:
            handler = self.partialFnExpr()
            handler.location = self._location(l.location)
            return handler

    def finallyHandler(self):
        self._nextTag(FINALLY)
        return self.expr()

    def blockExpr(self):
        l = self._peek().location
        stmts = self._parseBlock(self.blockStmt, "block expression")
        return ast.BlockExpression(stmts, self._location(l))

    def blockStmt(self):
        tag = self._peekTag()
        if tag in (ATTRIB, VAR, LET, DEF, CLASS, TRAIT, ARRAYELEMENTS):
            return self.defn()
        elif tag is IMPORT:
            return self.importStmt()
        else:
            e = self.expr()
            return e

    def lambdaExpr(self):
        l = self._nextTag(LAMBDA)
        params = self._parseOption(LPAREN, self.parameters)
        e = self.expr()
        return ast.LambdaExpression(params, e, self._location(l.location))

    def returnExpr(self):
        l = self._nextTag(RETURN)
        if self._peekTag() in self.EXPR_START_TOKENS:
            e = self.expr()
        else:
            e = None
        return ast.ReturnExpression(e, self._location(l.location))

    # Literals
    def literal(self):
        tok = self._peek()
        if tok.tag is LBRACE:
            return self.unitLiteral()
        elif tok.tag is INTEGER:
            return self.intLiteral()
        elif tok.tag is FLOAT:
            return self.floatLiteral()
        elif tok.tag is STRING:
            return self.stringLiteral()
        elif tok.tag is TRUE:
            lit = ast.BooleanLiteral(True, tok.location)
        elif tok.tag is FALSE:
            lit = ast.BooleanLiteral(False, tok.location)
        elif tok.tag is NULL:
            lit = ast.NullLiteral(tok.location)
        else:
            self._error("literal")
        self._next()
        return lit

    def unitLiteral(self):
        l = self._nextTag(LBRACE).location
        self._nextTag(RBRACE)
        return ast.UnitLiteral(self._location(l))

    def intLiteral(self):
        tok = self._nextTag(INTEGER)
        m = re.match("([+-]?)(0[BbXx])?([0-9A-Fa-f]+)(?:i([0-9]+))?", tok.text)
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
        return ast.IntegerLiteral(tok.text, value, width, tok.location)

    def floatLiteral(self):
        tok = self._nextTag(FLOAT)
        m = re.match("([^f]*)(?:f([0-9]+))?", tok.text)
        value = float(m.group(1))
        width = int(m.group(2)) if m.group(2) is not None else 64
        return ast.FloatLiteral(tok.text, value, width, tok.location)

    def stringLiteral(self):
        tok = self._nextTag(STRING)
        value = tryDecodeString(tok.text)
        if value is None:
            raise ParseException(tok.location, "could not understand string: %s" % tok.text)
        return ast.StringLiteral(value, tok.location)

    # Miscellaneous
    def scopePrefix(self):
        components = [self.scopePrefixComponent()]
        while self._peekTag() is DOT:
            # Hack: peek two tokens forward to look for _ in import statements.
            if self.pos + 1 < len(self.tokens) and self.tokens[self.pos + 1].tag is UNDERSCORE:
                return components
            self._next()
            components.append(self.scopePrefixComponent())
        return components

    def scopePrefixComponent(self):
        l = self._peek().location
        name = self.symbol()
        typeArgs = self._parseOption(LBRACK, self.typeArguments)
        return ast.ScopePrefixComponent(name, typeArgs, self._location(l))

    def typeParameters(self):
        return self._parseList(self.typeParameter, "type parameter", LBRACK, COMMA, RBRACK)

    def typeParameter(self):
        l = self._peek().location
        ats = self.attribs()
        variance = None
        if self._peekTag() is OPERATOR:
            tok = self._next()
            if tok.text != "+" and tok.text != "-":
                self._error("+, -, or symbol")
            variance = tok.text
        name = self.symbol()
        upperBound = None
        if self._peekTag() is SUBTYPE:
            self._next()
            upperBound = self.ty()
        lowerBound = None
        if self._peekTag() is SUPERTYPE:
            self._next()
            lowerBound = self.ty()
        loc = self._location(l)
        return ast.TypeParameter(ats, variance, name, upperBound, lowerBound, loc)

    def parameters(self):
        return self._parseList(self.parameter, "parameter", LPAREN, COMMA, RPAREN)

    def parameter(self):
        l = self._peek().location
        ats = self.attribs()
        var = None
        if self._peekTag() is VAR:
            var = self._next().text
        pat = self.simplePattern()
        return ast.Parameter(ats, var, pat, self._location(l))

    def typeArguments(self):
        return self._parseList(self.ty, "type arguments", LBRACK, COMMA, RBRACK)

    def arguments(self):
        return self._parseList(self.maybeBinopExpr, "arguments", LPAREN, COMMA, RPAREN)

    def attribs(self):
        ats = []
        while self._peekTag() is ATTRIB:
            tok = self._next()
            ats.append(ast.Attribute(tok.text, tok.location))
        return ats

    def ident(self):
        tag = self._peekTag()
        if tag is SYMBOL:
            return self.symbol()
        elif tag is OPERATOR:
            return self._next().text
        else:
            self._error("identifier")

    def symbol(self):
        sym = self._nextTag(SYMBOL).text
        if sym.startswith("`"):
            sym = tryDecodeString(sym)
            if sym is None:
                raise ParseException(self.location, "invalid symbol")
        return sym

    # Utility methods
    def atEnd(self):
        return self.pos >= len(self.tokens) - 1

    def nearEnd(self):
        return all(t.tag in (OUTDENT, NEWLINE, EOF) for t in self.tokens[self.pos:])

    def _parseBinop(self, simpleParser, astCtor, level):
        if level == 0:
            termParser = simpleParser
        else:
            termParser = lambda: self._parseBinop(simpleParser, astCtor, level - 1)
        terms = [termParser()]
        ops = []
        tok = self._peek()
        while tok.tag is OPERATOR and self._precedence(tok.text) == level:
            self._next()
            ops.append(tok)
            terms.append(termParser())
            tok = self._peek()
        if len(terms) == 1:
            return terms[0]

        associativity = self._associativity(ops[0].text)
        if associativity is self.LEFT_ASSOC:
            e = terms[0]
            for i, op in enumerate(ops):
                term = terms[i + 1]
                if self._associativity(op.text) is not self.LEFT_ASSOC:
                    raise ParseException(
                        op.location, "left and right associative operators are mixed together")
                loc = e.location.combine(term.location)
                e = astCtor(op.text, e, term, loc)
        else:
            e = terms[-1]
            for i in xrange(len(ops) - 1, -1, -1):
                op = ops[i]
                term = terms[i]
                if self._associativity(op.text) is not self.RIGHT_ASSOC:
                    raise ParseException(
                        op.location, "left and right associative operators are mixed together")
                loc = term.location.combine(e.location)
                e = astCtor(op.text, term, e, loc)
        return e

    def _parseOption(self, hintTag, parser):
        if self._peekTag() is hintTag:
            return parser()
        else:
            return None

    def _parseBlock(self, parser, label):
        self._nextTag(NEWLINE)
        return self._parseList(parser, label, INDENT, NEWLINE, OUTDENT)

    def _parseList(self, parser, label, left=None, sep=None, right=EOF):
        if left:
            if self._peekTag() is left:
                self._next()
            else:
                self._error(label)
        elems = []
        first = True
        while True:
            if self._peekTag() is right:
                self._next()
                break
            if not first and sep:
                if self._peekTag() is not sep:
                    self._error(sep)
                self._next()
            if self._peekTag() is right:
                self._next()
                break
            elem = parser()
            elems.append(elem)
            first = False
        return elems

    def _parseRepSep(self, parser, sep):
        elems = [parser()]
        while self._peekTag() is sep:
            self._next()
            elems.append(parser())
        return elems

    def _peekHang(self):
        if self._peekTag(-1) is OUTDENT and self._peekTag(0) is NEWLINE:
            return self._peekTag(1)

    def _peekTag(self, n=0):
        i = self.pos + n
        if i < 0 or i >= len(self.tokens):
            return None
        return self.tokens[i].tag

    def _peek(self):
        return self.tokens[self.pos]

    def _next(self):
        tok = self.tokens[self.pos]
        self.pos += 1
        self.location = tok.location
        return tok

    def _nextTag(self, expectedTag, expectedText=None):
        if expectedText is None:
            expectedText = expectedTag
        if not self._peekTag() is expectedTag:
            self._error(expectedText)
        return self._next()

    def _error(self, expected):
        tok = self._peek()
        raise ParseException(tok.location, "expected %s but found %s" % (expected, tok.text))

    def _location(self, begin):
        return begin.combine(self.location)

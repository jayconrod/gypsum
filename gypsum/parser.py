# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.

import re

import ast
from errors import ParseException
from location import (Location, NoLoc)
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
        self.location = Location(self.fileName, 1, 1, 1, 1)

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
        defns = self._parseStatements(self.defnOrImport, EOF)
        return ast.Module(defns, self._location(l))

    def defnOrImport(self):
        while True:  # repeat
            lead = self.leadComments()
            tag = self._peekTag()
            if tag is NEWLINE:
                # Blank line, possibly preceeded by comment block.
                if len(lead) > 0:
                    cg = ast.CommentGroup(lead, [])
                    cg.setLocationFromChildren()
                    return cg
                else:
                    self._next()
                    continue
            elif tag is EOF:
                # Comments at end of file
                assert len(lead) > 0
                cg = ast.CommentGroup(lead, [])
                cg.setLocationFromChildren()
                return cg
            else:
                if tag is IMPORT:
                    stmt = self.importStmt()
                else:
                    stmt = self.defn()
                stmt.comments.before = lead + stmt.comments.before
                stmt.comments.setLocationFromChildren()
                return stmt

    def importStmt(self):
        lead = self.leadComments()
        cg = ast.CommentGroup(lead, [])
        l = self._nextTag(IMPORT).location
        prefix = self.scopePrefix()
        if self._peekTag() is DOT:
            self._next()
            self._nextTag(UNDERSCORE)
            imp = ast.ImportStatement(prefix, None, cg, self._location(l))
            self._tailComment(imp)
            return imp

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
            bindings.append(
                ast.ImportBinding(lastComponent.name, None, lastComponent.location))
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

        imp = ast.ImportStatement(prefix, bindings, cg, self._location(l))
        self._tailComment(imp)
        return imp

    # Definitions
    def defn(self):
        lead = self.leadComments()
        l = self._peek().location
        ats = self.attribs()
        tag = self._peek().tag
        if tag in (LET, VAR):
            d = self.varDefn(l, ats)
        elif tag is DEF:
            d = self.functionDefn(l, ats)
        elif tag is CLASS:
            d = self.classDefn(l, ats)
        elif tag is TRAIT:
            d = self.traitDefn(l, ats)
        elif tag is ARRAYELEMENTS:
            d = self.arrayElementsStmt(l, ats)
        else:
            raise self._error("definition")
        d.comments.before = lead + d.comments.before
        d.comments.setLocationFromChildren()
        return d

    def varDefn(self, l, ats):
        tok = self._next()
        if tok.tag not in (LET, VAR):
            self._error("let or var")
        var = tok.text
        pat = self.pattern()
        if self._peekTag() is EQ:
            self._next()
            # TODO: this hack is here to avoid assignment. Remove when assignment is
            # no longer an expression.
            if self._peekTag() is NEWLINE:
                e = self.blockExpr()
            else:
                e = self.maybeTupleExpr()
        else:
            e = None
        d = ast.VariableDefinition(ats, var, pat, e, None, self._location(l))
        if e is not None:
            self._liftComments(d, None, e)
        else:
            self._liftComments(d, None, pat)
        return d

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
        loc = self._location(l)
        d = ast.FunctionDefinition(ats, name, tps, ps, rty, body, None, loc)
        if body is not None:
            self._liftComments(d, None, body)
        elif rty is not None:
            self._liftComments(d, None, rty)
        else:
            self._tailComment(d)
        return d

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
        ms = self._parseBlock(self.defnOrImport)
        loc = self._location(l)
        d = ast.ClassDefinition(ats, name, tps, ctor, scl, sargs, strs, ms, None, loc)
        if ms is not None:
            d.comments = ast.CommentGroup()
        elif strs is not None:
            self._liftComments(d, None, strs[-1])
        else:
            self._tailComment(d)
        return d

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
        ms = self._parseBlock(self.defnOrImport)
        loc = self._location(l)
        d = ast.TraitDefinition(ats, name, tps, sts, ms, None, loc)
        if ms is not None:
            d.comments = ast.CommentGroup()
        elif sts is not None:
            self._liftComments(d, None, sts[-1])
        else:
            self._tailComment(d)
        return d

    def arrayElementsStmt(self, l, ats):
        l = self._nextTag(ARRAYELEMENTS).location
        ety = self.ty()
        self._nextTag(COMMA)
        getDefn = self.arrayAccessorDefn()
        self._nextTag(COMMA)
        setDefn = self.arrayAccessorDefn()
        self._nextTag(COMMA)
        lengthDefn = self.arrayAccessorDefn()
        cg = ast.CommentGroup()
        loc = self._location(l)
        return ast.ArrayElementsStatement(ats, ety, getDefn, setDefn, lengthDefn, cg, loc)

    def arrayAccessorDefn(self):
        lead = self.leadComments()
        l = self._peek().location
        ats = self.attribs()
        name = self.ident()
        loc = self._location(l)
        tail = self.tailComment()
        cg = ast.CommentGroup(lead, tail)
        cg.setLocationFromChildren()
        return ast.ArrayAccessorDefinition(ats, name, cg, loc)

    def constructor(self):
        if self._peekTag() not in (ATTRIB, LPAREN):
            return None
        l = self._peek().location
        ats = self.attribs()
        params = self.parameters()
        cg = ast.CommentGroup()
        loc = self._location(l)
        return ast.PrimaryConstructorDefinition(ats, params, cg, loc)

    # Patterns
    def pattern(self):
        return self.maybeTuplePattern()

    def maybeTuplePattern(self):
        l = self._peek().location
        elems = self._parseRepSep(self.maybeBinopPattern, COMMA)
        if len(elems) == 1:
            return elems[0]
        else:
            p = ast.TuplePattern(elems, None, self._location(l))
            self._liftComments(p, elems[0], elems[-1])
            return p

    def maybeBinopPattern(self):
        return self._parseBinop(self.simplePattern, ast.BinaryPattern, self.LAST_BINOP_LEVEL)

    def simplePattern(self):
        lead = self.leadComments()
        tag = self._peekTag()
        if tag is SYMBOL:
            p = self.prefixedPattern()
        elif tag is UNDERSCORE:
            p = self.blankPattern()
        elif tag in (TRUE, FALSE, NULL, INTEGER, FLOAT, STRING):
            p = self.literalPattern()
        elif tag is OPERATOR:
            p = self.unopPattern()
        elif tag is LPAREN:
            p = self.groupPattern()
        else:
            self._error("pattern")
        p.comments.before = lead + p.comments.before
        p.comments.setLocationFromChildren()
        return p

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
            result = ast.VariablePattern(prefix[0].name, ty, None, loc)
            if ty is None:
                self._tailComment(result)
            else:
                self._liftComments(result, None, ty)
        elif args is None and ty is None:
            mayHaveTypeArgs = False
            result = ast.ValuePattern(prefix[:-1], prefix[-1].name, None, loc)
            self._tailComment(result)
        elif args is not None:
            mayHaveTypeArgs = True
            result = ast.DestructurePattern(prefix, args, None, loc)
            self._tailComment(result)
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
            p = ast.BlankPattern(ty, None, self._location(l))
            self._liftComments(p, None, ty)
        else:
            p = ast.BlankPattern(None, None, self._location(l))
            self._tailComment(p)
        return p

    def unopPattern(self):
        tok = self._nextTag(OPERATOR)
        p = self.simplePattern()
        u = ast.UnaryPattern(tok.text, p, None, self._location(tok.location))
        self._liftComments(u, None, p)
        return u

    def literalPattern(self):
        lit = self.literal()
        p = ast.LiteralPattern(lit, None, lit.location)
        self._tailComment(p)
        return p

    def groupPattern(self):
        l = self._nextTag(LPAREN).location
        if self._peekTag() is RPAREN:
            self._next()
            loc = self._location(l)
            lit = ast.UnitLiteral(loc)
            p = ast.LiteralPattern(lit, None, loc)
        else:
            i = self.pattern()
            self._nextTag(RPAREN)
            p = ast.GroupPattern(i, None, self._location(l))
        self._tailComment(p)
        return p

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
            fty = ast.FunctionType(ptys, ty, None, loc)
            self._liftComments(fty, pty, ty)
            ty = fty
        return ty

    def simpleType(self):
        lead = self.leadComments()
        tok = self._peek()
        if tok.tag is LPAREN:
            t = self.tupleType()
        elif tok.tag is SYMBOL:
            t = self.classType()
        elif tok.tag is FORSOME:
            t = self.existentialType()
        else:
            if tok.tag is UNIT:
                t = ast.UnitType(None, tok.location)
            elif tok.tag is I8:
                t = ast.I8Type(None, tok.location)
            elif tok.tag is I16:
                t = ast.I16Type(None, tok.location)
            elif tok.tag is I32:
                t = ast.I32Type(None, tok.location)
            elif tok.tag is I64:
                t = ast.I64Type(None, tok.location)
            elif tok.tag is F32:
                t = ast.F32Type(None, tok.location)
            elif tok.tag is F64:
                t = ast.F64Type(None, tok.location)
            elif tok.tag is BOOLEAN:
                t = ast.BooleanType(None, tok.location)
            elif tok.tag is UNDERSCORE:
                t = ast.BlankType(None, tok.location)
            else:
                self._error("type")
            self._next()
            self._tailComment(t)
        t.comments.before = lead + t.comments.before
        t.comments.setLocationFromChildren()
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
        t = ast.TupleType(tys, flags, None, loc)
        self._tailComment(t)
        return t

    def classType(self):
        l = self._peek().location
        prefixComponents = self.scopePrefix()
        prefix = prefixComponents[:-1]
        last = prefixComponents[-1]
        name = last.name
        typeArgs = last.typeArguments if last.typeArguments is not None else []
        flags = self.typeFlags()
        t = ast.ClassType(prefix, name, typeArgs, flags, None, self._location(l))
        self._tailComment(t)
        return t

    def existentialType(self):
        l = self._nextTag(FORSOME)
        tps = self.typeParameters()
        t = self.ty()
        e = ast.ExistentialType(tps, t, None, self._location(l.location))
        self._liftComments(e, None, t)
        return e

    def typeFlags(self):
        tok = self._peek()
        flags = set()
        if tok.tag is OPERATOR and tok.text == "?":
            self._next()
            flags.add("?")
        return flags

    # Expressions
    def expr(self):
        if self._peekTag() is NEWLINE:
            return self.blockExpr()
        else:
            return self.maybeAssignExpr()

    def maybeAssignExpr(self):
        es = self._parseRepSep(self.maybeTupleExpr, EQ)
        e = es.pop()
        while len(es) > 0:
            l = es.pop()
            a = ast.AssignExpression(l, e, None, l.location.combine(e.location))
            self._liftComments(a, l, e)
            e = a
        return e

    def maybeTupleExpr(self):
        l = self._peek().location
        elems = self._parseRepSep(self.maybeBinopExpr, COMMA)
        if len(elems) == 1:
            return elems[0]
        else:
            e = ast.TupleExpression(elems, None, self._location(l))
            self._liftComments(e, elems[0], elems[-1])
            return e

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
        if self._peekTag(-1) is OUTDENT:
            return e
        tag = self._peekTag()
        while tag in (DOT, LBRACK, LPAREN):
            if tag is DOT:
                self._next()
                name = self.symbol()
                callee = ast.PropertyExpression(
                    e, name, None, e.location.combine(self.location))
                self._liftComments(callee, e, None)
                self._tailComment(callee)
            else:
                callee = e
            typeArgs = self._parseOption(LBRACK, self.typeArguments)
            args = self._parseOption(LPAREN, self.arguments)
            if typeArgs is not None or args is not None:
                c = ast.CallExpression(callee, typeArgs, args, None, self._location(e.location))
                self._liftComments(c, callee, None)
                self._tailComment(c)
                e = c
            else:
                e = callee
            tag = self._peek().tag
        return e

    def receiverExpr(self):
        lead = self.leadComments()
        tag = self._peek().tag
        if tag is OPERATOR:
            e = self.unaryExpr()
        elif tag in (INTEGER, FLOAT, STRING, TRUE, FALSE, NULL):
            e = self.literalExpr()
        elif tag is SYMBOL:
            e = self.varExpr()
        elif tag is THIS:
            e = self.thisExpr()
        elif tag is SUPER:
            e = self.superExpr()
        elif tag is LPAREN:
            e = self.groupExpr()
        elif tag is NEW:
            e = self.newArrayExpr()
        elif tag is IF:
            e = self.ifExpr()
        elif tag is WHILE:
            e = self.whileExpr()
        elif tag is BREAK:
            e = self.breakExpr()
        elif tag is CONTINUE:
            e = self.continueExpr()
        elif tag is MATCH:
            e = self.matchExpr()
        elif tag is THROW:
            e = self.throwExpr()
        elif tag is TRY:
            e = self.tryExpr()
        elif tag is LAMBDA:
            e = self.lambdaExpr()
        elif tag is RETURN:
            e = self.returnExpr()
        else:
            self._error("expression")
        e.comments.before = lead + e.comments.before
        e.comments.setLocationFromChildren()
        return e

    def unaryExpr(self):
        op = self._nextTag(OPERATOR, "expression")
        e = self.maybeCallExpr()
        u = ast.UnaryExpression(op.text, e, None, op.location.combine(e.location))
        self._liftComments(u, None, e)
        return u

    def literalExpr(self):
        lit = self.literal()
        e = ast.LiteralExpression(lit, None, lit.location)
        self._tailComment(e)
        return e

    def varExpr(self):
        sym = self.symbol()
        e = ast.VariableExpression(sym, ast.CommentGroup(), self.location)
        self._tailComment(e)
        return e

    def thisExpr(self):
        self._nextTag(THIS)
        e = ast.ThisExpression(None, self.location)
        self._tailComment(e)
        return e

    def superExpr(self):
        self._nextTag(SUPER)
        e = ast.SuperExpression(None, self.location)
        self._tailComment(e)
        return e

    def groupExpr(self):
        l = self._nextTag(LPAREN).location
        if self._peekTag() is RPAREN:
            self._next()
            loc = self._location(l)
            lit = ast.UnitLiteral(loc)
            e = ast.LiteralExpression(lit, None, loc)
        else:
            i = self.expr()
            self._nextTag(RPAREN)
            e = ast.GroupExpression(i, None, self._location(l))
        self._tailComment(e)
        return e

    def newArrayExpr(self):
        l = self._nextTag(NEW)
        self._nextTag(LPAREN)
        length = self.expr()
        self._nextTag(RPAREN)
        ty = self.ty()
        args = self._parseOption(LPAREN, self.arguments)
        e = ast.NewArrayExpression(length, ty, args, None, self._location(l.location))
        self._tailComment(e)
        return e

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
        e = ast.IfExpression(condExpr, trueExpr, falseExpr, None, self._location(l.location))
        self._liftComments(e, None, falseExpr if falseExpr is not None else trueExpr)
        return e

    def whileExpr(self):
        l = self._nextTag(WHILE)
        self._nextTag(LPAREN)
        condExpr = self.expr()
        self._nextTag(RPAREN)
        bodyExpr = self.expr()
        e = ast.WhileExpression(condExpr, bodyExpr, None, self._location(l.location))
        self._liftComments(e, None, bodyExpr)
        return e

    def breakExpr(self):
        self._nextTag(BREAK)
        e = ast.BreakExpression(None, self.location)
        self._tailComment(e)
        return e

    def continueExpr(self):
        self._nextTag(CONTINUE)
        e = ast.ContinueExpression(None, self.location)
        self._tailComment(e)
        return e

    def matchExpr(self):
        l = self._nextTag(MATCH)
        self._nextTag(LPAREN)
        e = self.expr()
        self._nextTag(RPAREN)
        m = self.partialFnExpr()
        return ast.MatchExpression(e, m, ast.CommentGroup(), self._location(l.location))

    def partialFnExpr(self):
        l = self._peek().location
        cases = self._parseBlock(self.partialFnCase)
        if cases is None:
            raise self._error("partial function expression")
        return ast.PartialFunctionExpression(cases, ast.CommentGroup(), self._location(l))

    def partialFnCase(self):
        lead = self.leadComments()
        l = self._nextTag(CASE)
        p = self.pattern()
        if self._peekTag() is IF:
            self._next()
            c = self.expr()
        else:
            c = None
        self._nextTag(BIG_ARROW)
        e = self.expr()
        case = ast.PartialFunctionCase(p, c, e, None, self._location(l.location))
        self._liftComments(case, None, e)
        case.comments.before = lead + case.comments.before
        return case

    def throwExpr(self):
        l = self._nextTag(THROW)
        e = self.expr()
        t = ast.ThrowExpression(e, None, self._location(l.location))
        self._liftComments(t, None, e)
        return t

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
        t = ast.TryCatchExpression(e, catchOpt, finallyOpt, None, self._location(l.location))
        self._liftComments(t, None, finallyOpt)
        return t

    def catchHandler(self):
        l = self._nextTag(CATCH)
        if self._peekTag() is LPAREN:
            self._next()
            p = self.pattern()
            self._nextTag(RPAREN)
            e = self.expr()
            loc = self._location(l.location)
            case = ast.PartialFunctionCase(p, None, e, ast.CommentGroup(), loc)
            return ast.PartialFunctionExpression([case], ast.CommentGroup(), loc)
        else:
            handler = self.partialFnExpr()
            handler.location = self._location(l.location)
            return handler

    def finallyHandler(self):
        self._nextTag(FINALLY)
        return self.expr()

    def blockExpr(self):
        l = self._peek().location
        stmts = self._parseBlock(self.blockStmt)
        if stmts is None:
            self._error("block expression")
        return ast.BlockExpression(stmts, ast.CommentGroup(), self._location(l))

    def blockStmt(self):
        while True:  # repeat
            lead = self.leadComments()
            tag = self._peekTag()
            if tag in (NEWLINE, OUTDENT):
                # Blank line, possibly preceeded by comment block.
                if len(lead) > 0:
                    cg = ast.CommentGroup(lead, [])
                    cg.setLocationFromChildren()
                    return cg
                else:
                    self._next()
                    continue
            else:
                if tag is IMPORT:
                    stmt = self.importStmt()
                elif tag in (ATTRIB, VAR, LET, DEF, CLASS, TRAIT, ARRAYELEMENTS):
                    stmt = self.defn()
                else:
                    stmt = self.expr()
                stmt.comments.before = lead + stmt.comments.before
                stmt.comments.setLocationFromChildren()
                return stmt

    def lambdaExpr(self):
        l = self._nextTag(LAMBDA)
        params = self._parseOption(LPAREN, self.parameters)
        e = self.expr()
        lam = ast.LambdaExpression(params, e, None, self._location(l.location))
        self._liftComments(lam, None, e)
        return lam

    def returnExpr(self):
        l = self._nextTag(RETURN)
        if self._peekTag() in self.EXPR_START_TOKENS:
            e = self.expr()
            r = ast.ReturnExpression(e, None, self._location(l.location))
            self._liftComments(r, None, e)
        else:
            r = ast.ReturnExpression(None, None, self._location(l.location))
            self._tailComment(r)
        return r

    # Literals
    def literal(self):
        tok = self._peek()
        if tok.tag is LPAREN:
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
        l = self._nextTag(LPAREN).location
        self._nextTag(RPAREN)
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
        lead = self.leadComments()
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
        cg = ast.CommentGroup(lead, [])
        loc = self._location(l)
        tp = ast.TypeParameter(ats, variance, name, upperBound, lowerBound, cg, loc)
        self._tailComment(tp)
        return tp

    def parameters(self):
        return self._parseList(self.parameter, "parameter", LPAREN, COMMA, RPAREN)

    def parameter(self):
        lead = self.leadComments()
        l = self._peek().location
        ats = self.attribs()
        var = None
        if self._peekTag() is VAR:
            var = self._next().text
        pat = self.simplePattern()
        p = ast.Parameter(ats, var, pat, None, self._location(l))
        self._liftComments(p, None, pat)
        p.comments.before = lead + p.comments.before
        return p

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

    def leadComments(self):
        lead = []
        while self._peekTag() is COMMENT:
            lead.append(self.comment())
            if self._peekTag() is NEWLINE:
                self._next()
        return lead

    def tailComment(self):
        tail = []
        if self._peekTag() is COMMENT:
            tail.append(self.comment())
        return tail

    def comment(self):
        tok = self._nextTag(COMMENT)
        return ast.Comment(tok.text, tok.location)

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
                b = astCtor(op.text, e, term, None, loc)
                self._liftComments(b, e, term)
                e = b
        else:
            e = terms[-1]
            for i in xrange(len(ops) - 1, -1, -1):
                op = ops[i]
                term = terms[i]
                if self._associativity(op.text) is not self.RIGHT_ASSOC:
                    raise ParseException(
                        op.location, "left and right associative operators are mixed together")
                loc = term.location.combine(e.location)
                b = astCtor(op.text, term, e, None, loc)
                self._liftComments(b, term, e)
                e = b
        return e

    def _parseOption(self, hintTag, parser):
        if self._peekTag() is hintTag:
            return parser()
        else:
            return None

    def _parseBlock(self, parser):
        # Search forward for an INDENT. There may be any number of NEWLINEs first.
        i = 0
        while self._peekTag(i) is NEWLINE:
            i += 1
        if i == 0 or self._peekTag(i) is not INDENT:
            return None
        for _ in xrange(i):
            self._next()
        self._nextTag(INDENT)

        # Parse statements in the block.
        stmts = self._parseStatements(parser, OUTDENT)
        self._nextTag(OUTDENT)
        return stmts

    def _parseStatements(self, parser, end):
        stmts = []
        while True:
            lead = self.leadComments()
            tag = self._peekTag()
            if tag in (NEWLINE, end) and len(lead) > 0:
                cg = ast.CommentGroup(lead, [])
                cg.setLocationFromChildren()
                stmts.append(cg)
            if tag is end:
                break
            elif tag is NEWLINE:
                stmts.append(ast.BlankLine(self.location))
                self._next()
            else:
                stmt = parser()
                stmt.comments.before = lead + stmt.comments.before
                stmt.comments.setLocationFromChildren()
                stmts.append(stmt)
                if self._peekTag(-1) is not OUTDENT and self._peekTag() is not end:
                    self._nextTag(NEWLINE)
        return stmts

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

    def _commented(self, parser):
        lead = []
        while self._peekTag() is COMMENT:
            lead.append(self.comment())
            if self._peekTag() is NEWLINE:
                self._next()

        x = parser()

        tail = []
        if self._peekTag() is COMMENT:
            tail.append(self.comment())

        if x.comments is None:
            x.comments = ast.CommentGroup(lead, tail, NoLoc)
        else:
            x.comments.before = lead + x.comments.before
            x.comments.after.extend(tail)
        x.comments.setLocationFromChildren()
        return x

    def _tailComment(self, e):
        if e.comments is None:
            e.comments = ast.CommentGroup()
        if self._peekTag() is COMMENT:
            e.comments.after.append(self.comment())
            e.comments.setLocationFromChildren()

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

    def _liftComments(self, parent, left, right):
        assert parent.comments is None
        parent.comments = ast.CommentGroup([], [])
        if left is not None:
            parent.comments.before = left.comments.before
            left.comments.before = []
            left.comments.setLocationFromChildren()
        if right is not None:
            parent.comments.after = right.comments.after
            right.comments.after = []
            right.comments.setLocationFromChildren()
        parent.comments.setLocationFromChildren()

    def _location(self, begin):
        return begin.combine(self.location)

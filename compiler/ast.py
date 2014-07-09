# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import StringIO

from equality import Equality
from visitor import Visitor
from utils import *


class AstNode(Equality):
    def __str__(self):
        buf = StringIO.StringIO()
        printer = AstPrinter(buf)
        printer.visit(self)
        return buf.getvalue()

    def data(self):
        return ""

    def children(self):
        return []


class AstModule(AstNode):
    def __init__(self, definitions):
        self.definitions = definitions

    def __repr__(self):
        return "AstModule(%s)" % repr(self.definitions)

    def tag(self):
        return "AstModule"

    def children(self):
        return self.definitions


class AstFlag(AstNode):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "AstFlag(%s)" % self.name

    def tag(self):
        return "Flag"

    def data(self):
        return self.name


class AstDefinition(AstNode):
    def __init__(self, flags):
        self.flags = flags


class AstVariableDefinition(AstDefinition):
    def __init__(self, flags, pattern, expression):
        super(AstVariableDefinition, self).__init__(flags)
        self.pattern = pattern
        self.expression = expression

    def __repr__(self):
        return "AstVariableDefinition(%s, %s)" % (repr(self.pattern), repr(self.expression))

    def tag(self):
        return "VariableDefinition"

    def children(self):
        return self.flags + [self.pattern, self.expression]


class AstFunctionDefinition(AstDefinition):
    def __init__(self, flags, name, typeParameters, parameters, returnType, body):
        super(AstFunctionDefinition, self).__init__(flags)
        self.name = name
        self.typeParameters = typeParameters
        self.parameters = parameters
        self.returnType = returnType
        self.body = body

    def __repr__(self):
        return "AstFunctionDefinition(%s, %s, %s, %s, %s)" % \
            (repr(self.name), repr(self.typeParameters), repr(self.parameters),
             repr(self.returnType), repr(self.body))

    def tag(self):
        return "FunctionDefinition"

    def data(self):
        return self.name

    def children(self):
        return self.flags + self.typeParameters + self.parameters + [self.returnType, self.body]

    def isConstructor(self):
        return self.name == "this"


class AstClassDefinition(AstDefinition):
    def __init__(self, flags, name, typeParameters, constructor, supertypes, members):
        super(AstClassDefinition, self).__init__(flags)
        self.name = name
        self.typeParameters = typeParameters
        self.constructor = constructor
        self.supertypes = supertypes
        self.members = members

    def __repr__(self):
        return "AstClassDefinition(%s, %s, %s, %s, %s)" % \
            (repr(self.name), repr(self.typeParameters), repr(self.constructor),
             repr(self.supertypes), repr(self.members))

    def tag(self):
        return "ClassDefinition"

    def data(self):
        return self.name

    def children(self):
        children = self.flags + self.typeParameters + self.supertypes
        if self.constructor is not None:
            children.append(self.constructor)
        children.extend(self.members)
        return children

    def hasConstructors(self):
        return self.constructor is not None or \
               any(isinstance(member, AstFunctionDefinition) and member.isConstructor()
                   for member in self.members)


class AstPrimaryConstructorDefinition(AstDefinition):
    def __init__(self, flags, parameters):
        super(AstPrimaryConstructorDefinition, self).__init__(flags)
        self.parameters = parameters

    def __repr__(self):
        return "AstPrimaryConstructorDefinition(%s)" % self.parameters

    def tag(self):
        return "PrimaryConstructorDefinition"

    def children(self):
        return self.flags + self.parameters


class AstTypeParameter(AstNode):
    def __init__(self, name, upperBound, lowerBound):
        self.name = name
        self.upperBound = upperBound
        self.lowerBound = lowerBound

    def __repr__(self):
        return "AstTypeParameter(%s, %s, %s)" % \
            (repr(self.name), repr(self.upperBound), repr(self.lowerBound))

    def tag(self):
        return "TypeParameter"

    def data(self):
        return self.name

    def children(self):
        return [self.upperBound, self.lowerBound]


class AstParameter(AstDefinition):
    def __init__(self, flags, pattern):
        super(AstParameter, self).__init__(flags)
        self.pattern = pattern

    def __repr__(self):
        return "AstParameter(%s)" % repr(self.pattern)

    def tag(self):
        return "Parameter"

    def children(self):
        return self.flags + [self.pattern]


class AstPattern(AstNode):
    pass


class AstVariablePattern(AstNode):
    def __init__(self, name, ty):
        self.name = name
        self.ty = ty

    def __repr__(self):
        return "AstVariablePattern(%s, %s)" % (repr(self.name), repr(self.ty))

    def tag(self):
        return "VariablePattern"

    def data(self):
        return self.name

    def children(self):
        return [self.ty] if self.ty else []


class AstType(AstNode):
    pass


class AstUnitType(AstType):
    def __repr__(self):
        return "AstUnitType"

    def tag(self):
        return "UnitType"


class AstI8Type(AstType):
    def __repr__(self):
        return "AstI8Type"

    def tag(self):
        return "I8Type"


class AstI16Type(AstType):
    def __repr__(self):
        return "AstI16Type"

    def tag(self):
        return "I16Type"


class AstI32Type(AstType):
    def __repr__(self):
        return "AstI32Type"

    def tag(self):
        return "I32Type"


class AstI64Type(AstType):
    def __repr__(self):
        return "AstI64Type"

    def tag(self):
        return "I64Type"


class AstF32Type(AstType):
    def __repr__(self):
        return "AstF32Type"

    def tag(self):
        return "F32Type"


class AstF64Type(AstType):
    def __repr__(self):
        return "AstF64Type"

    def tag(self):
        return "F64Type"


class AstBooleanType(AstType):
    def __repr__(self):
        return "AstBooleanType"

    def tag(self):
        return "BooleanType"


class AstClassType(AstType):
    def __init__(self, name, typeArguments, flags=None):
        if flags is None:
            flags = set()
        self.name = name
        self.typeArguments = typeArguments
        self.flags = flags

    def __repr__(self):
        return "AstClassType(%s, %s, %s)" % \
               (repr(self.name), repr(self.typeArguments),
                ", ".join(self.flags))

    def tag(self):
        return "ClassType"

    def data(self):
        return self.name + " " + ", ".join(self.flags)

    def children(self):
        return self.typeArguments


class AstExpression(AstNode):
    pass


class AstLiteralExpression(AstExpression):
    def __init__(self, literal):
        self.literal = literal

    def __repr__(self):
        return "AstLiteralExpression(%s)" % repr(self.literal)

    def tag(self):
        return "LiteralExpression"

    def children(self):
        return [self.literal]


class AstVariableExpression(AstExpression):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "AstVariableExpression(%s)" % repr(self.name)

    def tag(self):
        return "VariableExpression"

    def data(self):
        return self.name


class AstThisExpression(AstExpression):
    def __repr__(self):
        return "AstThisExpression"

    def tag(self):
        return "ThisExpression"


class AstSuperExpression(AstExpression):
    def __repr__(self):
        return "AstSuperExpression"

    def tag(self):
        return "SuperExpression"


class AstBlockExpression(AstExpression):
    def __init__(self, statements):
        self.statements = statements

    def __repr__(self):
        return "AstBlockExpression(%s)" % repr(self.statements)

    def tag(self):
        return "BlockExpression"

    def children(self):
        return self.statements


class AstAssignExpression(AstExpression):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __repr__(self):
        return "AstAssignExpression(%s, %s)" % (repr(self.left), repr(self.right))

    def tag(self):
        return "AssignExpression"

    def children(self):
        return [self.left, self.right]


class AstPropertyExpression(AstExpression):
    def __init__(self, receiver, propertyName):
        self.receiver = receiver
        self.propertyName = propertyName

    def __repr__(self):
        return "AstPropertyExpression(%s, %s)" % (repr(self.receiver), self.propertyName)

    def tag(self):
        return "AstPropertyExpression"

    def data(self):
        return self.propertyName

    def children(self):
        return [self.receiver]


class AstCallExpression(AstExpression):
    def __init__(self, callee, typeArguments, arguments):
        self.callee = callee
        self.typeArguments = typeArguments
        self.arguments = arguments

    def __repr__(self):
        return "AstCallExpression(%s, %s, %s)" % \
            (repr(self.callee), repr(self.typeArguments), repr(self.arguments))

    def tag(self):
        return "CallExpression"

    def children(self):
        return [self.callee] + self.typeArguments + self.arguments


class AstUnaryExpression(AstExpression):
    def __init__(self, operator, expr):
        self.operator = operator
        self.expr = expr

    def __repr__(self):
        return "AstUnaryExpression(%s, %s)" % (repr(self.operator), self.expr)

    def tag(self):
        return "UnaryExpression"

    def data(self):
        return self.operator

    def children(self):
        return [self.expr]


class AstBinaryExpression(AstExpression):
    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right

    def __repr__(self):
        return "AstBinaryExpression(%s, %s, %s)" % (self.operator, self.left, self.right)

    def tag(self):
        return "BinaryExpression"

    def data(self):
        return self.operator

    def children(self):
        return [self.left, self.right]


class AstFunctionValueExpression(AstExpression):
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return "AstFunctionValueExpression(%s)" % repr(self.expr)

    def tag(self):
        return "FunctionValueExpression"

    def children(self):
        return [self.expr]


class AstIfExpression(AstExpression):
    def __init__(self, condition, trueExpr, falseExpr):
        self.condition = condition
        self.trueExpr = trueExpr
        self.falseExpr = falseExpr

    def __repr__(self):
        return "AstIfExpression(%s, %s, %s)" % \
            (repr(self.condition), repr(self.trueExpr), repr(self.falseExpr))

    def tag(self):
        return "IfExpression"

    def children(self):
        return [self.condition, self.trueExpr, self.falseExpr]


class AstWhileExpression(AstExpression):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

    def __repr__(self):
        return "AstWhileExpression(%s, %s)" % \
            (repr(self.condition), repr(self.body))

    def tag(self):
        return "WhileExpression"

    def children(self):
        return [self.condition, self.body]


class AstBreakExpression(AstExpression):
    def __repr__(self):
        return "AstBreakExpression"

    def tag(self):
        return "BreakExpression"


class AstContinueExpression(AstExpression):
    def __repr__(self):
        return "AstContinueExpression"

    def tag(self):
        return "ContinueExpression"


class AstPartialFunctionExpression(AstExpression):
    def __init__(self, cases):
        self.cases = cases

    def __repr__(self):
        return "AstPartialFunctionExpression(%s)" % repr(self.cases)

    def tag(self):
        return "PartialFunctionExpression"

    def children(self):
        return self.cases


class AstPartialFunctionCase(AstNode):
    def __init__(self, pattern, condition, expression):
        self.pattern = pattern
        self.condition = condition
        self.expression = expression

    def __repr__(self):
        return "AstPartialFunctionCase(%s, %s, %s)" % \
            (repr(self.pattern), repr(self.condition), repr(self.expression))

    def tag(self):
        return "PartialFunctionCase"

    def children(self):
        return [self.pattern, self.condition, self.expression]


class AstMatchExpression(AstExpression):
    def __init__(self, expression, matcher):
        self.expression = expression
        self.matcher = matcher

    def __repr__(self):
        return "AstMatchExpression(%s, %s)" % \
            (repr(self.expression), repr(self.matcher))

    def tag(self):
        return "MatchExpression"

    def children(self):
        return [self.expression, self.matcher]


class AstThrowExpression(AstExpression):
    def __init__(self, exception):
        self.exception = exception

    def __repr__(self):
        return "AstThrowExpression(%s)" % repr(self.exception)

    def tag(self):
        return "ThrowExpression"

    def children(self):
        return [self.exception]


class AstTryCatchExpression(AstExpression):
    def __init__(self, expression, catchHandler, finallyHandler):
        self.expression = expression
        self.catchHandler = catchHandler
        self.finallyHandler = finallyHandler

    def __repr__(self):
        return "AstTryCatchExpression(%s, %s, %s)" % \
            (repr(self.expression), repr(self.catchHandler), repr(self.finallyHandler))

    def tag(self):
        return "TryCatchExpression"

    def children(self):
        return [self.expression, self.catchHandler, self.finallyHandler]


class AstLambdaExpression(AstExpression):
    def __init__(self, name, typeParameters, parameters, body):
        self.name = name
        self.typeParameters = typeParameters
        self.parameters = parameters
        self.body = body

    def __repr__(self):
        return "AstLambdaExpression(%s, %s, %s, %s)" % \
            (repr(self.name), repr(self.typeParameters), repr(self.parameters), repr(self.body))

    def tag(self):
        return "LambdaExpression"

    def data(self):
        return self.name

    def children(self):
        return self.typeParameters + self.parameters + [self.body]


class AstReturnExpression(AstExpression):
    def __init__(self, expression):
        self.expression = expression

    def __repr__(self):
        return "AstReturnExpression(%s)" % repr(self.expression)

    def tag(self):
        return "ReturnExpression"

    def children(self):
        return [self.expression]


class AstLiteral(AstNode):
    pass


class AstIntegerLiteral(AstLiteral):
    def __init__(self, value, width=64):
        self.value = value
        self.width = width

    def __repr__(self):
        return "AstIntegerLiteral(%d, %d)" % (self.value, self.width)

    def tag(self):
        return "IntegerLiteral"

    def data(self):
        return "%d_%d" % (self.value, self.width)


class AstFloatLiteral(AstLiteral):
    def __init__(self, value, width=64):
        self.value = value
        self.width = width

    def __repr__(self):
        return "AstFloatLiteral(%f, %d)" % (self.value, self.width)

    def tag(self):
        return "FloatLiteral"

    def data(self):
        return "%f_%d" % (self.value, self.width)


class AstBooleanLiteral(AstLiteral):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "AstBooleanLiteral(%s)" % self.value

    def tag(self):
        return "BooleanLiteral"

    def data(self):
        return str(self.value)


class AstNullLiteral(AstLiteral):
    def __repr__(self):
        return "AstNullLiteral"

    def tag(self):
        return "NullLiteral"


class AstStringLiteral(AstLiteral):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "AstStringLiteral(%s)" % encodeString(self.value)

    def tag(self):
        return "StringLiteral"

    def data(self):
        return encodeString(self.value)


class AstNodeVisitor(Visitor):
    def visitChildren(self, node):
        for child in node.children():
            if child is not None:
                self.visit(child)


class AstPrinter(AstNodeVisitor):
    def __init__(self, out):
        self.indentLevel = 0
        self.out = out

    def indentStr(self):
        return "  " * self.indentLevel

    def visitDefault(self, node):
        idStr = " #%d" % node.id if hasattr(node, "id") else ""
        self.out.write("%s%s %s%s\n" % (self.indentStr(), node.tag(), node.data(), idStr))

    def postVisit(self, node):
        self.indentLevel += 1
        self.visitChildren(node)
        self.indentLevel -= 1


class AstEnumerator(AstNodeVisitor):
    def __init__(self):
        self.counter = Counter()

    def visitDefault(self, node):
        node.id = self.counter()

    def postVisit(self, node):
        self.visitChildren(node)


def addNodeIds(ast):
    enumerator = AstEnumerator()
    enumerator.visit(ast)

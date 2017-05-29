# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import StringIO

from ids import AstId
from location import NoLoc
import utils
import visitor


class Node(object):
    def __init__(self, location):
        self.id = None
        self.location = location

    def __str__(self):
        buf = StringIO.StringIO()
        printer = Printer(buf)
        printer.visit(self)
        return buf.getvalue()

    def tag(self):
        return self.__class__.__name__

    def __eq__(self, other):
        return (isinstance(other, self.__class__) and
                all(v == other.__dict__[k]
                    for k, v in self.__dict__.iteritems()
                    if k != "location"))

    def __ne__(self, other):
        return not self.__eq__(other)

    def data(self):
        return ""

    def children(self):
        return []

    def setLocationFromChildren(self):
        children = self.children()
        if len(children) == 0:
            self.location = NoLoc
        elif len(children) == 1:
            self.location = children[0].location
        else:
            self.location = children[0].location.combine(children[-1].location)


class Package(Node):
    def __init__(self, modules, location):
        super(Package, self).__init__(location)
        self.modules = modules

    def __repr__(self):
        return "Package(%s)" % repr(self.modules)

    def children(self):
        return self.modules


class Module(Node):
    def __init__(self, definitions, location):
        super(Module, self).__init__(location)
        self.definitions = definitions

    def __repr__(self):
        return "Module(%s)" % repr(self.definitions)

    def children(self):
        return self.definitions


class Comment(Node):
    def __init__(self, text, location):
        super(Comment, self).__init__(location)
        self.text = text

    def __repr__(self):
        return "Comment(%s)" % self.text

    def data(self):
        return self.text


class CommentGroup(Node):
    def __init__(self, before=None, after=None, location=NoLoc):
        if before is None:
            before = []
        if after is None:
            after = []
        super(CommentGroup, self).__init__(location)
        self.before = before
        self.after = after

    def __repr__(self):
        return ("CommentGroup(%s, %s)" % (repr(self.before), repr(self.after)))

    def children(self):
        return self.before + self.after

    def isEmpty(self):
        return len(self.before) + len(self.after) == 0


class CommentedNode(Node):
    def __init__(self, comments, location):
        super(CommentedNode, self).__init__(location)
        self.comments = comments

    # TODO: comments should be children


class Attribute(Node):
    def __init__(self, name, location):
        super(Attribute, self).__init__(location)
        self.name = name

    def __repr__(self):
        return "Attribute(%s)" % self.name

    def data(self):
        return self.name


class Definition(CommentedNode):
    def __init__(self, attribs, comments, location):
        super(Definition, self).__init__(comments, location)
        self.attribs = attribs


class VariableDefinition(Definition):
    def __init__(self, attribs, keyword, pattern, expression, comments, location):
        super(VariableDefinition, self).__init__(attribs, comments, location)
        self.keyword = keyword
        self.pattern = pattern
        self.expression = expression

    def __repr__(self):
        return "VariableDefinition(%s, %s, %s)" % \
            (self.keyword, repr(self.pattern), repr(self.expression))

    def data(self):
        return self.keyword

    def children(self):
        return self.attribs + [self.pattern, self.expression]


class FunctionDefinition(Definition):
    def __init__(self, attribs, name, typeParameters, parameters,
                 returnType, body, comments, location):
        super(FunctionDefinition, self).__init__(attribs, comments, location)
        self.name = name
        self.typeParameters = typeParameters
        self.parameters = parameters
        self.returnType = returnType
        self.body = body

    def __repr__(self):
        return "FunctionDefinition(%s, %s, %s, %s, %s)" % \
            (repr(self.name), repr(self.typeParameters), repr(self.parameters),
             repr(self.returnType), repr(self.body))

    def data(self):
        return self.name

    def children(self):
        return (self.attribs +
                utils.listOpt(self.typeParameters) +
                utils.listOpt(self.parameters) +
               [self.returnType, self.body])

    def isConstructor(self):
        return self.name == "this"


class ClassDefinition(Definition):
    def __init__(self, attribs, name, typeParameters, constructor,
                 superclass, superArgs, supertraits, members, comments, location):
        super(ClassDefinition, self).__init__(attribs, comments, location)
        self.name = name
        self.typeParameters = typeParameters
        self.constructor = constructor
        self.superclass = superclass
        self.superArgs = superArgs
        self.supertraits = supertraits
        self.members = members

    def __repr__(self):
        return "ClassDefinition(%s, %s, %s, %s, %s, %s, %s)" % \
            (repr(self.name), repr(self.typeParameters), repr(self.constructor),
             repr(self.superclass), repr(self.superArgs), repr(self.supertraits),
             repr(self.members))

    def data(self):
        return self.name

    def children(self):
        children = list(self.attribs)
        if self.typeParameters is not None:
            children.extend(self.typeParameters)
        if self.constructor is not None:
            children.append(self.constructor)
        if self.superclass is not None:
            children.append(self.superclass)
        if self.superArgs is not None:
            children.extend(self.superArgs)
        if self.supertraits is not None:
            children.extend(self.supertraits)
        if self.members is not None:
            children.extend(self.members)
        return children

    def hasConstructors(self):
        return self.constructor is not None or \
               any(isinstance(member, FunctionDefinition) and member.isConstructor()
                   for member in utils.iterOpt(self.members))


class PrimaryConstructorDefinition(Definition):
    def __init__(self, attribs, parameters, comments, location):
        super(PrimaryConstructorDefinition, self).__init__(attribs, comments, location)
        self.parameters = parameters

    def __repr__(self):
        return "PrimaryConstructorDefinition(%s)" % self.parameters

    def children(self):
        return self.attribs + self.parameters


class ArrayElementsStatement(Definition):
    def __init__(self, attribs, elementType, getDefn, setDefn, lengthDefn, comments, location):
        super(ArrayElementsStatement, self).__init__(attribs, comments, location)
        self.elementType = elementType
        self.getDefn = getDefn
        self.setDefn = setDefn
        self.lengthDefn = lengthDefn

    def __repr__(self):
        return "ArrayElementsStatement(%s, %s, %s, %s)" % \
            (repr(self.elementType), repr(self.getDefn),
             repr(self.setDefn), repr(self.lengthDefn))

    def children(self):
        return self.attribs + [self.elementType, self.getDefn, self.setDefn, self.lengthDefn]


class ArrayAccessorDefinition(Definition):
    def __init__(self, attribs, name, comments, location):
        super(ArrayAccessorDefinition, self).__init__(attribs, comments, location)
        self.name = name

    def __repr__(self):
        return "ArrayAccessorDefinition(%s)" % repr(self.name)


class TraitDefinition(Definition):
    def __init__(self, attribs, name, typeParameters, supertypes, members, comments, location):
        super(TraitDefinition, self).__init__(attribs, comments, location)
        self.name = name
        self.typeParameters = typeParameters
        self.supertypes = supertypes
        self.members = members
        self.location = location

    def __repr__(self):
        return "TraitDefinition(%s, %s, %s, %s)" % \
            (repr(self.name), repr(self.typeParameters),
             repr(self.supertypes), repr(self.members))

    def data(self):
        return name

    def children(self):
        children = []
        children.extend(self.attribs)
        children.extend(utils.iterOpt(self.typeParameters))
        children.extend(utils.iterOpt(self.supertypes))
        children.extend(utils.iterOpt(self.members))
        return children


class ImportStatement(CommentedNode):
    def __init__(self, prefix, bindings, comments, location):
        super(ImportStatement, self).__init__(comments, location)
        self.prefix = prefix
        self.bindings = bindings

    def __repr__(self):
        return "ImportStatement(%s, %s)" % (self.prefix, self.bindings)

    def children(self):
        return self.prefix + ([] if self.bindings is None else self.bindings)


class ImportBinding(Node):
    def __init__(self, name, asName, location):
        super(ImportBinding, self).__init__(location)
        self.name = name
        self.asName = asName

    def __repr__(self):
        return "ImportBinding(%s, %s)" % (self.name, self.asName)

    def data(self):
        return self.name + (" as %s" % self.asName if self.asName is not None else "")


class ScopePrefixComponent(Node):
    def __init__(self, name, typeArguments, location):
        super(ScopePrefixComponent, self).__init__(location)
        self.name = name
        self.typeArguments = typeArguments

    def __repr__(self):
        return "ScopePrefixComponent(%s, %s)" % (self.name, self.typeArguments)

    def data(self):
        return self.name

    def children(self):
        return self.typeArguments if self.typeArguments is not None else []


class TypeParameter(Definition):
    def __init__(self, attribs, variance, name, upperBound, lowerBound, comments, location):
        super(TypeParameter, self).__init__(attribs, comments, location)
        self.name = name
        self.variance = variance
        self.upperBound = upperBound
        self.lowerBound = lowerBound

    def __repr__(self):
        return "TypeParameter(%s, %s, %s, %s)" % \
            (repr(self.variance), repr(self.name), repr(self.upperBound), repr(self.lowerBound))

    def data(self):
        varianceStr = self.variance if self.variance else ""
        return varianceStr + self.name

    def children(self):
        return self.attribs + [self.upperBound, self.lowerBound]


class Parameter(Definition):
    def __init__(self, attribs, var, pattern, comments, location):
        super(Parameter, self).__init__(attribs, comments, location)
        self.var = var
        self.pattern = pattern

    def __repr__(self):
        return "Parameter(%s, %s)" % (self.var if self.var else "let", repr(self.pattern))

    def data(self):
        return self.var

    def children(self):
        return self.attribs + [self.pattern]


class Pattern(CommentedNode):
    pass


class VariablePattern(Pattern):
    def __init__(self, name, ty, comments, location):
        super(VariablePattern, self).__init__(comments, location)
        self.name = name
        self.ty = ty

    def __repr__(self):
        return "VariablePattern(%s, %s)" % (repr(self.name), repr(self.ty))

    def data(self):
        return self.name

    def children(self):
        return [self.ty] if self.ty else []


class BlankPattern(Pattern):
    def __init__(self, ty, comments, location):
        super(BlankPattern, self).__init__(comments, location)
        self.ty = ty

    def __repr__(self):
        return "BlankPattern(%s)" % (repr(self.ty))

    def children(self):
        return [self.ty] if self.ty else []


class LiteralPattern(Pattern):
    def __init__(self, literal, comments, location):
        super(LiteralPattern, self).__init__(comments, location)
        self.literal = literal

    def __repr__(self):
        return "LiteralPattern(%s)" % repr(self.literal)

    def children(self):
        return [self.literal]


class TuplePattern(Pattern):
    def __init__(self, patterns, comments, location):
        super(TuplePattern, self).__init__(comments, location)
        self.patterns = patterns

    def __repr__(self):
        return "TuplePattern(%s)" % repr(self.patterns)

    def children(self):
        return self.patterns


class ValuePattern(Pattern):
    def __init__(self, prefix, name, comments, location):
        super(ValuePattern, self).__init__(comments, location)
        self.prefix = prefix
        self.name = name

    def __repr__(self):
        return "ValuePattern(%s, %s)" % (repr(self.prefix), self.name)

    def children(self):
        return self.prefix


class DestructurePattern(Pattern):
    def __init__(self, prefix, patterns, comments, location):
        super(DestructurePattern, self).__init__(comments, location)
        self.prefix = prefix
        self.patterns = patterns

    def __repr__(self):
        return "DestructurePattern(%s, %s)" % \
            (repr(self.prefix), self.patterns)

    def children(self):
        return self.prefix + self.patterns


class UnaryPattern(Pattern):
    def __init__(self, operator, pattern, comments, location):
        super(UnaryPattern, self).__init__(comments, location)
        self.operator = operator
        self.pattern = pattern
        self.matcherId = None

    def __repr__(self):
        return "UnaryPattern(%s, %s)" % (repr(self.operator), repr(self.pattern))

    def data(self):
        return self.operator

    def children(self):
        return [self.pattern]


class BinaryPattern(Pattern):
    def __init__(self, operator, left, right, comments, location):
        super(BinaryPattern, self).__init__(comments, location)
        self.operator = operator
        self.left = left
        self.right = right
        self.matcherId = None

    def __repr__(self):
        return "BinaryPattern(%s, %s, %s)" % \
            (repr(self.operator), repr(self.left), repr(self.right))

    def data(self):
        return self.operator

    def children(self):
        return [self.left, self.right]


class GroupPattern(Pattern):
    def __init__(self, pattern, comments, location):
        super(GroupPattern, self).__init__(comments, location)
        self.pattern = pattern

    def __repr__(self):
        return "GroupPattern(%s)" % self.pattern

    def children(self):
        return [self.pattern]


class Type(CommentedNode):
    pass


class UnitType(Type):
    def __repr__(self):
        return "UnitType"


class I8Type(Type):
    def __repr__(self):
        return "I8Type"


class I16Type(Type):
    def __repr__(self):
        return "I16Type"


class I32Type(Type):
    def __repr__(self):
        return "I32Type"


class I64Type(Type):
    def __repr__(self):
        return "I64Type"


class F32Type(Type):
    def __repr__(self):
        return "F32Type"


class F64Type(Type):
    def __repr__(self):
        return "F64Type"


class BooleanType(Type):
    def __repr__(self):
        return "BooleanType"


class ClassType(Type):
    def __init__(self, prefix, name, typeArguments, flags, comments, location):
        super(ClassType, self).__init__(comments, location)
        self.prefix = prefix
        self.name = name
        self.typeArguments = typeArguments
        self.flags = flags

    def __repr__(self):
        return "ClassType(%s, %s, %s, %s)" % \
               (repr(self.prefix), repr(self.name), repr(self.typeArguments),
                ", ".join(self.flags))

    def data(self):
        return self.name + " " + ", ".join(self.flags)

    def children(self):
        nodes = []
        if self.prefix is not None:
            nodes.extend(self.prefix)
        if self.typeArguments is not None:
            nodes.extend(self.typeArguments)
        return nodes


class TupleType(Type):
    def __init__(self, types, flags, comments, location):
        super(TupleType, self).__init__(comments, location)
        self.types = types
        self.flags = flags

    def __repr__(self):
        return "TupleType(%s, %s)" % (repr(self.types), ", ".join(self.flags))

    def children(self):
        return self.types


class BlankType(Type):
    def __repr__(self):
        return "BlankType"


class ExistentialType(Type):
    def __init__(self, typeParameters, type, comments, location):
        super(ExistentialType, self).__init__(comments, location)
        self.typeParameters = typeParameters
        self.type = type

    def __repr__(self):
        return "ExistentialType(%s, %s)" % (repr(self.typeParameters), repr(self.type))

    def children(self):
        return self.typeParameters + [self.type]


class FunctionType(Type):
    def __init__(self, parameterTypes, returnType, comments, location):
        super(FunctionType, self).__init__(comments, location)
        self.parameterTypes = parameterTypes
        self.returnType = returnType

    def __repr__(self):
        return "FunctionType(%s, %s)" % (repr(self.parameterTypes), repr(self.returnType))

    def children(self):
        return self.parameterTypes + [self.returnType]


class Expression(CommentedNode):
    pass


class LiteralExpression(Expression):
    def __init__(self, literal, comments, location):
        super(LiteralExpression, self).__init__(comments, location)
        self.literal = literal

    def __repr__(self):
        return "LiteralExpression(%s)" % repr(self.literal)

    def children(self):
        return [self.literal]


class VariableExpression(Expression):
    def __init__(self, name, comments, location):
        super(VariableExpression, self).__init__(comments, location)
        self.name = name

    def __repr__(self):
        return "VariableExpression(%s)" % repr(self.name)

    def data(self):
        return self.name


class ThisExpression(Expression):
    def __repr__(self):
        return "ThisExpression"


class SuperExpression(Expression):
    def __repr__(self):
        return "SuperExpression"


class BlockExpression(Expression):
    def __init__(self, statements, comments, location):
        super(BlockExpression, self).__init__(comments, location)
        self.statements = statements

    def __repr__(self):
        return "BlockExpression(%s)" % repr(self.statements)

    def children(self):
        return self.statements


class AssignExpression(Expression):
    def __init__(self, left, right, comments, location):
        super(AssignExpression, self).__init__(comments, location)
        self.left = left
        self.right = right

    def __repr__(self):
        return "AssignExpression(%s, %s)" % (repr(self.left), repr(self.right))

    def children(self):
        return [self.left, self.right]


class PropertyExpression(Expression):
    def __init__(self, receiver, propertyName, comments, location):
        super(PropertyExpression, self).__init__(comments, location)
        self.receiver = receiver
        self.propertyName = propertyName

    def __repr__(self):
        return "PropertyExpression(%s, %s)" % (repr(self.receiver), self.propertyName)

    def data(self):
        return self.propertyName

    def children(self):
        return [self.receiver]


class CallExpression(Expression):
    def __init__(self, callee, typeArguments, arguments, comments, location):
        super(CallExpression, self).__init__(comments, location)
        self.callee = callee
        self.typeArguments = typeArguments
        self.arguments = arguments

    def __repr__(self):
        return "CallExpression(%s, %s, %s)" % \
            (repr(self.callee), repr(self.typeArguments), repr(self.arguments))

    def children(self):
        result = [self.callee]
        if self.typeArguments is not None:
            result.extend(self.typeArguments)
        if self.arguments is not None:
            result.extend(self.arguments)
        return result


class NewArrayExpression(Expression):
    def __init__(self, length, ty, arguments, comments, location):
        super(NewArrayExpression, self).__init__(comments, location)
        self.length = length
        self.ty = ty
        self.arguments = arguments

    def __repr__(self):
        return "NewArrayExpression(%s, %s, %s)" % \
            (repr(self.length), repr(self.ty), repr(self.arguments))

    def children(self):
        children = [self.length, self.ty]
        if self.arguments is not None:
            children += self.arguments
        return children


class UnaryExpression(Expression):
    def __init__(self, operator, expr, comments, location):
        super(UnaryExpression, self).__init__(comments, location)
        self.operator = operator
        self.expr = expr

    def __repr__(self):
        return "UnaryExpression(%s, %s)" % (repr(self.operator), repr(self.expr))

    def data(self):
        return self.operator

    def children(self):
        return [self.expr]


class BinaryExpression(Expression):
    def __init__(self, operator, left, right, comments, location):
        super(BinaryExpression, self).__init__(comments, location)
        self.operator = operator
        self.left = left
        self.right = right

    def __repr__(self):
        return ("BinaryExpression(%s, %s, %s)" %
                (self.operator, repr(self.left), repr(self.right)))

    def data(self):
        return self.operator

    def children(self):
        return [self.left, self.right]


class TupleExpression(Expression):
    def __init__(self, expressions, comments, location):
        super(TupleExpression, self).__init__(comments, location)
        self.expressions = expressions

    def __repr__(self):
        return "TupleExpression(%s)" % repr(self.expressions)

    def children(self):
        return self.expressions


class IfExpression(Expression):
    def __init__(self, condition, trueExpr, falseExpr, comments, location):
        super(IfExpression, self).__init__(comments, location)
        self.condition = condition
        self.trueExpr = trueExpr
        self.falseExpr = falseExpr

    def __repr__(self):
        return "IfExpression(%s, %s, %s)" % \
            (repr(self.condition), repr(self.trueExpr), repr(self.falseExpr))

    def children(self):
        return [self.condition, self.trueExpr, self.falseExpr]


class WhileExpression(Expression):
    def __init__(self, condition, body, comments, location):
        super(WhileExpression, self).__init__(comments, location)
        self.condition = condition
        self.body = body

    def __repr__(self):
        return "WhileExpression(%s, %s)" % \
            (repr(self.condition), repr(self.body))

    def children(self):
        return [self.condition, self.body]


class BreakExpression(Expression):
    def __repr__(self):
        return "BreakExpression"


class ContinueExpression(Expression):
    def __repr__(self):
        return "ContinueExpression"


class PartialFunctionExpression(Expression):
    def __init__(self, cases, comments, location):
        super(PartialFunctionExpression, self).__init__(comments, location)
        self.cases = cases

    def __repr__(self):
        return "PartialFunctionExpression(%s)" % repr(self.cases)

    def children(self):
        return self.cases


class PartialFunctionCase(CommentedNode):
    def __init__(self, pattern, condition, expression, comments, location):
        super(PartialFunctionCase, self).__init__(comments, location)
        self.pattern = pattern
        self.condition = condition
        self.expression = expression

    def __repr__(self):
        return "PartialFunctionCase(%s, %s, %s)" % \
            (repr(self.pattern), repr(self.condition), repr(self.expression))

    def children(self):
        return [self.pattern, self.condition, self.expression]


class MatchExpression(Expression):
    def __init__(self, expression, matcher, comments, location):
        super(MatchExpression, self).__init__(comments, location)
        self.expression = expression
        self.matcher = matcher

    def __repr__(self):
        return "MatchExpression(%s, %s)" % \
            (repr(self.expression), repr(self.matcher))

    def children(self):
        return [self.expression, self.matcher]


class ThrowExpression(Expression):
    def __init__(self, exception, comments, location):
        super(ThrowExpression, self).__init__(comments, location)
        self.exception = exception

    def __repr__(self):
        return "ThrowExpression(%s)" % repr(self.exception)

    def children(self):
        return [self.exception]


class TryCatchExpression(Expression):
    def __init__(self, expression, catchHandler, finallyHandler, comments, location):
        super(TryCatchExpression, self).__init__(comments, location)
        self.expression = expression
        self.catchHandler = catchHandler
        self.finallyHandler = finallyHandler

    def __repr__(self):
        return "TryCatchExpression(%s, %s, %s)" % \
            (repr(self.expression), repr(self.catchHandler), repr(self.finallyHandler))

    def children(self):
        return [self.expression, self.catchHandler, self.finallyHandler]


class LambdaExpression(Expression):
    def __init__(self, parameters, body, comments, location):
        super(LambdaExpression, self).__init__(comments, location)
        self.parameters = parameters
        self.body = body

    def __repr__(self):
        return "LambdaExpression(%s, %s)" % \
            (repr(self.parameters), repr(self.body))

    def children(self):
        return utils.listOpt(self.parameters) + [self.body]


class ReturnExpression(Expression):
    def __init__(self, expression, comments, location):
        super(ReturnExpression, self).__init__(comments, location)
        self.expression = expression

    def __repr__(self):
        return "ReturnExpression(%s)" % repr(self.expression)

    def children(self):
        return [self.expression]


class GroupExpression(Expression):
    def __init__(self, expression, comments, location):
        super(GroupExpression, self).__init__(comments, location)
        self.expression = expression

    def __repr__(self):
        return "GroupExpression(%s)" % repr(self.expression)

    def children(self):
        return [self.expression]


class Literal(Node):
    pass


class UnitLiteral(Literal):
    def __repr__(self):
        return "UnitLiteral"


class IntegerLiteral(Literal):
    def __init__(self, text, value, width, location):
        super(IntegerLiteral, self).__init__(location)
        self.text = text
        self.value = value
        self.width = width

    def __repr__(self):
        return "IntegerLiteral(%d, %d)" % (self.value, self.width)

    def data(self):
        return "%d_%d" % (self.value, self.width)


class FloatLiteral(Literal):
    def __init__(self, text, value, width, location):
        super(FloatLiteral, self).__init__(location)
        self.text = text
        self.value = value
        self.width = width

    def __repr__(self):
        return "FloatLiteral(%f, %d)" % (self.value, self.width)

    def data(self):
        return "%f_%d" % (self.value, self.width)


class BooleanLiteral(Literal):
    def __init__(self, value, location):
        super(BooleanLiteral, self).__init__(location)
        self.value = value

    def __repr__(self):
        return "BooleanLiteral(%s)" % self.value

    def data(self):
        return str(self.value)


class NullLiteral(Literal):
    def __repr__(self):
        return "NullLiteral"


class StringLiteral(Literal):
    def __init__(self, value, location):
        super(StringLiteral, self).__init__(location)
        self.value = value

    def __repr__(self):
        return "StringLiteral(%s)" % utils.encodeString(self.value)

    def data(self):
        return utils.encodeString(self.value)


class NodeVisitor(visitor.Visitor):
    def visitChildren(self, node, *args, **kwargs):
        for child in node.children():
            if child is not None:
                self.visit(child, *args, **kwargs)


class Printer(NodeVisitor):
    def __init__(self, out):
        self.indentLevel = 0
        self.out = out

    def indentStr(self):
        return "  " * self.indentLevel

    def visitDefault(self, node):
        idStr = " #%d" % node.id.id if node.id is not None else ""
        self.out.write("%s%s %s%s\n" % (self.indentStr(), node.tag(), node.data(), idStr))

    def postVisit(self, node):
        self.indentLevel += 1
        self.visitChildren(node)
        self.indentLevel -= 1


class Enumerator(NodeVisitor):
    def __init__(self):
        self.counter = utils.Counter()

    def visitUnaryPattern(self, node):
        self.visitDefault(node)
        node.matcherId = AstId(self.counter())

    def visitBinaryPattern(self, node):
        self.visitDefault(node)
        node.matcherId = AstId(self.counter())

    def visitDefault(self, node):
        node.id = AstId(self.counter())

    def postVisit(self, node):
        self.visitChildren(node)


def addNodeIds(ast):
    enumerator = Enumerator()
    enumerator.visit(ast)

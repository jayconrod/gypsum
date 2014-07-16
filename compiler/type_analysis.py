# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from ast import *
from errors import *
from scope_analysis import *
from ir import *
from ir_types import *
from builtins import *


def analyzeTypes(info):
    """Analyzes a syntax, determines a type for each node, and reports any inconsistencies."""
    # Add supertypes for each class. Scope analysis creates the inheritance graph and ensures
    # there are no cycles, so this is pretty simple.
    supertypeVisitor = ClassSupertypeVisitor(info)
    supertypeVisitor.visit(info.ast)

    # Add type annotations for AST nodes which need them, and add type information to
    # the package.
    analysis = TypeVisitor(info)
    analysis.visit(info.ast)

    # Overloads and overrides are resolved lazily when something calls them. If nothing calls
    # an overloaded / overriden method, we need to resolve that here.
    for scope in info.scopes.values():
        scope.resolveOverrides()


class ClassSupertypeVisitor(AstNodeVisitor):
    def __init__(self, info):
        self.info = info

    def visitAstClassDefinition(self, node):
        irClass = self.info.getDefnInfo(node).irDefn
        classInfo = self.info.getClassInfo(node)
        irSuperclass = classInfo.superclassInfo.irDefn
        irClass.supertypes = [ClassType(irSuperclass, ())]
        self.visitChildren(node)

    def visitDefault(self, node):
        self.visitChildren(node)


class TypeVisitor(AstNodeVisitor):
    def __init__(self, info):
        self.info = info

        # functionStack keeps track of the function we're currently analyzing. It contains
        # FunctionState for functions or None if we're analyzing something that is not a
        # function (like a class). This is used to resolve return types and to detect recursion.
        self.functionStack = []

        # receiverTypeStack keeps track of the receiver type for the current class.
        # TODO: this seems unnecessary. Can we remove this?
        self.receiverTypeStack = []

        # scopeStack tracks the current scope. It is automatically pushed/popped when we visit
        # a node that has a scope. Scopes are needed for looking up symbols and for marking
        # symbols as defined after we process their definitions.
        self.scopeStack = []

    def scope(self):
        return self.scopeStack[-1]

    def visitAstModule(self, node):
        for defn in node.definitions:
            self.visit(defn)

    def visitAstVariableDefinition(self, node):
        if node.expression is not None:
            exprTy = self.visit(node.expression)
        else:
            exprTy = None
        self.visit(node.pattern, exprTy)

    def visitAstFunctionDefinition(self, node):
        self.handleFunctionCommon(node, node.returnType, node.body)

    def visitAstClassDefinition(self, node):
        irClass = self.info.getDefnInfo(node).irDefn
        thisType = ClassType(irClass, ())
        self.functionStack.append(None)
        self.receiverTypeStack.append(thisType)

        if node.constructor is not None:
            self.visit(node.constructor)

        if not node.hasConstructors():
            assert len(irClass.constructors) == 1
            irDefaultCtor = irClass.constructors[0]
            irDefaultCtor.parameterTypes = [thisType]
            irDefaultCtor.variables[0].type = thisType
            irDefaultCtor.returnType = UnitType

        irInitializer = irClass.initializer
        irInitializer.parameterTypes = [thisType]
        irInitializer.variables[0].type = thisType
        irInitializer.returnType = UnitType
        for member in node.members:
            self.visit(member)
        self.receiverTypeStack.pop()
        self.functionStack.pop()

    def visitAstPrimaryConstructorDefinition(self, node):
        self.handleFunctionCommon(node, None, None)

    def visitAstParameter(self, node):
        ty = self.visit(node.pattern, None)
        if self.info.hasDefnInfo(node):
            self.info.getDefnInfo(node).irDefn.type = ty
        return ty

    def visitAstVariablePattern(self, node, exprTy, mode=COMPILE_FOR_VALUE):
        if node.ty is not None:
            varTy = self.visit(node.ty)
        else:
            varTy = None
        if varTy is None and exprTy is None:
            raise TypeException("%s: type not specified" % node.name)
        elif varTy is not None:
            if mode is COMPILE_FOR_VALUE and \
               exprTy is not None and \
               not exprTy.isSubtypeOf(varTy):
                raise TypeException("%s: expression doesn't match declared type" % node.name)
            patTy = varTy
        else:
            patTy = exprTy
        irDefn = self.info.getDefnInfo(node).irDefn
        irDefn.type = patTy
        self.scope().define(node.name)
        return patTy

    def visitAstUnitType(self, node):
        return UnitType

    def visitAstBooleanType(self, node):
        return BooleanType

    def visitAstI8Type(self, node):
        return I8Type

    def visitAstI16Type(self, node):
        return I16Type

    def visitAstI32Type(self, node):
        return I32Type

    def visitAstI64Type(self, node):
        return I64Type

    def visitAstF32Type(self, node):
        return F32Type

    def visitAstF64Type(self, node):
        return F64Type

    def visitAstClassType(self, node):
        def astFlagToIrFlag(flag):
            if flag == "?":
                return NULLABLE_TYPE_FLAG
            else:
                raise NotImplementedError
        nameInfo = self.scope().lookup(node.name, localOnly=False, mayBeAssignment=False)
        if nameInfo.isOverloaded() or not isinstance(nameInfo.getDefnInfo().irDefn, Class):
            raise TypeException("%s: does not refer to a type" % node.name)
        defnInfo = nameInfo.getDefnInfo()
        self.scope().use(defnInfo, node.id, USE_AS_TYPE)

        irClass = nameInfo.getDefnInfo().irDefn
        flags = frozenset(astFlagToIrFlag(flag) for flag in node.flags)
        classTy = ClassType(irClass, (), flags)
        return classTy

    def visitAstLiteralExpression(self, node):
        ty = self.visit(node.literal)
        return ty

    def visitAstVariableExpression(self, node):
        ty = self.handlePossibleCall(self.scope(), node.name, node.id, None, [], [], False)
        return ty

    def visitAstThisExpression(self, node):
        scope = self.scope()
        nameInfo = scope.lookup("this", localOnly=False, mayBeAssignment=False)
        defnInfo = nameInfo.getDefnInfo()
        scope.use(defnInfo, node.id, USE_AS_VALUE)
        ty = defnInfo.irDefn.type
        return ty

    def visitAstSuperExpression(self, node):
        scope = self.scope()
        nameInfo = scope.lookup("this", localOnly=False, mayBeAssignment=False)
        defnInfo = nameInfo.getDefnInfo()
        scope.use(defnInfo, node.id, USE_AS_VALUE)
        thisType = defnInfo.irDefn.type
        assert isinstance(thisType, ClassType) and \
               len(thisType.clas.supertypes) > 0
        superType = thisType.clas.supertypes[0]
        return superType

    def visitAstBlockExpression(self, node):
        lastTy = UnitType
        for stmt in node.statements:
            stmtTy = self.visit(stmt)
            assert stmtTy is not None or isinstance(stmt, AstDefinition)
            lastTy = stmtTy if stmtTy else UnitType
        return lastTy

    def visitAstAssignExpression(self, node):
        rightTy = self.visit(node.right)
        leftTy = self.visit(node.left)
        if not rightTy.isSubtypeOf(leftTy):
            raise TypeException("type error")
        return leftTy

    def visitAstPropertyExpression(self, node):
        receiverTy = self.visit(node.receiver)
        ty = self.handleMethodCall(node.propertyName, node.id,
                                   receiverTy, [], [], mayAssign=False)
        return ty

    def visitAstCallExpression(self, node):
        typeArgs = map(self.visit, node.typeArguments)
        argTypes = map(self.visit, node.arguments)
        if isinstance(node.callee, AstVariableExpression):
            ty = self.handlePossibleCall(self.scope(), node.callee.name, node.id,
                                         None, typeArgs, argTypes, False)
        elif isinstance(node.callee, AstPropertyExpression):
            receiverType = self.visit(node.callee.receiver)
            ty = self.handleMethodCall(node.callee.propertyName, node.id,
                                       receiverType, typeArgs, argTypes, False)
        elif isinstance(node.callee, AstThisExpression) or \
             isinstance(node.callee, AstSuperExpression):
            receiverType = self.visit(node.callee)
            self.handleMethodCall("$constructor", node.id,
                                  receiverType, typeArgs, argTypes, False)
            ty = UnitType
        else:
            # TODO: callable expression
            raise NotImplementedError
        return ty

    def visitAstUnaryExpression(self, node):
        receiverType = self.visit(node.expr)
        ty = self.handleMethodCall(node.operator, node.id, receiverType, [], [], False)
        return ty

    def visitAstBinaryExpression(self, node):
        leftTy = self.visit(node.left)
        rightTy = self.visit(node.right)
        if node.operator in ["&&", "||"]:
            # Logic operators are handled separately from other builtins because of
            # short circuiting.
            if not self.isConditionType(leftTy) or not self.isConditionType(rightTy):
                raise TypeException("type error: expected condition types for logic operands")
            ty = BooleanType
        else:
            ty = self.handleMethodCall(node.operator, node.id, leftTy, [], [rightTy], True)
        return ty

    def visitAstIfExpression(self, node):
        condTy = self.visit(node.condition)
        if not self.isConditionType(condTy):
            raise TypeException("type error: condition must be boolean")
        trueTy = self.visit(node.trueExpr)
        if node.falseExpr is not None:
            falseTy = self.visit(node.falseExpr)
            ifTy = trueTy.combine(falseTy)
        else:
            ifTy = UnitType
        return ifTy

    def visitAstWhileExpression(self, node):
        condTy = self.visit(node.condition)
        if not self.isConditionType(condTy):
            raise TypeException("type error: condition must be boolean")
        self.visit(node.body)
        return UnitType

    def visitAstThrowExpression(self, node):
        exnTy = self.visit(node.exception)
        if not exnTy.isSubtypeOf(ClassType(getExceptionClass())):
            raise TypeException("type error: throw expression must produce an Exception")
        return NoType

    def visitAstTryCatchExpression(self, node):
        tryTy = self.visit(node.expression)
        exnTy = ClassType(getExceptionClass(), ())
        if node.catchHandler is None:
            catchTy = NoType
        else:
            catchTy = self.visit(node.catchHandler, exnTy)
        if node.finallyHandler is not None:
            self.visit(node.finallyHandler)
        ty = tryTy.combine(catchTy)
        return ty

    def visitAstPartialFunctionExpression(self, node, valueTy):
        ty = NoType
        for case in node.cases:
            caseTy = self.visit(case, valueTy)
            ty = ty.combine(caseTy)
        return ty

    def visitAstPartialFunctionCase(self, node, valueTy):
        self.visit(node.pattern, valueTy, COMPILE_FOR_MATCH)
        if node.condition is not None:
            conditionTy = self.visit(node.condition)
            if not self.isConditionType(conditionTy):
                raise TypeException("type error: condition must have boolean type")
        ty = self.visit(node.expression)
        return ty

    def visitAstReturnExpression(self, node):
        if not self.isAnalyzingFunction():
            raise TypeException("type error: return not valid in this position")
        retTy = self.visit(node.expression)
        self.functionStack[-1].handleReturn(retTy)
        return NoType

    def visitAstIntegerLiteral(self, node):
        typeMap = { 8: I8Type, 16: I16Type, 32: I32Type, 64: I64Type }
        if node.width not in typeMap:
            raise TypeException("invalid integer literal width: %d" % node.width)
        minValue = -2 ** (node.width - 1)
        maxValue = 2 ** (node.width - 1) - 1
        if node.value < minValue or maxValue < node.value:
            raise TypeException("interger literal value %d does not fit in %d bits" %
                                (node.value, node.width))
        return typeMap[node.width]

    def visitAstFloatLiteral(self, node):
        typeMap = { 32: F32Type, 64: F64Type }
        if node.width not in typeMap:
            raise TypeException("invalid float literal width: %d" % node.width)
        return typeMap[node.width]

    def visitAstStringLiteral(self, node):
        return getStringType()

    def visitAstBooleanLiteral(self, node):
        return BooleanType

    def visitAstNullLiteral(self, node):
        return getNullType()

    def handleFunctionCommon(self, node, astReturnType, astBody):
        defnInfo = self.info.getDefnInfo(node)
        irFunction = defnInfo.irDefn
        if irFunction.returnType is not None:
            # Type already known, do not process
            return
        elif self.isFunctionIdOnStack(node.id):
            # Recursive or mutually recursive function without full type info
            raise TypeException("recursive function must have full type specified")
        else:
            # Found function with unknown type, process it. We can't patiently wait until we
            # get to its AST node, since we need to know its return type.

            # Process parameter types first, including "this".
            self.functionStack.append(None)
            self.ensureParamTypeInfoForDefn(irFunction)

            # Process return type, if specified.
            if astReturnType is not None:
                if irFunction.isConstructor():
                    raise TypeException("constructors must not declare return type")
                returnType = self.visit(astReturnType)
                irFunction.returnType = returnType
            self.functionStack[-1] = FunctionState(node.id, irFunction.returnType)

            # Process body.
            if astBody is None:
                if irFunction.isConstructor():
                    bodyType = UnitType
                elif astReturnType is not None:
                    bodyType = irFunction.returnType
                else:
                    raise TypeException("return type must be specified for abstract function")
            else:
                bodyType = self.visit(astBody)
                if bodyType is not NoType:
                    bodyType = self.functionStack[-1].handleReturn(bodyType)
                else:
                    bodyType = self.functionStack[-1].getReturnType()
            self.functionStack.pop()
            if irFunction.returnType is None:
                if irFunction.isConstructor():
                    irFunction.returnType = UnitType
                else:
                    irFunction.returnType = bodyType
            else:
                if irFunction.returnType != bodyType:
                    raise TypeException("body type does not match declared return type")

    def isConditionType(self, ty):
        return ty == BooleanType or ty == NoType

    def ensureParamTypeInfoForDefn(self, irDefn):
        if not isinstance(irDefn, Function) or \
           irDefn.parameterTypes is not None:
            return
        astDefn = irDefn.astDefn
        if self.info.hasScope(astDefn):
            self.scopeStack.append(self.info.getScope(astDefn))
        irDefn.parameterTypes = []
        if irDefn.isMethod():
            thisType = ClassType(irDefn.clas, ())
            irDefn.parameterTypes.append(thisType)
            irDefn.variables[0].type = thisType
        for param in irDefn.astDefn.parameters:
            paramTy = self.visit(param)
            irDefn.parameterTypes.append(paramTy)
        if self.info.hasScope(astDefn):
            self.scopeStack.pop()

    def ensureTypeInfoForDefn(self, irDefn):
        if isinstance(irDefn, Function):
            if irDefn.returnType is None:
                self.visit(irDefn.astDefn)
        elif isinstance(irDefn, Field) or \
             isinstance(irDefn, Variable) or \
             isinstance(irDefn, Global):
            if irDefn.type is None:
                self.visit(irDefn.astVarDefn)
        elif isinstance(irDefn, Class):
            for ctor in irDefn.constructors:
                self.ensureTypeInfoForDefn(ctor)
        else:
            raise NotImplementedError

    def handleMethodCall(self, name, useAstId, receiverType, typeArgs, argTypes, mayAssign):
        irClass = getClassFromType(receiverType)
        scope = self.info.getScope(irClass)
        return self.handlePossibleCall(scope, name, useAstId,
                                       receiverType, typeArgs, argTypes,
                                       mayAssign)

    def handlePossibleCall(self, scope, name, useAstId,
                           receiverType, typeArgs, argTypes,
                           mayAssign):
        receiverIsExplicit = receiverType is not None
        if not receiverIsExplicit and len(self.receiverTypeStack) > 0:
            receiverType = self.receiverTypeStack[-1]   # may still be None
        if name == "$constructor":
            assert receiverIsExplicit
            useKind = USE_AS_CONSTRUCTOR
        elif receiverIsExplicit:
            useKind = USE_AS_PROPERTY
        else:
            useKind = USE_AS_VALUE

        nameInfo = scope.lookup(name, localOnly=receiverIsExplicit, mayBeAssignment=mayAssign)
        if nameInfo.isClass():
            irClass = nameInfo.getDefnInfo().irDefn
            nameInfo = nameInfo.getInfoForConstructors(self.info)
            receiverType = ClassType(irClass)
            receiverIsExplicit = True
            useKind = USE_AS_CONSTRUCTOR
        for overload in nameInfo.iterOverloads():
            self.ensureParamTypeInfoForDefn(overload.irDefn)
        defnInfo = nameInfo.findDefnInfoWithArgTypes(receiverType, receiverIsExplicit,
                                                     typeArgs, argTypes)
        self.scope().use(defnInfo, useAstId, useKind)
        irDefn = defnInfo.irDefn
        self.ensureTypeInfoForDefn(irDefn)
        if isinstance(irDefn, Function):
            if irDefn.isConstructor():
                return irDefn.parameterTypes[0]
            else:
                assert len(typeArgs) == len(irDefn.typeParameters)
                ty = irDefn.returnType.substitute(irDefn.typeParameters, typeArgs)
                return ty
        else:
            assert isinstance(irDefn, Variable) or \
                   isinstance(irDefn, Field) or \
                   isinstance(irDefn, Global)
            return irDefn.type

    def handleResult(self, node, result, *unused):
        if result is not None:
            self.info.setType(node, result)
        return result

    def preVisit(self, node, *unused):
        if self.info.hasScope(node.id):
            self.scopeStack.append(self.info.getScope(node.id))

    def postVisit(self, node, *unused):
        if self.info.hasScope(node.id):
            assert self.scopeStack[-1] is self.info.getScope(node.id)
            self.scopeStack.pop()

    def isFunctionIdOnStack(self, id):
        for state in self.functionStack:
            if state is not None and state.id == id:
                return True
        return False

    def isAnalyzingFunction(self):
        return len(self.functionStack) > 0 and self.functionStack[-1] is not None


class FunctionState(object):
    def __init__(self, id, declaredReturnType):
        self.id = id
        self.declaredReturnType = declaredReturnType
        self.bodyReturnType = None

    def handleReturn(self, returnType):
        if self.declaredReturnType:
            if not returnType.isSubtypeOf(self.declaredReturnType):
                raise TypeException("type error: incorrect return type")
            return self.declaredReturnType
        elif self.bodyReturnType is None:
            self.bodyReturnType = returnType
            return self.bodyReturnType
        else:
            combinedType = self.bodyReturnType.combine(returnType)  # will raise if incompatible
            self.bodyReturnType = combinedType
            return self.bodyReturnType

    def getReturnType(self):
        if self.declaredReturnType:
            return self.declaredReturnType
        elif self.bodyReturnType:
            return self.bodyReturnType
        else:
            return ClassType(getNothingClass())

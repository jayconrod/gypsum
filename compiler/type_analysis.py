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
    # Establish type information for class supertypes and type parameter upper/lower bounds.
    # This is needed for Type.isSubtypeOf to work, which is needed to type expressions.
    subtypeVisitor = SubtypeVisitor(info)
    subtypeVisitor.visit(info.ast)

    # Add type annotations for AST nodes which need them, and add type information to
    # the package.
    analysis = TypeVisitor(info)
    analysis.visit(info.ast)

    # Overloads and overrides are resolved lazily when something calls them. If nothing calls
    # an overloaded / overriden method, we need to resolve that here.
    for scope in info.scopes.values():
        scope.resolveOverrides()

    # Check that each overriding function has a return type which is a subtype of the
    # overriden function. The return type is not used to make override decisions, so this needs
    # to be done after overrides are resolved.
    for func in info.package.functions:
        if hasattr(func, "override"):
            overridenReturnType = func.override.returnType.substituteForInheritance(
                func.clas, func.override.clas)
            if not func.returnType.isSubtypeOf(overridenReturnType):
                raise TypeException(func.getLocation(),
                                    "%s: return type is not subtype of overriden function" %
                                    func.name)


class TypeVisitorCommon(AstNodeVisitor):
    """Provides common functionality for SubtypeVisitor and TypeVisitor, namely the visitor
    functions for the various AstType subclasses. Also tracks the current scope. Both visitors
    may jump around the AST randomly, and this is needed to keep track of which scope we were
    in when we return from one of these jumps."""
    def __init__(self, info):
        self.info = info
        self.scopeStack = []

    def scope(self):
        return self.scopeStack[-1]

    def preVisit(self, node, *unused):
        if self.info.hasScope(node.id):
            self.scopeStack.append(self.info.getScope(node.id))

    def postVisit(self, node, *unused):
        if self.info.hasScope(node.id):
            assert self.scopeStack[-1] is self.info.getScope(node.id)
            self.scopeStack.pop()

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
        typeArgs = map(self.visit, node.typeArguments)
        nameInfo = self.scope().lookup(node.name, node.location,
                                       ignoreDefnOrder=self.shouldIgnoreDefnOrder())
        if nameInfo.isOverloaded() or not nameInfo.getDefnInfo().irDefn.isTypeDefn():
            raise TypeException(node.location, "%s: does not refer to a type" % node.name)
        defnInfo = nameInfo.getDefnInfo()
        self.scope().use(defnInfo, node.id, USE_AS_TYPE, node.location)
        irDefn = nameInfo.getDefnInfo().irDefn
        self.ensureTypeInfoForDefn(irDefn)

        flags = frozenset(map(astTypeFlagToIrTypeFlag, node.flags))

        if isinstance(irDefn, Class):
            explicitTypeParams = getExplicitTypeParameters(irDefn)
            if len(typeArgs) != len(explicitTypeParams):
                raise TypeException(node.location,
                                    "%s: wrong number of type arguments; expected %d but have %d\n" %
                                    (node.name, len(explicitTypeParams), len(typeArgs)))
            for ta, tp in zip(typeArgs, explicitTypeParams):
                if not (tp.lowerBound.isSubtypeOf(ta) and
                        ta.isSubtypeOf(tp.upperBound)):
                    raise TypeException(node.location,
                                        "%s: type argument is not in bounds" % irDefn.name)
            allTypeArgs = tuple(map(VariableType, getImplicitTypeParameters(irDefn)) + typeArgs)
            return ClassType(irDefn, allTypeArgs, flags)
        else:
            assert isinstance(irDefn, TypeParameter)
            if flags != frozenset():
                raise TypeException(node.location, "invalid flags for variable type")
            if len(typeArgs) > 0:
                raise TypeException(node.location,
                                    "%s: type parameter cannot accept type arguments" %
                                    irDefn.name)
            return VariableType(irDefn)

    def handleResult(self, node, result, *unused):
        if result is not None:
            self.info.setType(node, result)
        return result

    def ensureTypeInfoForDefn(self, irDefn):
        raise NotImplementedError

    def shouldIgnoreDefnOrder(self):
        raise NotImplementedError



class SubtypeVisitor(TypeVisitorCommon):
    """Analyzes classes and type parameters and saves supertypes, upper bounds, and
    lower bounds. This must be done before we can start typing expressions, because we need
    a fully functional Type.isSubtypeOf method, which relies on this information. We use
    isSubtypeOf here, too, but only on definitions we've already processed. This is guaranteed
    to terminate, since we checked the subtype graph for cycles in an earlier phase."""

    def visitAstModule(self, node):
        self.visitChildren(node)

    def visitAstFunctionDefinition(self, node):
        for param in node.typeParameters:
            self.visit(param)
        if isinstance(node.body, AstBlockExpression):
            self.visit(node.body)

    def visitAstClassDefinition(self, node):
        irClass = self.info.getDefnInfo(node).irDefn
        if irClass.supertypes is not None:
            return

        for param in node.typeParameters:
            self.visit(param)
        if len(node.supertypes) == 0:
            irClass.supertypes = [getRootClassType()]
        else:
            def visitSupertype(sty):
                ty = self.visit(sty)
                if ty.isNullable():
                    raise TypeException(node.location,
                                        "%s: supertype may not be nullable" % node.name)
                return ty
            irClass.supertypes = map(visitSupertype, node.supertypes)
        for member in node.members:
            self.visit(member)

    def visitAstTypeParameter(self, node):
        irParam = self.info.getDefnInfo(node).irDefn
        if irParam.upperBound is not None:
            return

        def visitBound(bound, default):
            if bound is None:
                return default
            else:
                ty = self.visit(bound)
                if ty.isNullable():
                    raise TypeException(bound.location,
                                        "%s: bound may not be nullable" % node.name)
                return ty

        irParam.upperBound = visitBound(node.upperBound, getRootClassType())
        irParam.lowerBound = visitBound(node.lowerBound, getNothingClassType())
        if not irParam.lowerBound.isSubtypeOf(irParam.upperBound):
            raise TypeException(node.location,
                                "%s: lower bound is not subtype of upper bound" % node.name)

    def visitAstBlockExpression(self, node):
        self.visitChildren(node)

    def visitDefault(self, node):
        pass

    def ensureTypeInfoForDefn(self, irDefn):
        if hasattr(irDefn, "astDefn"):
            self.visit(irDefn.astDefn)

    def shouldIgnoreDefnOrder(self):
        # Type parameters must be defined in order, but they are not marked as defined until
        # TypeVisitor processes them. In order to avoid tripped over errors now, we ignore
        # definition order at this time.
        return True


class TypeVisitor(TypeVisitorCommon):
    """Traverses the AST and records type information for every node that requires it,
    specifically every expression, pattern, and type node. This also adds type information to
    IR definitions, including functions (parameter and return types), fields, variables, etc."""
    def __init__(self, info):
        super(TypeVisitor, self).__init__(info)

        # functionStack keeps track of the function we're currently analyzing. It contains
        # FunctionState for functions or None if we're analyzing something that is not a
        # function (like a class). This is used to resolve return types and to detect recursion.
        self.functionStack = []

        # receiverTypeStack keeps track of the receiver type for the current class.
        self.receiverTypeStack = []

    def preVisit(self, node, *unused):
        super(TypeVisitor, self).preVisit(node, *unused)
        if not self.info.hasDefnInfo(node):
            return
        irDefn = self.info.getDefnInfo(node).irDefn
        if isinstance(irDefn, Class) or isinstance(irDefn, Function):
            irDefn = self.info.getDefnInfo(node).irDefn
            if isinstance(irDefn, Function):
                functionState = FunctionState(irDefn)
                enclosingClass = self.scope().findEnclosingClass()
                receiverType = ClassType.forReceiver(enclosingClass) \
                               if enclosingClass is not None \
                               else None
            else:
                assert isinstance(irDefn, Class)
                functionState = None
                receiverType = ClassType.forReceiver(irDefn)
            self.functionStack.append(functionState)
            self.receiverTypeStack.append(receiverType)

    def postVisit(self, node, *unused):
        super(TypeVisitor, self).postVisit(node, *unused)
        if not self.info.hasDefnInfo(node):
            return
        irDefn = self.info.getDefnInfo(node).irDefn
        if isinstance(irDefn, Class) or isinstance(irDefn, Function):
            self.functionStack.pop()
            self.receiverTypeStack.pop()

    def visitAstModule(self, node):
        self.visitChildren(node)

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
        thisType = ClassType.forReceiver(irClass)

        for typeParam in node.typeParameters:
            self.visit(typeParam)

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

    def visitAstPrimaryConstructorDefinition(self, node):
        self.handleFunctionCommon(node, None, None)

    def visitAstTypeParameter(self, node):
        self.scope().define(node.name)
        irTypeParam = self.info.getDefnInfo(node).irDefn
        if node.upperBound is not None:
            irTypeParam.upperBound = self.visit(node.upperBound)
        else:
            irTypeParam.upperBound = getRootClassType()
        if node.lowerBound is not None:
            irTypeParam.lowerBound = self.visit(node.lowerBound)
        else:
            irTypeParam.lowerBound = getNothingClassType()

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
            raise TypeException(node.location, "%s: type not specified" % node.name)
        elif varTy is not None:
            if mode is COMPILE_FOR_VALUE and \
               exprTy is not None and \
               not exprTy.isSubtypeOf(varTy):
                raise TypeException(node.location,
                                    "%s: expression doesn't match declared type" % node.name)
            patTy = varTy
        else:
            patTy = exprTy
        irDefn = self.info.getDefnInfo(node).irDefn
        irDefn.type = patTy
        self.scope().define(node.name)
        return patTy

    def visitAstLiteralExpression(self, node):
        ty = self.visit(node.literal)
        return ty

    def visitAstVariableExpression(self, node):
        ty = self.handlePossibleCall(self.scope(), node.name, node.id, None,
                                     [], [], False, node.location)
        return ty

    def visitAstThisExpression(self, node):
        scope = self.scope()
        nameInfo = scope.lookup("this", node.location, localOnly=False, mayBeAssignment=False)
        defnInfo = nameInfo.getDefnInfo()
        scope.use(defnInfo, node.id, USE_AS_VALUE, node.location)
        ty = defnInfo.irDefn.type
        return ty

    def visitAstSuperExpression(self, node):
        scope = self.scope()
        nameInfo = scope.lookup("this", node.location, localOnly=False, mayBeAssignment=False)
        defnInfo = nameInfo.getDefnInfo()
        scope.use(defnInfo, node.id, USE_AS_VALUE, node.location)
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
            raise TypeException(node.location, "type error")
        return leftTy

    def visitAstPropertyExpression(self, node):
        receiverTy = self.visit(node.receiver)
        ty = self.handleMethodCall(node.propertyName, node.id,
                                   receiverTy, [], [], False, node.location)
        return ty

    def visitAstCallExpression(self, node):
        typeArgs = map(self.visit, node.typeArguments)
        argTypes = map(self.visit, node.arguments)
        if isinstance(node.callee, AstVariableExpression):
            ty = self.handlePossibleCall(self.scope(), node.callee.name, node.id,
                                         None, typeArgs, argTypes, False, node.location)
        elif isinstance(node.callee, AstPropertyExpression):
            receiverType = self.visit(node.callee.receiver)
            ty = self.handleMethodCall(node.callee.propertyName, node.id,
                                       receiverType, typeArgs, argTypes, False, node.location)
        elif isinstance(node.callee, AstThisExpression) or \
             isinstance(node.callee, AstSuperExpression):
            receiverType = self.visit(node.callee)
            self.handleMethodCall("$constructor", node.id,
                                  receiverType, typeArgs, argTypes, False, node.location)
            ty = UnitType
        else:
            # TODO: callable expression
            raise NotImplementedError
        return ty

    def visitAstUnaryExpression(self, node):
        receiverType = self.visit(node.expr)
        ty = self.handleMethodCall(node.operator, node.id, receiverType,
                                   [], [], False, node.location)
        return ty

    def visitAstBinaryExpression(self, node):
        leftTy = self.visit(node.left)
        rightTy = self.visit(node.right)
        if node.operator in ["&&", "||"]:
            # Logic operators are handled separately from other builtins because of
            # short circuiting.
            if not self.isConditionType(leftTy) or not self.isConditionType(rightTy):
                raise TypeException(node.location,
                                    "expected condition types for logic operands")
            ty = BooleanType
        else:
            ty = self.handleMethodCall(node.operator, node.id, leftTy,
                                       [], [rightTy], True, node.location)
        return ty

    def visitAstIfExpression(self, node):
        condTy = self.visit(node.condition)
        if not self.isConditionType(condTy):
            raise TypeException(node.location, "condition must be boolean")
        trueTy = self.visit(node.trueExpr)
        if node.falseExpr is not None:
            falseTy = self.visit(node.falseExpr)
            ifTy = trueTy.combine(falseTy, node.location)
        else:
            ifTy = UnitType
        return ifTy

    def visitAstWhileExpression(self, node):
        condTy = self.visit(node.condition)
        if not self.isConditionType(condTy):
            raise TypeException(node.condition.location, "condition must be boolean")
        self.visit(node.body)
        return UnitType

    def visitAstThrowExpression(self, node):
        exnTy = self.visit(node.exception)
        if not exnTy.isSubtypeOf(ClassType(getExceptionClass())):
            raise TypeException(node.location, "throw expression must produce an Exception")
        return NoType

    def visitAstTryCatchExpression(self, node):
        tryTy = self.visit(node.expression)
        exnTy = ClassType.forReceiver(getExceptionClass())
        if node.catchHandler is None:
            catchTy = NoType
        else:
            catchTy = self.visit(node.catchHandler, exnTy)
        if node.finallyHandler is not None:
            self.visit(node.finallyHandler)
        ty = tryTy.combine(catchTy, node.location)
        return ty

    def visitAstPartialFunctionExpression(self, node, valueTy):
        ty = NoType
        for case in node.cases:
            caseTy = self.visit(case, valueTy)
            ty = ty.combine(caseTy, node.location)
        return ty

    def visitAstPartialFunctionCase(self, node, valueTy):
        self.visit(node.pattern, valueTy, COMPILE_FOR_MATCH)
        if node.condition is not None:
            conditionTy = self.visit(node.condition)
            if not self.isConditionType(conditionTy):
                raise TypeException(node.condition.location, "condition must have boolean type")
        ty = self.visit(node.expression)
        return ty

    def visitAstReturnExpression(self, node):
        if not self.isAnalyzingFunction() or \
           (self.functionStack[-1].irDefn.isConstructor() and node.expression is not None):
            raise TypeException(node.location, "return not valid in this position")
        if node.expression is None:
            retTy = UnitType
        else:
            retTy = self.visit(node.expression)
        self.functionStack[-1].handleReturn(retTy)
        return NoType

    def visitAstIntegerLiteral(self, node):
        typeMap = { 8: I8Type, 16: I16Type, 32: I32Type, 64: I64Type }
        if node.width not in typeMap:
            raise TypeException(node.location, "invalid integer literal width: %d" % node.width)
        minValue = -2 ** (node.width - 1)
        maxValue = 2 ** (node.width - 1) - 1
        if node.value < minValue or maxValue < node.value:
            raise TypeException(node.location,
                                "interger literal value %d does not fit in %d bits" %
                                (node.value, node.width))
        return typeMap[node.width]

    def visitAstFloatLiteral(self, node):
        typeMap = { 32: F32Type, 64: F64Type }
        if node.width not in typeMap:
            raise TypeException(node.location, "invalid float literal width: %d" % node.width)
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
        elif self.isRecursiveFunctionWithoutType(irFunction):
            # Recursive or mutually recursive function without full type info
            raise TypeException(node.location,
                                "recursive function must have full type specified")
        else:
            # Found function with unknown type, process it. We can't patiently wait until we
            # get to its AST node, since we need to know its return type.

            # Process parameter types first, including "this".
            self.ensureParamTypeInfoForDefn(irFunction)

            # Process return type, if specified.
            if astReturnType is not None:
                if irFunction.isConstructor():
                    raise TypeException(node.location,
                                        "constructors must not declare return type")
                irFunction.returnType = self.visit(astReturnType)
            self.functionStack[-1].declaredReturnType = irFunction.returnType

            # Process body.
            if astBody is None:
                if irFunction.isConstructor():
                    bodyType = UnitType
                elif astReturnType is not None:
                    bodyType = irFunction.returnType
                else:
                    raise TypeException(node.location,
                                        "return type must be specified for abstract function")
            else:
                bodyType = self.visit(astBody)
                if bodyType is not NoType:
                    bodyType = self.functionStack[-1].handleReturn(bodyType)
                else:
                    bodyType = self.functionStack[-1].getReturnType()
            if irFunction.returnType is None:
                if irFunction.isConstructor():
                    irFunction.returnType = UnitType
                else:
                    irFunction.returnType = bodyType
            else:
                if irFunction.returnType != bodyType:
                    raise TypeException(node.location,
                                        "body type does not match declared return type")

    def isConditionType(self, ty):
        return ty == BooleanType or ty == NoType

    def ensureParamTypeInfoForDefn(self, irDefn):
        if isinstance(irDefn, Class):
            for ctor in irDefn.constructors:
                self.ensureParamTypeInfoForDefn(ctor)
        elif isinstance(irDefn, Function):
            if irDefn.parameterTypes is not None:
                return
            astDefn = irDefn.astDefn
            self.preVisit(astDefn)

            if isinstance(astDefn, AstFunctionDefinition):
                for typeParam in astDefn.typeParameters:
                    self.visit(typeParam)

            irDefn.parameterTypes = []
            if irDefn.isMethod():
                receiverType = self.receiverTypeStack[-1]
                irDefn.parameterTypes.append(receiverType)
                assert irDefn.variables[0].name == "$this"
                irDefn.variables[0].type = receiverType
            for param in irDefn.astDefn.parameters:
                paramTy = self.visit(param)
                irDefn.parameterTypes.append(paramTy)

            self.postVisit(astDefn)

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
                self.ensureParamTypeInfoForDefn(ctor)
        elif isinstance(irDefn, TypeParameter):
            pass   # already done in SubtypeVisitor
        else:
            raise NotImplementedError

    def shouldIgnoreDefnOrder(self):
        return False

    def handleMethodCall(self, name, useAstId, receiverType,
                         typeArgs, argTypes, mayAssign, loc):
        irClass = getClassFromType(receiverType)
        scope = self.info.getScope(irClass)
        return self.handlePossibleCall(scope, name, useAstId,
                                       receiverType, typeArgs, argTypes,
                                       mayAssign, loc)

    def handlePossibleCall(self, scope, name, useAstId,
                           receiverType, typeArgs, argTypes,
                           mayAssign, loc):
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

        nameInfo = scope.lookup(name, loc,
                                localOnly=receiverIsExplicit, mayBeAssignment=mayAssign)
        if nameInfo.isClass():
            irClass = nameInfo.getDefnInfo().irDefn
            self.ensureParamTypeInfoForDefn(irClass)
            explicitTypeParams = getExplicitTypeParameters(irClass)
            if len(typeArgs) != len(explicitTypeParams):
                raise TypeException(loc,
                                    "wrong number of type arguments: expected %d but have %d" %
                                    (len(typeArgs), len(explicitTypeParams)))
            if not all(tp.contains(ta) for tp, ta in zip(explicitTypeParams, typeArgs)):
                raise TypeException(loc, "type error in type arguments for constructor")
            implicitTypeParams = getImplicitTypeParameters(irClass)
            classTypeArgs = tuple([VariableType(tp) for tp in implicitTypeParams] + typeArgs)
            receiverType = ClassType(irClass, classTypeArgs, None)
            typeArgs = []
            nameInfo = nameInfo.getInfoForConstructors(self.info)
            receiverIsExplicit = True
            useKind = USE_AS_CONSTRUCTOR

        for overload in nameInfo.iterOverloads():
            self.ensureParamTypeInfoForDefn(overload.irDefn)
        (defnInfo, allTypeArgs, allArgTypes) = \
            nameInfo.findDefnInfoWithArgTypes(receiverType, receiverIsExplicit,
                                              typeArgs, argTypes, loc)
        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))

        self.scope().use(defnInfo, useAstId, useKind, loc)
        irDefn = defnInfo.irDefn
        self.ensureTypeInfoForDefn(irDefn)
        if isinstance(irDefn, Function):
            if irDefn.isConstructor():
                return receiverType
            else:
                assert len(allTypeArgs) == len(irDefn.typeParameters)
                ty = irDefn.returnType.substitute(irDefn.typeParameters, allTypeArgs)
                return ty
        elif isinstance(irDefn, Field):
            ty = irDefn.type
            if receiverIsExplicit and isinstance(receiverType, ClassType):
                fieldClass = self.findBaseClassForField(receiverType.clas, irDefn)
                ty = ty.substituteForInheritance(receiverType.clas, fieldClass)
                ty = ty.substitute(receiverType.clas.typeParameters, receiverType.typeArguments)
            return ty
        else:
            assert isinstance(irDefn, Variable) or \
                   isinstance(irDefn, Global)
            return irDefn.type

    def findBaseClassForField(self, receiverClass, field):
        # At this point, classes haven't been flattened yet, so we have to search up the
        # inheritance chain for the first class that contains the field.
        for clas in receiverClass.superclasses():
            if any(True for f in clas.fields if f is field):
                return clas
        assert False, "field is not defined in this class or any superclass"

    def isTypeParameterAvailable(self, irTypeParameter):
        if self.functionStack[-1] is None:
            return False
        irFunction = self.functionStack[-1].irDefn
        return any(irTypeParameter is param for param in irFunction.typeParameters)

    def isRecursiveFunctionWithoutType(self, irDefn):
        for state in self.functionStack[:-1]:
            if state is not None and state.irDefn is irDefn:
                return True
        return False

    def isAnalyzingFunction(self):
        return len(self.functionStack) > 0 and self.functionStack[-1] is not None


def astTypeFlagToIrTypeFlag(flag):
    if flag == "?":
        return NULLABLE_TYPE_FLAG
    else:
        raise NotImplementedError


class FunctionState(object):
    def __init__(self, irDefn):
        self.irDefn = irDefn
        self.declaredReturnType = None
        self.bodyReturnType = None

    def handleReturn(self, returnType):
        if self.declaredReturnType:
            if not returnType.isSubtypeOf(self.declaredReturnType):
                raise TypeException(self.irDefn.getLocation(), "incorrect return type")
            return self.declaredReturnType
        elif self.bodyReturnType is None:
            self.bodyReturnType = returnType
            return self.bodyReturnType
        else:
            # combine will raise if incompatible
            combinedType = self.bodyReturnType.combine(returnType, self.irDefn.getLocation())
            self.bodyReturnType = combinedType
            return self.bodyReturnType

    def getReturnType(self):
        if self.declaredReturnType:
            return self.declaredReturnType
        elif self.bodyReturnType:
            return self.bodyReturnType
        else:
            return ClassType.forReceiver(getNothingClass())

# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from functools import partial

import ast
from bytecode import W8, W16, W32, W64, BUILTIN_TYPE_CLASS_ID, BUILTIN_TYPE_CTOR_ID, instInfoByCode, BUILTIN_MATCH_EXCEPTION_CLASS_ID, BUILTIN_MATCH_EXCEPTION_CTOR_ID, BUILTIN_STRING_EQ_OP_ID
from ir import IrTopDefn, Class, Field, Function, Global, LOCAL, Package, Trait, Variable
from ir_types import Type, NoType, UnitType, BooleanType, I8Type, I16Type, I32Type, I64Type, F32Type, F64Type, ObjectType, ClassType, VariableType, ExistentialType, NULLABLE_TYPE_FLAG, getExceptionClassType, getClassFromType, getStringType, getRootClassType
import ir_instructions
from compile_info import CONTEXT_CONSTRUCTOR_HINT, CLOSURE_CONSTRUCTOR_HINT, PACKAGE_INITIALIZER_HINT, ARRAY_ELEMENT_GET_HINT, ARRAY_ELEMENT_SET_HINT, ARRAY_ELEMENT_LENGTH_HINT, DefnInfo, NORMAL_MODE, STD_MODE, NOSTD_MODE
from flags import ABSTRACT, STATIC, LET, ARRAY, NATIVE
from errors import SemanticException
from builtins import getTypeClass, getExceptionClass, getRootClass, getStringClass, getBuiltinFunctionById, getBuiltinClassById
import type_analysis
from utils import Counter, COMPILE_FOR_EFFECT, COMPILE_FOR_VALUE, COMPILE_FOR_UNINITIALIZED, COMPILE_FOR_MATCH, each
from name import (
    PACKAGE_INIT_NAME,
    RECEIVER_SUFFIX,
)


def compile(info):
    for clas in info.package.classes:
        assignFieldIndices(clas, info)
    init = info.package.addFunction(PACKAGE_INIT_NAME, returnType=UnitType,
                                    typeParameters=[], parameterTypes=[], variables=[],
                                    compileHint=PACKAGE_INITIALIZER_HINT)
    info.package.initFunction = init.id
    for function in info.package.functions:
        compiler = CompileVisitor(function, info)
        compiler.compile()


def assignFieldIndices(clas, info):
    for index, field in enumerate(clas.fields):
        assert field.index is None or field.index == index
        field.index = index


class TryState(object):
    def __init__(self, parent, ast, mode, tryStackHeight, finallyBlock):
        self.parent = parent
        self.ast = ast
        self.mode = mode
        self.tryStackHeight = tryStackHeight
        self.finallyBlock = finallyBlock
        self.returnFound = False
        self.isInCatch = False
        self.tryReturnFinallyBlock = None
        self.catchReturnFinallyBlock = None
        self.finallyReturnBlock = None

    def hasReturn(self):
        return self.returnFound

    def hasFinally(self):
        currentState = self
        while currentState is not None:
            if currentState.finallyBlock is not None:
                return True
            currentState = currentState.parent
        return False

    def moveToCatch(self):
        self.isInCatch = True

    def getReturnStackHeight(self):
        # Normally when we return, we want to drop the stack to where the try started. If the
        # return is inside a catch clause where there is also a finally clause, we implicitly
        # wrap the catch inside an additional try. So the stack height of that implicit try
        # is where we pop to.
        exceptionSlot = 1 if self.isInCatch and self.finallyBlock is not None else 0
        return self.tryStackHeight + exceptionSlot

    def getReturnFinallyBlock(self):
        return self.catchReturnFinallyBlock if self.isInCatch else self.tryReturnFinallyBlock

    def setReturnFinallyBlock(self, block):
        if self.isInCatch:
            self.catchReturnFinallyBlock = block
        else:
            self.tryReturnFinallyBlock = block
        currentState = self
        while currentState is not None:
            currentState.returnFound = True
            currentState = currentState.parent


class CompileVisitor(ast.NodeVisitor):
    def __init__(self, function, info):
        self.function = function
        self.astDefn = function.astDefn if hasattr(function, "astDefn") else None
        self.compileHint = function.compileHint if hasattr(function, "compileHint") else None
        assert self.astDefn is not None or self.compileHint is not None
        self.info = info
        self.blocks = []
        self.stackHeights = []
        self.nextBlockId = Counter()
        self.currentBlock = None
        self.currentStackHeight = None
        self.unreachable = False
        self.tryStateStack = []
        self.labels = []

        firstBlock = self.newBlock()
        self.setStackHeightForBlock(firstBlock, 0)
        self.setCurrentBlock(firstBlock)

    def compile(self):
        # Handle special implicit functions.
        if self.compileHint:
            self.compileWithHint()
            return

        # Get the body of the function as a list of statements. Also parameters.
        parameters, statements = self.getParametersAndStatements()
        if statements is None:
            return

        # Set ids (and therefore, fp-offsets) for each local variable.
        self.enumerateLocals()
        self.enumerateParameters(parameters)

        # If this is a constructor, some different handling is required. We need to unpack
        # parameters early, since they may be stored in fields or passed to superconstructors.
        # We also need to check for calls to superconstructors or alternate constructors.
        if self.function.isConstructor():
            parameters, statements = self.constructorPreamble(parameters, statements)

        # Compile those statements.
        mode = COMPILE_FOR_EFFECT if self.function.isConstructor() else COMPILE_FOR_VALUE
        self.compileStatements(self.getScopeId(), parameters, statements, mode)

        # Add a return if there was no explicit return.
        if self.currentBlock is not None:
            if mode is COMPILE_FOR_EFFECT:
                self.unit()
            self.ret()

        # Sort the blocks in reverse-post-order and remove any unreachable blocks.
        self.orderBlocks()
        self.function.blocks = self.blocks

    def compileWithHint(self):
        if self.compileHint is CONTEXT_CONSTRUCTOR_HINT:
            # Values in contexts are initialized after the context object is constructed, so
            # the context constructor doesn't need to do anything.
            self.unit()
            self.ret()
        elif self.compileHint is CLOSURE_CONSTRUCTOR_HINT:
            # Closures contain a bunch of contexts. These context parameters have the same order
            # as the corresponding fields, so we just need to load and store them.
            fields = self.function.definingClass.fields
            for i in xrange(len(fields)):
                paramIndex = i + 1   # skip receiver
                self.ldlocal(paramIndex)
                self.loadThis()
                self.storeField(fields[i])
            self.unit()
            self.ret()
        elif self.compileHint is PACKAGE_INITIALIZER_HINT:
            # This function initializes all the global variables.
            for module in self.info.ast.modules:
                for defn in module.definitions:
                    if isinstance(defn, ast.VariableDefinition):
                        self.visit(defn, COMPILE_FOR_EFFECT)
            self.unit()
            self.ret()
        elif self.compileHint is ARRAY_ELEMENT_GET_HINT:
            self.ldlocal(1)  # index
            self.loadThis()
            self.lde()
            self.ret()
        elif self.compileHint is ARRAY_ELEMENT_SET_HINT:
            self.ldlocal(2)  # value
            self.ldlocal(1)  # index
            self.loadThis()
            self.ste()
            self.unit()
            self.ret()
        else:
            assert self.compileHint is ARRAY_ELEMENT_LENGTH_HINT
            self.loadThis()
            clas = self.function.definingClass
            length = next(f for f in clas.fields if ARRAY in f.flags)
            self.loadField(length)
            self.ret()

        self.function.blocks = self.blocks

    def getParametersAndStatements(self):
        if isinstance(self.astDefn, ast.FunctionDefinition):
            if self.astDefn.body is None:
                assert ABSTRACT in self.function.flags or NATIVE in self.function.flags
                return None, None
            parameters = self.astDefn.parameters
            if isinstance(self.astDefn.body, ast.BlockExpression):
                statements = self.astDefn.body.statements
            else:
                statements = [self.astDefn.body]
        elif isinstance(self.astDefn, ast.ClassDefinition):
            parameters = []
            statements = self.astDefn.members
        else:
            assert isinstance(self.astDefn, ast.PrimaryConstructorDefinition)
            parameters = self.astDefn.parameters
            statements = []
        return parameters, statements

    def constructorPreamble(self, parameters, statements):
        assert self.function.isConstructor()

        # Unpack parameters. They may be stored in fields. They may be needed to call
        # superconstructors.
        self.unpackParameters(parameters)
        parameters = []

        # The first statement may be a `this` or `super` call.
        altCtorCalled = False
        superCtorCalled = False
        if len(statements) > 0 and \
           isinstance(statements[0], ast.CallExpression):
            if isinstance(statements[0].callee, ast.ThisExpression):
                self.visitCallThisExpression(statements[0], COMPILE_FOR_EFFECT)
                altCtorCalled = True
                superCtorCalled = True
                statements = statements[1:]
            elif isinstance(statements[0].callee, ast.SuperExpression):
                self.visitCallSuperExpression(statements[0], COMPILE_FOR_EFFECT)
                superCtorCalled = True
                statements = statements[1:]
        elif isinstance(self.astDefn, ast.ClassDefinition) and \
             self.info.hasUseInfo(self.astDefn):
            superCtorCalled = True
            arguments = self.astDefn.superArgs if self.astDefn.superArgs is not None else []
            self.buildCall(self.info.getUseInfo(self.astDefn),
                           self.info.getCallInfo(self.astDefn),
                           None, arguments, COMPILE_FOR_EFFECT, allowAllocation=False)
        elif isinstance(self.astDefn, ast.PrimaryConstructorDefinition):
            superCtorCalled = True
            astClassDefn = self.function.definingClass.astDefn
            arguments = astClassDefn.superArgs if astClassDefn.superArgs is not None else []
            self.buildCall(self.info.getUseInfo(astClassDefn),
                           self.info.getCallInfo(astClassDefn),
                           None, arguments, COMPILE_FOR_EFFECT, allowAllocation=False)

        # If no superconstructor was called, try to find a default superconstructor,
        # and call that.
        if not superCtorCalled:
            supertype = self.function.definingClass.supertypes[0]
            superclass = supertype.clas
            defaultSuperCtors = [ctor for ctor in superclass.constructors if
                                 len(ctor.parameterTypes) == 1]
            assert len(defaultSuperCtors) <= 1
            if len(defaultSuperCtors) == 0:
                raise SemanticException(self.function.definingClass.getLocation(),
                                        "no default constructor in superclass %s" %
                                        superclass.name)
            self.loadThis()
            self.buildStaticTypeArguments(supertype.typeArguments)
            self.callg(defaultSuperCtors[0])
            self.drop()

        # If no alternate constructor was called, call the initializer.
        if not altCtorCalled:
            irInitializer = self.function.definingClass.initializer
            if irInitializer is not None:
                self.loadThis()
                self.buildImplicitStaticTypeArguments(self.function.typeParameters)
                self.callg(irInitializer)
                self.drop()

        return parameters, statements

    def visitVariableDefinition(self, defn, mode):
        assert mode is COMPILE_FOR_EFFECT
        if defn.expression is None:
            if defn.keyword == "let":
                raise SemanticException(defn.location,
                                        "constant definition must have assignment")
            self.visit(defn.pattern, COMPILE_FOR_UNINITIALIZED)
        else:
            self.visit(defn.expression, COMPILE_FOR_VALUE)
            self.visit(defn.pattern, COMPILE_FOR_EFFECT, self.info.getType(defn.expression))

    def visitFunctionDefinition(self, defn, mode):
        assert mode is COMPILE_FOR_EFFECT
        pass

    def visitClassDefinition(self, defn, mode):
        assert mode is COMPILE_FOR_EFFECT
        pass

    def visitArrayElementsStatement(self, defn, mode):
        assert mode is COMPILE_FOR_EFFECT
        pass

    def visitParameter(self, param, id):
        self.unpackParameter(param, id)

    def visitConstructorParameter(self, param, id):
        self.unpackParameter(param, id)

    def visitVariablePattern(self, pat, mode, ty=None, failBlock=None):
        assert mode is COMPILE_FOR_UNINITIALIZED or ty is not None
        defnInfo = self.info.getDefnInfo(pat)
        if mode is COMPILE_FOR_UNINITIALIZED and isinstance(defnInfo.irDefn, Global):
            # Globals are automatically uninitialized.
            return

        mustMatch = type_analysis.patternMustMatch(pat, ty, self.info) if ty else None

        if mode is COMPILE_FOR_EFFECT:
            assert mustMatch
        elif mode is COMPILE_FOR_MATCH:
            if self.info.hasUseInfo(pat):
                assert failBlock is not None
                self.buildLoadOrNullaryCall(pat, mode)
                self.dupi(1)  # value
                patTy = self.info.getType(pat)
                self.buildEquals(patTy)
                successBlock = self.newBlock()
                self.branchif(successBlock.id, failBlock.id)
                self.setCurrentBlock(successBlock)
                self.drop()
                return

            if not mustMatch:
                assert failBlock is not None
                successBlock = self.newBlock()
                ty = self.info.getType(pat)
                self.buildType(self.info.getType(pat), pat.location)
                self.castcbr(successBlock.id, failBlock.id)
                self.setCurrentBlock(successBlock)
        elif mode is COMPILE_FOR_UNINITIALIZED:
            self.buildUninitialized(self.info.getType(pat))
        self.storeVariable(defnInfo, ty)

    def visitBlankPattern(self, pat, mode, ty=None, failBlock=None):
        if mode is COMPILE_FOR_UNINITIALIZED:
            return
        assert ty is not None

        patTy = None if pat.ty is None else self.info.getType(pat.ty)
        if patTy is None or ty.isSubtypeOf(patTy):
            self.drop()
        else:
            assert patTy.isSubtypeOf(ty)
            self.buildType(patTy, pat.ty.location)
            castBlock = self.newBlock()
            self.castcbr(castBlock.id, failBlock.id)
            self.setCurrentBlock(castBlock)
            self.drop()

    def visitLiteralPattern(self, pat, mode, ty=None, failBlock=None):
        if mode is COMPILE_FOR_UNINITIALIZED:
            return
        lit = pat.literal
        ty = self.info.getType(pat)
        if isinstance(lit, ast.StringLiteral):
            # String.== is an actual method, so the literal needs to be the receiver.
            self.buildLiteral(lit)
            self.dupi(1)  # value
            self.callg(getBuiltinFunctionById(BUILTIN_STRING_EQ_OP_ID))
        elif isinstance(lit, ast.NullLiteral):
            self.dup()
            self.null(),
            self.eqp()
        else:
            self.dup()  # value
            self.buildLiteral(lit)
            self.buildEquals(ty)
        successBlock = self.newBlock()
        self.branchif(successBlock.id, failBlock.id)
        self.setCurrentBlock(successBlock)
        self.drop()

    def visitTuplePattern(self, pat, mode, ty=None, failBlock=None):
        if mode is COMPILE_FOR_UNINITIALIZED:
            return

        # Check whether the object being matched is actually a tuple.
        tupleClass = self.info.getTupleClass(len(pat.patterns), pat.location)
        typeArgs = None
        if isinstance(ty, ClassType) and ty.clas is tupleClass:
            typeArgs = ty.typeArguments
        else:
            erasedTupleTypeArgs = tuple(VariableType(p) for p in tupleClass.typeParameters)
            erasedTupleType = ClassType(tupleClass, erasedTupleTypeArgs)
            self.buildType(erasedTupleType, pat.location)
            isTupleBlock = self.newBlock()
            self.castcbr(isTupleBlock.id, failBlock.id)
            self.setCurrentBlock(isTupleBlock)
            typeArgs = [getRootClassType()] * len(pat.patterns)

        # Find the last non-blank pattern. We don't need to duplicate the tuple for it.
        def isBlank(p, pty):
            return isinstance(p, ast.BlankPattern) and \
                   type_analysis.patternMustMatch(p, pty, self.info)

        lastMatchingIndex = None
        for i in xrange(len(pat.patterns) - 1, -1, -1):
            if not isBlank(pat.patterns[i], typeArgs[i]):
                lastMatchingIndex = i
                break

        # Check if all the patterns will match. If so, we don't need failure code.
        elementsMustMatch = all(type_analysis.patternMustMatch(p, pty, self.info)
                                for p, pty in zip(pat.patterns, typeArgs))
        elementFailBlock = None if elementsMustMatch else self.newBlock()

        # Recurse into each pattern. Don't bother with blank patterns without types.
        for i, (p, pty) in enumerate(zip(pat.patterns, typeArgs)):
            if not isBlank(p, pty):
                if i != lastMatchingIndex:
                    self.dup()
                self.loadField(tupleClass.fields[i])
                self.visit(p, mode, pty, elementFailBlock)

        # Clean up.
        if not elementsMustMatch:
            successState = self.saveCurrentBlock()
            self.setCurrentBlock(elementFailBlock)
            self.drop()
            self.branch(failBlock.id)
            self.restoreCurrentBlock(successState)

        # Consume tuple value on success if no patterns were matched.
        if lastMatchingIndex is None:
            self.drop()

    def visitValuePattern(self, pat, mode, ty=None, failBlock=None):
        if mode is COMPILE_FOR_UNINITIALIZED:
            return
        assert mode is COMPILE_FOR_MATCH

        def buildValue():
            self.buildPrefix(pat.prefix)
            self.buildLoadOrNullaryCall(pat, COMPILE_FOR_VALUE,
                                        receiverIsExplicit=(len(pat.prefix) > 0))

        if ty.isPrimitive():
            # The == operator for primitives is symmetric. Putting the matched value
            # first is a little more compact.
            self.dup()
            buildValue()
            self.buildEquals(ty)
        else:
            # The == operator for the matching value could be anything, so we want to make
            # sure to call the method on the value from this pattern.
            buildValue()
            self.dupi(1)
            self.buildEquals(ty)
        successBlock = self.newBlock()
        self.branchif(successBlock.id, failBlock.id)
        self.setCurrentBlock(successBlock)
        self.drop()

    def visitDestructurePattern(self, pat, mode, ty=None, failBlock=None):
        if mode is COMPILE_FOR_UNINITIALIZED:
            for subPat in pat.patterns:
                self.visit(subPat, COMPILE_FOR_UNINITIALIZED)
            return
        assert mode is COMPILE_FOR_MATCH

        irDefn = self.info.getUseInfo(pat).defnInfo.irDefn
        callInfo = self.info.getCallInfo(pat)

        # Compile the prefix. Note that the matcher may be explicitly referenced in the last
        # prefix component, so we don't compile that here.
        if not self.info.hasUseInfo(pat.prefix[-1]):
            self.buildPrefix(pat.prefix[:-1])
            hasPrefix = len(pat.prefix) > 1
        else:
            self.buildPrefix(pat.prefix)
            hasPrefix = len(pat.prefix) > 0

        # The rest is the same as other destructures.
        self.buildDestructure(irDefn, callInfo.receiverType, hasPrefix, callInfo.typeArguments,
                              pat.patterns, failBlock, pat.location)

    def visitUnaryPattern(self, pat, mode, ty=None, failBlock=None):
        if mode is COMPILE_FOR_UNINITIALIZED:
            self.visit(pat.pattern, COMPILE_FOR_UNINITIALIZED)
            return
        assert mode is COMPILE_FOR_MATCH

        if self.info.hasUseInfo(pat) and \
           not isinstance(self.info.getUseInfo(pat).defnInfo.irDefn, Class):
            self.buildLoadOrNullaryCall(pat, COMPILE_FOR_VALUE)

        irDefn = self.info.getUseInfo(pat.matcherId).defnInfo.irDefn
        callInfo = self.info.getCallInfo(pat.matcherId)
        self.buildDestructure(irDefn, callInfo.receiverType, False, callInfo.typeArguments,
                              [pat.pattern], failBlock, pat.location)

    def visitBinaryPattern(self, pat, mode, ty=None, failBlock=None):
        if mode is COMPILE_FOR_UNINITIALIZED:
            self.visit(pat.left, COMPILE_FOR_UNINITIALIZED)
            self.visit(pat.right, COMPILE_FOR_UNINITIALIZED)
            return
        assert mode is COMPILE_FOR_MATCH

        if self.info.hasUseInfo(pat) and \
           not isinstance(self.info.getUseInfo(pat).defnInfo.irDefn, Class):
            self.buildLoadOrNullaryCall(pat, COMPILE_FOR_VALUE)

        irDefn = self.info.getUseInfo(pat.matcherId).defnInfo.irDefn
        callInfo = self.info.getCallInfo(pat.matcherId)
        subPatterns = [pat.left, pat.right]
        self.buildDestructure(irDefn, callInfo.receiverType, False, callInfo.typeArguments,
                              subPatterns, failBlock, pat.location)

    def visitLiteralExpression(self, expr, mode):
        self.buildLiteral(expr.literal)
        self.dropForEffect(mode)

    def visitVariableExpression(self, expr, mode):
        self.buildLoadOrNullaryCall(expr, mode)

    def visitThisExpression(self, expr, mode):
        self.buildLoadOrNullaryCall(expr, mode)

    def visitSuperExpression(self, expr, mode):
        raise SemanticException(expr.location, "`super` is only valid as part of a call")

    def visitBlockExpression(self, expr, mode):
        scopeId = self.info.getScope(expr).scopeId
        self.compileStatements(scopeId, None, expr.statements, mode)

    def visitAssignExpression(self, expr, mode):
        ty = self.info.getType(expr.right)
        lvalue = self.compileLValue(expr.left, ty)
        self.visit(expr.right, COMPILE_FOR_VALUE)
        self.buildAssignment(lvalue, mode)

    def visitPropertyExpression(self, expr, mode):
        if not self.info.hasScopePrefixInfo(expr.receiver):
            self.visit(expr.receiver, COMPILE_FOR_VALUE)
        self.buildLoadOrNullaryCall(expr, mode, receiverIsExplicit=True)

    def visitCallExpression(self, expr, mode):
        if not isinstance(expr.callee, ast.VariableExpression) and \
           not isinstance(expr.callee, ast.PropertyExpression):
            raise SemanticException(expr.location, "uncallable expression")

        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr) if self.info.hasCallInfo(expr) else None
        if isinstance(expr.callee, ast.PropertyExpression) and \
           not self.info.hasScopePrefixInfo(expr.callee.receiver):
            receiver = expr.callee.receiver
        else:
            receiver = None
        self.buildCall(useInfo, callInfo, receiver, expr.arguments, mode)

    def visitCallThisExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr)
        self.buildCall(useInfo, callInfo, expr.callee, expr.arguments,
                       mode, allowAllocation=False)

    def visitCallSuperExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr)
        self.buildCall(useInfo, callInfo, expr.callee, expr.arguments,
                       mode, allowAllocation=False)

    def visitNewArrayExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr)

        # Allocate the array.
        self.visit(expr.length, COMPILE_FOR_VALUE)
        self.buildStaticTypeArguments(callInfo.typeArguments)
        irClass = self.info.getType(expr).clas
        if irClass.isForeign():
            self.allocarrf(irClass)
        else:
            self.allocarr(irClass)

        # Call the constructor normally without allowing allocation. This works similarly to
        # how constructors can call other constructors.
        self.buildCall(useInfo, callInfo, self.HAVE_RECEIVER, expr.arguments, mode, False)

    def visitUnaryExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr)
        self.buildCall(useInfo, callInfo, expr.expr, [], mode)

    def visitBinaryExpression(self, expr, mode):
        opName = expr.operator
        if opName in ["&&", "||"]:
            # short-circuit logic operators
            longBlock = self.newBlock()
            joinBlock = self.newBlock()
            self.visit(expr.left, COMPILE_FOR_VALUE)
            self.dup()
            if opName == "&&":
                self.branchif(longBlock.id, joinBlock.id)
            else:
                self.branchif(joinBlock.id, longBlock.id)
            self.setCurrentBlock(longBlock)
            self.drop()
            self.visit(expr.right, COMPILE_FOR_VALUE)
            self.branch(joinBlock.id)
            self.setCurrentBlock(joinBlock)
            self.dropForEffect(mode)
        else:
            # regular operators
            useInfo = self.info.getUseInfo(expr)
            callInfo = self.info.getCallInfo(expr)
            isCompoundAssignment = opName == useInfo.defnInfo.irDefn.name.short() + "="
            if isCompoundAssignment:
                ty = self.info.getType(expr.right)
                receiver = self.compileLValue(expr.left, ty)
            else:
                receiver = expr.left
            self.buildCall(useInfo, callInfo, receiver, [expr.right], mode)

    def visitTupleExpression(self, expr, mode):
        ty = self.info.getType(expr)
        typeArgs = map(self.info.getType, expr.expressions)
        tupleClass = ty.clas
        langMode = self.info.languageMode()
        assert langMode is not NOSTD_MODE

        # Allocate the tuple.
        self.buildStaticTypeArguments(typeArgs)
        if langMode is NORMAL_MODE:
            self.allocobjf(tupleClass)
        else:
            assert langMode is STD_MODE
            self.allocobj(tupleClass)
        if mode is COMPILE_FOR_VALUE:
            self.dup()

        # Compile sub-expressions and type arguments.
        for subexpr in expr.expressions:
            self.visit(subexpr, mode)
        for subexpr in expr.expressions:
            argType = self.info.getType(subexpr)
            self.buildStaticTypeArgument(argType)

        # Construct the tuple.
        ctor = tupleClass.constructors[0]
        assert len(ctor.parameterTypes) == 1 + len(expr.expressions)
        self.callFunction(ctor)
        self.drop()

    def visitIfExpression(self, expr, mode):
        self.visit(expr.condition, COMPILE_FOR_VALUE)
        trueBlock = self.newBlock()
        if expr.falseExpr is None:
            joinBlock = self.newBlock()
            self.branchif(trueBlock.id, joinBlock.id)
            self.setCurrentBlock(trueBlock)
            with UnreachableScope(self):
                self.visit(expr.trueExpr, COMPILE_FOR_EFFECT)
                self.branch(joinBlock.id)
            self.setCurrentBlock(joinBlock)
            if mode is COMPILE_FOR_VALUE:
                self.unit()
        else:
            falseBlock = self.newBlock()
            joinBlock = self.newBlock()
            self.branchif(trueBlock.id, falseBlock.id)
            self.setCurrentBlock(trueBlock)
            with UnreachableScope(self):
                self.visit(expr.trueExpr, mode)
                self.branch(joinBlock.id)
                trueUnreachable = self.unreachable
            self.setCurrentBlock(falseBlock)
            with UnreachableScope(self):
                self.visit(expr.falseExpr, mode)
                self.branch(joinBlock.id)
                falseUnreachable = self.unreachable
            if trueUnreachable and falseUnreachable:
                self.setUnreachable()
            self.setCurrentBlock(joinBlock)

    def visitWhileExpression(self, expr, mode):
        condBlock = self.newBlock()
        self.branch(condBlock.id)
        self.setCurrentBlock(condBlock)
        self.visit(expr.condition, COMPILE_FOR_VALUE)
        bodyBlock = self.newBlock()
        endBlock = self.newBlock()
        self.branchif(bodyBlock.id, endBlock.id)
        self.setCurrentBlock(bodyBlock)
        with UnreachableScope(self):
            self.visit(expr.body, COMPILE_FOR_EFFECT)
            self.branch(condBlock.id)
        self.setCurrentBlock(endBlock)
        if mode is COMPILE_FOR_VALUE:
            self.unit()

    def visitMatchExpression(self, expr, mode):
        self.visit(expr.expression, COMPILE_FOR_VALUE)
        exprTy = self.info.getType(expr.expression)

        doneBlock = self.newBlock()
        shouldHandleMismatch = not type_analysis.partialFunctionMustMatch(expr.matcher,
                                                                          exprTy, self.info)
        if shouldHandleMismatch:
            failBlock = self.newBlock()
            # we generate the failure code before compiling the partial function, since we may
            # be unreachable if all the cases terminate.
            self.setStackHeightForBlock(failBlock, self.currentStackHeight)
            blockState = self.saveCurrentBlock()
            self.setCurrentBlock(failBlock)
            self.buildMatchException()
            self.throw()
            self.restoreCurrentBlock(blockState)
        else:
            failBlock = None
        self.visitPartialFunctionExpression(expr.matcher, mode, exprTy, doneBlock, failBlock)
        self.setCurrentBlock(doneBlock)

    def visitThrowExpression(self, expr, mode):
        self.visit(expr.exception, COMPILE_FOR_VALUE)
        self.throw()
        self.setUnreachable()

    def visitTryCatchExpression(self, expr, mode):
        if self.unreachable:
            return

        # Stack discipline:
        # - before: nothing on the stack before we enter the try-catch-finally expression will
        #     be accessed or modified.
        # - try: when entering the try clause, we execute pushtry, which pushes an entry on
        #     the exception handler stack, which is separate from the value stack. The try
        #     expression is expected to push one value if mode is COMPILE_FOR_VALUE. Otherwise,
        #     the stack should be the same height.
        # - catch: the catch clause should not be reachable; it should only be mentioned by
        #     the poptry instruction used to enter the try clause. When entering, there will
        #     be one additional value on the stack: the exception. If there is also a finally
        #     clause, the content of the catch clause will be wrapped in another
        #     pushtry/poptry pair to catch any thrown or rethrown exceptions. If the catch
        #     clause finishes normally and there's no finally clause and the mode is
        #     COMPILE_FOR_VALUE, there should be one additional value on the stack, the result.
        #     If the mode is COMPILE_FOR_EFFECT, the stack should be at the starting height.
        #     If there is a finally clause, see below for additional values.
        # - finally: if there is a finally clause, it must be executed regardless of whether
        #    evaluation completes normally, an exception was thrown, or a return was executed.
        #    The finally clause expects four additional values on stack: the result of
        #    evaluation (may be uninitialized or zero of the correct type), the exception if
        #    there was one (uninitialized if not), a return value (may be uninitialized or
        #    zero), and a continuation label that the finally clause will branch to after
        #    evaluation. Note that finally clause may need to branch to an outer finally
        #    clause before branching to the continuation. If a finally clause throws or returns,
        #    that replaces the thrown exception or return value and the continuation on stack.

        haveCatch = expr.catchHandler is not None
        haveFinally = expr.finallyHandler is not None
        haveBoth = haveCatch and haveFinally
        exnTy = getExceptionClassType()
        mustMatch = haveCatch and \
            type_analysis.partialFunctionMustMatch(expr.catchHandler, exnTy, self.info)

        initialStackHeight = self.currentStackHeight

        # Create blocks. Some blocks may not be needed, depending on which handlers we have.
        tryBlock = self.newBlock()
        tryCatchBlock = self.newBlock() if haveBoth else None
        catchBlock = self.newBlock() if haveCatch else None
        catchNormalBlock = self.newBlock() if haveBoth else None
        catchMissBlock = self.newBlock() if haveBoth and not mustMatch else None
        catchThrowBlock = self.newBlock() if haveCatch and not haveFinally else None
        catchThrowFinallyBlock = self.newBlock() if haveBoth else None
        catchNormalFinallyBlock = self.newBlock() if haveBoth else None
        tryFinallyBlock = self.newBlock() if haveFinally else None
        throwFinallyBlock = self.newBlock() if haveFinally and not haveCatch else None
        finallyBlock = self.newBlock() if haveFinally else None
        finallyRethrowBlock = self.newBlock() if haveFinally else None
        doneBlock = self.newBlock()

        # Add some aliases for try and catch outcomes to simplify the code here.
        if haveBoth:
            tryNormalBlock = tryFinallyBlock
            exceptionBlock = tryCatchBlock
            catchSuccessBlock = catchNormalBlock
            catchFailBlock = catchMissBlock
        elif haveCatch:
            tryNormalBlock = doneBlock
            exceptionBlock = catchBlock
            catchSuccessBlock = doneBlock
            catchFailBlock = catchThrowBlock
        else:
            assert haveFinally
            tryNormalBlock = tryFinallyBlock
            exceptionBlock = throwFinallyBlock
            catchSuccessBlock = None
            catchFailBlock = None

        # Set up try state.
        finallyStackHeight = self.currentStackHeight + 3
        parentTryState = self.tryStateStack[-1] if len(self.tryStateStack) > 0 else None
        tryState = TryState(parentTryState, expr, mode, self.currentStackHeight, finallyBlock)
        self.tryStateStack.append(tryState)

        # Enter the try expression.
        # Stack at this point: [...]
        self.pushtry(tryBlock.id, exceptionBlock.id)

        # Compile the try expression.
        # Stack at this point: [| ...]
        self.setCurrentBlock(tryBlock)
        with UnreachableScope(self):
            self.visit(expr.expression, mode)
            self.poptry(tryNormalBlock.id)
            unreachableAfterTry = self.unreachable

        # If we have both catch and finally, we need to make sure we still execute the finally
        # if the catch throws an exception. So we wrap the catch in another try.
        # Stack at this point: [exception ...]
        if tryCatchBlock is not None:
            self.setCurrentBlock(tryCatchBlock)
            self.pushtry(catchBlock.id, catchThrowFinallyBlock.id)

        # If we have a catch handler, we compile it like a normal pattern match.
        # Stack at this point: [| exception ...]
        if catchBlock is not None:
            tryState.moveToCatch()
            self.setCurrentBlock(catchBlock)
            if haveFinally:
                # We're inside another try, so we can't consume the exception.
                self.dup()
            with UnreachableScope(self):
                self.visitPartialFunctionExpression(expr.catchHandler, mode,
                                                       exnTy, catchSuccessBlock, catchFailBlock)
                unreachableAfterCatch = self.unreachable
            assert self.isDetached()
        else:
            unreachableAfterCatch = True

        # If we have both catch and finally, once we handle an exception, we need to break out
        # of the try wrapped around the catch handler.
        # Stack at this point: [result | exception ...]
        if catchNormalBlock is not None and not unreachableAfterCatch:
            self.setCurrentBlock(catchNormalBlock)
            self.poptry(catchNormalFinallyBlock.id)

        # If we have both catch and finally, if we can't handle an exception, we need to break
        # out of the try wrapped around the catch handler.
        # Stack at this point: [exception | exception ...]
        if catchMissBlock is not None:
            self.setCurrentBlock(catchMissBlock)
            self.poptry(catchThrowFinallyBlock.id)

        # If we could not handle an exception, or if a new exception was thrown while we were
        # handling an exception, we need to rethrow it here.
        # Stack at this point: [exception ...]
        if catchThrowBlock is not None:
            self.setStackHeightForBlock(catchThrowBlock, initialStackHeight + 1)
            self.setCurrentBlock(catchThrowBlock)
            self.throw()

        # If we have both catch and finally, we need to handle exceptions thrown by the catch
        # handler. We also come here if an exception couldn't be handled. The exception on
        # top replaces the other exception.
        # Stack at this point: [exception exception ...]
        if catchThrowFinallyBlock is not None:
            self.setStackHeightForBlock(catchThrowFinallyBlock, initialStackHeight + 2)
            self.setCurrentBlock(catchThrowFinallyBlock)
            self.swap()
            self.drop()
            if mode is COMPILE_FOR_VALUE:
                self.buildUninitialized(self.info.getType(expr))
                self.swap()
            if tryState.hasReturn():
                self.buildUninitialized(self.function.returnType)
            self.label(finallyRethrowBlock.id)
            self.branch(finallyBlock.id)

        # If we have both catch and finally, after we've handled the exception and broken out
        # of the try, we need to adjust the stack for the finally and tell it to continue
        # normally once it's done.
        # Stack at this point: [result exception ...]
        if catchNormalFinallyBlock is not None and not unreachableAfterCatch:
            self.setCurrentBlock(catchNormalFinallyBlock)
            if mode is COMPILE_FOR_VALUE:
                self.swap()
            self.drop()
            self.buildUninitialized(getExceptionClassType())
            if tryState.hasReturn():
                self.buildUninitialized(self.function.returnType)
            self.label(doneBlock.id)
            self.branch(finallyBlock.id)

        # If we have finally, after the try expression completes normally, adjust the stack
        # for the finally and tell it to continue normally once it's done.
        # Stack at this point: [result ...]
        if tryFinallyBlock is not None:
            self.setStackHeightForBlock(tryFinallyBlock,
                                        initialStackHeight + \
                                            (1 if mode is COMPILE_FOR_VALUE else 0))
            self.setCurrentBlock(tryFinallyBlock)
            self.buildUninitialized(getExceptionClassType())
            if tryState.hasReturn():
                self.buildUninitialized(self.function.returnType)
            self.label(doneBlock.id)
            self.branch(finallyBlock.id)

        # If we have finally but not catch, after the try expression throws an exception,
        # adjust the stack for the finally and tell it to rethrow once it's done.
        # Stack at this point: [exception ...]
        if throwFinallyBlock is not None:
            self.setStackHeightForBlock(throwFinallyBlock, initialStackHeight + 1)
            self.setCurrentBlock(throwFinallyBlock)
            if mode is COMPILE_FOR_VALUE:
                self.buildUninitialized(self.info.getType(expr))
                self.swap()
            if tryState.hasReturn():
                self.buildUninitialized(self.function.returnType)
            self.label(finallyRethrowBlock.id)
            self.branch(finallyBlock.id)

        # If there was a return expression in the try clause or the catch clause and there is
        # a finally clause in this expression or any above it, we must ensure the finally
        # clauses are executed. The return expressions branch into
        # tryState.{try,catch}ReturnFinallyBlock. We need to generate those and chain them.
        if tryState.hasReturn() and tryState.hasFinally():
            # Generate blocks to chain finally clauses together, if needed. This might have
            # been partially done already. We work from the top down.
            previousTryState = None
            for currentTryState in self.tryStateStack:
                if currentTryState.finallyBlock is None or \
                   currentTryState.finallyReturnBlock is not None:
                    continue
                currentTryState.finallyReturnBlock = self.newBlock()
                self.setStackHeightForBlock(
                    currentTryState.finallyReturnBlock,
                    initialStackHeight + (3 if mode is COMPILE_FOR_VALUE else 2))
                self.setCurrentBlock(currentTryState.finallyReturnBlock)
                if previousTryState is None:
                    self.ret()
                else:
                    if mode is COMPILE_FOR_VALUE and \
                       previousTryState.mode is COMPILE_FOR_EFFECT:
                        self.swap2()  # swap return and value
                        self.drop()   # drop value
                        self.swap()   # swap return and exception
                    elif mode is COMPILE_FOR_EFFECT and \
                         previousTryState.mode is COMPILE_FOR_VALUE:
                        self.buildUninitialized(self.info.getType(previousTryState.ast)) # value
                        self.swap2()  # swap value and exception
                        self.swap()   # swap exception and return
                    elif mode is COMPILE_FOR_VALUE and \
                         previousTryState.mode is COMPILE_FOR_VALUE:
                        exprType = self.info.getType(expr)
                        previousType = self.info.getType(previousTryState.ast)
                        if exprType.uninitializedType() != previousType.uninitializedType():
                            self.swap2()  # swap return and value
                            self.drop()   # drop value
                            self.buildUninitialized(previousType)
                            self.swap2()  # swap value and return
                    else:
                        assert mode is previousTryState.mode
                    self.label(previousTryState.finallyReturnBlock.id)
                    self.branch(previousTryState.finallyBlock.id)
                previousTryState = currentTryState

            # Generate code to get from return to a finally. We need different code for returns
            # inside try and catch expressions, since the catch has an exception on stack which
            # isn't there with try. We assume that the return branches to these blocks with
            # a poptry instruction.
            lastTryStateWithFinally = next(tryState
                                           for tryState in reversed(self.tryStateStack)
                                           if tryState.finallyBlock is not None)
            if tryState.tryReturnFinallyBlock is not None:
                # Stack at this point: [return ...]
                self.setCurrentBlock(tryState.tryReturnFinallyBlock)
                if mode is COMPILE_FOR_VALUE:
                    self.buildUninitialized(self.info.getType(lastTryStateWithFinally.ast))
                    self.swap()
                self.buildUninitialized(getExceptionClassType())
                self.swap()
                self.label(lastTryStateWithFinally.finallyReturnBlock.id)
                self.branch(lastTryStateWithFinally.finallyBlock.id)

            if tryState.catchReturnFinallyBlock is not None:
                # Stack at this point: [return exception ...]
                self.setCurrentBlock(tryState.catchReturnFinallyBlock)
                if finallyBlock is not None:
                    # Drop the exception if it wasn't consumed.
                    self.swap()
                    self.drop()
                if mode is COMPILE_FOR_VALUE:
                    self.buildUninitialized(self.info.getType(lastTryStateWithFinally.ast))
                    self.swap()
                self.buildUninitialized(getExceptionClassType())
                self.swap()
                self.label(lastTryStateWithFinally.finallyReturnBlock.id)
                self.branch(lastTryStateWithFinally.finallyBlock.id)

        # Pop tryStateStack. This prevents return expressions inside the finally clause from
        # branching to the top of the finally clause, causing infinite loops.
        self.tryStateStack.pop()

        # If we have finally, we execute the finally handler, then branch to the continuation.
        # We may proceed normally (possibly with a result), rethrow an exception, return
        # from the function, or chain to a higher finally handler (in case of return). The
        # continuation tells us where to go, and everything we need is on the stack.
        # Stack at this point: [continuation return exception value ...]
        if finallyBlock is not None:
            finallyStackHeight = initialStackHeight + 2 + \
                                 (1 if mode is COMPILE_FOR_VALUE else 0) + \
                                 (1 if tryState.hasReturn() else 0)
            self.setStackHeightForBlock(finallyBlock, finallyStackHeight)
            self.setCurrentBlock(finallyBlock)
            self.visit(expr.finallyHandler, COMPILE_FOR_EFFECT)
            finallyBlockIds = [finallyRethrowBlock.id]
            if not (unreachableAfterTry and unreachableAfterCatch):
                finallyBlockIds.append(doneBlock.id)
            if tryState.hasReturn():
                finallyBlockIds.append(tryState.finallyReturnBlock.id)
            self.branchl(*finallyBlockIds)
            unreachableAfterFinally = self.unreachable
        else:
            unreachableAfterFinally = False

        # If we have finally, and an exception was thrown, we rethrow the exception after the
        # finally. This is one of the possible continuations.
        # Stack at this point: [return exception value ...]
        if finallyRethrowBlock is not None:
            self.setCurrentBlock(finallyRethrowBlock)
            if tryState.hasReturn():
                self.drop()
            self.throw()

        # If we were unreachable after both try and catch or if we were unreachable after
        # finally, we cannot continue normally.
        if unreachableAfterFinally or (unreachableAfterTry and unreachableAfterCatch):
            self.setUnreachable()

        # Everything has been handled. If we came through a finally, we need to pop the extra
        # values off the stack.
        # Stack at this point: [return exception result ...]
        self.setCurrentBlock(doneBlock)
        if haveFinally:
            if tryState.hasReturn():
                self.dropi(2)
            else:
                self.drop()

    def visitPartialFunctionExpression(self, expr, mode, ty, doneBlock, failBlock):
        allCasesTerminate = True
        mustMatchCaseTerminates = False

        for case in expr.cases[:-1]:
            nextBlock = self.newBlock()
            with UnreachableScope(self):
                self.visitPartialFunctionCase(case, mode, ty, doneBlock, nextBlock)
                caseTerminates = self.unreachable
                mustMatchCaseTerminates |= \
                    type_analysis.partialFunctionCaseMustMatch(case, ty, self.info)
                allCasesTerminate &= caseTerminates
            if mustMatchCaseTerminates:
                self.setUnreachable()
            self.setCurrentBlock(nextBlock)
        with UnreachableScope(self):
            self.visitPartialFunctionCase(expr.cases[-1], mode, ty, doneBlock, failBlock)
            allCasesTerminate &= self.unreachable
        if allCasesTerminate or mustMatchCaseTerminates:
            self.setUnreachable()
        assert self.isDetached()

    def visitPartialFunctionCase(self, expr, mode, ty, doneBlock, failBlock):
        if expr.condition is None:
            self.visit(expr.pattern, COMPILE_FOR_MATCH, ty, failBlock)
        else:
            # If there's a condition, duplicate the value before matching the pattern. The
            # pattern will consume the value on a successful match, but we need to preserve
            # it if the condition is false.
            assert self.unreachable or failBlock is not None
            failBlockId = failBlock.id if failBlock is not None else -1
            self.dup()
            mustMatch = type_analysis.patternMustMatch(expr.pattern, ty, self.info)
            if mustMatch:
                self.visit(expr.pattern, COMPILE_FOR_MATCH, ty, None)
            else:
                dropBlock = self.newBlock()
                self.visit(expr.pattern, COMPILE_FOR_MATCH, ty, dropBlock)
            self.visit(expr.condition, COMPILE_FOR_VALUE)
            successBlock = self.newBlock()
            self.branchif(successBlock.id, failBlockId)
            if not mustMatch:
                self.setCurrentBlock(dropBlock)
                self.drop()
                self.branch(failBlockId)
            self.setCurrentBlock(successBlock)
            self.drop()
        self.visit(expr.expression, mode)
        self.branch(doneBlock.id)
        self.detach()

    def visitReturnExpression(self, expr, mode):
        tryState = self.tryStateStack[-1] if len(self.tryStateStack) > 0 else None
        hasFinally = tryState is not None and tryState.hasFinally()
        if hasFinally:
            # We need to execute the finally expression before returning. Before evaluating
            # the return expression, we drop values on the stack to match the height of the
            # finally expression.
            stackDelta = self.currentStackHeight - tryState.getReturnStackHeight()
            assert stackDelta >= 0
            if stackDelta == 1:
                self.drop()
            elif stackDelta > 1:
                self.dropi(stackDelta)

        # Evaluate the expression being returned.
        if expr.expression is None:
            self.unit()
        else:
            self.visit(expr.expression, COMPILE_FOR_VALUE)

        if hasFinally:
            returnFinallyBlock = tryState.getReturnFinallyBlock()
            if returnFinallyBlock is None and not self.unreachable:
                # This block will be filled in when the finally clause is compiled.
                returnFinallyBlock = self.newBlock()
                tryState.setReturnFinallyBlock(returnFinallyBlock)
            self.poptry(returnFinallyBlock.id)
        else:
            self.ret()
        self.setUnreachable()

    def dropForEffect(self, mode):
        if mode is COMPILE_FOR_EFFECT:
            self.drop()

    def add(self, inst):
        if self.unreachable:
            return
        self.currentBlock.instructions.append(inst)
        self.currentStackHeight += inst.stackDelta()
        assert self.currentStackHeight >= 0
        if inst.isTerminator():
            for succId in inst.successorIds():
                if self.stackHeights[succId] is None:
                    self.stackHeights[succId] = self.currentStackHeight
                else:
                    assert self.stackHeights[succId] == self.currentStackHeight
            if isinstance(inst, ir_instructions.pushtry):
                # exception is implicitly pushed on stack
                self.stackHeights[inst.successorIds()[1]] += 1

    def newBlock(self):
        if self.unreachable:
            return ir_instructions.BasicBlock(-1, [])
        block = ir_instructions.BasicBlock(self.nextBlockId(), [])
        self.blocks.append(block)
        self.stackHeights.append(None)
        return block

    def setCurrentBlock(self, block):
        if self.unreachable:
            return
        self.currentBlock = block
        assert self.stackHeights[block.id] is not None
        self.currentStackHeight = self.stackHeights[block.id]

    def saveCurrentBlock(self):
        return self.currentBlock, self.currentStackHeight

    def restoreCurrentBlock(self, state):
        if self.unreachable:
            return
        self.currentBlock = state[0]
        self.currentStackHeight = state[1]

    def setStackHeightForBlock(self, block, stackHeight):
        assert self.stackHeights[block.id] is None or self.stackHeights[block.id] == stackHeight
        self.stackHeights[block.id] = stackHeight

    def setUnreachable(self):
        self.unreachable = True
        self.currentBlock = None
        self.currentStackHeight = None

    def detach(self):
        self.currentBlock = None
        self.currentStackHeight = None

    def isDetached(self):
        return self.currentBlock is None

    def compileLValue(self, expr, ty):
        useInfo = self.info.getUseInfo(expr)
        irDefn = useInfo.defnInfo.irDefn
        if LET in irDefn.flags:
            raise SemanticException(expr.location, "left side of assignment is constant")
        if (isinstance(expr, ast.VariableExpression) and \
            (isinstance(irDefn, Variable) or \
             isinstance(irDefn, Field) or \
             isinstance(irDefn, Global))) or \
           (isinstance(expr, ast.PropertyExpression) and \
            isinstance(irDefn, Global) and irDefn.isForeign()):
            return VarLValue(expr, self, ty, useInfo)
        elif isinstance(expr, ast.PropertyExpression) and isinstance(irDefn, Field):
            self.visit(expr.receiver, COMPILE_FOR_VALUE)
            return PropertyLValue(expr, self, useInfo)
        else:
            raise SemanticException(expr.location, "left side of assignment is unassignable")

    def enumerateLocals(self):
        nextLocalIndex = Counter(-1, -1)
        for var in self.function.variables:
            if var.kind is LOCAL:
                var.index = nextLocalIndex()

    def enumerateParameters(self, parameters):
        if self.function.isMethod():
            if self.function.variables[0].name.short() == RECEIVER_SUFFIX:
                # Receiver may be captured, so don't assume it's a regular variable.
                self.function.variables[0].index = 0
            implicitParamCount = 1
        else:
            implicitParamCount = 0
        if parameters is not None:
            for index, param in enumerate(parameters):
                if isinstance(param.pattern, ast.VariablePattern):
                    defnInfo = self.info.getDefnInfo(param.pattern)
                    if isinstance(defnInfo.irDefn, Variable):
                       defnInfo.irDefn.index = index + implicitParamCount

    def unpackParameters(self, parameters):
        implicitParameterCount = 1 if self.function.isMethod() else 0
        for index, param in enumerate(parameters):
            self.unpackParameter(param, index + implicitParameterCount)

    def unpackParameter(self, param, index):
        paramType = self.info.getType(param)
        if isinstance(param.pattern, ast.VariablePattern):
            defnInfo = self.info.getDefnInfo(param.pattern)
            if isinstance(defnInfo.irDefn, Variable):
                defnInfo.irDefn.index = index
            else:
                self.ldlocal(index)
                self.storeVariable(defnInfo, paramType)
        elif isinstance(param.pattern, ast.BlankPattern):
            pass
        else:
            self.ldlocal(index)
            self.visit(param.pattern, COMPILE_FOR_EFFECT, paramType)

    def compileStatements(self, scopeId, parameters, statements, mode):
        # Create a context if needed.
        if self.isContextNeeded(scopeId):
            contextInfo = self.info.getContextInfo(scopeId)
            if contextInfo.irContextClass is not None:
                self.createContext(contextInfo)

        # Unpack parameters, if we have them.
        if parameters is not None:
            self.unpackParameters(parameters)

        # Handle any non-variable definitions.
        self.buildDeclarations(statements)

        # Compile all statements but the last one. The values produced by these statements
        # are ignored.
        for stmt in statements[:-1]:
            self.visit(stmt, COMPILE_FOR_EFFECT)

        # Compile the last statement. If this is an expression, the result is the result of the
        # whole block. Otherwise, we need to push the unit value.
        if len(statements) > 0:
            if isinstance(statements[-1], ast.Expression):
                self.visit(statements[-1], mode)
                needUnit = False
            else:
                self.visit(statements[-1], COMPILE_FOR_EFFECT)
                needUnit = True
        else:
            needUnit = True
        if mode is COMPILE_FOR_VALUE and needUnit:
            self.unit()

    def buildPrefix(self, prefix):
        # Locate the end of the prefix. None of the stuff before this is compiled.
        prefixIndex = len(prefix) - 1
        while prefixIndex >= 0:
            if self.info.hasScopePrefixInfo(prefix[prefixIndex]):
                break
            prefixIndex -= 1

        # Generate instructions for each component.
        index = prefixIndex + 1
        while index < len(prefix):
            self.buildLoadOrNullaryCall(prefix[index], COMPILE_FOR_VALUE,
                                        receiverIsExplicit=(index > 0))
            index += 1

    def buildLoadOrNullaryCall(self, node, mode, receiverIsExplicit=False):
        useInfo = self.info.getUseInfo(node)
        defnInfo = useInfo.defnInfo
        irDefn = defnInfo.irDefn
        if isinstance(irDefn, Variable):
            self.ldlocal(irDefn.index)
        elif isinstance(irDefn, Global):
            self.loadGlobal(irDefn)
        elif isinstance(irDefn, Field):
            if not receiverIsExplicit:
                self.loadContext(defnInfo.scopeId)
            self.loadField(irDefn)
        elif isinstance(irDefn, Function):
            callInfo = self.info.getCallInfo(node)
            if irDefn.isMethod():
                if not receiverIsExplicit:
                    self.loadImplicitReceiver(irDefn)
                self.buildStaticTypeArguments(callInfo.typeArguments)
                self.callMethod(irDefn, callInfo.receiverType)
            else:
                assert not receiverIsExplicit
                self.buildStaticTypeArguments(callInfo.typeArguments)
                self.callFunction(irDefn)
        else:
            assert isinstance(irDefn, Package)
            self.pkg(irDefn)
        self.dropForEffect(mode)

    def loadVariable(self, varOrDefnInfo):
        if isinstance(varOrDefnInfo, Variable):
            var = varOrDefnInfo
            self.ldlocal(var.index)
        else:
            assert isinstance(varOrDefnInfo, DefnInfo)
            defnInfo = varOrDefnInfo
            irDefn = defnInfo.irDefn
            if isinstance(irDefn, Variable):
                self.loadVariable(irDefn)
            elif isinstance(irDefn, Field):
                self.loadContext(defnInfo.scopeId)
                self.loadField(irDefn)
            else:
                assert isinstance(irDefn, Global)
                self.loadGlobal(irDefn)

    def loadGlobal(self, globl):
        if globl.isForeign():
            self.ldgf(globl)
        else:
            self.ldg(globl)

    def storeVariable(self, varOrDefnInfo, valueType=None):
        if isinstance(varOrDefnInfo, Variable):
            var = varOrDefnInfo
            if valueType is not None and valueType != var.type:
                self.buildCast(var.type)
            self.stlocal(var.index)
        else:
            assert isinstance(varOrDefnInfo, DefnInfo)
            defnInfo = varOrDefnInfo
            irDefn = defnInfo.irDefn
            if isinstance(irDefn, Variable):
                self.storeVariable(irDefn, valueType)
            elif isinstance(irDefn, Field):
                self.loadContext(defnInfo.scopeId)
                self.storeField(irDefn)
            else:
                assert isinstance(irDefn, Global)
                self.storeGlobal(irDefn)

    def storeGlobal(self, globl):
        if globl.isForeign():
            self.stgf(globl)
        else:
            self.stg(globl)

    def loadContext(self, scopeId):
        closureInfo = self.info.getClosureInfo(self.getScopeId())
        loc = closureInfo.irClosureContexts[scopeId]
        if isinstance(loc, Variable):
            self.ldlocal(loc.index)
        elif isinstance(loc, Field):
            self.loadThis()
            self.loadField(loc)
        else:
            assert loc is None
            self.loadThis()

    def loadField(self, field):
        if field.definingClass is None:
            import pdb; pdb.set_trace()
        if field.definingClass.isForeign():
            self.ldff(field)
        else:
            self.ldf(field)

    def storeField(self, field):
        if field.definingClass.isForeign():
            self.stff(field)
        else:
            self.stf(field)

    def loadThis(self):
        assert self.function.isMethod()
        self.ldlocal(0)

    def loadImplicitReceiver(self, method):
        if self.info.hasClosureInfo(method):
            closureVar = self.info.getClosureInfo(method).irClosureVar  # may be None
        else:
            closureVar = None
        if closureVar is not None:
            self.loadVariable(closureVar)
        else:
            self.loadContext(self.info.getDefnInfo(method).scopeId)

    def isContextNeeded(self, scopeId):
        return scopeId is not None and \
               not (self.getScopeId() == scopeId and
                    (isinstance(self.astDefn, ast.ClassDefinition) or
                     isinstance(self.astDefn, ast.PrimaryConstructorDefinition))) and \
               self.info.hasContextInfo(scopeId)

    def createContext(self, contextInfo):
        contextClass = contextInfo.irContextClass
        assert not contextClass.isForeign()
        contextId = contextInfo.id
        assert len(contextClass.constructors) == 1
        contextCtor = contextClass.constructors[0]
        assert contextClass.typeParameters == contextCtor.typeParameters
        self.buildImplicitStaticTypeArguments(contextClass.typeParameters)
        self.allocobj(contextClass)
        self.dup()
        self.buildImplicitStaticTypeArguments(contextCtor.typeParameters)
        self.callg(contextCtor)
        self.drop()
        irContextVar = self.info.getClosureInfo(contextId).irClosureContexts[contextId]
        self.storeVariable(irContextVar)

    def buildDeclarations(self, statements):
        # Handle any non-variable definitions
        for stmt in statements:
            if isinstance(stmt, ast.FunctionDefinition):
                closureInfo = self.info.getClosureInfo(stmt)
                closureClass = closureInfo.irClosureClass
                if closureClass is None or \
                   closureClass is self.info.getDefnInfo(self.astDefn).irDefn:
                    continue
                assert not closureClass.isForeign() and len(closureClass.constructors) == 1
                closureCtor = closureClass.constructors[0]
                assert closureClass.typeParameters == closureCtor.typeParameters
                capturedScopeIds = closureInfo.capturedScopeIds()
                assert len(closureCtor.parameterTypes) == len(capturedScopeIds) + 1
                self.buildImplicitStaticTypeArguments(closureClass.typeParameters)
                self.allocobj(closureClass)
                self.dup()
                for id in capturedScopeIds:
                    self.loadContext(id)
                self.buildImplicitStaticTypeArguments(closureCtor.typeParameters)
                self.callg(closureCtor)
                self.drop()
                self.storeVariable(closureInfo.irClosureVar)
            elif isinstance(stmt, ast.ClassDefinition):
                raise NotImplementedError

    def buildLiteral(self, lit):
        if isinstance(lit, ast.IntegerLiteral):
            value = lit.value
            if value >= 2 ** (lit.width - 1):
                value -= 2 ** lit.width
            assert -(2 ** (lit.width - 1)) <= value and value < 2 ** (lit.width - 1)
            if lit.width == 8:
                self.i8(value)
            elif lit.width == 16:
                self.i16(value)
            elif lit.width == 32:
                self.i32(value)
            else:
                assert lit.width == 64
                self.i64(value)
        elif isinstance(lit, ast.FloatLiteral):
            if lit.width == 32:
                self.f32(lit.value)
            else:
                assert lit.width == 64
                self.f64(lit.value)
        elif isinstance(lit, ast.StringLiteral):
            id = self.info.package.findOrAddString(lit.value)
            self.string(id)
        elif isinstance(lit, ast.BooleanLiteral):
            if lit.value:
                self.true()
            else:
                self.false()
        elif isinstance(lit, ast.NullLiteral):
            self.null()
        else:
            raise NotImplementedError

    def buildUninitialized(self, ty):
        if ty is UnitType or ty is NoType:
            self.unit()
        elif ty is BooleanType:
            self.false()
        elif ty is I8Type:
            self.i8(0)
        elif ty is I16Type:
            self.i16(0)
        elif ty is I32Type:
            self.i32(0)
        elif ty is I64Type:
            self.i64(0)
        elif ty is F32Type:
            self.f32(0.)
        elif ty is F64Type:
            self.f64(0.)
        else:
            assert ty.isObject()
            if ty.isNullable():
                self.null()
            else:
                self.uninitialized()

    def buildCallNamedMethod(self, receiverType, name, mode):
        receiverClass = getClassFromType(receiverType)
        method = receiverClass.findMethodBySourceName(name)
        self.callMethod(method, receiverType)
        self.dropForEffect(mode)

    def callMethod(self, method, receiverType):
        """Builds a call to a non-static method.

        If the method has instructions specified (nearly all primitive methods), those are
        inlined directly. If the method is final, it is called statically. If the method is
        defined in a class or the receiver type is a class, it will be called virtually.

        Args:
            method (Function): the method to call.
            receiverType (Type?): the type of the receiver.
        """
        # TODO: receiverType may be None because calls to regular functions (which don't have
        # receiver types) can be converted into method calls during closure conversion.
        # Closure conversion should set receiver types. This won't affect code generation.

        if method.insts is not None:
            self.addBuiltinInstructions(method.insts)
        elif method.isFinal():
            self.callFunction(method)
        else:
            if method.isForeign():
                self.callvf(method)
            else:
                self.callv(method)

    HAVE_RECEIVER = "HAVE_RECEIVER"

    def buildCall(self, useInfo, callInfo, receiver, argExprs, mode, allowAllocation=True):
        shouldDropForEffect = mode is COMPILE_FOR_EFFECT
        defnInfo = useInfo.defnInfo
        irDefn = defnInfo.irDefn
        assert isinstance(irDefn, Function)
        if self.info.hasScope(irDefn) and self.info.hasClosureInfo(irDefn):
            closureInfo = self.info.getClosureInfo(irDefn)
        else:
            closureInfo = None

        def compileReceiver():
            assert receiver is not None and receiver is not self.HAVE_RECEIVER
            if isinstance(receiver, LValue):
                if receiver.onStack():
                    self.dup()
                receiver.evaluate()
            elif isinstance(receiver, ast.SuperExpression):
                # Special case: load `super` as `this`
                self.visitThisExpression(receiver, COMPILE_FOR_VALUE)
            else:
                assert isinstance(receiver, ast.Expression)
                self.visit(receiver, COMPILE_FOR_VALUE)

        def compileArgs():
            if argExprs is not None:
                for arg in argExprs:
                    self.visit(arg, COMPILE_FOR_VALUE)

        def compileTypeArgs():
            self.buildStaticTypeArguments(callInfo.typeArguments)

        if not irDefn.isConstructor() and not irDefn.isMethod():
            # Global or static function.
            if receiver is not None:
                assert receiver is not self.HAVE_RECEIVER
                compileReceiver()
            compileArgs()
            compileTypeArgs()
            self.callFunction(irDefn)

        elif irDefn.isConstructor():
            # Constructor.
            if allowAllocation:
                compileTypeArgs()
                if irDefn.definingClass.isForeign():
                    self.allocobjf(irDefn.definingClass)
                else:
                    self.allocobj(irDefn.definingClass)
            elif receiver is None:
                # This is a constructor called from another constructor. We'll load `this`.
                self.loadThis()
            if mode is COMPILE_FOR_VALUE:
                self.dup()
            if receiver is not None and receiver is not self.HAVE_RECEIVER:
                compileReceiver()
            compileArgs()
            compileTypeArgs()
            self.callFunction(irDefn)
            self.drop()
            shouldDropForEffect = False
        else:
            # Method

            # Compile the receiver
            if receiver is None:
                # Load implicit receiver
                if closureInfo is not None and \
                   closureInfo.irClosureVar is not None:
                    # This is a closure, so load the closure object.
                    if isinstance(closureInfo.irClosureVar, Variable):
                        # Local closure
                        self.loadVariable(closureInfo.irClosureVar)
                    else:
                        # Closure from captured scope
                        assert isinstance(closureInfo.irClosureVar, Field)
                        self.loadContext(defnInfo.scopeId)
                        self.loadField(closureInfo.irClosureVar)
                else:
                    # This is a regular method. Load "this".
                    self.loadContext(defnInfo.scopeId)
            elif receiver is not self.HAVE_RECEIVER:
                # Compile explicit receiver
                compileReceiver()

            # Compile the arguments and call the method.
            compileArgs()
            compileTypeArgs()
            self.callMethod(irDefn, callInfo.receiverType)

            if isinstance(receiver, LValue):
                self.buildAssignment(receiver, mode)
                shouldDropForEffect = False

        if shouldDropForEffect:
            self.drop()

    def callFunction(self, function):
        if function.isForeign():
            self.callgf(function)
        else:
            self.callg(function)

    def buildDestructure(self, irDefn, receiverType, hasPrefix, typeArgs,
                         subPatterns, failBlock, loc):
        """Generates code to destructure a value into sub-patterns with a matcher function.

        The value to be destructured should already be on the stack. If the matcher has an
        explicit receiver, that should also be on the stack (on top). If the matcher has
        an implicit receiver, code will be generated to load it. Code will be generated to
        call the matcher method. If the return value is a `Some`, its value will be extracted,
        split up, and used to match `subPatterns`.

        Args:
            irDefn: the matcher function or method. If `hasPrefix` is true, and a receiver is
                required, it should be loaded on the stack.
            receiverType: the type of the receiver if the matcher function is a non-static
                method. `None` otherwise.
            hasPrefix: `True` if there was an explicit prefix before the matcher. This is
                used to determine if an implicit receiver needs to be loaded.
            typeArgs: a list of `Type` arguments passed to the matcher.
            subPatterns: a list of `Pattern`s which will be compiled for matching.
            failBlock: code will branch here if the match fails. The matching value will be
                left on the stack.
            loc: the location of the matching pattern. Used for errors.
        """
        # Call the matcher function.
        if irDefn.isMethod():
            if not hasPrefix:
                self.loadImplicitReceiver(irDefn)
            self.dupi(1)
            self.buildStaticTypeArguments(typeArgs)
            self.callMethod(irDefn, receiverType)
        else:
            self.dup()
            self.buildStaticTypeArguments(typeArgs)
            self.callFunction(irDefn)
        returnType = irDefn.returnType.substitute(irDefn.typeParameters, typeArgs)
        optionClass = self.info.getStdClass("Option", loc)
        optionType = returnType.substituteForBase(optionClass)
        valueType = optionType.getTypeArguments()[0]

        # Check if it returned Some.
        self.dup()
        self.buildStaticTypeArgument(valueType)
        self.buildCallNamedMethod(optionType, "is-defined", COMPILE_FOR_VALUE)
        matcherSuccessBlock = self.newBlock()
        dropBlock = self.newBlock()
        self.branchif(matcherSuccessBlock.id, dropBlock.id)

        # Get the value out of the Some. Cast it to a tuple if we need to.
        self.setCurrentBlock(matcherSuccessBlock)
        self.buildStaticTypeArgument(valueType)
        self.buildCallNamedMethod(optionType, "get", COMPILE_FOR_VALUE)

        # If there is just one sub-pattern, pass the value to that pattern. Otherwise, the
        # value must be a tuple. Load each value out of the tuple and pass those to the
        # sub-patterns.
        successState = None
        n = len(subPatterns)
        if n == 1:
            self.visit(subPatterns[0], COMPILE_FOR_MATCH, valueType, dropBlock)
            successState = self.saveCurrentBlock()
        else:
            tupleClass = valueType.clas
            valueTypeArgs = valueType.getTypeArguments()
            dropFieldBlock = self.newBlock()
            mustMatch = True
            for i in xrange(n - 1):
                self.dup()
                self.loadField(tupleClass.fields[i])
                self.visit(subPatterns[i], COMPILE_FOR_MATCH, valueTypeArgs[i], dropFieldBlock)
                mustMatch &= type_analysis.patternMustMatch(subPatterns[i], valueTypeArgs[i],
                                                           self.info)
            self.loadField(tupleClass.fields[n - 1])
            self.visit(subPatterns[-1], COMPILE_FOR_MATCH, valueTypeArgs[-1], dropBlock)
            successState = self.saveCurrentBlock()

            # If one of the sub-patterns failed to match, we need to drop the field.
            if not mustMatch:
                self.setCurrentBlock(dropFieldBlock)
                self.drop()
                self.branch(dropBlock.id)

        # If the matcher did not return Some, drop the return value.
        self.setCurrentBlock(dropBlock)
        self.drop()
        self.branch(failBlock.id)

        # Go back to the success block and drop value passed to the matcher.
        self.restoreCurrentBlock(successState)
        self.drop()

    def buildAssignment(self, lvalue, mode):
        if mode is COMPILE_FOR_VALUE:
            self.dup()
            if lvalue.onStack():
                self.swap2()
        else:
            if lvalue.onStack():
                self.swap()
        lvalue.assign()

    def buildEquals(self, ty):
        """Compares the two values on top of the stack for equality. For objects, reference
        equality is used."""
        if ty.isPrimitive():
            self.buildCallNamedMethod(ty, "==", COMPILE_FOR_VALUE)
        else:
            self.buildCallNamedMethod(getRootClassType(), "===", COMPILE_FOR_VALUE)

    def buildType(self, ty, loc):
        """Builds a type on the static type argument stack and simultaneously builds an
        equivalent Type object on the value stack. This is useful for checked casts and other
        type tests."""
        if isinstance(ty, ClassType):
            assert len(ty.clas.typeParameters) == len(ty.typeArguments)
            for param, arg in zip(ty.clas.typeParameters, ty.typeArguments):
                self.buildType(arg, loc)
            if isinstance(ty.clas, Class):
                if ty.clas.isForeign():
                    self.tycdf(ty.clas)
                else:
                    self.tycd(ty.clas)
            else:
                assert isinstance(ty.clas, Trait)
                if ty.clas.isForeign():
                    self.tytdf(ty.clas)
                else:
                    self.tytd(ty.clas)
            if ty.isNullable():
                self.tyflagd(1)
        elif isinstance(ty, VariableType):
            if ty.typeParameter.isForeign():
                self.tyvdf(ty.typeParameter)
            else:
                self.tyvd(ty.typeParameter)
        else:
            assert isinstance(ty, ExistentialType)
            for v in ty.variables:
                self.buildType(VariableType(v), loc)
            self.buildType(ty.ty, loc)
            self.tyxd(len(ty.variables))

    def buildImplicitStaticTypeArguments(self, typeParams):
        for param in typeParams:
            assert not param.isForeign()
            self.tyvs(param)

    def buildStaticTypeArguments(self, types):
        for ty in types:
            self.buildStaticTypeArgument(ty)

    def buildStaticTypeArgument(self, ty):
        if isinstance(ty, ClassType):
            for arg in ty.typeArguments:
                self.buildStaticTypeArgument(arg)
            if isinstance(ty.clas, Class):
                if ty.clas.isForeign():
                    self.tycsf(ty.clas)
                else:
                    self.tycs(ty.clas)
            else:
                assert isinstance(ty.clas, Trait)
                if ty.clas.isForeign():
                    self.tytsf(ty.clas)
                else:
                    self.tyts(ty.clas)
            if ty.isNullable():
                self.tyflags(1)
        elif isinstance(ty, VariableType):
            assert not ty.typeParameter.isForeign()
            self.tyvs(ty.typeParameter)
        else:
            assert isinstance(ty, ExistentialType)
            for v in ty.variables:
                self.buildStaticTypeArgument(VariableType(v))
            self.buildStaticTypeArgument(ty.ty)
            self.tyxs(len(ty.variables))

    def buildCast(self, ty):
        self.buildStaticTypeArgument(ty)
        self.cast()

    def buildMatchException(self):
        self.allocobj(getBuiltinClassById(BUILTIN_MATCH_EXCEPTION_CLASS_ID))
        self.dup()
        self.callg(getBuiltinFunctionById(BUILTIN_MATCH_EXCEPTION_CTOR_ID))
        self.drop()

    def addBuiltinInstructions(self, insts):
        for instName in insts:
            inst = getattr(ir_instructions, instName)
            self.add(inst())

    def getScopeId(self):
        if isinstance(self.astDefn, ast.PrimaryConstructorDefinition):
            astDefn = self.function.definingClass.astDefn
        else:
            astDefn = self.astDefn
        return self.info.getScope(astDefn).scopeId

    def label(self, id):
        inst = self.label_(id)
        self.labels.append(inst)
        return inst

    def orderBlocks(self):
        # Clear the "id" attribute of each block. None will indicate a block has not been
        # been visited yet. -1 indicates a block is being visited but doesn't have an id yet.
        # Other values are new ids.
        for block in self.blocks:
            block.id = None

        # Assign new ids to the blocks. Ids are assigned in post-order, and we reverse this
        # after traversing the CFG. When visiting children, the last child is visited first so
        # that true branches will be ordered before false branches.
        self.nextBlockId = Counter()
        def visitBlock(block):
            if block.id is not None:
                return
            block.id = -1
            for succ in reversed(block.successorIds()):
                visitBlock(self.blocks[succ])
            block.id = self.nextBlockId()
        visitBlock(self.blocks[0])

        # Reverse the order. The first block should come first.
        liveBlockCount = self.nextBlockId.value()
        for block in self.blocks:
            if block.id is not None:
                block.id = liveBlockCount - block.id - 1

        # Update terminating instructions and labels to point to the new block ids.
        for block in self.blocks:
            if block.id is None:   # dead block
                continue
            inst = block.instructions[-1]
            successorIds = [self.blocks[id].id for id in inst.successorIds()]
            inst.setSuccessorIds(successorIds)

        for label in self.labels:
            newId = self.blocks[label.blockId()].id
            if newId is None:
                # Labels may point to dead blocks, likely because the corresponding branchl
                # instruction was unreachable. When this happens, point to -1, which the
                # interpreter will handle specially.
                newId = -1
            label.setBlockId(newId)

        # Rebuild the block list with the new order.
        orderedBlockList = [None] * liveBlockCount
        for block in self.blocks:
            if block.id is not None:
                orderedBlockList[block.id] = block
        self.blocks = orderedBlockList


def _makeInstBuilder(instClass):
    def instBuilder(self, *operands):
        inst = instClass(*operands)
        self.add(inst)
        return inst
    return instBuilder

for _inst in instInfoByCode:
    _name = _inst.name + ("_" if hasattr(CompileVisitor, _inst.name) else "")
    setattr(CompileVisitor, _name, _makeInstBuilder(ir_instructions.__dict__[_inst.name]))


class UnreachableScope(object):
    def __init__(self, compiler):
        self.compiler = compiler

    def __enter__(self):
        self.wasUnreachable = self.compiler.unreachable

    def __exit__(self, exc_type, exc_value, traceback):
        if self.compiler.unreachable and not self.wasUnreachable:
            self.compiler.unreachable = False


class LValue(object):
    def __init__(self, expr, compiler):
        self.expr = expr
        self.compiler = compiler

    def onStack(self):
        raise NotImplementedError

    def assign(self):
        raise NotImplementedError

    def evaluate(self):
        raise NotImplementedError


class VarLValue(LValue):
    def __init__(self, expr, compiler, ty, useInfo):
        super(VarLValue, self).__init__(expr, compiler)
        self.ty = ty
        self.useInfo = useInfo
        self.var = useInfo.defnInfo.irDefn

    def onStack(self):
        return False

    def assign(self):
        self.compiler.storeVariable(self.useInfo.defnInfo, self.ty)

    def evaluate(self):
        self.compiler.loadVariable(self.useInfo.defnInfo)


class PropertyLValue(LValue):
    def __init__(self, expr, compiler, useInfo):
        super(PropertyLValue, self).__init__(expr, compiler)
        self.field = useInfo.defnInfo.irDefn

    def onStack(self):
        return True

    def assign(self):
        self.compiler.storeField(self.field)

    def evaluate(self):
        self.compiler.loadField(self.field)

__all__ = ["compile"]

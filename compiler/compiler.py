# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from functools import partial

import ast
from bytecode import W8, W16, W32, W64, BUILTIN_TYPE_CLASS_ID, BUILTIN_TYPE_CTOR_ID, instInfoByCode, BUILTIN_MATCH_EXCEPTION_CLASS_ID, BUILTIN_MATCH_EXCEPTION_CTOR_ID, BUILTIN_STRING_EQ_OP_ID
from ir import IrTopDefn, Class, Field, Function, Global, LOCAL, Package, PACKAGE_INIT_NAME, RECEIVER_SUFFIX, Variable
from ir_types import UnitType, ClassType, VariableType, NULLABLE_TYPE_FLAG, getExceptionClassType, getClassFromType, getStringType, getRootClassType
import ir_instructions
from compile_info import CONTEXT_CONSTRUCTOR_HINT, CLOSURE_CONSTRUCTOR_HINT, PACKAGE_INITIALIZER_HINT, DefnInfo, NORMAL_MODE, STD_MODE, NOSTD_MODE
from flags import ABSTRACT, STATIC, LET
from errors import SemanticException
from builtins import getTypeClass, getExceptionClass, getRootClass, getStringClass, getBuiltinFunctionById, getBuiltinClassById
import type_analysis
from utils import Counter, COMPILE_FOR_EFFECT, COMPILE_FOR_VALUE, COMPILE_FOR_UNINITIALIZED, COMPILE_FOR_MATCH, each

def compile(info):
    for clas in info.package.classes:
        assignFieldIndices(clas, info)
    init = info.package.addFunction(PACKAGE_INIT_NAME, None, UnitType, [], [], [], None, frozenset())
    init.compileHint = PACKAGE_INITIALIZER_HINT
    info.package.initFunction = init.id
    for function in info.package.functions:
        compiler = CompileVisitor(function, info)
        compiler.compile()


def assignFieldIndices(clas, info):
    for index, field in enumerate(clas.fields):
        assert not hasattr(field, "index") or field.index == index
        field.index = index


class CompileVisitor(ast.AstNodeVisitor):
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
            fields = self.function.getReceiverClass().fields
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
                    if isinstance(defn, ast.AstVariableDefinition):
                        self.visit(defn, COMPILE_FOR_EFFECT)
            self.unit()
            self.ret()

        self.function.blocks = self.blocks

    def getParametersAndStatements(self):
        if isinstance(self.astDefn, ast.AstFunctionDefinition):
            if self.astDefn.body is None:
                assert ABSTRACT in self.function.flags
                return None, None
            parameters = self.astDefn.parameters
            if isinstance(self.astDefn.body, ast.AstBlockExpression):
                statements = self.astDefn.body.statements
            else:
                statements = [self.astDefn.body]
        elif isinstance(self.astDefn, ast.AstClassDefinition):
            parameters = []
            statements = self.astDefn.members
        else:
            assert isinstance(self.astDefn, ast.AstPrimaryConstructorDefinition)
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
           isinstance(statements[0], ast.AstCallExpression):
            if isinstance(statements[0].callee, ast.AstThisExpression):
                self.visitCallThisExpression(statements[0], COMPILE_FOR_EFFECT)
                altCtorCalled = True
                superCtorCalled = True
                statements = statements[1:]
            elif isinstance(statements[0].callee, ast.AstSuperExpression):
                self.visitCallSuperExpression(statements[0], COMPILE_FOR_EFFECT)
                superCtorCalled = True
                statements = statements[1:]
        elif isinstance(self.astDefn, ast.AstClassDefinition) and \
             self.info.hasUseInfo(self.astDefn):
            superCtorCalled = True
            arguments = self.astDefn.superArgs if self.astDefn.superArgs is not None else []
            self.buildCall(self.info.getUseInfo(self.astDefn),
                           self.info.getCallInfo(self.astDefn),
                           None, arguments, COMPILE_FOR_EFFECT, allowAllocation=False)
        elif isinstance(self.astDefn, ast.AstPrimaryConstructorDefinition):
            superCtorCalled = True
            astClassDefn = self.function.getReceiverClass().astDefn
            arguments = astClassDefn.superArgs if astClassDefn.superArgs is not None else []
            self.buildCall(self.info.getUseInfo(astClassDefn),
                           self.info.getCallInfo(astClassDefn),
                           None, arguments, COMPILE_FOR_EFFECT, allowAllocation=False)

        # If no superconstructor was called, try to find a default superconstructor,
        # and call that.
        if not superCtorCalled:
            supertype = self.function.getReceiverClass().supertypes[0]
            superclass = supertype.clas
            defaultSuperCtors = [ctor for ctor in superclass.constructors if
                                 len(ctor.parameterTypes) == 1]
            assert len(defaultSuperCtors) <= 1
            if len(defaultSuperCtors) == 0:
                raise SemanticException(self.function.getReceiverClass().getLocation(),
                                        "no default constructor in superclass %s" %
                                        superclass.name)
            self.loadThis()
            self.buildStaticTypeArguments(supertype.typeArguments)
            self.callg(defaultSuperCtors[0])
            self.drop()

        # If no alternate constructor was called, call the initializer.
        if not altCtorCalled:
            irInitializer = self.function.getReceiverClass().initializer
            if irInitializer is not None:
                self.loadThis()
                self.buildImplicitStaticTypeArguments(self.function.typeParameters)
                self.callg(irInitializer)
                self.drop()

        return parameters, statements

    def visitAstVariableDefinition(self, defn, mode):
        assert mode is COMPILE_FOR_EFFECT
        if defn.expression is None:
            if defn.keyword == "let":
                raise SemanticException(defn.location,
                                        "constant definition must have assignment")
            self.visit(defn.pattern, COMPILE_FOR_UNINITIALIZED)
        else:
            self.visit(defn.expression, COMPILE_FOR_VALUE)
            self.visit(defn.pattern, COMPILE_FOR_EFFECT, self.info.getType(defn.expression))

    def visitAstFunctionDefinition(self, defn, mode):
        assert mode is COMPILE_FOR_EFFECT
        pass

    def visitAstClassDefinition(self, defn, mode):
        assert mode is COMPILE_FOR_EFFECT
        pass

    def visitAstParameter(self, param, id):
        self.unpackParameter(param, id)

    def visitAstConstructorParameter(self, param, id):
        self.unpackParameter(param, id)

    def visitAstVariablePattern(self, pat, mode, ty=None, failBlock=None):
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
                self.buildCallNamedMethod(patTy, "==", COMPILE_FOR_VALUE)
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
            self.uninitialized()
        self.storeVariable(defnInfo, ty)

    def visitAstBlankPattern(self, pat, mode, ty=None, failBlock=None):
        if mode is COMPILE_FOR_UNINITIALIZED:
            return
        self.drop()

    def visitAstLiteralPattern(self, pat, mode, ty=None, failBlock=None):
        if mode is COMPILE_FOR_UNINITIALIZED:
            return
        lit = pat.literal
        ty = self.info.getType(pat)
        if isinstance(lit, ast.AstStringLiteral):
            # String.== is an actual method, so the literal needs to be the receiver.
            self.buildLiteral(lit)
            self.dupi(1)  # value
            self.callg(getBuiltinFunctionById(BUILTIN_STRING_EQ_OP_ID))
        elif isinstance(lit, ast.AstNullLiteral):
            self.dup()
            self.null(),
            self.eqp()
        else:
            self.dup()  # value
            self.buildLiteral(lit)
            self.buildCallNamedMethod(ty, "==", COMPILE_FOR_VALUE)
        successBlock = self.newBlock()
        self.branchif(successBlock.id, failBlock.id)
        self.setCurrentBlock(successBlock)
        self.drop()

    def visitAstTuplePattern(self, pat, mode, ty=None, failBlock=None):
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
            return isinstance(p, ast.AstBlankPattern) and \
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
                self.ldp(i)
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

    def visitAstValuePattern(self, pat, mode, ty=None, failBlock=None):
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
            self.buildCallNamedMethod(ty, "==", COMPILE_FOR_VALUE)
        else:
            # The == operator for the matching value could be anything, so we want to make
            # sure to call the method on the value from this pattern.
            buildValue()
            self.dupi(1)
            self.buildCallNamedMethod(ty, "==", COMPILE_FOR_VALUE)
        successBlock = self.newBlock()
        self.branchif(successBlock.id, failBlock.id)
        self.setCurrentBlock(successBlock)
        self.drop()

    def visitAstDestructurePattern(self, pat, mode, ty=None, failBlock=None):
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

        # Call the matcher function.
        if irDefn.isMethod():
            if not hasPrefix:
                self.loadImplicitReceiver(irDefn)
            self.dupi(1)
            self.buildStaticTypeArguments(callInfo.typeArguments)
            self.callMethod(irDefn)
        else:
            self.dup()
            self.buildStaticTypeArguments(callInfo.typeArguments)
            self.callFunction(irDefn)

        # Check if it returned Some.
        someClass = self.info.getStdClass("Some", pat.location)
        someType = ClassType.forReceiver(someClass)
        self.buildType(someType, pat.location)
        matcherSuccessBlock = self.newBlock()
        dropSomeBlock = self.newBlock()
        self.castcbr(matcherSuccessBlock.id, dropSomeBlock.id)

        # Get the value out of the Some. Cast it to a tuple if we need to.
        self.setCurrentBlock(matcherSuccessBlock)
        self.buildStaticTypeArgument(getRootClassType())
        self.buildCallNamedMethod(someType, "get", COMPILE_FOR_VALUE)
        n = len(pat.patterns)
        if n > 1:
            tupleClass = self.info.getTupleClass(n, pat.location)
            tupleTypeArgs = tuple(VariableType(p) for p in tupleClass.typeParameters)
            tupleType = ClassType(tupleClass, tupleTypeArgs)
            self.buildType(tupleType, pat.location)
            self.castc()

        # If there is just one pattern inside, pass the value to that pattern. Otherwise,
        # load each value out of the tuple and pass those to the patterns. Note that we pass
        # Object as the expression type for each tuple. We haven't properly type checked the
        # value we pulled out of the Some[_]. We need to type check these values at runtime,
        # but the type checks should never fail.
        successState = None
        if n == 1:
            self.visit(pat.patterns[0], mode, getRootClassType(), dropSomeBlock)
            successState = self.saveCurrentBlock()
        else:
            dropFieldBlock = self.newBlock()
            for i in xrange(n - 1):
                self.dup()
                self.ldpc(i)
                self.visit(pat.patterns[i], mode, getRootClassType(), dropFieldBlock)
            self.ldpc(n - 1)
            self.visit(pat.patterns[-1], mode, getRootClassType(), dropSomeBlock)
            successState = self.saveCurrentBlock()

            # If one of the sub-patterns failed to match, we need to drop the field.
            self.setCurrentBlock(dropFieldBlock)
            self.drop()
            self.branch(dropSomeBlock.id)

        # If the matcher did not return Some, drop the return value.
        self.setCurrentBlock(dropSomeBlock)
        self.drop()
        self.branch(failBlock.id)

        # Go back to the success block and drop the last field.
        self.restoreCurrentBlock(successState)
        self.drop()

    def visitAstLiteralExpression(self, expr, mode):
        self.buildLiteral(expr.literal)
        self.dropForEffect(mode)

    def visitAstVariableExpression(self, expr, mode):
        self.buildLoadOrNullaryCall(expr, mode)

    def visitAstThisExpression(self, expr, mode):
        self.buildLoadOrNullaryCall(expr, mode)

    def visitAstSuperExpression(self, expr, mode):
        raise SemanticException(expr.location, "`super` is only valid as part of a call")

    def visitAstBlockExpression(self, expr, mode):
        scopeId = self.info.getScope(expr).scopeId
        self.compileStatements(scopeId, None, expr.statements, mode)

    def visitAstAssignExpression(self, expr, mode):
        ty = self.info.getType(expr.right)
        lvalue = self.compileLValue(expr.left, ty)
        self.visit(expr.right, COMPILE_FOR_VALUE)
        self.buildAssignment(lvalue, mode)

    def visitAstPropertyExpression(self, expr, mode):
        if not self.info.hasScopePrefixInfo(expr.receiver):
            self.visit(expr.receiver, COMPILE_FOR_VALUE)
        self.buildLoadOrNullaryCall(expr, mode, receiverIsExplicit=True)

    def visitAstCallExpression(self, expr, mode):
        if not isinstance(expr.callee, ast.AstVariableExpression) and \
           not isinstance(expr.callee, ast.AstPropertyExpression):
            raise SemanticException(expr.location, "uncallable expression")

        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr) if self.info.hasCallInfo(expr) else None
        if isinstance(expr.callee, ast.AstPropertyExpression) and \
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

    def visitAstUnaryExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr)
        self.buildCall(useInfo, callInfo, expr.expr, [], mode)

    def visitAstBinaryExpression(self, expr, mode):
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
            # regular operators (handled like methods)
            useInfo = self.info.getUseInfo(expr)
            callInfo = self.info.getCallInfo(expr)
            isCompoundAssignment = opName == useInfo.defnInfo.irDefn.name.short() + "="
            if isCompoundAssignment:
                ty = self.info.getType(expr.right)
                receiver = self.compileLValue(expr.left, ty)
            else:
                receiver = expr.left
            self.buildCall(useInfo, callInfo, receiver, [expr.right], mode)

    def visitAstTupleExpression(self, expr, mode):
        ty = self.info.getType(expr)
        tupleClass = ty.clas
        langMode = self.info.languageMode()
        assert langMode is not NOSTD_MODE

        # Allocate the tuple.
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

    def visitAstIfExpression(self, expr, mode):
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

    def visitAstWhileExpression(self, expr, mode):
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

    def visitAstMatchExpression(self, expr, mode):
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
        self.visitAstPartialFunctionExpression(expr.matcher, mode, exprTy, doneBlock, failBlock)
        self.setCurrentBlock(doneBlock)

    def visitAstThrowExpression(self, expr, mode):
        self.visit(expr.exception, COMPILE_FOR_VALUE)
        self.throw()
        self.setUnreachable()

    def visitAstTryCatchExpression(self, expr, mode):
        haveCatch = expr.catchHandler is not None
        haveFinally = expr.finallyHandler is not None
        haveBoth = haveCatch and haveFinally
        exnTy = getExceptionClassType()
        mustMatch = haveCatch and \
            type_analysis.partialFunctionMustMatch(expr.catchHandler, exnTy, self.info)

        # Create blocks. Some blocks may not be needed, depending on which handlers we have.
        tryBlock = self.newBlock()
        catchTryBlock = self.newBlock() if haveBoth else None
        catchBlock = self.newBlock() if haveCatch else None
        catchNormalBlock = self.newBlock() if haveBoth else None
        catchMissBlock = self.newBlock() if haveBoth and not mustMatch else None
        catchThrowFinallyBlock = self.newBlock() if haveBoth else None
        catchNormalFinallyBlock = self.newBlock() if haveBoth else None
        tryFinallyBlock = self.newBlock() if haveFinally else None
        throwFinallyBlock = self.newBlock() \
                            if haveFinally and not haveCatch and mode is COMPILE_FOR_VALUE \
                            else None
        finallyBlock = self.newBlock() if haveFinally else None
        rethrowBlock = self.newBlock() if haveFinally or (haveCatch and not mustMatch) else None
        doneBlock = self.newBlock()

        # Add some aliases for try and catch outcomes to simplify the code here.
        if haveBoth:
            tryNormalBlock = tryFinallyBlock
            exceptionBlock = catchTryBlock
            catchSuccessBlock = catchNormalBlock
            catchFailBlock = catchMissBlock
        elif haveCatch:
            tryNormalBlock = doneBlock
            exceptionBlock = catchBlock
            catchSuccessBlock = doneBlock
            catchFailBlock = rethrowBlock
        else:
            assert haveFinally
            tryNormalBlock = tryFinallyBlock
            if mode is COMPILE_FOR_VALUE:
                exceptionBlock = throwFinallyBlock
            else:
                exceptionBlock = finallyBlock
            catchSuccessBlock = None
            catchFailBlock = None

        # Enter the try expression.
        # Stack at this point: [...]
        self.pushtry(tryBlock.id, exceptionBlock.id)

        # Compile the try expression.
        # Stack at this point: [| ...]
        self.setCurrentBlock(tryBlock)
        with UnreachableScope(self):
            self.visit(expr.expression, mode)
            self.poptry(tryNormalBlock.id)

        # If we have both catch and finally, we need to make sure we still execute the finally
        # if the catch throws an exception. So we wrap the catch in another try.
        # Stack at this point: [exception ...]
        if catchTryBlock is not None:
            self.setCurrentBlock(catchTryBlock)
            self.pushtry(catchBlock.id, catchThrowFinallyBlock.id)

        # If we have a catch handler, we compile it like a normal pattern match.
        # Stack at this point: [| exception ...]
        if catchBlock is not None:
            self.setCurrentBlock(catchBlock)
            if haveFinally:
                # We're inside another try, so we can't consume the exception.
                self.dup()
            with UnreachableScope(self):
                self.visitAstPartialFunctionExpression(expr.catchHandler, mode,
                                                       exnTy, catchSuccessBlock, catchFailBlock)
            assert self.isDetached()

        # If we have both catch and finally, once we handle an exception, we need to break out
        # of the try wrapped around the catch handler.
        # Stack at this point: [result | exception ...]
        if catchNormalBlock is not None:
            self.setCurrentBlock(catchNormalBlock)
            self.poptry(catchNormalFinallyBlock.id)

        # If we have both catch and finally, if we can't handle an exception, we need to break
        # out of the try wrapped around the catch handler.
        # Stack at this point: [exception | exception ...]
        if catchMissBlock is not None:
            self.setCurrentBlock(catchMissBlock)
            self.poptry(catchThrowFinallyBlock.id)

        # If we have both catch and finally, we need to handle exceptions thrown by the catch
        # handler. We also come here if an exception couldn't be handled. The exception on
        # top replaces the other exception.
        # Stack at this point: [exception exception ...]
        if catchThrowFinallyBlock is not None:
            self.setCurrentBlock(catchThrowFinallyBlock)
            self.swap()
            self.drop()
            if mode is COMPILE_FOR_VALUE:
                self.uninitialized()
                self.swap()
            self.branch(finallyBlock.id)

        # If we have both catch and finally, after we've handled the exception and broken out
        # of the try, we need to push null so the finally doesn't try to throw anything.
        # Stack at this point: [result exception ...]
        if catchNormalFinallyBlock is not None:
            self.setCurrentBlock(catchNormalFinallyBlock)
            if mode is COMPILE_FOR_VALUE:
                self.swap()
            self.drop()
            self.null()
            self.branch(finallyBlock.id)

        # If we have finally, after the try expression completes normally, we need to push null
        # so the finally doesn't try to throw anything.
        # Stack at this point: [result ...]
        if tryFinallyBlock is not None:
            self.setCurrentBlock(tryFinallyBlock)
            self.null()
            self.branch(finallyBlock.id)

        # If we have finally but not catch and we're compiling for value, after the try
        # expression throws an exception, we need to push a fake value so the finally block
        # has a fixed-height stack.
        # Stack at this point: [exception ...]
        if throwFinallyBlock is not None:
            self.setCurrentBlock(throwFinallyBlock)
            self.uninitialized()
            self.swap()
            self.branch(finallyBlock.id)

        # If we have finally, we execute the finally handler then check whether there was an
        # exception. null indicates no exception.
        # Stack at this point: [exception? result ...]
        if finallyBlock is not None:
            self.setCurrentBlock(finallyBlock)
            self.visit(expr.finallyHandler, COMPILE_FOR_EFFECT)
            self.dup()
            self.null()
            self.eqp()
            self.branchif(doneBlock.id, rethrowBlock.id)

        # If we could not handle an exception, or if a new exception was thrown while we were
        # handling an exception, we need to rethrow it here.
        # Stack at this point: [exception ...]
        if rethrowBlock is not None:
            self.setCurrentBlock(rethrowBlock)
            self.throw()

        # Everything has been handled. If we came through a finally, we need to pop the null
        # exception off the stack.
        # Stack at this point: [result, ...]
        self.setCurrentBlock(doneBlock)
        if haveFinally:
            self.drop()

    def visitAstPartialFunctionExpression(self, expr, mode, ty, doneBlock, failBlock):
        allCasesTerminate = True
        mustMatchCaseTerminates = False

        for case in expr.cases[:-1]:
            nextBlock = self.newBlock()
            with UnreachableScope(self):
                self.visitAstPartialFunctionCase(case, mode, ty, doneBlock, nextBlock)
                caseTerminates = self.unreachable
                mustMatchCaseTerminates |= \
                    type_analysis.partialFunctionCaseMustMatch(case, ty, self.info)
                allCasesTerminate &= caseTerminates
            if mustMatchCaseTerminates:
                self.setUnreachable()
            self.setCurrentBlock(nextBlock)
        with UnreachableScope(self):
            self.visitAstPartialFunctionCase(expr.cases[-1], mode, ty, doneBlock, failBlock)
            allCasesTerminate &= self.unreachable
        if allCasesTerminate or mustMatchCaseTerminates:
            self.setUnreachable()
        assert self.isDetached()

    def visitAstPartialFunctionCase(self, expr, mode, ty, doneBlock, failBlock):
        if expr.condition is not None:
            assert self.unreachable or failBlock is not None
            failBlockId = failBlock.id if failBlock is not None else -1
            self.dup()
        self.visit(expr.pattern, COMPILE_FOR_MATCH, ty, failBlock)
        if expr.condition is not None:
            self.visit(expr.condition, COMPILE_FOR_VALUE)
            successBlock = self.newBlock()
            self.branchif(successBlock.id, failBlockId)
            self.setCurrentBlock(successBlock)
            self.drop()
        self.visit(expr.expression, mode)
        self.branch(doneBlock.id)
        self.detach()

    def visitAstReturnExpression(self, expr, mode):
        if expr.expression is None:
            self.unit()
        else:
            self.visit(expr.expression, COMPILE_FOR_VALUE)
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
        assert self.stackHeights[block.id] is None
        self.stackHeights[block.id] = stackHeight

    def setUnreachable(self):
        self.unreachable = True
        self.currentBlock = None

    def detach(self):
        self.currentBlock = None

    def isDetached(self):
        return self.currentBlock is None

    def compileLValue(self, expr, ty):
        useInfo = self.info.getUseInfo(expr)
        irDefn = useInfo.defnInfo.irDefn
        if LET in irDefn.flags:
            raise SemanticException(expr.location, "left side of assignment is constant")
        if (isinstance(expr, ast.AstVariableExpression) and \
            (isinstance(irDefn, Variable) or \
             isinstance(irDefn, Field) or \
             isinstance(irDefn, Global))) or \
           (isinstance(expr, ast.AstPropertyExpression) and \
            isinstance(irDefn, Global) and irDefn.isForeign()):
            return VarLValue(expr, self, ty, useInfo)
        elif isinstance(expr, ast.AstPropertyExpression) and isinstance(irDefn, Field):
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
                if isinstance(param.pattern, ast.AstVariablePattern):
                    defnInfo = self.info.getDefnInfo(param.pattern)
                    if isinstance(defnInfo.irDefn, Variable):
                       defnInfo.irDefn.index = index + implicitParamCount

    def unpackParameters(self, parameters):
        implicitParameterCount = 1 if self.function.isMethod() else 0
        for index, param in enumerate(parameters):
            self.unpackParameter(param, index + implicitParameterCount)

    def unpackParameter(self, param, index):
        paramType = self.info.getType(param)
        if isinstance(param.pattern, ast.AstVariablePattern):
            defnInfo = self.info.getDefnInfo(param.pattern)
            if isinstance(defnInfo.irDefn, Variable):
                defnInfo.irDefn.index = index
            else:
                self.ldlocal(index)
                self.storeVariable(defnInfo, paramType)
        elif isinstance(param.pattern, ast.AstBlankPattern):
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
            if isinstance(statements[-1], ast.AstExpression):
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
                self.callMethod(irDefn)
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
        ty = field.type
        if ty.isObject():
            if ty.isNullable():
                inst = ir_instructions.ldp
            else:
                inst = ir_instructions.ldpc
        elif ty.width == W8:
            inst = ir_instructions.ld8
        elif ty.width == W16:
            inst = ir_instructions.ld16
        elif ty.width == W32:
            inst = ir_instructions.ld32
        elif ty.width == W64:
            inst = ir_instructions.ld64
        self.add(inst(field.index))

    def storeField(self, field):
        if field.type.isObject():
            inst = ir_instructions.stp
        elif field.type.width == W8:
            inst = ir_instructions.st8
        elif field.type.width == W16:
            inst = ir_instructions.st16
        elif field.type.width == W32:
            inst = ir_instructions.st32
        elif field.type.width == W64:
            inst = ir_instructions.st64
        self.add(inst(field.index))

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
                    (isinstance(self.astDefn, ast.AstClassDefinition) or
                     isinstance(self.astDefn, ast.AstPrimaryConstructorDefinition))) and \
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
            if isinstance(stmt, ast.AstFunctionDefinition):
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
            elif isinstance(stmt, ast.AstClassDefinition):
                raise NotImplementedError

    def buildLiteral(self, lit):
        if isinstance(lit, ast.AstIntegerLiteral):
            if lit.width == 8:
                self.i8(lit.value)
            elif lit.width == 16:
                self.i16(lit.value)
            elif lit.width == 32:
                self.i32(lit.value)
            else:
                assert lit.width == 64
                self.i64(lit.value)
        elif isinstance(lit, ast.AstFloatLiteral):
            if lit.width == 32:
                self.f32(lit.value)
            else:
                assert lit.width == 64
                self.f64(lit.value)
        elif isinstance(lit, ast.AstStringLiteral):
            id = self.info.package.findOrAddString(lit.value)
            self.string(id)
        elif isinstance(lit, ast.AstBooleanLiteral):
            if lit.value:
                self.true()
            else:
                self.false()
        elif isinstance(lit, ast.AstNullLiteral):
            self.null()
        else:
            raise NotImplementedError

    def buildCallNamedMethod(self, receiverType, name, mode):
        receiverClass = getClassFromType(receiverType)
        method = receiverClass.getMethod(name)
        self.callMethod(method)
        self.dropForEffect(mode)

    def callMethod(self, method):
        if hasattr(method, "insts"):
            self.addBuiltinInstructions(method.insts)
        elif method.isFinal():
            self.callFunction(method)
        else:
            index = method.getReceiverClass().getMethodIndex(method)
            self.callv(len(method.parameterTypes), index)

    def buildCall(self, useInfo, callInfo, receiver, argExprs, mode, allowAllocation=True):
        shouldDropForEffect = mode is COMPILE_FOR_EFFECT
        defnInfo = useInfo.defnInfo
        irDefn = defnInfo.irDefn
        assert isinstance(irDefn, Function)
        if self.info.hasScope(irDefn) and self.info.hasClosureInfo(irDefn):
            closureInfo = self.info.getClosureInfo(irDefn)
        else:
            closureInfo = None
        argCount = len(argExprs)

        def compileArgs():
            for arg in argExprs:
                self.visit(arg, COMPILE_FOR_VALUE)

        def compileTypeArgs():
            self.buildStaticTypeArguments(callInfo.typeArguments)

        if not irDefn.isConstructor() and not irDefn.isMethod():
            # Global or static function.
            assert receiver is None
            compileArgs()
            compileTypeArgs()
            self.callFunction(irDefn)

        elif receiver is None and irDefn.isConstructor():
            # Constructor.
            assert receiver is None
            if allowAllocation:
                compileTypeArgs()
                if irDefn.getReceiverClass().isForeign():
                    self.allocobjf(irDefn.getReceiverClass())
                else:
                    self.allocobj(irDefn.getReceiverClass())
            else:
                # This is a constructor called from another constructor. Load receiver instead
                # of allocating a new object.
                self.loadThis()
            if mode is COMPILE_FOR_VALUE:
                self.dup()
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
            else:
                # Compile explicit receiver
                if isinstance(receiver, LValue):
                    if receiver.onStack():
                        self.dup()
                    receiver.evaluate()
                elif isinstance(receiver, ast.AstSuperExpression):
                    # Special case: load `super` as `this`
                    self.visitAstThisExpression(receiver, COMPILE_FOR_VALUE)
                else:
                    assert isinstance(receiver, ast.AstExpression)
                    self.visit(receiver, COMPILE_FOR_VALUE)

            # Compile the arguments and call the method.
            compileArgs()
            compileTypeArgs()
            if hasattr(irDefn, "insts"):
                self.addBuiltinInstructions(irDefn.insts)
            elif irDefn.isFinal():
                # Calls to final methods can be made directly. This includes constructors and
                # primitive methods which can't be called virtually.
                self.callFunction(irDefn)
            else:
                index = irDefn.getReceiverClass().getMethodIndex(irDefn)
                self.callv(argCount + 1, index)

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

    def buildAssignment(self, lvalue, mode):
        if mode is COMPILE_FOR_VALUE:
            self.dup()
            if lvalue.onStack():
                self.swap2()
        else:
            if lvalue.onStack():
                self.swap()
        lvalue.assign()

    def buildType(self, ty, loc):
        """Builds a type on the static type argument stack and simultaneously builds an
        equivalent Type object on the value stack. This is useful for checked casts and other
        type tests."""
        if isinstance(ty, VariableType):
            if ty.typeParameter.isForeign():
                self.tyvdf(ty.typeParameter)
            else:
                self.tyvd(ty.typeParameter)
        else:
            assert isinstance(ty, ClassType)
            assert len(ty.clas.typeParameters) == len(ty.typeArguments)
            for param, arg in zip(ty.clas.typeParameters, ty.typeArguments):
                if STATIC in param.flags and arg != VariableType(param):
                    raise SemanticException(
                        loc, "cannot dynamically check a type with static type parameters")
                self.buildType(arg, loc)
            if ty.clas.isForeign():
                self.tycdf(ty.clas)
            else:
                self.tycd(ty.clas)
            if ty.isNullable():
                self.tyflagd(1)

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
            if ty.clas.isForeign():
                self.tycsf(ty.clas)
            else:
                self.tycs(ty.clas)
            if ty.isNullable():
                self.tyflags(1)
        elif isinstance(ty, VariableType):
            assert not ty.typeParameter.isForeign()
            self.tyvs(ty.typeParameter)
        else:
            raise NotImplementedError

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
        if isinstance(self.astDefn, ast.AstPrimaryConstructorDefinition):
            astDefn = self.function.getReceiverClass().astDefn
        else:
            astDefn = self.astDefn
        return self.info.getScope(astDefn).scopeId

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

        # Update terminating instructions to point to the new block ids.
        for block in self.blocks:
            if block.id is None:   # dead block
                continue
            inst = block.instructions[-1]
            successorIds = [self.blocks[id].id for id in inst.successorIds()]
            inst.setSuccessorIds(successorIds)

        # Rebuild the block list with the new order.
        orderedBlockList = [None] * liveBlockCount
        for block in self.blocks:
            if block.id is not None:
                orderedBlockList[block.id] = block
        self.blocks = orderedBlockList


def _makeInstBuilder(instClass):
    return lambda self, *operands: self.add(instClass(*operands))

for _inst in instInfoByCode:
    setattr(CompileVisitor, _inst.name, _makeInstBuilder(ir_instructions.__dict__[_inst.name]))


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

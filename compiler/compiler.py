# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from functools import partial

import ast
from bytecode import W8, W16, W32, W64, BUILTIN_TYPE_CLASS_ID, BUILTIN_TYPE_CTOR_ID, instInfoByCode
from ir import Global, Variable, Field, Function, Class, LOCAL
from ir_types import UnitType, ClassType, VariableType, NULLABLE_TYPE_FLAG, getExceptionClassType
import ir_instructions
from compile_info import CONTEXT_CONSTRUCTOR_HINT, CLOSURE_CONSTRUCTOR_HINT, PACKAGE_INITIALIZER_HINT, DefnInfo
from flags import ABSTRACT, STATIC, LET
from errors import SemanticException
from builtins import getTypeClass, getExceptionClass, getRootClass
from utils import Counter, COMPILE_FOR_EFFECT, COMPILE_FOR_VALUE, COMPILE_FOR_UNINITIALIZED, COMPILE_FOR_MATCH

def compile(info):
    for clas in info.package.classes:
        assignFieldIndices(clas, info)
    init = info.package.addFunction("$init", None, UnitType, [], [], [], None, frozenset())
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
        self.info = info
        self.blocks = []
        self.nextBlockId = Counter()
        self.currentBlock = None
        self.unreachable = False
        self.setCurrentBlock(self.newBlock())
        assert self.astDefn is not None or self.compileHint is not None

    def compile(self):
        # Handle special implicit functions.
        if self.compileHint:
            self.compileWithHint()
            return

        # Get the body of the function as a list of statements. Also parameters.
        if isinstance(self.astDefn, ast.AstFunctionDefinition):
            if self.astDefn.body is None:
                assert ABSTRACT in self.function.flags
                return
            parameters = self.astDefn.parameters
            if isinstance(self.astDefn.body, ast.AstBlockExpression):
                statements = self.astDefn.body.statements
            else:
                statements = [self.astDefn.body]
        elif isinstance(self.astDefn, ast.AstClassDefinition):
            parameters = None
            statements = self.astDefn.members
        else:
            assert isinstance(self.astDefn, ast.AstPrimaryConstructorDefinition)
            parameters = self.astDefn.parameters
            statements = []

        # Set ids (and therefore, fp-offsets) for each local variable.
        self.enumerateLocals()
        self.enumerateParameters(parameters)

        # If this is a constructor, the first statement may be a "this" or "super" call.
        altCtorCalled = False
        superCtorCalled = False
        if self.function.isConstructor() and \
           len(statements) > 0 and \
           isinstance(statements[0], ast.AstCallExpression):
            if isinstance(statements[0].callee, ast.AstThisExpression):
                self.visitCallThisExpression(statements[0], COMPILE_FOR_EFFECT)
                altCtorCalled = True
                superCtorCalled = True
                del statements[0]
            elif isinstance(statements[0].callee, ast.AstSuperExpression):
                self.visitCallSuperExpression(statements[0], COMPILE_FOR_EFFECT)
                superCtorCalled = True
                del statements[0]

        # If this is a constructor that doesn't call any alternate constructor or super
        # constructor, try to find a default super constructor, and call that.
        if self.function.isConstructor() and not superCtorCalled:
            supertype = self.function.clas.supertypes[0]
            superclass = supertype.clas
            defaultSuperCtors = [ctor for ctor in superclass.constructors if
                                 len(ctor.parameterTypes) == 1]
            assert len(defaultSuperCtors) <= 1
            if len(defaultSuperCtors) == 0:
                raise SemanticException(self.function.clas.getLocation(),
                                        "no default constructor in superclass %s" %
                                        superclass.name)
            self.loadThis()
            self.buildStaticTypeArguments(supertype.typeArguments)
            self.callg(defaultSuperCtors[0].id)
            self.drop()

        # If this is a primary constructor, unpack the parameters before calling the
        # initializer. In this case, unpacking the parameters means storing them into the
        # object. The initializer may need to access them.
        if self.function.isConstructor() and \
           isinstance(self.function.astDefn, ast.AstPrimaryConstructorDefinition):
            self.unpackParameters(parameters)
            parameters = None

        # If this is a constructor that doesn't call any alternate constructor, call the
        # initializer before we evaluate the body.
        if self.function.isConstructor() and not altCtorCalled:
            irInitializer = self.function.clas.initializer
            if irInitializer is not None:
                self.loadThis()
                self.buildImplicitStaticTypeArguments(self.function.typeParameters)
                self.callg(irInitializer.id)
                self.drop()

        # Compile those statements.
        mode = COMPILE_FOR_EFFECT if self.function.isConstructor() else COMPILE_FOR_VALUE
        self.compileStatements(self.astDefn.id, parameters, statements, mode)

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
            fields = self.function.clas.fields
            for i in xrange(len(fields)):
                paramIndex = i + 1   # skip receiver
                self.ldlocal(paramIndex)
                self.loadThis()
                self.storeField(fields[i])
            self.unit()
            self.ret()
        elif self.compileHint is PACKAGE_INITIALIZER_HINT:
            # This function initializes all the global variables.
            for defn in self.info.ast.definitions:
                if isinstance(defn, ast.AstVariableDefinition):
                    self.visit(defn, COMPILE_FOR_EFFECT)
            self.unit()
            self.ret()

        self.function.blocks = self.blocks

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

    def visitAstVariablePattern(self, pat, mode, ty=None, successBlock=None, failBlock=None):
        defnInfo = self.info.getDefnInfo(pat)
        if mode is COMPILE_FOR_UNINITIALIZED and \
           isinstance(defnInfo.irDefn, Global):
            # Globals are automatically uninitialized.
            return

        if mode is COMPILE_FOR_MATCH:
            patTy = self.info.getType(pat)
            typeClass = getTypeClass()
            self.dup()
            self.buildType(patTy, pat.location)
            isSubtypeOfMethod = typeClass.getMethod("is-subtype-of")
            index = typeClass.getMethodIndex(isSubtypeOfMethod)
            self.callv(2, index)
            self.branchif(successBlock.id, failBlock.id)
            self.setCurrentBlock(successBlock)
        elif mode is COMPILE_FOR_UNINITIALIZED:
            self.uninitialized()
        self.storeVariable(defnInfo, ty)

    def visitAstClassType(self, node, param):
        assert STATIC in param.flags
        ty = self.info.getType(node)
        self.buildStaticTypeArgument(ty)

    def visitAstLiteralExpression(self, expr, mode):
        lit = expr.literal
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
        self.dropForEffect(mode)

    def visitAstVariableExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        irDefn = useInfo.defnInfo.irDefn
        if isinstance(irDefn, Variable) or \
           isinstance(irDefn, Field) or \
           isinstance(irDefn, Global):
            # Parameter, local, or context variable.
            self.loadVariable(useInfo.defnInfo)
            self.dropForEffect(mode)
        else:
            assert isinstance(irDefn, Function) or isinstance(irDefn, Class)
            callInfo = self.info.getCallInfo(expr)
            self.buildCall(useInfo, callInfo, None, [], [], mode)

    def visitAstThisExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        irDefn = useInfo.defnInfo.irDefn
        assert isinstance(irDefn, Variable) or isinstance(irDefn, Field)
        self.loadVariable(useInfo.defnInfo)
        self.dropForEffect(mode)

    def visitAstSuperExpression(self, expr, mode):
        raise SemanticException(expr.location, "`super` is only valid as part of a call")

    def visitAstBlockExpression(self, expr, mode):
        self.compileStatements(expr.id, None, expr.statements, mode)

    def visitAstAssignExpression(self, expr, mode):
        ty = self.info.getType(expr.right)
        lvalue = self.compileLValue(expr.left, ty)
        self.visit(expr.right, COMPILE_FOR_VALUE)
        self.buildAssignment(lvalue, mode)

    def visitAstPropertyExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        irDefn = useInfo.defnInfo.irDefn
        if isinstance(irDefn, Field):
            self.visit(expr.receiver, COMPILE_FOR_VALUE)
            self.loadField(irDefn)
            self.dropForEffect(mode)
        else:
            assert isinstance(irDefn, Function)
            callInfo = self.info.getCallInfo(expr)
            self.buildCall(useInfo, callInfo, expr.receiver, [], [], mode)

    def visitAstCallExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr) if self.info.hasCallInfo(expr) else None
        if isinstance(expr.callee, ast.AstVariableExpression):
            self.buildCall(useInfo, callInfo, None, expr.typeArguments, expr.arguments, mode)
        elif isinstance(expr.callee, ast.AstPropertyExpression):
            self.buildCall(useInfo, callInfo, expr.callee.receiver,
                           expr.typeArguments, expr.arguments, mode)
        else:
            raise SemanticException(expr.location, "uncallable expression")

    def visitCallThisExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr)
        self.buildCall(useInfo, callInfo, expr.callee, [], expr.arguments, mode)

    def visitCallSuperExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr)
        self.buildCall(useInfo, callInfo, expr.callee, [], expr.arguments, mode)

    def visitAstUnaryExpression(self, expr, mode):
        useInfo = self.info.getUseInfo(expr)
        callInfo = self.info.getCallInfo(expr)
        self.buildCall(useInfo, callInfo, expr.expr, [], [], mode)

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
            isCompoundAssignment = opName == useInfo.defnInfo.irDefn.name + "="
            if isCompoundAssignment:
                ty = self.info.getType(expr.right)
                receiver = self.compileLValue(expr.left, ty)
            else:
                receiver = expr.left
            self.buildCall(useInfo, callInfo, receiver, [], [expr.right], mode)

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
                self.unreachable = True
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

    def visitAstThrowExpression(self, expr, mode):
        self.visit(expr.exception, COMPILE_FOR_VALUE)
        self.throw()
        self.unreachable = True

    def visitAstTryCatchExpression(self, expr, mode):
        # Create blocks. If there is a finally handler, we need some extra logic.
        tryBlock = self.newBlock()
        catchBlock = self.newBlock()
        doneBlock = self.newBlock()
        rethrowBlock = self.newBlock()
        if expr.finallyHandler is None:
            assert expr.catchHandler is not None
            successBlock = doneBlock
            failBlock = rethrowBlock
        elif expr.catchHandler is None:
            assert expr.finallyHandler is not None
            successBlock = self.newBlock()
            failBlock = catchBlock
            finallyBlock = self.newBlock()
        else:
            successBlock = self.newBlock()
            failBlock = self.newBlock()
            finallyBlock = self.newBlock()

        # Enter the try expression.
        # Stack at this point: [...]
        self.pushtry(tryBlock.id, catchBlock.id)

        # Compile the try expression.
        # Stack at this point: [...]
        self.setCurrentBlock(tryBlock)
        with UnreachableScope(self):
            self.visit(expr.expression, mode)
            if expr.finallyHandler is not None:
                self.poptry(successBlock.id)
            else:
                self.poptry(doneBlock.id)

        # Compile the catch. When we land here, the stack is reset to the same height as the
        # beginning of the try, and the Exception object is pushed on top.
        # Stack at this point: [exception, ...]
        if expr.catchHandler is not None:
            self.setCurrentBlock(catchBlock)
            self.visitAstPartialFunctionExpression(expr.catchHandler, mode,
                                                   getExceptionClassType(),
                                                   successBlock, failBlock)

        if expr.finallyHandler is not None:
            # Compile the finally handler if there is one. Before entering this block, a
            # nullable pointer to the exception should be pushed. If the pointer is non-null,
            # the exception will be rethrown; otherwise, execution will continue normally.
            # Stack at this point: [exception, ...]
            self.setCurrentBlock(failBlock)
            if mode is COMPILE_FOR_VALUE:
                self.uninitialized()
            self.swap()
            self.branch(finallyBlock.id)

            # Stack at this point: [result, ...]
            self.setCurrentBlock(successBlock)
            self.null()
            self.branch(finallyBlock.id)

            # Stack at this point: [exception, result, ...]
            self.setCurrentBlock(finallyBlock)
            self.visit(expr.finallyHandler, COMPILE_FOR_EFFECT)
            exnTy = ClassType(getExceptionClass(), (), NULLABLE_TYPE_FLAG)
            self.dup()
            self.null()
            self.eqp()
            self.branchif(doneBlock.id, rethrowBlock.id)

            # Stack at this point: [exception, dummy-result, ...]
            self.setCurrentBlock(rethrowBlock)
            self.throw()

            # Stack at this point: [exception, result, ...]
            self.setCurrentBlock(doneBlock)
            self.drop()

        else:
            # If there is no finally handler, our lives are much easier.
            # Stack at this point: [exception, ...]
            self.setCurrentBlock(rethrowBlock)
            self.throw()

            # Stack at this point: [result, ...]
            self.setCurrentBlock(doneBlock)

    def visitAstPartialFunctionExpression(self, expr, mode, ty, doneBlock, failBlock):
        self.dup()
        typeofMethod = getRootClass().getMethod("typeof")
        self.buildCallSimpleMethod(typeofMethod, COMPILE_FOR_VALUE)
        for case in expr.cases[:-1]:
            nextBlock = self.newBlock()
            self.visitAstPartialFunctionCase(case, mode, ty, doneBlock, nextBlock)
            self.setCurrentBlock(nextBlock)
        self.visitAstPartialFunctionCase(expr.cases[-1], mode, ty, doneBlock, failBlock)
        self.setCurrentBlock(failBlock)
        self.drop()
        self.setCurrentBlock(None)

    def visitAstPartialFunctionCase(self, expr, mode, ty, doneBlock, failBlock):
        successBlock = self.newBlock()
        self.visit(expr.pattern, COMPILE_FOR_MATCH, ty, successBlock, failBlock)
        assert self.currentBlock is successBlock or self.unreachable
        if expr.condition is not None:
            self.visit(expr.condition, COMPILE_FOR_VALUE)
            successBlock = self.newBlock()
            self.branchif(successBlock.id, failBlock.id)
            self.setCurrentBlock(successBlock)
        self.drop()  # type
        self.drop()  # value
        self.visit(expr.expression, mode)
        self.branch(doneBlock.id)
        self.setCurrentBlock(None)

    def visitAstReturnExpression(self, expr, mode):
        if expr.expression is None:
            self.unit()
        else:
            self.visit(expr.expression, COMPILE_FOR_VALUE)
        self.ret()
        self.unreachable = True

    def dropForEffect(self, mode):
        if mode is COMPILE_FOR_EFFECT:
            self.drop()

    def add(self, inst):
        if self.unreachable:
            return
        self.currentBlock.instructions.append(inst)

    def newBlock(self):
        if self.unreachable:
            return ir_instructions.BasicBlock(-1, [])
        block = ir_instructions.BasicBlock(self.nextBlockId(), [])
        self.blocks.append(block)
        return block

    def setCurrentBlock(self, block):
        if self.unreachable:
            return
        self.currentBlock = block

    def compileLValue(self, expr, ty):
        useInfo = self.info.getUseInfo(expr)
        irDefn = useInfo.defnInfo.irDefn
        if LET in irDefn.flags:
            raise SemanticException(expr.location, "left side of assignment is constant")
        if isinstance(expr, ast.AstVariableExpression) and \
           (isinstance(irDefn, Variable) or \
            isinstance(irDefn, Field) or \
            isinstance(irDefn, Global)):
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
            if self.function.variables[0].name == "$this":
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
        else:
            self.ldlocal(index)
            self.visit(param.pattern, COMPILE_FOR_EFFECT)

    def compileStatements(self, scopeId, parameters, statements, mode):
        # Create a context if needed.
        if scopeId is not None and \
           not (self.astDefn.id == scopeId and
                isinstance(self.astDefn, ast.AstClassDefinition)) and \
           self.info.hasContextInfo(scopeId):
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

    def loadVariable(self, varOrDefnInfo):
        if isinstance(varOrDefnInfo, Variable):
            var = varOrDefnInfo
            self.ldlocal(var.index)
        else:
            assert isinstance(varOrDefnInfo, DefnInfo)
            defnInfo = varOrDefnInfo
            if isinstance(defnInfo.irDefn, Variable):
                self.loadVariable(defnInfo.irDefn)
            elif isinstance(defnInfo.irDefn, Field):
                self.loadContext(defnInfo.scopeId)
                self.loadField(defnInfo.irDefn)
            else:
                assert isinstance(defnInfo.irDefn, Global)
                self.ldg(defnInfo.irDefn.id)

    def storeVariable(self, varOrDefnInfo, valueType=None):
        if isinstance(varOrDefnInfo, Variable):
            var = varOrDefnInfo
            if valueType is not None and valueType != var.type:
                self.buildCast(var.type)
            self.stlocal(var.index)
        else:
            assert isinstance(varOrDefnInfo, DefnInfo)
            defnInfo = varOrDefnInfo
            if isinstance(defnInfo.irDefn, Variable):
                self.storeVariable(defnInfo.irDefn, valueType)
            elif isinstance(defnInfo.irDefn, Field):
                self.loadContext(defnInfo.scopeId)
                self.storeField(defnInfo.irDefn)
            else:
                assert isinstance(defnInfo.irDefn, Global)
                self.stg(defnInfo.irDefn.id)

    def loadContext(self, scopeId):
        closureInfo = self.info.getClosureInfo(self.getScopeAstDefn())
        loc = closureInfo.irClosureContexts[scopeId]
        if isinstance(loc, Variable):
            self.loadVariable(loc)
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

    def createContext(self, contextInfo):
        contextClass = contextInfo.irContextClass
        contextId = contextInfo.id
        assert len(contextClass.constructors) == 1
        contextCtor = contextClass.constructors[0]
        assert contextClass.typeParameters == contextCtor.typeParameters
        self.buildImplicitStaticTypeArguments(contextClass.typeParameters)
        self.allocobj(contextClass.id)
        self.dup()
        self.buildImplicitStaticTypeArguments(contextCtor.typeParameters)
        self.callg(contextCtor.id)
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
                assert len(closureClass.constructors) == 1
                closureCtor = closureClass.constructors[0]
                assert closureClass.typeParameters == closureCtor.typeParameters
                capturedScopeIds = closureInfo.capturedScopeIds()
                assert len(closureCtor.parameterTypes) == len(capturedScopeIds) + 1
                self.buildImplicitStaticTypeArguments(closureClass.typeParameters)
                self.allocobj(closureClass.id)
                self.dup()
                for id in capturedScopeIds:
                    self.loadContext(id)
                self.buildImplicitStaticTypeArguments(closureCtor.typeParameters)
                self.callg(closureCtor.id)
                self.drop()
                self.storeVariable(closureInfo.irClosureVar)
            elif isinstance(stmt, ast.AstClassDefinition):
                raise NotImplementedError

    def buildCallSimpleMethod(self, method, mode):
        index = method.clas.getMethodIndex(method)
        self.callv(len(method.parameterTypes), index)
        self.dropForEffect(mode)

    def buildCall(self, useInfo, callInfo, receiver, typeArgs, argExprs, mode):
        shouldDropForEffect = mode is COMPILE_FOR_EFFECT
        defnInfo = useInfo.defnInfo
        irDefn = defnInfo.irDefn
        closureInfo = self.info.getClosureInfo(irDefn) \
                      if self.info.hasClosureInfo(irDefn) \
                      else None
        assert isinstance(irDefn, Function)
        argCount = len(argExprs)

        def compileArgs():
            for arg in argExprs:
                self.visit(arg, COMPILE_FOR_VALUE)

        def compileTypeArgs():
            self.buildStaticTypeArguments(callInfo.typeArguments)

        if not irDefn.isConstructor() and not irDefn.isMethod():
            # Global or static function
            assert receiver is None
            compileArgs()
            compileTypeArgs()
            self.callg(irDefn.id)

        elif receiver is None and irDefn.isConstructor():
            # Constructor
            assert receiver is None
            compileTypeArgs()
            self.allocobj(irDefn.clas.id)
            if mode is COMPILE_FOR_VALUE:
                self.dup()
            compileArgs()
            compileTypeArgs()
            self.callg(irDefn.id)
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
                for instName in irDefn.insts:
                    inst = getattr(ir_instructions,instName)
                    self.add(inst())
            elif irDefn.isFinal():
                # Calls to final methods can be made directly. This includes constructors and
                # primitive methods which can't be called virtually.
                self.callg(irDefn.id)
            else:
                index = irDefn.clas.getMethodIndex(irDefn)
                self.callv(argCount + 1, index)

            if isinstance(receiver, LValue):
                self.buildAssignment(receiver, mode)
                shouldDropForEffect = False

        if shouldDropForEffect:
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

    def buildType(self, ty, location):
        assert isinstance(ty, ClassType)
        if any(STATIC in param.flags for param in ty.clas.typeParameters):
            raise SemanticException(location, "cannot match type with static parameters")
        self.allocarri(BUILTIN_TYPE_CLASS_ID, 1)
        self.dup()
        self.cls(ty.clas.id)
        self.callg(BUILTIN_TYPE_CTOR_ID)
        self.drop()

    def buildImplicitStaticTypeArguments(self, typeParams):
        for param in typeParams:
            self.tyv(param.id)

    def buildStaticTypeArguments(self, types):
        for ty in types:
            self.buildStaticTypeArgument(ty)

    def buildStaticTypeArgument(self, ty):
        if isinstance(ty, ClassType):
            for arg in ty.typeArguments:
                self.buildStaticTypeArgument(arg)
            self.tyc(ty.clas.id)
        elif isinstance(ty, VariableType):
            self.tyv(ty.typeParameter.id)
        else:
            raise NotImplementedError

    def buildCast(self, ty):
        self.buildStaticTypeArgument(ty)
        self.cast()

    def getScopeAstDefn(self):
        if isinstance(self.astDefn, ast.AstPrimaryConstructorDefinition):
            return self.function.clas.astDefn
        else:
            return self.astDefn

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


def _makeInstBuilder(inst):
    return lambda self, *operands: self.add(inst(*operands))

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

# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import ast
import compile_info
from errors import ScopeException, TypeException
from ids import GLOBAL_SCOPE_ID
import ir
import ir_types as ir_t
from builtins import getExceptionClass, getPackageClass, getNothingClass
from utils import COMPILE_FOR_VALUE, COMPILE_FOR_MATCH, COMPILE_FOR_UNINITIALIZED, COMPILE_FOR_EFFECT, each
from compile_info import USE_AS_VALUE, USE_AS_TYPE, USE_AS_PROPERTY, USE_AS_CONSTRUCTOR, NORMAL_MODE, STD_MODE, NOSTD_MODE, CallInfo, ScopePrefixInfo
from flags import COVARIANT, CONTRAVARIANT, CONSTRUCTOR, INITIALIZER, PROTECTED, PUBLIC, STATIC, ARRAY
import scope_analysis


def analyzeTypes(info):
    """Analyzes a syntax, determines a type for each node, and reports any inconsistencies."""
    # Establish type information for class supertypes, type parameter upper/lower bounds,
    # and function parameter types. This is needed for `Type.isSubtypeOf` and for typing
    # function calls in expressions.
    declarationVisitor = DeclarationTypeVisitor(info)
    declarationVisitor.visit(info.ast)
    declarationVisitor.checkTypes()

    # Resolve all function overrides. This simplifies lookups later.
    resolveAllOverrides(info)

    # Add type annotations for AST nodes which need them, and add type information to
    # the package.
    analysis = DefinitionTypeVisitor(info)
    analysis.visit(info.ast)

    # Check that each overriding function has a return type which is a subtype of the
    # overriden function. The return type is not used to make override decisions, so this needs
    # to be done after overrides are resolved.
    for func in info.package.functions:
        if func.override is not None:
            overridenReturnType = func.override.returnType.substituteForInheritance(
                func.definingClass, func.override.definingClass)
            if not func.returnType.isSubtypeOf(overridenReturnType):
                raise TypeException(func.getLocation(),
                                    "%s: return type is not subtype of overriden function" %
                                    func.name)


def patternMustMatch(pat, ty, info):
    """Returns true if a pattern will match any value of the given type. This is required for
    patterns in parameters and variable definitions. Type analysis must have already run on
    the pattern for this to work."""
    if isinstance(pat, ast.VariablePattern):
        if info.hasUseInfo(pat):
            # This pattern compares the expression to another value, rather than defining
            # a new variable.
            return False
        else:
            patTy = info.getType(pat)
            return ty.isSubtypeOf(patTy)
    elif isinstance(pat, ast.TuplePattern):
        tupleClass = info.getTupleClass(len(pat.patterns), pat.location)
        if isinstance(ty, ir_t.ClassType) and ty.clas is tupleClass:
            return all(patternMustMatch(p, ety, info)
                       for p, ety in zip(pat.patterns, ty.typeArguments))
        else:
            return False
    elif isinstance(pat, ast.BlankPattern):
        patTy = None if pat.ty is None else info.getType(pat.ty)
        return patTy is None or ty.isSubtypeOf(patTy)
    else:
        return False


def partialFunctionCaseMustMatch(case, ty, info):
    """Returns True if the partial function case must match any value of the given type.
    This is undecidable, so this actually just returns True if the pattern must match and
    there is no condition."""
    assert isinstance(case, ast.PartialFunctionCase)
    return patternMustMatch(case.pattern, ty, info) and case.condition is None


def partialFunctionMustMatch(expr, ty, info):
    """Returns True if the partial function must match any value of the given type. Currently,
    this just returns True if there is any individual case which must match, but in the future
    it may account for disjoint cases matching everything."""
    assert isinstance(expr, ast.PartialFunctionExpression)
    return any(partialFunctionCaseMustMatch(case, ty, info) for case in expr.cases)


def resolveAllOverrides(info):
    """Iterates over each `NameInfo` in each scope and resolves overrides. This determines
    whether methods with the same name as inherited methods overload or override."""
    for scope in info.scopes.values():
        scope.resolveOverrides()


def typeCanBeTested(ty, existentialVarIds=None):
    """Returns whether a type can be tested at runtime, for example with a `cast` or `castcbr`
    instruction.

    Type declaration must be complete before this is called."""
    if existentialVarIds is None:
        existentialVarIds = frozenset()

    if isinstance(ty, ir_t.ClassType):
        for tp, ta in zip(ty.clas.typeParameters, ty.typeArguments):
            staticArgCanBeTested = STATIC in tp.flags and \
                                   isinstance(ta, ir_t.VariableType) and \
                                   ta.typeParameter.id in existentialVarIds
            dynamicArgCanBeTested = STATIC not in tp.flags and \
                                    typeCanBeTested(ta, existentialVarIds)
            if not staticArgCanBeTested and not dynamicArgCanBeTested:
                return False
        return True
    elif isinstance(ty, ir_t.VariableType):
        return STATIC not in ty.typeParameter.flags
    elif isinstance(ty, ir_t.ExistentialType):
        return typeCanBeTested(ty.ty, existentialVarIds | frozenset(v.id for v in ty.variables))
    else:
        # Primitive types cannot be tested.
        return False


class TypeVisitorBase(ast.NodeVisitor):
    """Provides common functionality for type visitors, namely the visitor functions for the
    various ast.Type subclasses."""
    def __init__(self, info):
        self.info = info

        # Stack containing the current scope. Used for lookups.
        self.scopeStack = []

        # Stack containing the current implicit receiver type. Used for building method
        # parameter types and for lookups. Entries may be `None` for scopes that don't have
        # an implicit receiver type.
        self.receiverTypeStack = []

    def scope(self):
        return self.scopeStack[-1]

    def hasReceiverType(self):
        return len(self.receiverTypeStack) > 0 and self.receiverTypeStack[-1] is not None

    def getReceiverType(self):
        assert self.hasReceiverType()
        return self.receiverTypeStack[-1]

    def preVisit(self, node, *unusedArgs, **unusedKwargs):
        if self.info.hasScope(node.id):
            scope = self.info.getScope(node.id)
            self.scopeStack.append(scope)
            enclosingClass = scope.findEnclosingClass()
            if enclosingClass is not None:
                receiverType = ir_t.ClassType.forReceiver(enclosingClass)
                self.receiverTypeStack.append(receiverType)
            else:
                self.receiverTypeStack.append(None)

    def postVisit(self, node, *unusedArgs, **unusedKwargs):
        if self.info.hasScope(node.id):
            assert self.scopeStack[-1] is self.info.getScope(node.id)
            self.scopeStack.pop()
            self.receiverTypeStack.pop()

    def visitUnitType(self, node):
        return ir_t.UnitType

    def visitBooleanType(self, node):
        return ir_t.BooleanType

    def visitI8Type(self, node):
        return ir_t.I8Type

    def visitI16Type(self, node):
        return ir_t.I16Type

    def visitI32Type(self, node):
        return ir_t.I32Type

    def visitI64Type(self, node):
        return ir_t.I64Type

    def visitF32Type(self, node):
        return ir_t.F32Type

    def visitF64Type(self, node):
        return ir_t.F64Type

    def visitClassType(self, node):
        scope, prefixTypeArgs = self.handleScopePrefix(node.prefix)
        hasPrefix = len(node.prefix) > 0
        nameInfo = scope.lookup(node.name, node.location, fromExternal=hasPrefix)
        if nameInfo.isOverloaded():
            raise TypeException(node.location, "%s: not a type definition" % node.name)
        defnInfo = nameInfo.getDefnInfo()
        irDefn = defnInfo.irDefn
        if not isinstance(irDefn, ir.IrDefinition) or not irDefn.isTypeDefn():
            raise TypeException(node.location, "%s: not a type definition" % node.name)
        self.scope().use(defnInfo, node.id, USE_AS_TYPE, node.location)

        flags = frozenset(map(astTypeFlagToIrTypeFlag, node.flags))

        if isinstance(irDefn, ir.Class):
            explicitTypeParams = ir.getExplicitTypeParameters(irDefn)
            if len(node.typeArguments) != len(explicitTypeParams):
                raise TypeException(node.location,
                                    "%s: wrong number of type arguments; expected %d but have %d" %
                                    (node.name,
                                     len(explicitTypeParams),
                                     len(node.typeArguments)))
            if not hasPrefix:
                # If there is no prefix, the class is defined in a parent scope. There may be
                # some type parameters
                assert len(prefixTypeArgs) == 0
                implicitTypeParams = ir.getImplicitTypeParameters(irDefn)
                prefixTypeArgs = map(ir_t.VariableType, implicitTypeParams)
            explicitTypeArgs, existentialTypeParams = \
                self.handleClassTypeArgs(irDefn, node.typeArguments)
            typeArgs = tuple(prefixTypeArgs + list(explicitTypeArgs))
            ty = ir_t.ClassType(irDefn, typeArgs, flags)
            if len(existentialTypeParams) > 0:
                ty = ir_t.ExistentialType(existentialTypeParams, ty)
            return ty
        else:
            assert isinstance(irDefn, ir.TypeParameter)
            if len(node.typeArguments) > 0:
                raise TypeException(node.location,
                                    "%s: variable type does not accept type arguments" %
                                    node.name)
            return ir_t.VariableType(irDefn, flags)

    def visitTupleType(self, node):
        clas = self.info.getTupleClass(len(node.types), node.location)
        types, existentialTypeParams = self.handleClassTypeArgs(clas, node.types)
        flags = frozenset(map(astTypeFlagToIrTypeFlag, node.flags))
        ty = ir_t.ClassType(clas, types, flags)
        if len(existentialTypeParams) > 0:
            ty = ir_t.ExistentialType(existentialTypeParams, ty)
        return ty

    def visitBlankType(self, node):
        raise TypeException(node.location, "blank type can only be used as a type argument")

    def visitExistentialType(self, node):
        each(self.visit, node.typeParameters)
        variables = tuple(self.info.getDefnInfo(v).irDefn for v in node.typeParameters)
        innerType = self.visit(node.type)
        return ir_t.ExistentialType(variables, innerType)

    def visitIntegerLiteral(self, node):
        typeMap = { 8: ir_t.I8Type, 16: ir_t.I16Type, 32: ir_t.I32Type, 64: ir_t.I64Type }
        if node.width not in typeMap:
            raise TypeException(node.location, "invalid integer literal width: %d" % node.width)
        minValue = -2 ** (node.width - 1)
        maxValue = 2 ** (node.width - 1) - 1
        if node.value < minValue or maxValue < node.value:
            raise TypeException(node.location,
                                "interger literal value %d does not fit in %d bits" %
                                (node.value, node.width))
        return typeMap[node.width]

    def visitFloatLiteral(self, node):
        typeMap = { 32: ir_t.F32Type, 64: ir_t.F64Type }
        if node.width not in typeMap:
            raise TypeException(node.location, "invalid float literal width: %d" % node.width)
        return typeMap[node.width]

    def visitStringLiteral(self, node):
        return ir_t.getStringType()

    def visitBooleanLiteral(self, node):
        return ir_t.BooleanType

    def visitNullLiteral(self, node):
        return ir_t.getNullType()

    def visitLvalue(self, node):
        if isinstance(node, ast.PropertyExpression):
            return self.visit(node, isLvalue=True)
        else:
            return self.visit(node)

    def isPrefixNode(self, node):
        return isinstance(node, ast.VariableExpression) or \
               isinstance(node, ast.PropertyExpression) or \
               (isinstance(node, ast.CallExpression) and \
                node.arguments is None and \
                self.isPrefixNode(node.callee))

    def visitPossiblePrefix(self, node):
        if self.isPrefixNode(node):
            return self.visit(node, mayBePrefix=True)
        else:
            return self.visit(node)

    def handleScopePrefix(self, prefix):
        """Processes the components of a scope prefix (a list of ast.ScopePrefixComponent). This
        looks up each component in the list and checks type arguments (using
        handleClassTypeArgs). Returns a scope and a list of Types, which will be implicit
        type arguments for whatever is after the prefix."""
        scope = self.scope()
        typeArgs = []
        hasPrefix = False
        for component in prefix:
            # Look up the next component.
            nameInfo = scope.lookup(component.name, component.location, fromExternal=hasPrefix)
            if not nameInfo.isScope():
                raise TypeException(component.location,
                                    "%s: does not refer to a scope" % component.name)
            defnInfo = nameInfo.getDefnInfo()
            irDefn = defnInfo.irDefn

            # Check type arguments.
            if isinstance(irDefn, ir.Class):
                explicitTypeParams = ir.getExplicitTypeParameters(irDefn)
                astTypeArgs = component.typeArguments \
                              if component.typeArguments is not None \
                              else []
                if len(astTypeArgs) != len(explicitTypeParams):
                    raise TypeException(component.location,
                                        "%s: wrong number of type arguments; expected %d but have %d\n" %
                                        (component.name,
                                         len(explicitTypeParams),
                                         len(astTypeArgs)))
                for astTypeArg in astTypeArgs:
                    if isinstance(astTypeArg, ast.BlankType):
                        raise TypeException(astTypeArg.location,
                                            "blank type not allowed as scope prefix type argument")
                if not hasPrefix:
                    # If this is the first prefix, the class is defined in a parent scope.
                    # There may be some type parameters defined in one of its parent scopes
                    # which are implied here.
                    implicitTypeParams = ir.getImplicitTypeParameters(irDefn)
                    implicitTypeArgs = map(ir_t.VariableType, implicitTypeParams)
                    typeArgs.extend(implicitTypeArgs)
                explicitTypeArgs, existentialTypeParams = \
                    self.handleClassTypeArgs(irDefn, astTypeArgs)
                assert len(existentialTypeParams) == 0
                typeArgs.extend(explicitTypeArgs)
            else:
                if component.typeArguments is not None:
                    raise TypeException(component.location,
                                        "%s: non-type definition does not accept type arguments" %
                                        component.name)

            # Find the next scope.
            if isinstance(irDefn, ir.PackagePrefix):
                scope = self.info.getScope(defnInfo.scopeId).scopeForPrefix(component.name,
                                                                            component.location)
            else:
                scope = self.info.getScope(irDefn)
            hasPrefix = True

        return scope, typeArgs

    def handleClassTypeArgs(self, irClass, nodes):
        """Builds and checks explicit type arguments for the given class.

        Builds type arguments from `nodes`. During the declaration stage, bounds checking is
        deferred until all definitions have been visited (`Type.isSubtypeOf` doesn't work
        until then). During the definition stage, bounds checking is performed immediately.
        For each `BlankType` in `nodes`, a new `TypeParameter` is introduced (for use in
        an `ExistentialType`).

        Note that this function doesn't return a full list of type arguments. The caller is
        responsible for building implicit type arguments and including them at the beginning
        of any type argument list.

        Args:
            irClass (ir.Class): the class we are building type arguments for
            nodes (list(ast.Type)): a list of explicit type arguments in the AST. The length
                must equal the number of explicit type parameters in `irClass`.

        Returns:
            (tuple(ir.Type), tuple(ir.TypeParameter)): A list of explicit type arguments
            compiled from `nodes`, and a list of introduced parameters for an existential
            type."""
        raise NotImplementedError

    def introduceExistentialTypeParameter(self, param, node):
        """Introduces an existential type parameter based on a class type parameter.

        This method may be called multiple times. A new type parameter will only be added the
        first time. After that, the same type parameter will be returned.

        Args:
            param (ir.TypeParameter): a type parameter belonging to a class. The returned
                type parameter will have the same bounds.
            node (ast.BlankType): the node which causes this type parameter to be added.
                `DefnInfo` will be created (if it doesn't already exist) with this node's id
                as the key. The node's location may also be used for error reporting.

        Returns:
            (ir.TypeParameter, ir.VariableType): the introduced type parameter and a variable
            type which wraps it.
        """
        assert isinstance(node, ast.BlankType)
        if not self.info.hasDefnInfo(node):
            paramName = ir.Name(self.scope().prefix + [ir.EXISTENTIAL_SUFFIX, ir.BLANK_SUFFIX])
            blankParam = self.info.package.addTypeParameter(paramName, astDefn=node,
                                                            upperBound=param.upperBound,
                                                            lowerBound=param.lowerBound)
            defnInfo = compile_info.DefnInfo(blankParam, self.scope().scopeId, isVisible=False)
            self.info.setDefnInfo(node, defnInfo)
        else:
            blankParam = self.info.getDefnInfo(node).irDefn
        return blankParam, ir_t.VariableType(blankParam)

    def handleResult(self, node, result, *unusedArgs, **unusedKwargs):
        if result is not None:
            assert not self.info.hasType(node) or self.info.getType(node) == result
            self.info.setType(node, result)
        return result


class DeclarationTypeVisitor(TypeVisitorBase):
    """Analyzes functions, classes, and type parameters and saves supertypes, upper bounds,
    lower bounds, and parameter types. The analysis proceeds in lexical order over the AST.
    This must be done before we can start typing expressions, because we need a fully
    functional Type.isSubtypeOf method, which relies on this information. We use isSubtypeOf
    here, too, but only on definitions we've already processed. This is guaranteed to
    terminate, since we checked the subtype graph for cycles in an earlier phase."""

    def __init__(self, info):
        super(DeclarationTypeVisitor, self).__init__(info)

        # List of type parameters to check after the AST traversal. We want to ensure that
        # lower bounds are subtypes of upper bounds. This is not needed to ensure termination,
        # since we checked the subtype graph for cycles during inheritance analysis. We are
        # mainly concerned with bounds that are unrelated, e.g., sibling classes or variable
        # types that may overlap.
        self.typeParamsToCheck = []

        # List of type arguments to bounds-check after the AST traversal. We have to defer
        # checking these because `Type.isSubtypeOf` won't work until the traversal is complete.
        self.typeArgsToCheck = []

    def checkTypes(self):
        """Called after analyzing all declrations in the AST to verify that type arguments are
        in bounds and all type parameters have correct bounds. While traversing the AST, we
        make a list of type arguments and parameters to check, and we check everything in that
        list here."""
        for tp, loc in self.typeParamsToCheck:
            if not tp.lowerBound.isSubtypeOf(tp.upperBound):
                raise TypeException(loc,
                                    "%s: lower bound is not subtype of upper bound" % tp.name)

        for ta, tp, loc in self.typeArgsToCheck:
            if not ta.isSubtypeOf(tp.upperBound) or \
               not tp.lowerBound.isSubtypeOf(ta):
                raise TypeException(loc, "%s: type argument is not in bounds" % tp.name)

    def visitPackage(self, node):
        self.visitChildren(node)

    def visitModule(self, node):
        self.visitChildren(node)

    def visitFunctionDefinition(self, node):
        irFunction = self.info.getDefnInfo(node).irDefn
        if irFunction.isMethod():
            self.setMethodReceiverType(irFunction)
        for param in node.typeParameters:
            self.visit(param)
        irFunction.parameterTypes = []
        if irFunction.isMethod():
            irFunction.parameterTypes.append(self.getReceiverType())
        irFunction.parameterTypes.extend(map(self.visit, node.parameters))
        if node.body is not None:
            self.visit(node.body)

    def visitPrimaryConstructorDefinition(self, node):
        irFunction = self.info.getDefnInfo(node).irDefn
        irFunction.parameterTypes = [self.getReceiverType()] + map(self.visit, node.parameters)
        self.setMethodReceiverType(irFunction)

    def visitClassDefinition(self, node):
        irClass = self.info.getDefnInfo(node).irDefn
        for param in node.typeParameters:
            self.visit(param)
        if node.constructor is not None:
            self.visit(node.constructor)
        if node.supertype is None:
            irClass.supertypes = [ir_t.getRootClassType()]
        else:
            supertype = self.visit(node.supertype)
            if supertype.isNullable():
                raise TypeException(node.location,
                                    "%s: supertype may not be nullable" % node.name)
            if supertype == ir_t.getNothingClassType():
                raise TypeException(node.location,
                                    "%s: Nothing cannot be a supertype" % node.name)
            irClass.supertypes = [supertype]
        if node.superArgs is not None:
            for arg in node.superArgs:
                self.visit(arg)
        for member in node.members:
            self.visit(member)
        if not node.hasConstructors():
            defaultCtor = irClass.constructors[0]
            self.setMethodReceiverType(defaultCtor)
            defaultCtor.parameterTypes = [self.getReceiverType()]
        self.setMethodReceiverType(irClass.initializer)
        irClass.initializer.parameterTypes = [self.getReceiverType()]

    def visitArrayElementsStatement(self, node):
        elementType = self.visit(node.elementType)
        receiverType = self.getReceiverType()
        irClass = self.scope().getIrDefn()
        assert isinstance(irClass, ir.Class) and ARRAY in irClass.flags
        irClass.elementType = elementType

        getMethod = self.info.getDefnInfo(node.getDefn).irDefn
        getMethod.returnType = elementType
        getMethod.parameterTypes = [receiverType, ir_t.I32Type]
        getMethod.variables[0].type = receiverType
        getMethod.compileHint = compile_info.ARRAY_ELEMENT_GET_HINT

        setMethod = self.info.getDefnInfo(node.setDefn).irDefn
        setMethod.returnType = ir_t.UnitType
        setMethod.parameterTypes = [receiverType, ir_t.I32Type, elementType]
        setMethod.variables[0].type = receiverType
        setMethod.compileHint = compile_info.ARRAY_ELEMENT_SET_HINT
        if any(a.name == "final" for a in node.attribs):
            setMethod.flags |= frozenset([INITIALIZER])

        lengthMethod = self.info.getDefnInfo(node.lengthDefn).irDefn
        lengthMethod.returnType = ir_t.I32Type
        lengthMethod.parameterTypes = [receiverType]
        lengthMethod.variables[0].type = receiverType
        lengthMethod.compileHint = compile_info.ARRAY_ELEMENT_LENGTH_HINT

    def visitImportStatement(self, node):
        scope, typeArgs = self.handleScopePrefix(node.prefix)
        importedDefnInfos = self.info.getImportInfo(node).importedDefnInfos
        for defnInfo in importedDefnInfos:
            defnInfo.importedTypeArguments = typeArgs

    def visitTypeParameter(self, node):
        irParam = self.info.getDefnInfo(node).irDefn

        def visitBound(bound, default):
            if bound is None:
                return default
            else:
                ty = self.visit(bound)
                if not ty.mayUseAsBound():
                    raise TypeException(bound.location,
                                        "type may not be used as a bound")
                return ty

        self.scope().define(node.name)

        irParam.upperBound = visitBound(node.upperBound, ir_t.getRootClassType())
        irParam.lowerBound = visitBound(node.lowerBound, ir_t.getNothingClassType())
        self.typeParamsToCheck.append((irParam, node.location))

    def visitParameter(self, node):
        patTy = self.visit(node.pattern, True)
        if not patternMustMatch(node.pattern, patTy, self.info):
            raise TypeException(node.location,
                                "patterns which might not match can't be used as parameters")
        return patTy

    def visitVariablePattern(self, node, isParam=False):
        if isParam and node.ty is None:
            raise TypeException(node.location, "%s: type not specified" % node.name)
        return self.visit(node.ty) if node.ty is not None else None

    def visitBlankPattern(self, node, isParam=False):
        if isParam and node.ty is None:
            raise TypeException(node.location, "type not specified")
        return self.visit(node.ty) if node.ty is not None else None

    def visitLiteralPattern(self, node, isParam=False):
        return self.visit(node.literal)

    def visitTuplePattern(self, node, isParam=False):
        patternTypes = tuple(self.visit(p, True) for p in node.patterns)
        tupleClass = self.info.getTupleClass(len(node.patterns), node.location)
        return ir_t.ClassType(tupleClass, patternTypes)

    def visitValuePattern(self, node, isParam=False):
        # Need to raise this early, since patternMustMatch is only called after a type is
        # returned. We may not be able to determine the type of this pattern, since it can
        # involve function calls as part of the scope prefix.
        if isParam:
            raise TypeException(node.location, "value pattern can't be used in a parameter")

    def visitDestructurePattern(self, node, isParam=False):
        # Need to raise this early, since patternMustMatch is only called after a type
        # is returned.
        if isParam:
            raise TypeException(node.location,
                                "destructure pattern can't be used in a parameter")

    def visitUnaryPattern(self, node, isParam=False):
        # Need to raise this early, since patternMustMatch is only called after a type
        # is returned.
        if isParam:
            raise TypeException(node.location, "unary pattern can't be used in a parameter")

    def visitBinaryPattern(self, node, isParam=False):
        # Need to raise this early, since patternMustMatch is only called after a type
        # is returned.
        if isParam:
            raise TypeException(node.location, "binary pattern can't be used in a parameter")

    def visitDefault(self, node):
        self.visitChildren(node)

    def handleClassTypeArgs(self, irClass, nodes):
        typeParams = ir.getExplicitTypeParameters(irClass)
        assert len(typeParams) == len(nodes)
        typeArgs = []
        introducedTypeParams = []
        for param, node in zip(typeParams, nodes):
            if isinstance(node, ast.BlankType):
                introducedTypeParam, arg = self.introduceExistentialTypeParameter(param, node)
                introducedTypeParams.append(introducedTypeParam)
            else:
                arg = self.visit(node)
                self.typeArgsToCheck.append((arg, param, node.location))
            typeArgs.append(arg)
        return tuple(typeArgs), tuple(introducedTypeParams)

    def setMethodReceiverType(self, irFunction):
        assert irFunction.variables[0].name.short() == ir.RECEIVER_SUFFIX
        irFunction.variables[0].type = self.getReceiverType()


class DefinitionTypeVisitor(TypeVisitorBase):
    """Records type information for every node that requires it, including expression nodes,
    pattern nodes, and type nodes. Type information is saved in `CompileInfo.typeInfo` (keyed
    by `AstId`). It is also added to the relevant places in the IR, including function
    return types, fields, variables, etc. Note that unlike `DeclarationTypeVisitor`, this
    analysis does not traverse the AST in order. When a function with no explicit return type
    is called in an expression, this visitor jumps to the function definition to determine the
    return type so the call expression can be typed."""
    def __init__(self, info):
        super(DefinitionTypeVisitor, self).__init__(info)

        # functionStack keeps track of the function we're currently analyzing. It contains
        # FunctionState for functions or None if we're analyzing something that is not a
        # function (like a class). This is used to resolve return types and to detect recursion.
        self.functionStack = []

        # varianceClass indicates what class is being visited, for the purpose of restricting
        # variance for type parameters. May be `None` when variance doesn't matter.
        self.varianceClass = None

        # variance restricts which type parameters can be used. May be one of:
        # COVARIANT: covariant or invariant type parameters may be used
        # CONTRAVARIANT: contravariant or invariant type parameters may be used
        # INVARIANT: invariant type parameters may be used
        # BIVARIANT: any type parameters may be used
        # Type parameters from outer scopes are not affected by this restriction; only type
        # parameters defined in `varianceClass` are restricted.
        self.variance = ir_t.BIVARIANT

    def preVisit(self, node, *args, **kwargs):
        super(DefinitionTypeVisitor, self).preVisit(node, *args, **kwargs)
        if not self.info.hasDefnInfo(node):
            return
        irDefn = self.info.getDefnInfo(node).irDefn
        if isinstance(irDefn, ir.Class):
            self.functionStack.append(None)
        elif isinstance(irDefn, ir.Function):
            self.functionStack.append(FunctionState(irDefn))

    def postVisit(self, node, *args, **kwargs):
        super(DefinitionTypeVisitor, self).postVisit(node, *args, **kwargs)
        if not self.info.hasDefnInfo(node):
            return
        irDefn = self.info.getDefnInfo(node).irDefn
        if isinstance(irDefn, ir.Class) or isinstance(irDefn, ir.Function):
            self.functionStack.pop()

    def visitPackage(self, node):
        self.visitChildren(node)

    def visitModule(self, node):
        self.visitChildren(node)

    def visitVariableDefinition(self, node):
        if node.expression is not None:
            exprTy = self.visit(node.expression)
            mode = COMPILE_FOR_EFFECT
        else:
            exprTy = None
            mode = COMPILE_FOR_UNINITIALIZED
        self.visit(node.pattern, exprTy, mode)
        assert exprTy is None or patternMustMatch(node.pattern, exprTy, self.info)

    def visitFunctionDefinition(self, node):
        self.handleFunctionCommon(node, node.returnType, node.body)
        irDefn = self.info.getDefnInfo(node).irDefn
        if self.isExternallyVisible(irDefn):
            self.checkPublicType(irDefn.returnType, node.name, node.location)
            each(lambda p: self.checkPublicTypeParameter(p, node.name, node.location),
                 irDefn.typeParameters)
            each(lambda ty: self.checkPublicType(ty, node.name, node.location),
                 irDefn.parameterTypes)

    def visitClassDefinition(self, node):
        irClass = self.info.getDefnInfo(node).irDefn
        thisType = ir_t.ClassType.forReceiver(irClass)

        for typeParam in node.typeParameters:
            self.visit(typeParam)

        if node.constructor is not None:
            self.visit(node.constructor)

        if not node.hasConstructors():
            assert len(irClass.constructors) == 1
            irDefaultCtor = irClass.constructors[0]
            irDefaultCtor.parameterTypes = [thisType]
            irDefaultCtor.variables[0].type = thisType
            irDefaultCtor.returnType = ir_t.UnitType

        hasPrimaryOrDefaultCtor = node.constructor is not None or \
                                  not node.hasConstructors()
        if node.superArgs is not None and not hasPrimaryOrDefaultCtor:
            raise TypeException(node.location,
                                ("%s: called superconstructor from class definition, but there "
                                 "is no primary or default constructor"))
        if node.superArgs is not None or hasPrimaryOrDefaultCtor:
            supertype = irClass.supertypes[0]
            superArgTypes = map(self.visit, node.superArgs) \
                            if node.superArgs is not None \
                            else []
            superScope = self.info.getScope(ir_t.getClassFromType(supertype))
            self.handlePropertyCall(ir.CONSTRUCTOR_SUFFIX, superScope, supertype,
                                    None, superArgTypes, True, True, False,
                                    node.id, node.location)

        irInitializer = irClass.initializer
        irInitializer.parameterTypes = [thisType]
        irInitializer.variables[0].type = thisType
        irInitializer.returnType = ir_t.UnitType

        for member in node.members:
            if isinstance(member, ast.VariableDefinition):
                variance = ir_t.INVARIANT if member.keyword == "var" else COVARIANT
                with VarianceScope(self, variance, irClass):
                    self.visit(member)
            else:
                self.visit(member)

        if self.isExternallyVisible(irClass):
            each(lambda p: self.checkPublicTypeParameter(p, node.name, node.location),
                 irClass.typeParameters)
            if node.constructor is not None:
                ctor = self.info.getDefnInfo(node.constructor).irDefn
                each(lambda arg: self.checkPublicType(arg, node.name, node.location),
                     ctor.parameterTypes)
            each(lambda ty: self.checkPublicType(ty, node.name, node.location),
                 irClass.supertypes)

    def visitPrimaryConstructorDefinition(self, node):
        self.handleFunctionCommon(node, None, None)

    def visitArrayElementsStatement(self, node):
        # The array element type is either covariant or invariant, depending on whether the
        # elements are immutable.
        variance = COVARIANT \
                   if any(a.name == "final" for a in node.attribs) \
                   else ir_t.INVARIANT
        with VarianceScope(self, variance, self.scope().getIrDefn()):
            self.visit(node.elementType)

    def visitImportStatement(self, node):
        # TypeDeclarationVisitor does all the work.
        pass

    def visitTypeParameter(self, node):
        # TypeDeclarationVisitor does all the work, including finding the types of the bounds.
        # We don't need to do anything here.
        pass

    def visitParameter(self, node):
        if self.variance is COVARIANT and node.var == "var":
            # A `var` parameter in a primary constructor. Since this defines a mutable field,
            # we have to go invariant.
            variance = ir_t.INVARIANT
        else:
            variance = self.variance
        with VarianceScope(self, variance):
            ty = self.visit(node.pattern, None, COMPILE_FOR_EFFECT)
        if self.info.hasDefnInfo(node):
            self.info.getDefnInfo(node).irDefn.type = ty
        return ty

    def visitVariablePattern(self, node, exprTy, mode):
        scope = self.scope()
        isShadow = mode is COMPILE_FOR_MATCH and scope.isShadow(node.name)
        if node.ty is not None:
            if isShadow:
                raise TypeException(node.location,
                                    "%s: pattern that refers to another definition cannot have a type" %
                                    node.name)
            varTy = self.visit(node.ty)
        else:
            if isShadow:
                # Inside a match, if we refer to a definition in an outside scope, we don't
                # define a new definition to shadow it; we check if the expression equals it,
                # which means that we treat this as a variable use. We can't detect this in
                # declaration analysis without making an extra pass.
                scope.deleteVar(node.name)
                varTy = self.handleSimpleVariable(node.name, node.id, node.location)
            else:
                varTy = None

        patTy = self.findPatternType(varTy, exprTy, mode, isShadow, node.name, node.location)

        if not isShadow:
            irDefn = self.info.getDefnInfo(node).irDefn
            if self.isExternallyVisible(irDefn):
                self.checkPublicType(patTy, node.name, node.location)
            irDefn.type = patTy
            scope.define(node.name)
        return patTy

    def visitBlankPattern(self, node, exprTy, mode):
        blankTy = self.visit(node.ty) if node.ty is not None else None
        patTy = self.findPatternType(blankTy, exprTy, mode, False, None, node.location)
        return patTy

    def visitLiteralPattern(self, node, exprTy, mode):
        litTy = self.visit(node.literal)
        patTy = self.findPatternType(litTy, exprTy, mode, False, None, node.location)
        return patTy

    def visitTuplePattern(self, node, exprTy, mode):
        tupleClass = self.info.getTupleClass(len(node.patterns), node.location)
        if isinstance(exprTy, ir_t.ClassType) and exprTy.clas is tupleClass:
            elementTypes = tuple(self.visit(p, ety, mode)
                                 for p, ety in zip(node.patterns, exprTy.typeArguments))
        else:
            elementTypes = tuple(self.visit(p, ir_t.getRootClassType(), mode)
                                 for p in node.patterns)
        tupleTy = ir_t.ClassType(tupleClass, elementTypes)
        patTy = self.findPatternType(tupleTy, exprTy, mode, True, None, node.location)
        return patTy

    def visitValuePattern(self, node, exprTy, mode):
        scope, receiverType = self.handlePatternScopePrefix(node.prefix)
        if len(node.prefix) > 0:
            hasReceiver = not self.info.hasScopePrefixInfo(node.prefix[-1])
            patTy = self.handlePropertyCall(node.name, scope, receiverType, None, None,
                                            hasReceiver, False, False, node.id, node.location)
        else:
            patTy = self.handleUnprefixedCall(node.name, None, None, node.id, node.location)
        return patTy

    def visitDestructurePattern(self, node, exprTy, mode):
        last = node.prefix[-1]
        if len(node.prefix) == 1:
            receiverType = self.getReceiverType() if self.hasReceiverType() else None
            nameInfo = self.scope().lookupFromSelf(last.name, last.location)
            receiverIsExplicit = False
        else:
            scope, receiverType = self.handlePatternScopePrefix(node.prefix[:-1])
            nameInfo = scope.lookupFromExternal(last.name, last.location)
            receiverIsExplicit = receiverType is not None
        typeArgs = map(self.visit, last.typeArguments) \
                   if last.typeArguments is not None \
                   else None
        return self.handleDestructure(nameInfo, receiverType, receiverIsExplicit, typeArgs,
                                      exprTy, node.patterns, mode,
                                      last.id, node.id, node.location)

    def visitUnaryPattern(self, node, exprTy, mode):
        receiverType = self.getReceiverType() if self.hasReceiverType() else None
        nameInfo = self.scope().lookupFromSelf(node.operator, node.location)
        return self.handleDestructure(nameInfo, receiverType, False, None,
                                      exprTy, [node.pattern], mode,
                                      node.id, node.matcherId, node.location)

    def visitBinaryPattern(self, node, exprTy, mode):
        receiverType = self.getReceiverType() if self.hasReceiverType() else None
        nameInfo = self.scope().lookupFromSelf(node.operator, node.location)
        return self.handleDestructure(nameInfo, receiverType, False, None,
                                      exprTy, [node.left, node.right], mode,
                                      node.id, node.matcherId, node.location)

    def visitLiteralExpression(self, node):
        ty = self.visit(node.literal)
        return ty

    def visitVariableExpression(self, node, mayBePrefix=False):
        if mayBePrefix:
            ty = self.handlePossiblePrefixSymbol(node.name, None, None, None,
                                                 node.id, node.location)
        else:
            ty = self.handleSimpleVariable(node.name, node.id, node.location)
        return ty

    def visitThisExpression(self, node):
        scope = self.scope()
        nameInfo = scope.lookupFromSelf("this", node.location, mayBeAssignment=False)
        defnInfo = nameInfo.getDefnInfo()
        scope.use(defnInfo, node.id, USE_AS_VALUE, node.location)
        ty = defnInfo.irDefn.type
        return ty

    def visitSuperExpression(self, node):
        scope = self.scope()
        nameInfo = scope.lookupFromSelf("this", node.location, mayBeAssignment=False)
        defnInfo = nameInfo.getDefnInfo()
        scope.use(defnInfo, node.id, USE_AS_VALUE, node.location)
        thisType = defnInfo.irDefn.type
        assert isinstance(thisType, ir_t.ClassType) and \
               len(thisType.clas.supertypes) > 0
        superType = thisType.clas.supertypes[0]
        return superType

    def visitBlockExpression(self, node):
        lastTy = ir_t.UnitType
        for stmt in node.statements:
            stmtTy = self.visit(stmt)
            assert stmtTy is not None or isinstance(stmt, ast.Definition)
            lastTy = stmtTy if stmtTy else ir_t.UnitType
        return lastTy

    def visitAssignExpression(self, node):
        rightTy = self.visit(node.right)
        leftTy = self.visitLvalue(node.left)
        if not rightTy.isSubtypeOf(leftTy):
            raise TypeException(node.location,
                                "for assignment, expected %s but was %s" %
                                (str(leftTy), str(rightTy)))
        return leftTy

    def visitPropertyExpression(self, node, mayBePrefix=False, isLvalue=False):
        receiverType = self.visitPossiblePrefix(node.receiver)
        if self.info.hasScopePrefixInfo(node.receiver):
            receiverScope = self.info.getScope(
                self.info.getScopePrefixInfo(node.receiver).scopeId)
            hasReceiver = False
        else:
            receiverScope = self.info.getScope(ir_t.getClassFromType(receiverType))
            hasReceiver = True

        if mayBePrefix and not hasReceiver:
            receiverType = self.info.getType(node.receiver)
            ty = self.handlePossiblePrefixSymbol(node.propertyName, receiverScope, receiverType,
                                                 None, node.id, node.location)
        else:
            receiverIsReceiver = isinstance(node.receiver, ast.ThisExpression) or \
                                 isinstance(node.receiver, ast.SuperExpression)
            ty = self.handlePropertyCall(node.propertyName, receiverScope, receiverType,
                                         None, None, hasReceiver, receiverIsReceiver,
                                         isLvalue, node.id, node.location)
        return ty

    def visitCallExpression(self, node, mayBePrefix=False):
        hasTypeArgs = node.typeArguments is not None
        typeArgs = map(self.visit, node.typeArguments) if hasTypeArgs else None
        hasArgs = node.arguments is not None
        argTypes = map(self.visit, node.arguments) if hasArgs else None
        if hasArgs:
            mayBePrefix = False

        if isinstance(node.callee, ast.VariableExpression):
            if mayBePrefix:
                ty = self.handlePossiblePrefixSymbol(node.callee.name, None, None, typeArgs,
                                                     node.id, node.location)
            else:
                ty = self.handleUnprefixedCall(node.callee.name, typeArgs, argTypes,
                                               node.id, node.location)
        elif isinstance(node.callee, ast.PropertyExpression):
            receiverType = self.visitPossiblePrefix(node.callee.receiver)
            if self.info.hasScopePrefixInfo(node.callee.receiver):
                receiverScope = self.info.getScope(
                    self.info.getScopePrefixInfo(node.callee.receiver).scopeId)
                hasReceiver = False
            else:
                receiverScope = self.info.getScope(ir_t.getClassFromType(receiverType))
                hasReceiver = True
            if mayBePrefix and not hasReceiver:
                ty = self.handlePossiblePrefixSymbol(node.callee.propertyName,
                                                     receiverScope, receiverType,
                                                     typeArgs, node.id,
                                                     node.location)
            else:
                ty = self.handlePropertyCall(node.callee.propertyName, receiverScope,
                                             receiverType, typeArgs, argTypes,
                                             hasReceiver, False, False, node.id, node.location)
        elif isinstance(node.callee, ast.ThisExpression) or \
             isinstance(node.callee, ast.SuperExpression):
            receiverType = self.visit(node.callee)
            receiverScope = self.info.getScope(ir_t.getClassFromType(receiverType))
            self.handlePropertyCall(ir.CONSTRUCTOR_SUFFIX, receiverScope, receiverType,
                                    typeArgs, argTypes, True, True, False,
                                    node.id, node.location)
            ty = ir_t.UnitType
        else:
            # TODO: callable expression
            raise NotImplementedError
        return ty

    def visitNewArrayExpression(self, node):
        indexType = self.visit(node.length)
        if indexType != ir_t.I32Type:
            raise TypeException(node.location, "array length must be i32")
        objectType = self.visit(node.ty)
        if not isinstance(objectType, ir_t.ClassType) or ARRAY not in objectType.clas.flags:
            raise TypeException(node.location, "not an array")
        argTypes = map(self.visit, node.arguments) if node.arguments is not None else None
        self.handleNewCall(objectType, argTypes, node.id, node.location)
        return objectType

    def visitUnaryExpression(self, node):
        receiverType = self.visit(node.expr)
        ty = self.handleOperatorCall(node.operator, receiverType, None, node.id, node.location)
        return ty

    def visitBinaryExpression(self, node):
        leftTy = self.visit(node.left)
        rightTy = self.visit(node.right)
        if node.operator in ["&&", "||"]:
            # Logic operators are handled separately from other builtins because of
            # short circuiting.
            if not self.isConditionType(leftTy) or not self.isConditionType(rightTy):
                raise TypeException(node.location,
                                    "expected condition types for logic operands")
            ty = ir_t.BooleanType
        else:
            ty = self.handleOperatorCall(node.operator, leftTy, rightTy, node.id, node.location)
        return ty

    def visitTupleExpression(self, node):
        types = tuple(map(self.visit, node.expressions))
        if len(types) > compile_info.MAX_TUPLE_LENGTH:
            raise TypeException(node.location,
                                "tuples longer than %d elements not supported" %
                                compile_info.MAX_TUPLE_LENGTH)
        for (expr, ty) in zip(node.expressions, types):
            if not ty.isObject():
                raise TypeException(expr.location,
                                    "expression with primitive type %s cannot be used in tuple" %
                                    str(ty))

        clas = self.info.getTupleClass(len(types), node.location)
        return ir_t.ClassType(clas, types)

    def visitIfExpression(self, node):
        condTy = self.visit(node.condition)
        if not self.isConditionType(condTy):
            raise TypeException(node.location, "condition must be boolean")
        trueTy = self.visit(node.trueExpr)
        if node.falseExpr is not None:
            falseTy = self.visit(node.falseExpr)
            ifTy = trueTy.combine(falseTy, node.location)
        else:
            ifTy = ir_t.UnitType
        return ifTy

    def visitWhileExpression(self, node):
        condTy = self.visit(node.condition)
        if not self.isConditionType(condTy):
            raise TypeException(node.condition.location, "condition must be boolean")
        self.visit(node.body)
        return ir_t.UnitType

    def visitMatchExpression(self, node):
        exprTy = self.visit(node.expression)
        resultTy = self.visit(node.matcher, exprTy)
        return resultTy

    def visitThrowExpression(self, node):
        exnTy = self.visit(node.exception)
        if not exnTy.isSubtypeOf(ir_t.ClassType(getExceptionClass())):
            raise TypeException(node.location, "throw expression must produce an Exception")
        return ir_t.NoType

    def visitTryCatchExpression(self, node):
        tryTy = self.visit(node.expression)
        exnTy = ir_t.ClassType.forReceiver(getExceptionClass())
        if node.catchHandler is None:
            catchTy = ir_t.NoType
        else:
            catchTy = self.visit(node.catchHandler, exnTy)
        if node.finallyHandler is not None:
            self.visit(node.finallyHandler)
        ty = tryTy.combine(catchTy, node.location)
        return ty

    def visitPartialFunctionExpression(self, node, valueTy):
        ty = ir_t.NoType
        for case in node.cases:
            caseTy = self.visit(case, valueTy)
            ty = ty.combine(caseTy, node.location)
        return ty

    def visitPartialFunctionCase(self, node, valueTy):
        self.visit(node.pattern, valueTy, COMPILE_FOR_MATCH)
        if node.condition is not None:
            conditionTy = self.visit(node.condition)
            if not self.isConditionType(conditionTy):
                raise TypeException(node.condition.location, "condition must have boolean type")
        ty = self.visit(node.expression)
        return ty

    def visitReturnExpression(self, node):
        if not self.isAnalyzingFunction() or \
           (self.functionStack[-1].irDefn.isConstructor() and node.expression is not None):
            raise TypeException(node.location, "return not valid in this position")
        if node.expression is None:
            retTy = ir_t.UnitType
        else:
            retTy = self.visit(node.expression)
        self.checkAndHandleReturnType(retTy, node.location)
        return ir_t.NoType

    def visitClassType(self, node):
        ty = super(DefinitionTypeVisitor, self).visitClassType(node)
        if isinstance(ty, ir_t.VariableType):
            param = ty.typeParameter
            if self.variance is not None and param.clas is self.varianceClass:
                if CONTRAVARIANT in param.flags and \
                   self.variance not in [CONTRAVARIANT, ir_t.BIVARIANT]:
                    raise TypeException(node.location,
                                        "contravariant type parameter used in invalid position")
                if COVARIANT in param.flags and \
                   self.variance not in [COVARIANT, ir_t.BIVARIANT]:
                    raise TypeException(node.location,
                                        "covariant type parameter used in invalid position")
        return ty

    def findPatternType(self, patTy, exprTy, mode, isDestructure, name, loc):
        """Determines the type of a pattern and checks for matching errors.

        Args:
            patTy (Type?): the declared type of the pattern. Should be `None` if the type is
                inferred from the expression.
            exprTy (Type?): the type of the expression being matched. May be `None` if there is
                no expression (for example, a parameter or variable declaration). Either `patTy`
                or `exprTy` must be specified.
            mode (symbol): either `COMPILE_FOR_VALUE` for variables and parameters or
                `COMPILE_FOR_MATCH` for pattern matching.
            isDestructure (bool): true if this is part of a destructuring pattern (just used
                for tuple patterns). Checking is a little stricter if false, since sub-patterns
                are not being matched.
            name (str?): the name of the variable in the pattern, if there is one. Used for
                error reporting.
            loc (Location): location in source code, used for error reporting.

        Returns:
            (Type): if `patTy` is set, it is returned. Otherwise, `exprTy`.

        Raises:
            TypeException: for many reasons, for example, if no type can be inferred or if
                types cannot be tested at runtime.
        """
        nameStr = "%s: " % name if name is not None else ""
        scope = self.scope()
        if patTy is None and exprTy is None:
            raise TypeException(loc, nameStr + "type not specified")
        elif patTy is not None:
            if mode is COMPILE_FOR_VALUE and \
               exprTy is not None and \
               not exprTy.isSubtypeOf(patTy):
                raise TypeException(loc, nameStr + "expression doesn't match declared type")
            elif mode is COMPILE_FOR_MATCH and \
                 exprTy.isDisjoint(patTy):
                raise TypeException(loc, nameStr + "expression cannot match declared type")
            elif mode is COMPILE_FOR_MATCH and \
                 not isDestructure and \
                 not exprTy.isSubtypeOf(patTy) and \
                 not typeCanBeTested(patTy):
                raise TypeException(loc, nameStr + "type cannot be tested at runtime")
            else:
                return patTy
        else:
            return exprTy

    def handleClassTypeArgs(self, irClass, typeArgs):
        typeParams = ir.getExplicitTypeParameters(irClass)
        assert len(typeParams) == len(typeArgs)
        types = []
        introducedTypeParams = []
        for tp, ta in zip(typeParams, typeArgs):
            if isinstance(ta, ast.BlankType):
                tp, ty = self.introduceExistentialTypeParameter(tp, ta)
                introducedTypeParams.append(tp)
            else:
                with VarianceScope.forArgument(self, tp.variance()):
                    ty = self.visit(ta)
                if not (tp.lowerBound.isSubtypeOf(ty) and
                        ty.isSubtypeOf(tp.upperBound)):
                    raise TypeException(ta.location,
                                        "%s: type argument is not in bounds" % tp.name)
            types.append(ty)
        return tuple(types), tuple(introducedTypeParams)

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

            # Process parameter types first. We already know the types, but we need to ensure
            # that variant type parameters are being used correctly.
            # TODO: should variance checks in general be done in DeclarationTypeVisitor?
            if isinstance(node, ast.PrimaryConstructorDefinition):
                vscope = VarianceScope(self, COVARIANT, irFunction.definingClass)
            elif irFunction.isMethod() and not irFunction.isConstructor():
                vscope = VarianceScope(self, CONTRAVARIANT, irFunction.definingClass)
            else:
                vscope = VarianceScope.clear(self)
            with vscope:
                for param in node.parameters:
                    self.visit(param)

            # Process return type, if specified.
            if astReturnType is not None:
                if irFunction.isConstructor():
                    raise TypeException(node.location,
                                        "constructors must not declare return type")
                if irFunction.isMethod():
                    vscope = VarianceScope(self, COVARIANT, irFunction.definingClass)
                else:
                    vscope = VarianceScope.clear(self)
                with vscope:
                    irFunction.returnType = self.visit(astReturnType)
            self.functionStack[-1].declaredReturnType = irFunction.returnType

            # Process body.
            if astBody is None:
                if irFunction.isConstructor():
                    bodyType = ir_t.UnitType
                elif astReturnType is not None:
                    bodyType = irFunction.returnType
                else:
                    raise TypeException(node.location,
                                        "return type must be specified for abstract function")
            else:
                bodyType = self.visit(astBody)
                if bodyType is not ir_t.NoType:
                    if irFunction.isMethod() and not irFunction.isConstructor():
                        vscope = VarianceScope(self, COVARIANT, irFunction.definingClass)
                    else:
                        vscope = VarianceScope.clear(self)
                    with vscope:
                        bodyType = self.checkAndHandleReturnType(bodyType, astBody.location)
                else:
                    bodyType = self.functionStack[-1].getReturnType()
            if irFunction.returnType is None:
                if irFunction.isConstructor():
                    irFunction.returnType = ir_t.UnitType
                else:
                    irFunction.returnType = bodyType
            else:
                if irFunction.returnType != bodyType:
                    raise TypeException(node.location,
                                        "body type does not match declared return type")

    def isConditionType(self, ty):
        return ty == ir_t.BooleanType or ty == ir_t.NoType

    def ensureTypeInfoForDefn(self, irDefn):
        if isinstance(irDefn, ir.Function):
            if irDefn.returnType is None:
                self.visit(irDefn.astDefn)
        elif isinstance(irDefn, ir.Field) or \
             isinstance(irDefn, ir.Variable) or \
             isinstance(irDefn, ir.Global):
            if irDefn.type is None:
                self.visit(irDefn.astVarDefn)
        elif isinstance(irDefn, ir.Class) or \
             isinstance(irDefn, ir.TypeParameter) or \
             isinstance(irDefn, ir.Package) or \
             isinstance(irDefn, ir.PackagePrefix):
            pass   # already done in previous pass.
        else:
            raise NotImplementedError

    def handlePatternScopePrefix(self, prefix):
        """Processes the components of a pattern prefix.

        Components can either be scope prefixes (for example, package names, class names) or
        regular loads or calls (field loads, nullary function calls). Scope lookups come first,
        if they are present. This method determines how many components are scope prefixes and
        saves `ScopePrefixInfo` for each of them. The regular set of `UseInfo` and `CallInfo`
        is saved for all components.

        Args:
            prefix: a list of `ast.ScopePrefixComponent`s to process. It may be empty.

        Returns:
            A tuple of `(Scope, Type|None)`. The scope is the last scope looked up from the
            component list. It can be used for further lookups. The type is the receiver type
            of the last component. It will be `None` if the component list is empty.

        Raises:
            ScopeException: if a definition with this name can't be found or used.
            TypeException: if the definition can't be used because of a type mismatch.
        """
        if len(prefix) == 0:
            return self.scope(), None

        scope = self.scope()
        hasPrefix = False
        firstTypeArgs = map(self.visit, prefix[0].typeArguments) \
                        if prefix[0].typeArguments is not None \
                        else None
        ty = self.handlePossiblePrefixSymbol(prefix[0].name, None, None, firstTypeArgs,
                                             prefix[0].id, prefix[0].location)
        self.info.setType(prefix[0], ty)

        i = 1
        while i < len(prefix) and self.info.hasScopePrefixInfo(prefix[i - 1]):
            hasPrefix = True
            scope = self.info.getScope(self.info.getScopePrefixInfo(prefix[i - 1]).scopeId)
            typeArgs = map(self.visit, prefix[i].typeArguments) \
                       if prefix[i].typeArguments is not None \
                       else None
            ty = self.handlePossiblePrefixSymbol(prefix[0].name, scope, ty, typeArgs,
                                                 prefix[0].id, prefix[0].location)
            self.info.setType(prefix[i], ty)
            i += 1
        while i < len(prefix):
            scope = self.info.getScope(ir_t.getClassFromType(ty))
            typeArgs = map(self.visit, prefix[i].typeArguments) \
                       if prefix[i].typeArguments is not None \
                       else None
            ty = self.handlePropertyCall(prefix[i].name, scope, receiverType, typeArgs, None,
                                         True, False, False, prefix[i].id, prefix[i].location)
            self.info.setType(prefix[i], ty)
            i += 1

        if self.info.hasScopePrefixInfo(prefix[-1]):
            scope = self.info.getScope(self.info.getScopePrefixInfo(prefix[i - 1]).scopeId)
        else:
            scope = self.info.getScope(ir_t.getClassFromType(ty))
        return scope, ty

    def handleSimpleVariable(self, name, useAstId, loc):
        """Handles a simple variable reference, not part of a call (explicitly) or prefix.

        Args:
            name: the name of the variable.
            useAstId: the AST id of the reference (used to save info).
            loc: the location of the reference (used in errors).

        Returns:
            The type of the definition that was referenced (with type substitution performed).
        """
        scope = self.scope()
        receiverType = self.getReceiverType() if self.hasReceiverType() else None
        nameInfo = scope.lookupFromSelf(name, loc)
        self.checkNameInfoIsValue(nameInfo, loc)
        defnInfo, allTypeArgs = self.chooseDefnFromNameInfo(nameInfo, receiverType,
                                                            None, None, loc)
        self.checkCallAllowed(defnInfo.irDefn, True, USE_AS_VALUE, loc)
        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))
        scope.use(defnInfo, useAstId, USE_AS_VALUE, loc)
        return self.getDefnType(receiverType, False, defnInfo.irDefn, allTypeArgs)

    def handlePossiblePrefixSymbol(self, name, scope, scopeType, typeArgs, useAstId, loc):
        """Processes a symbol reference which may be part of a scope prefix.

        If the symbol does turn out to be part of a prefix, `ScopePrefixInfo` will be saved
        at `useAstId`. The normal `UseInfo` and `CallInfo` will be saved in any case.

        Args:
            name: the symbol being referenced.
            scope: the `Scope` where the reference occurs (optional). If `None`, the reference
                is assumed to be made in the current scope, and a self-lookup will be performed.
                Otherwise, an external lookup will be performed.
            scopeType: the `Type` of the scope where the reference occurs (optional). This
                should be `None` iff `scope` is `None`. The receiver type will be taken from
                the current scope, if it has one.
            typeArgs: a list of type arguments that are part of the reference. Pass the empty
                list if there are none.
            useAstId: the AST id of the reference. This will be used to save info.
            loc: the location of the reference (used in errors).

        Returns:
            The `Type` of the definition being reference. If a package is being referenced,
            the package type will be returned.

        Raises:
            ScopeException: if a definition with this name can't be found or used.
            TypeException: if the definition can't be used because of a type mismatch.
        """
        if scope is None:
            assert scopeType is None
            receiverIsExplicit = False
            scopeType = self.getReceiverType() if self.hasReceiverType() else None
            nameInfo = self.scope().lookupFromSelf(name, loc)
        else:
            receiverIsExplicit = True
            nameInfo = scope.lookupFromExternal(name, loc)

        if nameInfo.isScope():
            defnInfo = nameInfo.getDefnInfo()
            if isinstance(defnInfo.irDefn, ir.Class):
                defnScopeId = self.info.getScope(defnInfo.irDefn).scopeId
            else:
                parentScope = self.info.getScope(defnInfo.scopeId)
                defnScopeId = parentScope.scopeForPrefix(name, loc).scopeId
            scopePrefixInfo = ScopePrefixInfo(defnInfo.irDefn, defnScopeId)
            self.info.setScopePrefixInfo(useAstId, scopePrefixInfo)

        if nameInfo.isClass():
            defnInfo = nameInfo.getDefnInfo()
            receiverType = self.getReceiverTypeForClass(defnInfo.irDefn, typeArgs, loc)
            allTypeArgs = receiverType.typeArguments
        else:
            receiverType = scopeType
            defnInfo, allTypeArgs = self.chooseDefnFromNameInfo(nameInfo, receiverType,
                                                                typeArgs, None, loc)

        self.checkCallAllowed(defnInfo.irDefn, False, USE_AS_PROPERTY, loc)
        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))
        self.scope().use(defnInfo, useAstId, USE_AS_PROPERTY, loc)
        return self.getDefnType(receiverType, True, defnInfo.irDefn, allTypeArgs)

    def handlePropertyCall(self, name, receiverScope, receiverType,
                           typeArgs, argTypes, hasReceiver, receiverIsReceiver,
                           isLvalue, useAstId, loc):
        """Handles a reference to a property inside an object or a scope.

        This can be used for methods, fields, and inner-class constructors with an explicit
        receiver or scope prefix. It cannot be used to process additional scope prefixes
        (use `handlePossibleScopePrefix` instead). `UseInfo` and `CallInfo` will be saved.

        Args:
            name (str): the name of the symbol being referenced.
            receiverScope (Scope): scope of the receiver or prefix.
            receiverType (Type): type of the receiver or the prefix. This is used to obtain
                type arguments which may be implied in this call.
            typeArgs (list(Type)?): type arguments passes as part of the call. Should be `None`
                if no type arguments were explicitly passed.
            argTypes (list(Type)?): types of arguments passed as part of the call. Should be
                `None` if no arguments were explicitly passed.
            hasReceiver (bool): whether the expression has a receiver. If the beginning of the
                expression is a scope prefix, there might be no receiver.
            receiverIsReceiver (bool): whether the receiver used in the call is the same as the
                caller's receiver. This is required for some calls.
            isLvalue (bool): whether the property is being assigned to. Normally, the type of
                a field or return type of a method may be upcast, but this is not true
                during assignments.
            useAstId (AstId): the AST id where the symbol is referenced. Used to save info.
            loc (Location): the location of the reference in source code. Used in errors.

        Returns:
            (Type): type of the definition that was referenced with type substitution performed.

        Raises:
            ScopeException: if a definition with this name can't be found or used.
            TypeException: if the definition can't be used because of a type mismatch.
        """
        nameInfo = receiverScope.lookupFromExternal(name, loc)
        useKind = USE_AS_PROPERTY
        receiverType, existentialVars = self.openExistentialReceiver(receiverType)

        if nameInfo.isClass() and argTypes is not None:
            defnInfo, allTypeArgs, receiverType  = self.chooseConstructorFromNameInfo(
                nameInfo, typeArgs, argTypes, loc)
            useKind = USE_AS_CONSTRUCTOR
        else:
            self.checkNameInfoIsValue(nameInfo, loc)
            defnInfo, allTypeArgs = self.chooseDefnFromNameInfo(nameInfo, receiverType,
                                                                typeArgs, argTypes, loc)

        irDefn = defnInfo.irDefn
        receiverNeeded = (isinstance(irDefn, ir.Field) or
                          (isinstance(irDefn, ir.Function) and
                           irDefn.isMethod() and
                           not irDefn.isConstructor()))
        if receiverNeeded and not hasReceiver:
            raise TypeException(loc, "%s: cannot access without receiver" % name)

        self.checkCallAllowed(defnInfo.irDefn, receiverIsReceiver, useKind, loc)
        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))
        self.scope().use(defnInfo, useAstId, useKind, loc)
        ty = self.getDefnType(receiverType, True, defnInfo.irDefn, allTypeArgs)
        ty = self.upcastExistentialVars(ty, existentialVars, isLvalue, name, loc)
        return ty

    def handleUnprefixedCall(self, name, typeArgs, argTypes, useAstId, loc):
        """Handles a call with arguments or type arguments without a prefix.

        This can be used for function calls or method calls with implicit receivers. Either
        type arguments or arguments or both must be passed (to rule out a number of other
        cases). This cannot be used for prefixes. `UseInfo` and `CallInfo` will be saved.

        Args:
            name: the name of the symbol being referenced.
            typeArgs: a list of `Type` arguments passed as part of the call or `None`. Must
                not be `None` if `argTypes` is `None`.
            argTypes: a list of `Type`s of arguments passed as part of the call. May be `None`.
                Must not be `None` if `typeArgs` is `None`.
            useAstId: the AST id where the symbol is referenced. Used to save info.
            loc: the location of the reference in source code. Used in errors.

        Returns:
            The `Type` of the definition that was referenced (with type substitution performed).

        Raises:
            ScopeException: if a definition with this name can't be found or used.
            TypeException: if the definition can't be used because of a type mismatch.
        """
        receiverType = self.getReceiverType() if self.hasReceiverType() else None
        useKind = USE_AS_VALUE
        nameInfo = self.scope().lookupFromSelf(name, loc)

        if nameInfo.isClass() and argTypes is not None:
            defnInfo, allTypeArgs, receiverType = self.chooseConstructorFromNameInfo(
                nameInfo, typeArgs, argTypes, loc)
            useKind = USE_AS_CONSTRUCTOR
        else:
            self.checkNameInfoIsValue(nameInfo, loc)
            defnInfo, allTypeArgs = self.chooseDefnFromNameInfo(nameInfo, receiverType,
                                                                typeArgs, argTypes, loc)
        irDefn = defnInfo.irDefn
        self.checkCallAllowed(irDefn, True, useKind, loc)
        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))
        self.scope().use(defnInfo, useAstId, useKind, loc)
        return self.getDefnType(receiverType, False, irDefn, allTypeArgs)

    def handleOperatorCall(self, name, firstType, secondType, useAstId, loc):
        """Handles a call to an operator.

        This can be used to call operators in the current scope or in the scope of the first
        operand. Operators in the current scope may be functions, methods (static or not) or
        classes (with unary or binary constructors). Operators in the first operands scope
        may only be methods. If the operator names ends with '=' and the whole name isn't "==",
        the operator may be considered an assignment. `UseInfo` and `CallInfo` will be saved.

        Args:
            name (str): name of the operator.
            firstType (Type): type of the first operand. For unary operators, this is the only
                operand. For binary operators, this is the operand on the left unless the
                name ends with ':', in which case, this is the operand on the right.
            secondType (Type?): type of the second operand or `None` for unary operators.
            useAstId (AstId): id where the operator is referenced. Used to save info.
            loc (Location): the location of the operator in source code. Used in errors.

        Returns:
            (Type) type of the definition that was referenced with type substitution performed.

        Raises:
            ScopeException: if a definition with this name can't be found or used.
            TypeException: if the definition can't be used because of a type mismatch.
        """
        mayBeAssignment = secondType is not None and name.endswith("=") and name != "=="

        # Try to find a compatible operator in the current scope.
        selfDefnInfo, selfAllTypeArgs, selfReceiverType, selfUseKind = None, None, None, None
        try:
            selfNameInfo = self.scope().lookupFromSelf(name, loc,
                                                       mayBeAssignment=mayBeAssignment)
            argTypes = [firstType] if secondType is None else [firstType, secondType]
            if selfNameInfo.isClass():
                selfDefnInfo, selfAllTypeArgs, selfReceiverType = \
                    self.chooseConstructorFromNameInfo(selfNameInfo, None, argTypes, loc)
                selfUseKind = USE_AS_CONSTRUCTOR
            else:
                self.checkNameInfoIsValue(selfNameInfo, loc)
                selfReceiverType = self.getReceiverType() if self.hasReceiverType() else None
                selfDefnInfo, selfAllTypeArgs = self.chooseDefnFromNameInfo(
                    selfNameInfo, selfReceiverType, None, argTypes, loc)
                selfUseKind = USE_AS_VALUE
        except (ScopeException, TypeException):
            pass

        # Try to find a compatible operator in the first operand's scope.
        operandDefnInfo, operandAllTypeArgs, operandUseKind = None, None, None
        operandReceiverType, operandExistentialVars = self.openExistentialReceiver(firstType)
        try:
            operandScope = self.info.getScope(ir_t.getClassFromType(firstType))
            operandNameInfo = operandScope.lookupFromExternal(name, loc,
                                                              mayBeAssignment=mayBeAssignment)
            argTypes = [secondType] if secondType is not None else []
            self.checkNameInfoIsValue(operandNameInfo, loc)
            operandDefnInfo, operandAllTypeArgs = self.chooseDefnFromNameInfo(
                operandNameInfo, operandReceiverType, None, argTypes, loc)
            operandUseKind = USE_AS_PROPERTY
        except (ScopeException, TypeException):
            pass

        # We should have found a match in exactly one of the scopes.
        if selfDefnInfo is None and operandDefnInfo is None:
            raise TypeException(loc, "%s: could not find compatible operator" % name)
        if selfDefnInfo is not None and operandDefnInfo is not None:
            raise TypeException(loc, "%s: ambiguous call to overloaded operator" % name)

        if selfDefnInfo is not None:
            defnInfo, allTypeArgs, receiverType, useKind = \
                selfDefnInfo, selfAllTypeArgs, selfReceiverType, selfUseKind
            isAssignment = name != selfNameInfo.name
            existentialVars = None
        else:
            defnInfo, allTypeArgs, receiverType, useKind = \
                operandDefnInfo, operandAllTypeArgs, operandReceiverType, operandUseKind
            isAssignment = name != operandNameInfo.name
            existentialVars = operandExistentialVars

        # Record information and return the resulting type.
        self.checkCallAllowed(defnInfo.irDefn, False, useKind, loc)
        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))
        self.scope().use(defnInfo, useAstId, useKind, loc)
        ty = self.getDefnType(receiverType, False, defnInfo.irDefn, allTypeArgs)
        ty = self.upcastExistentialVars(ty, existentialVars, False, name, loc)

        if isAssignment and not ty.isSubtypeOf(firstType):
            raise TypeException(loc,
                                "%s: operator returns an incompatible type for assignment" %
                                name)
        return ty

    def handleNewCall(self, objectType, argTypes, useAstId, loc):
        """Handles a call which creates a new object.

        This is only used for new array expressions, currently. The receiver type is specified
        with a type (as opposed to a variable or property expression). This will check whether
        the type is a class type and whether it's `Nothing` and throw an exception in
        those cases.

        Args:
            objectType: the `Type` of the object being allocated. This implies the type args.
            argTypes: a list of `Type`s of arguments to pass to the constructor.
            useAstId: the AST id of the call. Used to save info.
            loc: the location of the call in source code. Used in errors.

        Returns:
            `None`. The type of the expression is already known to be `objectType`.

        Raises:
            ScopeException: if an appropriate constructor can't be found or used.
            TypeException: if `objectType` is not a class type or is `Nothing`.
        """
        if not isinstance(objectType, ir_t.ClassType):
            raise TypeException(loc, "can only allocate instances of class types")
        irClass = objectType.clas
        if irClass is getNothingClass():
            raise TypeException(loc, "cannot instantiate Nothing")
        classScope = self.info.getScope(irClass)
        nameInfo = classScope.lookupFromExternal(ir.CONSTRUCTOR_SUFFIX, loc)
        defnInfo, allTypeArgs = self.chooseDefnFromNameInfo(nameInfo, objectType,
                                                            None, argTypes, loc)
        self.checkCallAllowed(defnInfo.irDefn, False, USE_AS_CONSTRUCTOR, loc)
        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))
        self.scope().use(defnInfo, useAstId, USE_AS_CONSTRUCTOR, loc)

    def handleDestructure(self, nameInfo, receiverType, receiverIsExplicit,
                          typeArgs, exprType, subPatterns, mode, useAstId, matcherAstId, loc):
        """Handles a destructuring pattern match.

        A destructuring pattern match works by passing the object being matched to a matcher
        function which returns a match result (an `Option`). The matcher function may be named
        explicitly, or can be a method of a named object, or can be a static method in a
        named class. If the match result is `Some`, the contents (either a single value or a
        tuple) are used to match sub-patterns.

        There are a few different patterns that can perform destructuring, so the caller is
        expected to have done some work already. This method handles the common functionality
        between all of them.

        nameInfo (NameInfo): info for the definition used to perform the match. If this is a
            (possibly overloaded) function, it is assumed to be an explicitly named matcher
            function. If this is a global, field, or variable, it is assumed to be a matcher
            object which has a "try-match" method that will be called. If this is a class,
            it is assumed to contain a static "try-match" method that will be called. Otherwise,
            the definition cannot be used and a `TypeException` will be raised.
        receiverType (Type?): receiver type for the matcher. May be `None`, for example
            for globals.
        receiverIsExplicit (bool): whether the receiver for the matcher was specified
            explicitly. This affects type substitution.
        typeArgs (list(Type)?): a list of type arguments passed explicitly to the matcher.
            Should be `None` if no type arguments were passed explicitly.
        exprType (Type): the type of the expression being matched. This is the type of the
            argument to the matcher function.
        subPatterns (list(Pattern)): sub-patterns being matched as part of this destructuring.
            They will be visiting recursively after we know what the matcher function returns.
            The match result should have the same number of types (single type or tuple type)
            as the number of sub-patterns in this list.
        mode (symbol): compilation mode for patterns.
        useAstId (AstId): id of the definition that contains the matcher method. Used to
            record `UseInfo` and `CallInfo`.
        matcherAstId (AstId): id of the matcher method itself. Some patterns (like
            `UnaryPattern`) have a separate id just for this.
        loc (Location): location of the match in source code. Used for error reporting.

        Returns:
            (Type): same as `exprType`. Destructures are not guaranteed, so we can't be any
            more specific.

        Raises:
            ScopeException: if a matcher function can't be found or called.
            TypeException: for many, many possible reasons.
        """
        useKind = USE_AS_PROPERTY if receiverIsExplicit else USE_AS_VALUE
        if nameInfo.isFunction():
            # Call to possibly overloaded matcher function.
            receiverType, existentialVars = self.openExistentialReceiver(receiverType)
            defnInfo, allTypeArgs = self.chooseDefnFromNameInfo(nameInfo, receiverType,
                                                                typeArgs, [exprType], loc)
            self.checkCallAllowed(defnInfo.irDefn, False, useKind, loc)
            self.info.setCallInfo(matcherAstId, CallInfo(allTypeArgs))
            self.scope().use(defnInfo, matcherAstId, useKind, loc)
            irDefn = defnInfo.irDefn
            self.ensureTypeInfoForDefn(irDefn)
            returnType = self.getDefnType(receiverType, receiverIsExplicit,
                                          irDefn, allTypeArgs)
            returnType = self.upcastExistentialVars(returnType, existentialVars,
                                                    False, nameInfo.name, loc)
        else:
            # The name refers to a matcher scope or definition.
            matcherDefnInfo = nameInfo.getDefnInfo()
            self.scope().use(matcherDefnInfo, useAstId, useKind, loc)
            matcherIrDefn = matcherDefnInfo.irDefn
            self.ensureTypeInfoForDefn(matcherIrDefn)
            if isinstance(matcherIrDefn, ir.Global) or \
               isinstance(matcherIrDefn, ir.Field) or \
               isinstance(matcherIrDefn, ir.Variable):
                # The name refers to a matcher object stored in a global, field, or variable.
                if typeArgs is not None:
                    raise TypeException(loc,
                                        "cannot apply type arguments to value in destructure pattern")
                matcherReceiverType = matcherIrDefn.type
                matcherHasReceiver = True
                matcherClass = ir_t.getClassFromType(matcherReceiverType)
            elif isinstance(matcherIrDefn, ir.Class):
                # The name refers to a class with a static matcher method.
                callTypeArgs = typeArgs if typeArgs is not None else ()
                if not matcherIrDefn.canApplyTypeArgs(callTypeArgs):
                    raise TypeException(loc,
                                        "%s: type arguments could not be applied for this class" %
                                        nameInfo.name)
                matcherScopeId = self.info.getScope(matcherIrDefn).scopeId
                matcherScopePrefixInfo = ScopePrefixInfo(matcherIrDefn, matcherScopeId)
                self.info.setScopePrefixInfo(useAstId, matcherScopePrefixInfo)
                matcherReceiverType = ir_t.ClassType(matcherIrDefn, callTypeArgs)
                matcherHasReceiver = False
                matcherClass = matcherIrDefn
            else:
                raise TypeException(loc, "%s: cannot use this definition for matching" %
                                    nameInfo.name)

            # Lookup the try-match method.
            matcherScope = self.info.getScope(matcherClass)
            try:
                returnType = self.handlePropertyCall("try-match", matcherScope,
                                                     matcherReceiverType, None, [exprType],
                                                     matcherHasReceiver, False, False,
                                                     matcherAstId, loc)
            except ScopeException:
                raise TypeException(loc, "cannot match without `try-match` method")

        # Determine the expression types of the sub-patterns, based on what the matcher returns.
        # If there is one pattern, it should return Option[T1], and T1 is the expression type
        # (T1 may be a tuple). If there are more, it should return Option[(T1, ..., Tn)], and
        # T1, ..., Tn are the expression types.
        optionClass = self.info.getStdClass("Option", loc)
        if not returnType.isObject() or \
           not ir_t.getClassFromType(returnType).isSubclassOf(optionClass):
            raise TypeException(loc, "matcher must return std.Option")
        returnType = returnType.substituteForBaseClass(optionClass)
        returnTypeArg = returnType.typeArguments[0]
        n = len(subPatterns)
        if n == 1:
            patternTypes = [returnTypeArg]
        else:
            tupleClass = self.info.getTupleClass(n, loc)
            if not returnTypeArg.isObject() or \
               not ir_t.getClassFromType(returnTypeArg).isSubclassOf(tupleClass):
                raise TypeException(loc, "matcher must return `std.Option[std.Tuple%d]`" % n)
            returnTypeArg = returnTypeArg.substituteForBaseClass(tupleClass)
            patternTypes = returnTypeArg.typeArguments
        for subPat, subExprType in zip(subPatterns, patternTypes):
            self.visit(subPat, subExprType, mode)

        # We return the expression type as the pattern type. Destructures are never guaranteed,
        # so we cannot be any more specific.
        return exprType

    def getReceiverTypeForClass(self, irClass, typeArgs, loc):
        """Returns a type with the given explicit type arguments applied.

        This also loads implicit type arguments implied by the class's scope. All type arguments
        arechecked to be in range of their parameters' bounds.
        """
        explicitTypeParams = ir.getExplicitTypeParameters(irClass)
        if typeArgs is None:
            typeArgs = []
        if len(typeArgs) != len(explicitTypeParams):
            raise TypeException(loc,
                                "wrong number of type arguments: expected %d but have %d" %
                                (len(explicitTypeParams), len(typeArgs)))
        if not all(tp.contains(ta) for tp, ta in zip(explicitTypeParams, typeArgs)):
            raise TypeException(loc, "type error in type arguments for class")
        implicitTypeParams = ir.getImplicitTypeParameters(irClass)
        implicitTypeArgs = [ir.VariableType(tp) for tp in implicitTypeParams]
        allTypeArgs = tuple(implicitTypeArgs + typeArgs)
        return ir_t.ClassType(irClass, allTypeArgs, None)

    def checkNameInfoIsValue(self, nameInfo, loc):
        """Checks that the named definition can be used as a value.

        Classes and package prefixes cannot be used as values. If the `NameInfo` references
        one of those, a `TypeException` will be raised.
        """
        if nameInfo.isClass():
            raise TypeException(loc, "%s: class can't be used as value" % nameInfo.name)
        if nameInfo.isPackagePrefix():
            raise TypeException(loc, "%s: package prefix can't be used as value" %
                                nameInfo.name)

    def chooseConstructorFromNameInfo(self, nameInfo, typeArgs, argTypes, loc):
        """Determines which constructor should be used from a `NameInfo` that refers to a class.

        Loads a separate `NameInfo` for constructors within the class, generates an appropriate
        receiver type, then calls `chooseDefnFromNameInfo` on the constructors.

        Returns:
            A tuple of `(DefnInfo, [Type], ClassType)` containing the constructor definition,
            the full list of type arguments applied to the constructor, and the receiver type.
            The type arguments are always the same as the ones on the receiver type.

        Raises:
            ScopeException: if a definition with this name can't be found or used.
            TypeException: if the definition can't be used because of a type mismatch or if
                the class has array elements (and should not be constructed outside of a
                `new` expression).
        """
        assert nameInfo.isClass() and argTypes is not None
        if ARRAY in nameInfo.getDefnInfo().irDefn.flags:
            raise TypeException(loc, "cannot construct instance without `new` expression")
        ctorNameInfo, receiverType = self.extractConstructorNameInfo(nameInfo, typeArgs, loc)
        ctorDefnInfo, ctorAllTypeArgs = self.chooseDefnFromNameInfo(ctorNameInfo, receiverType,
                                                                    None, argTypes, loc)
        return ctorDefnInfo, ctorAllTypeArgs, receiverType

    def extractConstructorNameInfo(self, nameInfo, typeArgs, loc):
        assert nameInfo.isClass()
        irClass = nameInfo.getDefnInfo().irDefn
        receiverType = self.getReceiverTypeForClass(irClass, typeArgs, loc)
        allTypeArgs = receiverType.typeArguments
        if irClass is getNothingClass():
            raise TypeException(loc, "cannot instantiate Nothing")
        nameInfo = nameInfo.getInfoForConstructors(self.info)
        return nameInfo, receiverType

    def chooseDefnFromNameInfo(self, nameInfo, receiverType, typeArgs, argTypes, loc):
        """Determines which definition should be used from a `NameInfo`.

        This is used to resolve overloaded functions, but it can be used on any list of
        `DefnInfo`. If a class is referenced, this will treat the reference as a constructor
        call and will pick one of the constructors. This cannot be used on scope prefixes.

        Args:
            nameInfo: a `NameInfo` returned from a scope lookup.
            receiverType: the type of the receiver in a method call or `None`. If the receiver
                might be implied, this is the type of the class containing the call.
            typeArgs: an optional list of `Type` arguments in the call.
            argTypes: an optional list of `Type`s of arguments in the call.
            loc: the location of the call (used for errors).

        Returns:
            A tuple of `(DefnInfo, [Type])` containing the single matching definition and the
            full list of type arguments.

        Raises:
            TypeException: if there were zero or multiple matches.
        """
        name = nameInfo.name
        candidate = None
        for defnInfo in nameInfo.overloads:
            irDefn = defnInfo.irDefn

            if not isinstance(irDefn, ir.Function) and \
               typeArgs is None and argTypes is None:
                # Non-function
                typesAndArgs = (None, None)
                match = True
            elif isinstance(irDefn, ir.Function):
                # Function, method, static method, or constructor.
                callTypeArgs = typeArgs if typeArgs is not None else []
                callArgTypes = argTypes if argTypes is not None else []
                typesAndArgs = ir.getAllArgumentTypes(irDefn, receiverType,
                                                      callTypeArgs, callArgTypes,
                                                      defnInfo.importedTypeArguments)
                match = typesAndArgs is not None
            else:
                match = False

            if match:
                if candidate is not None:
                    raise TypeException(loc, "%s: ambiguous call to overloaded function" % \
                                        name)
                allTypeArgs, _ = typesAndArgs
                candidate = (defnInfo, allTypeArgs)

        if candidate is None:
            raise TypeException(loc, "%s: could not find compatible definition" % name)
        return candidate

    def checkCallAllowed(self, irCallee, receiverIsReceiver, useKind, loc):
        """Checks whether a call to a function is valid.

        This may be called on any reference to a definition. For references to non-functions,
        it will do nothing. If the call is not allowed (for example, because the method is
        marked as an initializer and the caller is not also an initializer), an exception will
        be raised.

        Args:
            irCallee (IrDefinition): the definition being referenced.
            receiverIsReceiver (bool): True if the callee's receiver is also the caller's
                receiver. False otherwise. This is required for initializers.
            useKind (symbol): how the definition is being used.
            loc (Location): the location of the reference in source, used in errors.

        Raises:
            TypeException: if the call is not allowed.
        """
        if not isinstance(irCallee, ir.Function):
            return
        irCaller = self.getCallingFunction()
        assert irCaller is None or isinstance(irCaller, ir.Function)
        if len(frozenset([CONSTRUCTOR, INITIALIZER]) & irCallee.flags) > 0 and \
           useKind is not USE_AS_CONSTRUCTOR and \
           (irCaller is None or \
            len(frozenset([CONSTRUCTOR, INITIALIZER]) & irCaller.flags) == 0 or \
            not receiverIsReceiver):
            raise TypeException(loc, "cannot call initializer function here")

    def getDefnType(self, receiverType, receiverIsExplicit, irDefn, typeArgs):
        """Gets the type of a definition that was referenced by name.

        For variables and globals, this just returns the type. For fields, this returns the
        type with type substitution performed with the type arguments and class type parameters.
        For functions, this returns the return type with type substitution performed with the
        type arguments and function type parameters. For classes, `receiverType` is returned.
        For packages and package prefixes, this returns the package type.

        Note that type information may not be available yet for the definition. If this is the
        case, the definition will be analyzed immediately. This means the AST is not traversed
        in order.

        Args:
            receiverType (Type?): optional type of the receiver of a function or field.
            receiverIsExplicit (bool): whether a receiver was specified explicitly. If the
                definition is a field, type substitution is only performed if this is `True`.
                Type arguments are meaningless if `False`.
            irDefn (IrDefinition): the definition that we want to know the type of.
            typeArgs (list(Type)?): an optional list of `Type` arguments applied to a function
                or class. Used for type substitution in functions and fields.
        """
        self.ensureTypeInfoForDefn(irDefn)
        if isinstance(irDefn, ir.Function):
            if irDefn.isConstructor():
                return receiverType
            else:
                assert len(typeArgs) == len(irDefn.typeParameters)
                ty = irDefn.returnType.substitute(irDefn.typeParameters, typeArgs)
                return ty
        elif isinstance(irDefn, ir.Field):
            ty = irDefn.type
            if receiverIsExplicit and isinstance(receiverType, ir_t.ClassType):
                fieldClass = self.findBaseClassForField(receiverType.clas, irDefn)
                ty = ty.substituteForInheritance(receiverType.clas, fieldClass)
                ty = ty.substitute(receiverType.clas.typeParameters, receiverType.typeArguments)
            return ty
        elif isinstance(irDefn, ir.Variable) or \
             isinstance(irDefn, ir.Global):
            return irDefn.type
        elif isinstance(irDefn, ir.Package) or \
             isinstance(irDefn, ir.PackagePrefix):
            return ir_t.getPackageType()
        else:
            assert isinstance(irDefn, ir.Class)
            return receiverType

    def findBaseClassForField(self, receiverClass, field):
        # At this point, classes haven't been flattened yet, so we have to search up the
        # inheritance chain for the first class that contains the field.
        for clas in receiverClass.superclasses():
            if any(True for f in clas.fields if f is field):
                return clas
        assert False, "field is not defined in this class or any superclass"

    def openExistentialReceiver(self, receiverType):
        """Unwraps a possibly existential type to return the type underneath and the declared
        type variables.

        This may be called on any type or `None`. In the case of a nested existential, all
        existential layers are peeled off and all type variables are returned.

        Args:
            receiverType (Type?): any type. May be `None`.

        Returns:
            (Type?, list(TypeParameter)): if `receiverType` was `None` or some non-existential
            type, then `receiverType` and the empty list are returned. If `receiverType` was
            an existential type (possibly a nest), the underlying type and a list of
            type parameters are returned."""
        existentialVars = []
        while isinstance(receiverType, ir_t.ExistentialType):
            existentialVars += receiverType.variables
            receiverType = receiverType.ty
        return receiverType, existentialVars

    def upcastExistentialVars(self, ty, existentialVars, isLvalue, name, loc):
        """Upcasts the given type until none of the given existential variables are referenced.

        This is used when typing expressions that may have existential receivers. It's unsafe
        to return a type that has free existential variables, so we upcast to higher classes
        (eventually to `Object` if we have to) until the variables are no longer referenced.

        Args:
            ty (Type): the type of a field or method return accessed through a possibly
                existential receiver.
            existentialVars (list(Type)?): existential variables declared on the receiver.
            isLvalue (bool): whether we are checking the type of an lvalue. If this is True,
                we cannot upcast. We'll report any error if any of `existentialVars` are
                referenced.
            name (str): name of field or method. Used for error reporting.
            loc (Location): location in source code where the field or method is accessed.
                Used for error reporting.

        Raises:
            TypeException: if `ty` contains any variables from `existentialVars`.
        """
        # NOTE: We are being more conservative than we technically need to be. We could upcast
        # away the existential variables in rvalues. Currently, we use the same code to type
        # lvalues and rvalues, and upcast would make assignment unsafe.
        #
        # We may also want to add an "open" expression the user can bind existential variables
        # explicitly or do something like "wildcard capture" when we have type inference for
        # type arguments.
        if existentialVars is None or len(existentialVars) == 0 or not ty.isObject():
            return ty
        existentialVarIds = set(v.id for v in existentialVars)
        referencedVarIds = ty.findVariables()
        if isLvalue:
            if not existentialVarIds.isdisjoint(referencedVarIds):
                raise TypeException(loc, "%s: contains existential type variables" % name)
            else:
                return ty
        else:
            while not existentialVarIds.isdisjoint(referencedVarIds):
                ty = ty.getBaseClassType()
                referencedVarIds = ty.findVariables()
            return ty

    def checkAndHandleReturnType(self, ty, loc):
        self.checkTypeVariance(ty, loc)
        return self.functionStack[-1].handleReturn(ty)

    def checkTypeVariance(self, ty, loc):
        if isinstance(ty, ir_t.VariableType) and \
           ty.typeParameter.clas is self.varianceClass:
            variance = ty.typeParameter.variance()
            if variance is COVARIANT and \
               self.variance not in [COVARIANT, ir_t.BIVARIANT]:
                raise TypeException(loc, "covariant type prameter used in invalid position")
            elif variance is CONTRAVARIANT and \
                 self.variance not in [CONTRAVARIANT, ir_t.BIVARIANT]:
                raise TypeException(loc, "contravariant type parameter used in invalid position")
        elif isinstance(ty, ir_t.ClassType):
            typeParams = ir.getExplicitTypeParameters(ty.clas)
            typeArgs = ty.typeArguments[-len(typeParams):]
            for tp, ta in zip(typeParams, typeArgs):
                with VarianceScope(self, tp.variance(), tp.clas):
                    self.checkTypeVariance(ta, loc)

    def isExternallyVisible(self, irDefn):
        """Returns true if the definition is visible to other packages. This is true if the
        definition is public and is in the top-level scope, or if the definition is public
        or protected and is a member of an externally visible class."""
        scope = self.scope()
        if isinstance(irDefn, ir.Function) or isinstance(irDefn, ir.Class):
            scope = scope.parent
        if frozenset([PUBLIC, PROTECTED]).isdisjoint(irDefn.flags):
            return False
        while not isinstance(scope, scope_analysis.GlobalScope):
            if not isinstance(scope, scope_analysis.ClassScope):
                return False
            clas = scope.getIrDefn()
            if frozenset([PUBLIC, PROTECTED]).isdisjoint(clas.flags):
                return False
            scope = scope.parent
        return True

    def checkPublicTypeParameter(self, param, name, loc):
        self.checkPublicType(param.upperBound, name, loc)
        self.checkPublicType(param.lowerBound, name, loc)

    def checkPublicType(self, ty, name, loc):
        if isinstance(ty, ir_t.ClassType):
            if PUBLIC not in ty.clas.flags:
                raise TypeException(loc, "public definition %s depends on non-public class %s" %
                                    (name, ty.clas.name.short()))
            each(lambda arg: self.checkPublicType(arg, name, loc), ty.typeArguments)

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

    def getCallingFunction(self):
        if self.scope().scopeId is GLOBAL_SCOPE_ID:
            return None
        irCaller = self.scope().getIrDefn()
        if isinstance(irCaller, ir.Class):
            # Could actually be primary constructor, might not matter.
            irCaller = irCaller.initializer
        return irCaller


def astTypeFlagToIrTypeFlag(flag):
    if flag == "?":
        return ir_t.NULLABLE_TYPE_FLAG
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
            return ir_t.ClassType.forReceiver(getNothingClass())


class VarianceScope(object):
    def __init__(self, visitor, variance, varianceClass=None):
        self.visitor = visitor
        self.oldVarianceClass = self.visitor.varianceClass
        self.oldVariance = self.visitor.variance
        self.variance = variance
        self.varianceClass = varianceClass if varianceClass is not None \
                             else self.oldVarianceClass

    @staticmethod
    def forArgument(visitor, variance):
        newVariance = ir_t.changeVariance(visitor.variance, variance)
        return VarianceScope(visitor, newVariance, None)

    @staticmethod
    def clear(visitor):
        return VarianceScope(visitor, ir_t.BIVARIANT, None)

    def __enter__(self):
        self.visitor.varianceClass = self.varianceClass
        self.visitor.variance = self.variance

    def __exit__(self, *unused):
        self.visitor.varianceClass = self.oldVarianceClass
        self.visitor.variance = self.oldVariance

    def varianceValue(self, flag):
        if flag is COVARIANT:
            return +1
        elif flag is CONTRAVARIANT:
            return -1
        else:
            assert flag is ir_t.INVARIANT
            return 0

__all__ = ["analyzeTypes", "patternMustMatch"]

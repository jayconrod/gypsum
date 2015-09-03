# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import ast
import compile_info
from errors import ScopeException, TypeException
import ir
import ir_types as ir_t
from builtins import getExceptionClass, getPackageClass, getNothingClass
from utils import COMPILE_FOR_VALUE, COMPILE_FOR_MATCH, COMPILE_FOR_UNINITIALIZED, COMPILE_FOR_EFFECT, each
from compile_info import USE_AS_VALUE, USE_AS_TYPE, USE_AS_PROPERTY, USE_AS_CONSTRUCTOR, NORMAL_MODE, STD_MODE, NOSTD_MODE, CallInfo, ScopePrefixInfo
from flags import COVARIANT, CONTRAVARIANT, PROTECTED, PUBLIC, STATIC
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
        if hasattr(func, "override"):
            overridenReturnType = func.override.returnType.substituteForInheritance(
                func.getReceiverClass(), func.override.getReceiverClass())
            if not func.returnType.isSubtypeOf(overridenReturnType):
                raise TypeException(func.getLocation(),
                                    "%s: return type is not subtype of overriden function" %
                                    func.name)


def patternMustMatch(pat, ty, info):
    """Returns true if a pattern will match any value of the given type. This is required for
    patterns in parameters and variable definitions. Type analysis must have already run on
    the pattern for this to work."""
    if isinstance(pat, ast.AstVariablePattern):
        if info.hasUseInfo(pat):
            # This pattern compares the expression to another value, rather than defining
            # a new variable.
            return False
        else:
            patTy = info.getType(pat)
            return ty.isSubtypeOf(patTy)
    elif isinstance(pat, ast.AstTuplePattern):
        tupleClass = info.getTupleClass(len(pat.patterns), pat.location)
        if isinstance(ty, ir_t.ClassType) and ty.clas is tupleClass:
            return all(patternMustMatch(p, ety, info)
                       for p, ety in zip(pat.patterns, ty.typeArguments))
        else:
            return False
    elif isinstance(pat, ast.AstBlankPattern):
        patTy = None if pat.ty is None else info.getType(pat.ty)
        return patTy is None or ty.isSubtypeOf(patTy)
    else:
        return False


def partialFunctionCaseMustMatch(case, ty, info):
    """Returns True if the partial function case must match any value of the given type.
    This is undecidable, so this actually just returns True if the pattern must match and
    there is no condition."""
    assert isinstance(case, ast.AstPartialFunctionCase)
    return patternMustMatch(case.pattern, ty, info) and case.condition is None


def partialFunctionMustMatch(expr, ty, info):
    """Returns True if the partial function must match any value of the given type. Currently,
    this just returns True if there is any individual case which must match, but in the future
    it may account for disjoint cases matching everything."""
    assert isinstance(expr, ast.AstPartialFunctionExpression)
    return any(partialFunctionCaseMustMatch(case, ty, info) for case in expr.cases)


def resolveAllOverrides(info):
    """Iterates over each `NameInfo` in each scope and resolves overrides. This determines
    whether methods with the same name as inherited methods overload or override."""
    for scope in info.scopes.values():
        scope.resolveOverrides()


def findDefnInfoWithArgTypes(candidates, receiverType, receiverIsExplicit,
                             typeArgs, argTypes, name, loc):
    """Determines which definition should be used, from a set of candidates.

    This is used to resolve overloaded functions, but it can be use on any list of `DefnInfo`.

    Args:
        candidates: a list of `DefnInfo`.
        receiverType: the type of the receiver in a method call or `None`. If the receiver
            might be implied, this is the type of the class containing the call.
        receiverIsExplicit: `True` if the receiver was part of the call expression; `False` if
            the receiver was not present and might be implied.
        typeArgs: a list of type arguments in the call.
        argTypes: a list of types of arguments in the call.
        name: the symbol all the candidates were bound to (used for errors).
        loc: the location of the call (used for errors).

    Returns:
        A tuple of `(DefnInfo, [Type], [Type])` containing the single matching definition, the
        full list of type arguments, and the full list of argument types (including the
        receiver, if it was needed).

    Raises:
        TypeException: if there were zero or multiple matches.
    """
    candidate = None
    for defnInfo in candidates:
        irDefn = defnInfo.irDefn

        if not isinstance(irDefn, ir.Function) and \
           len(typeArgs) == 0 and len(argTypes) == 0:
            # Non-function
            typesAndArgs = (None, None)
            match = True
        elif isinstance(irDefn, ir.Function):
            # Function, method, static method, or constructor.
            typesAndArgs = ir.getAllArgumentTypes(irDefn, receiverType, typeArgs, argTypes,
                                                  defnInfo.importedTypeArguments)
            match = typesAndArgs is not None
        else:
            match = False

        if match:
            if candidate is not None:
                raise TypeException(loc, "ambiguous call to overloaded function: %s" % \
                                    name)
            allTypeArgs, allArgTypes = typesAndArgs
            candidate = (defnInfo, allTypeArgs, allArgTypes)

    if candidate is None:
        raise TypeException(loc, "could not find compatible definition: %s" % name)
    return candidate




class TypeVisitorBase(ast.AstNodeVisitor):
    """Provides common functionality for type visitors, namely the visitor functions for the
    various AstType subclasses."""
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

    def visitAstUnitType(self, node):
        return ir_t.UnitType

    def visitAstBooleanType(self, node):
        return ir_t.BooleanType

    def visitAstI8Type(self, node):
        return ir_t.I8Type

    def visitAstI16Type(self, node):
        return ir_t.I16Type

    def visitAstI32Type(self, node):
        return ir_t.I32Type

    def visitAstI64Type(self, node):
        return ir_t.I64Type

    def visitAstF32Type(self, node):
        return ir_t.F32Type

    def visitAstF64Type(self, node):
        return ir_t.F64Type

    def visitAstClassType(self, node):
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
            explicitTypeArgs = self.handleClassTypeArgs(irDefn, node.typeArguments)
            typeArgs = tuple(prefixTypeArgs + list(explicitTypeArgs))
            return ir_t.ClassType(irDefn, typeArgs, flags)
        else:
            assert isinstance(irDefn, ir.TypeParameter)
            if len(node.typeArguments) > 0:
                raise TypeException(node.location,
                                    "%s: variable type does not accept type arguments" %
                                    node.name)
            return ir_t.VariableType(irDefn)

    def visitAstTupleType(self, node):
        clas = self.info.getTupleClass(len(node.types), node.location)
        types = self.handleClassTypeArgs(clas, node.types)
        flags = frozenset(map(astTypeFlagToIrTypeFlag, node.flags))
        return ir_t.ClassType(clas, types, flags)

    def visitAstErasedType(self, node):
        raise TypeException(node.location, "erased type can only be used as a type argument")

    def visitAstIntegerLiteral(self, node):
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

    def visitAstFloatLiteral(self, node):
        typeMap = { 32: ir_t.F32Type, 64: ir_t.F64Type }
        if node.width not in typeMap:
            raise TypeException(node.location, "invalid float literal width: %d" % node.width)
        return typeMap[node.width]

    def visitAstStringLiteral(self, node):
        return ir_t.getStringType()

    def visitAstBooleanLiteral(self, node):
        return ir_t.BooleanType

    def visitAstNullLiteral(self, node):
        return ir_t.getNullType()

    def isPrefixNode(self, node):
        return isinstance(node, ast.AstVariableExpression) or \
               isinstance(node, ast.AstPropertyExpression) or \
               (isinstance(node, ast.AstCallExpression) and \
                node.arguments is None and \
                self.isPrefixNode(node.callee))

    def visitPossiblePrefix(self, node):
        if self.isPrefixNode(node):
            return self.visit(node, mayBePrefix=True)
        else:
            return self.visit(node)

    def handleScopePrefix(self, prefix):
        """Processes the components of a scope prefix (a list of AstScopePrefixComponent). This
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
                if len(component.typeArguments) != len(explicitTypeParams):
                    raise TypeException(component.location,
                                        "%s: wrong number of type arguments; expected %d but have %d\n" %
                                        (component.name,
                                         len(explicitTypeParams),
                                         len(component.typeArguments)))
                if not hasPrefix:
                    # If this is the first prefix, the class is defined in a parent scope.
                    # There may be some type parameters defined in one of its parent scopes
                    # which are implied here.
                    implicitTypeParams = ir.getImplicitTypeParameters(irDefn)
                    implicitTypeArgs = map(ir_t.VariableType, implicitTypeParams)
                    typeArgs.extend(implicitTypeArgs)
                explicitTypeArgs = self.handleClassTypeArgs(irDefn, component.typeArguments)
                typeArgs.extend(explicitTypeArgs)
            else:
                if len(component.typeArguments) > 0:
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
        raise NotImplementedError

    def buildClassTypeArg(self, irParam, node):
        if isinstance(node, ast.AstErasedType):
            return ir_t.VariableType(irParam)
        else:
            return self.visit(node)

    def handleResult(self, node, result, *unusedArgs, **unusedKwargs):
        if result is not None:
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

    def visitAstPackage(self, node):
        self.visitChildren(node)

    def visitAstModule(self, node):
        self.visitChildren(node)

    def visitAstFunctionDefinition(self, node):
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

    def visitAstPrimaryConstructorDefinition(self, node):
        irFunction = self.info.getDefnInfo(node).irDefn
        irFunction.parameterTypes = [self.getReceiverType()] + map(self.visit, node.parameters)
        self.setMethodReceiverType(irFunction)

    def visitAstClassDefinition(self, node):
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

    def visitAstImportStatement(self, node):
        scope, typeArgs = self.handleScopePrefix(node.prefix)
        importedDefnInfos = self.info.getImportInfo(node).importedDefnInfos
        for defnInfo in importedDefnInfos:
            defnInfo.importedTypeArguments = typeArgs

    def visitAstTypeParameter(self, node):
        irParam = self.info.getDefnInfo(node).irDefn

        def visitBound(bound, default):
            if bound is None:
                return default
            else:
                ty = self.visit(bound)
                if ty.isNullable():
                    raise TypeException(bound.location,
                                        "%s: bound may not be nullable" % node.name)
                return ty

        self.scope().define(node.name)

        irParam.upperBound = visitBound(node.upperBound, ir_t.getRootClassType())
        irParam.lowerBound = visitBound(node.lowerBound, ir_t.getNothingClassType())
        self.typeParamsToCheck.append((irParam, node.location))

    def visitAstParameter(self, node):
        patTy = self.visit(node.pattern, True)
        if not patternMustMatch(node.pattern, patTy, self.info):
            raise TypeException(node.location,
                                "patterns which might not match can't be used as parameters")
        return patTy

    # We visit patterns to find the correct types for function parameters. Some patterns can't
    # directly be part of function parameters, but they may be part of other patterns, so
    # we need to type them all.

    def visitAstVariablePattern(self, node, isParam=False):
        if not isParam:
            return None
        if node.ty is None:
            raise TypeException(node.location, "%s: type not specified" % node.name)
        return self.visit(node.ty)

    def visitAstBlankPattern(self, node, isParam=False):
        if not isParam:
            return None
        if node.ty is None:
            raise TypeException(node.location, "type not specified")
        return self.visit(node.ty)

    def visitAstLiteralPattern(self, node, isParam=False):
        return self.visit(node.literal)

    def visitAstTuplePattern(self, node, isParam=False):
        if not isParam:
            return None
        patternTypes = tuple(self.visit(p, True) for p in node.patterns)
        tupleClass = self.info.getTupleClass(len(node.patterns), node.location)
        return ir_t.ClassType(tupleClass, patternTypes)

    def visitAstValuePattern(self, node, isParam=False):
        if not isParam:
            return None
        # Need to raise this early, since patternMustMatch is only called after a type is
        # returned. We may not be able to determine the type of this pattern, since it can
        # involve function calls as part of the scope prefix.
        raise TypeException(node.location, "value pattern can't be used in a parameter")

    def visitAstDestructurePattern(self, node, isParam=False):
        if not isParam:
            return None
        # Need to raise this early, since patternMustMatch is only called after a type
        # is returned.
        raise TypeException(node.location, "destructure pattern can't be used in a parameter")

    def visitDefault(self, node):
        self.visitChildren(node)

    def handleClassTypeArgs(self, irClass, nodes):
        typeParams = ir.getExplicitTypeParameters(irClass)
        assert len(typeParams) == len(nodes)
        typeArgs = []
        for param, node in zip(typeParams, nodes):
            arg = self.buildClassTypeArg(param, node)
            self.typeArgsToCheck.append((arg, param, node.location))
            typeArgs.append(arg)
        return tuple(typeArgs)

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

    def visitAstPackage(self, node):
        self.visitChildren(node)

    def visitAstModule(self, node):
        self.visitChildren(node)

    def visitAstVariableDefinition(self, node):
        if node.expression is not None:
            exprTy = self.visit(node.expression)
            mode = COMPILE_FOR_EFFECT
        else:
            exprTy = None
            mode = COMPILE_FOR_UNINITIALIZED
        self.visit(node.pattern, exprTy, mode)
        assert exprTy is None or patternMustMatch(node.pattern, exprTy, self.info)

    def visitAstFunctionDefinition(self, node):
        self.handleFunctionCommon(node, node.returnType, node.body)
        irDefn = self.info.getDefnInfo(node).irDefn
        if self.isExternallyVisible(irDefn):
            self.checkPublicType(irDefn.returnType, node.name, node.location)
            each(lambda p: self.checkPublicTypeParameter(p, node.name, node.location),
                 irDefn.typeParameters)
            each(lambda ty: self.checkPublicType(ty, node.name, node.location),
                 irDefn.parameterTypes)

    def visitAstClassDefinition(self, node):
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
            self.handleMethodCallOld(ir.CONSTRUCTOR_SUFFIX, node.id, supertype,
                                  [], superArgTypes, True, False, node.location)

        irInitializer = irClass.initializer
        irInitializer.parameterTypes = [thisType]
        irInitializer.variables[0].type = thisType
        irInitializer.returnType = ir_t.UnitType

        for member in node.members:
            if isinstance(member, ast.AstVariableDefinition):
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

    def visitAstPrimaryConstructorDefinition(self, node):
        self.handleFunctionCommon(node, None, None)

    def visitAstImportStatement(self, node):
        # TypeDeclarationVisitor does all the work.
        pass

    def visitAstTypeParameter(self, node):
        # TypeDeclarationVisitor does all the work, including finding the types of the bounds.
        # We don't need to do anything here.
        pass

    def visitAstParameter(self, node):
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

    def visitAstVariablePattern(self, node, exprTy, mode):
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

        patTy = self.findPatternType(varTy, exprTy, mode, node.name, node.location)

        if not isShadow:
            irDefn = self.info.getDefnInfo(node).irDefn
            if self.isExternallyVisible(irDefn):
                self.checkPublicType(patTy, node.name, node.location)
            irDefn.type = patTy
            scope.define(node.name)
        return patTy

    def visitAstBlankPattern(self, node, exprTy, mode):
        blankTy = self.visit(node.ty) if node.ty is not None else None
        patTy = self.findPatternType(blankTy, exprTy, mode, None, node.location)
        return patTy

    def visitAstLiteralPattern(self, node, exprTy, mode):
        litTy = self.visit(node.literal)
        patTy = self.findPatternType(litTy, exprTy, mode, None, node.location)
        return patTy

    def visitAstTuplePattern(self, node, exprTy, mode):
        tupleClass = self.info.getTupleClass(len(node.patterns), node.location)
        if isinstance(exprTy, ir_t.ClassType) and exprTy.clas is tupleClass:
            elementTypes = tuple(self.visit(p, ety, mode)
                                 for p, ety in zip(node.patterns, exprTy.typeArguments))
        else:
            elementTypes = tuple(self.visit(p, ir_t.getRootClassType(), mode)
                                 for p in node.patterns)
        tupleTy = ir_t.ClassType(tupleClass, elementTypes)
        patTy = self.findPatternType(tupleTy, exprTy, mode, None, node.location)
        return patTy

    def visitAstValuePattern(self, node, exprTy, mode):
        scope, receiverType = self.handlePatternScopePrefix(node.prefix)
        if len(node.prefix) > 0:
            if self.info.hasScopePrefixInfo(node.prefix[-1]):
                patTy = self.handlePossibleCall(scope, node.name, node.id, receiverType, [], [],
                                                hasReceiver=(receiverType is not None),
                                                hasArgs=False, mayAssign=False, mayBePrefix=False,
                                                hasPrefix=(len(node.prefix) > 0), loc=node.location)
            else:
                patTy = self.handleMethodCall(node.name, receiverType,
                                              [], [], node.id, node.location)
        else:
            patTy = self.handlePossibleCall(scope, node.name, node.id, receiverType, [], [],
                                            hasReceiver=(receiverType is not None),
                                            hasArgs=False, mayAssign=False, mayBePrefix=False,
                                            hasPrefix=(len(node.prefix) > 0), loc=node.location)
        return patTy

    def visitAstDestructurePattern(self, node, exprTy, mode):
        scope, receiverType = self.handlePatternScopePrefix(node.prefix[:-1])
        hasPrefix = len(node.prefix) > 1
        last = node.prefix[-1]
        typeArgs = map(self.visit, last.typeArguments)
        nameInfo = scope.lookup(last.name, last.location, fromExternal=hasPrefix)
        if nameInfo.isOverloaded() or isinstance(nameInfo.getDefnInfo().irDefn, ir.Function):
            # Call to possibly overloaded matcher function.
            receiverIsExplicit = receiverType is not None
            if not receiverIsExplicit and not hasPrefix and self.hasReceiverType():
                receiverType = self.getReceiverType()
            defnInfo, allTypeArgs, allArgTypes = findDefnInfoWithArgTypes(
                nameInfo.overloads, receiverType, receiverIsExplicit,
                typeArgs, [exprTy], nameInfo.name, last.location)
            self.info.setCallInfo(node.id, CallInfo(allTypeArgs))
            scope.use(defnInfo, node.id,
                      USE_AS_PROPERTY if receiverIsExplicit else USE_AS_VALUE, node.location)
            irDefn = defnInfo.irDefn
            returnType = irDefn.returnType.substitute(irDefn.typeParameters, allTypeArgs)
        else:
            # This was just another prefix. Finish up the bookkeeping.
            matcherDefnInfo = nameInfo.getDefnInfo()
            scope.use(matcherDefnInfo, last.id,
                      USE_AS_PROPERTY if receiverType is not None else USE_AS_VALUE,
                      node.location)
            matcherIrDefn = matcherDefnInfo.irDefn
            self.ensureTypeInfoForDefn(matcherIrDefn)
            if isinstance(matcherIrDefn, ir.Global) or \
               isinstance(matcherIrDefn, ir.Field) or \
               isinstance(matcherIrDefn, ir.Variable):
                if len(typeArgs) > 0:
                    raise TypeException(last.location,
                                        "cannot apply type arguments to value in destructure pattern")
                matcherReceiverType = matcherIrDefn.type
                matcherHasReceiver = True
                matcherClass = ir_t.getClassFromType(matcherReceiverType)
            elif isinstance(matcherIrDefn, ir.Class):
                if not matcherIrDefn.canApplyTypeArgs(typeArgs):
                    raise TypeException(last.location,
                                        "type arguments could not be applied for this class")
                matcherScopeId = self.info.getScope(matcherIrDefn).scopeId
                matcherScopePrefixInfo = ScopePrefixInfo(matcherIrDefn, matcherScopeId)
                self.info.setScopePrefixInfo(last.id, matcherScopePrefixInfo)
                matcherReceiverType = ir_t.ClassType(matcherIrDefn, typeArgs)
                matcherHasReceiver = False
                matcherClass = matcherIrDefn
            else:
                raise TypeException(last.location, "cannot use this definition for matching")

            # Look up the try-match method.
            matcherScope = self.info.getScope(matcherClass)
            try:
                if matcherHasReceiver:
                    returnType = self.handleMethodCall("try-match", matcherReceiverType, [],
                                                       [exprTy], node.id, node.location)
                else:
                    returnType = self.handlePossibleCall(matcherScope, "try-match", node.id,
                                                         matcherReceiverType, [], [exprTy],
                                                         hasReceiver=matcherHasReceiver,
                                                         hasArgs=True, mayAssign=False,
                                                         mayBePrefix=False, hasPrefix=True,
                                                         loc=node.location)
            except ScopeException:
                raise TypeException(node.location, "cannot match without `try-match` method")

        # Determine the expression types of the sub-patterns, based on what the matcher returns.
        # If there is one pattern, it should return Option[T1], and T1 is the expression type
        # (T1 may be a tuple). If there are more, it should return Option[(T1, ..., Tn)], and
        # T1, ..., Tn are the expression types.
        optionClass = self.info.getStdClass("Option", node.location)
        if not returnType.isObject() or \
           not ir_t.getClassFromType(returnType).isSubclassOf(optionClass):
            raise TypeException(node.location, "matcher must return std.Option")
        returnType = returnType.substituteForBaseClass(optionClass)
        returnTypeArg = returnType.typeArguments[0]
        n = len(node.patterns)
        if n == 1:
            patternTypes = [returnTypeArg]
        else:
            tupleClass = self.info.getTupleClass(len(node.patterns), node.location)
            if not returnTypeArg.isObject() or \
               not ir_t.getClassFromType(returnTypeArg).isSubclassOf(tupleClass):
                raise TypeException(node.location,
                                    "matcher must return std.Option[std.Tuple%d]" % n)
            returnTypeArg = returnTypeArg.substituteForBaseClass(tupleClass)
            patternTypes = returnTypeArg.typeArguments
        for subPat, subExprTy in zip(node.patterns, patternTypes):
            self.visit(subPat, subExprTy, mode)

        # We return the Some type argument as the pattern type. This isn't really used though.
        return returnTypeArg

    def visitAstLiteralExpression(self, node):
        ty = self.visit(node.literal)
        return ty

    def visitAstVariableExpression(self, node, mayBePrefix=False):
        if mayBePrefix:
            ty = self.handlePossiblePrefixSymbol(node.name, [], node.id,
                                                 None, None, node.location)
        else:
            ty = self.handleSimpleVariable(node.name, node.id, node.location)
        return ty

    def visitAstThisExpression(self, node):
        scope = self.scope()
        nameInfo = scope.lookupFromSelf("this", node.location, mayBeAssignment=False)
        defnInfo = nameInfo.getDefnInfo()
        scope.use(defnInfo, node.id, USE_AS_VALUE, node.location)
        ty = defnInfo.irDefn.type
        return ty

    def visitAstSuperExpression(self, node):
        scope = self.scope()
        nameInfo = scope.lookupFromSelf("this", node.location, mayBeAssignment=False)
        defnInfo = nameInfo.getDefnInfo()
        scope.use(defnInfo, node.id, USE_AS_VALUE, node.location)
        thisType = defnInfo.irDefn.type
        assert isinstance(thisType, ir_t.ClassType) and \
               len(thisType.clas.supertypes) > 0
        superType = thisType.clas.supertypes[0]
        return superType

    def visitAstBlockExpression(self, node):
        lastTy = ir_t.UnitType
        for stmt in node.statements:
            stmtTy = self.visit(stmt)
            assert stmtTy is not None or isinstance(stmt, ast.AstDefinition)
            lastTy = stmtTy if stmtTy else ir_t.UnitType
        return lastTy

    def visitAstAssignExpression(self, node):
        rightTy = self.visit(node.right)
        leftTy = self.visit(node.left)
        if not rightTy.isSubtypeOf(leftTy):
            raise TypeException(node.location,
                                "for assignment, expected %s but was %s" %
                                (str(leftTy), str(rightTy)))
        return leftTy

    def visitAstPropertyExpression(self, node, mayBePrefix=False):
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
            ty = self.handlePossiblePrefixSymbol(node.propertyName, [], node.id,
                                                 receiverScope, receiverType, node.location)
        elif hasReceiver:
            ty = self.handleMethodCall(node.propertyName, receiverType,
                                       [], [], node.id, node.location)
        else:
            ty = self.handlePossibleCall(receiverScope, node.propertyName, node.id,
                                         receiverType, [], [], hasReceiver=False,
                                         hasArgs=False, mayAssign=False, mayBePrefix=False,
                                         hasPrefix=True, loc=node.location)
        return ty

    def visitAstCallExpression(self, node, mayBePrefix=False):
        typeArgs = map(self.visit, node.typeArguments)
        hasArgs = node.arguments is not None
        arguments = node.arguments if hasArgs else []
        argTypes = map(self.visit, arguments)
        if hasArgs:
            mayBePrefix = False

        if isinstance(node.callee, ast.AstVariableExpression):
            ty = self.handlePossibleCall(self.scope(), node.callee.name, node.id,
                                         None, typeArgs, argTypes,
                                         hasArgs=hasArgs, hasReceiver=False,
                                         mayAssign=False, mayBePrefix=mayBePrefix,
                                         hasPrefix=False, loc=node.location)
        elif isinstance(node.callee, ast.AstPropertyExpression):
            receiverType = self.visitPossiblePrefix(node.callee.receiver)
            if self.info.hasScopePrefixInfo(node.callee.receiver):
                receiverScope = self.info.getScope(
                    self.info.getScopePrefixInfo(node.callee.receiver).scopeId)
                if mayBePrefix:
                    ty = self.handlePossiblePrefixSymbol(node.callee.propertyName,
                                                         typeArgs, node.id,
                                                         receiverScope, receiverType,
                                                         node.location)
                else:
                    ty = self.handlePossibleCall(receiverScope, node.callee.propertyName, node.id,
                                                 receiverType, typeArgs, argTypes,
                                                 hasReceiver=False, hasArgs=hasArgs,
                                                 mayAssign=False, mayBePrefix=mayBePrefix,
                                                 hasPrefix=True, loc=node.location)
            else:
                receiverScope = self.info.getScope(ir_t.getClassFromType(receiverType))
                ty = self.handleMethodCall(node.callee.propertyName, receiverType,
                                           typeArgs, argTypes, node.id, node.location)
        elif isinstance(node.callee, ast.AstThisExpression) or \
             isinstance(node.callee, ast.AstSuperExpression):
            receiverType = self.visit(node.callee)
            self.handleMethodCallOld(ir.CONSTRUCTOR_SUFFIX, node.id,
                                  receiverType, typeArgs, argTypes,
                                  hasArgs, False, node.location)
            ty = ir_t.UnitType
        else:
            # TODO: callable expression
            raise NotImplementedError
        return ty

    def visitAstUnaryExpression(self, node):
        receiverType = self.visit(node.expr)
        ty = self.handleMethodCallOld(node.operator, node.id, receiverType,
                                   [], [], True, False, node.location)
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
            ty = ir_t.BooleanType
        elif node.operator[-1] == ":":
            # Right associative operator; the receiver is on the right.
            ty = self.handleMethodCallOld(node.operator, node.id, rightTy,
                                       [], [leftTy], True, True, node.location)
        else:
            # Left associative operator; the receiver is on the left.
            ty = self.handleMethodCallOld(node.operator, node.id, leftTy,
                                       [], [rightTy], True, True, node.location)
        return ty

    def visitAstTupleExpression(self, node):
        types = tuple(map(self.visit, node.expressions))
        if len(types) > compile_info.MAX_TUPLE_LENGTH:
            raise TypeException(node.location,
                                "tuples longer than %d elements not supported" % len(types))
        for (expr, ty) in zip(node.expressions, types):
            if not ty.isObject():
                raise TypeException(expr.location,
                                    "expression with primitive type %s cannot be used in tuple" %
                                    str(ty))

        clas = self.info.getTupleClass(len(types), node.location)
        return ir_t.ClassType(clas, types)

    def visitAstIfExpression(self, node):
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

    def visitAstWhileExpression(self, node):
        condTy = self.visit(node.condition)
        if not self.isConditionType(condTy):
            raise TypeException(node.condition.location, "condition must be boolean")
        self.visit(node.body)
        return ir_t.UnitType

    def visitAstMatchExpression(self, node):
        exprTy = self.visit(node.expression)
        resultTy = self.visit(node.matcher, exprTy)
        return resultTy

    def visitAstThrowExpression(self, node):
        exnTy = self.visit(node.exception)
        if not exnTy.isSubtypeOf(ir_t.ClassType(getExceptionClass())):
            raise TypeException(node.location, "throw expression must produce an Exception")
        return ir_t.NoType

    def visitAstTryCatchExpression(self, node):
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

    def visitAstPartialFunctionExpression(self, node, valueTy):
        ty = ir_t.NoType
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
            retTy = ir_t.UnitType
        else:
            retTy = self.visit(node.expression)
        self.checkAndHandleReturnType(retTy, node.location)
        return ir_t.NoType

    def visitAstClassType(self, node):
        ty = super(DefinitionTypeVisitor, self).visitAstClassType(node)
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

    def findPatternType(self, patTy, exprTy, mode, name, loc):
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
            else:
                return patTy
        else:
            return exprTy

    def handleClassTypeArgs(self, irClass, typeArgs):
        typeParams = ir.getExplicitTypeParameters(irClass)
        assert len(typeParams) == len(typeArgs)
        types = []
        for tp, ta in zip(typeParams, typeArgs):
            with VarianceScope.forArgument(self, tp.variance()):
                ty = self.buildClassTypeArg(tp, ta)
            if not (tp.lowerBound.isSubtypeOf(ty) and
                    ty.isSubtypeOf(tp.upperBound)):
                raise TypeException(ta.location, "%s: type argument is not in bounds" % tp.name)
            types.append(ty)
        return tuple(types)

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
            if isinstance(node, ast.AstPrimaryConstructorDefinition):
                vscope = VarianceScope(self, COVARIANT, irFunction.getReceiverClass())
            elif irFunction.isMethod() and not irFunction.isConstructor():
                vscope = VarianceScope(self, CONTRAVARIANT, irFunction.getReceiverClass())
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
                    vscope = VarianceScope(self, COVARIANT, irFunction.getReceiverClass())
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
                        vscope = VarianceScope(self, COVARIANT, irFunction.getReceiverClass())
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

    def handleMethodCallOld(self, name, useAstId, receiverType,
                            typeArgs, argTypes, hasArgs, mayAssign, loc):
        irClass = ir_t.getClassFromType(receiverType)
        scope = self.info.getScope(irClass)
        return self.handlePossibleCall(scope, name, useAstId,
                                       receiverType, typeArgs, argTypes,
                                       hasReceiver=True, hasArgs=hasArgs,
                                       mayAssign=mayAssign, mayBePrefix=False,
                                       hasPrefix=True, loc=loc)

    def handlePossibleCall(self, scope, name, useAstId,
                           receiverType, typeArgs, argTypes,
                           hasReceiver, hasArgs, mayAssign,
                           mayBePrefix, hasPrefix, loc):
        """Analyzes types for any pattern or expression that might be a call.

        This includes pretty much pattern or expression that involves a symbol. This method
        does the following:

        - Looks up the definition being referenced, possibly using an implied receiver type,
          resolving overloads if needed.
        - Saves `ScopePrefixInfo` for the location if it turns out this is a prefix.
        - Saves `CallInfo` with all type arguments (including those implied by the scope).
        - Saves `UseInfo` with the definition and the use kind. If the definition is being used
          as a value, this affects capturing later. Foreign definitions will be
          externalized later.
        - Analyzes type info for the referenced definition (e.g., to determine the implied
          return type in a function call).

        Args:
            scope: the scope containing the pattern or expression being processed.
            name: the symbol possibly being called.
            useAstId: the AST id of the calling expression. Used as a key to save various
                information and in errors.
            receiverType: the type of the receiver expression or `None` if there wasn't one.
                This may also be a prefix type (e.g., for a static method call).
            typeArgs: a list of type arguments passed as part of the call.
            argTypes: a list of types of arguments passed as part of the call, not including
                the receiver if there was one.
            hasReceiver: `True` if the receiver expression actually produces an object (as
                opposed to being a prefix).
            hasArgs: `True` if arguments were passed explicitly (including if there was an
                empty argument list). This affects whether a class is treated as a prefix
                or a constructor call.
            mayAssign: `True` if this may be a compound assignment. For example, a "+=" operator
                may be converted to an assignment with a "+" operator.
            mayBePrefix: `True` if this expression may be part of a scope prefix.
            hasPrefix: `True` if there was a prefix before this expression. This determines
                whether a lookup should be done in the current scope or the receiver scope.
            isOperator: `True` if this is a unary or binary operator expression or pattern.
                This enables lookups in this scope and in the receiver scope.
            loc: the location of the expression (used in errors).

        Returns:
            The type of the definition being referenced. For functions, the return type is
            returned. For classes, the receiver type is returned. For packages and package
            prefixes, the package type is returned.

        Raises:
            ScopeException: if a definition with this name can't be found or used.
            TypeException: if the definition can't be used because of a type mismatch.
        """
        receiverIsExplicit = receiverType is not None
        if not receiverIsExplicit and not hasPrefix and self.hasReceiverType():
            receiverType = self.getReceiverType()

        assert not mayBePrefix or not hasArgs

        assert name != ir.CONSTRUCTOR_SUFFIX or receiverIsExplicit
        if name == ir.CONSTRUCTOR_SUFFIX or receiverIsExplicit:
            useKind = USE_AS_PROPERTY
        else:
            useKind = USE_AS_VALUE

        nameInfo = scope.lookup(name, loc,
                                fromExternal=hasPrefix, mayBeAssignment=mayAssign)

        if nameInfo.isScope():
            isPrefix = mayBePrefix
            if isPrefix:
                defnInfo = nameInfo.getDefnInfo()
                if isinstance(defnInfo.irDefn, ir.Class):
                    defnScopeId = self.info.getScope(defnInfo.irDefn).scopeId
                else:
                    parentScope = self.info.getScope(defnInfo.scopeId)
                    defnScopeId = parentScope.scopeForPrefix(name, loc).scopeId
                scopePrefixInfo = ScopePrefixInfo(defnInfo.irDefn, defnScopeId)
                self.info.setScopePrefixInfo(useAstId, scopePrefixInfo)
            else:
                if nameInfo.isClass() and not hasArgs:
                    raise TypeException(loc, "class name can't be used as a value")
                elif nameInfo.isPackagePrefix():
                    raise TypeException(loc, "package prefix can't be used as a value")
        else:
            isPrefix = False

        if nameInfo.isClass():
            irClass = nameInfo.getDefnInfo().irDefn
            explicitTypeParams = ir.getExplicitTypeParameters(irClass)
            if len(typeArgs) != len(explicitTypeParams):
                raise TypeException(loc,
                                    "wrong number of type arguments: expected %d but have %d" %
                                    (len(typeArgs), len(explicitTypeParams)))
            if not all(tp.contains(ta) for tp, ta in zip(explicitTypeParams, typeArgs)):
                raise TypeException(loc, "type error in type arguments for constructor")
            implicitTypeParams = ir.getImplicitTypeParameters(irClass)
            classTypeArgs = tuple([ir_t.VariableType(tp) for tp in implicitTypeParams] + typeArgs)
            receiverType = ir_t.ClassType(irClass, classTypeArgs, None)
            if isPrefix:
                defnInfo = nameInfo.getDefnInfo()
                allTypeArgs = classTypeArgs
                allArgTypes = []
            else:
                if irClass is getNothingClass():
                    raise TypeException(loc, "cannot instantiate Nothing")
                nameInfo = nameInfo.getInfoForConstructors(self.info)
                useKind = USE_AS_CONSTRUCTOR
                defnInfo, allTypeArgs, allArgTypes = findDefnInfoWithArgTypes(
                    nameInfo.overloads, receiverType,
                    True,  # receiverIsExplicit
                    [], argTypes, nameInfo.name, loc)
        else:
            defnInfo, allTypeArgs, allArgTypes = findDefnInfoWithArgTypes(
                nameInfo.overloads, receiverType, receiverIsExplicit,
                typeArgs, argTypes, nameInfo.name, loc)

        irDefn = defnInfo.irDefn
        receiverExprNeeded = receiverIsExplicit and \
                             (isinstance(irDefn, ir.Field) or \
                              (isinstance(irDefn, ir.Function) and \
                               irDefn.isMethod() and \
                               not irDefn.isConstructor()))
        if receiverExprNeeded and not hasReceiver:
            raise TypeException(loc, "%s: cannot access without receiver" % name)
        callInfo = CallInfo(allTypeArgs)
        self.info.setCallInfo(useAstId, callInfo)

        self.scope().use(defnInfo, useAstId, useKind, loc)

        return self.getDefnType(receiverType, receiverIsExplicit, irDefn, allTypeArgs)

    def handlePatternScopePrefix(self, prefix):
        """Processes the components of a pattern prefix.

        Components can either be scope prefixes (for example, package names, class names) or
        regular loads or calls (field loads, nullary function calls). Scope lookups come first,
        if they are present. This method determines how many components are scope prefixes and
        saves `ScopePrefixInfo` for each of them. The regular set of `UseInfo` and `CallInfo`
        is saved for all components.

        Args:
            prefix: a list of `AstScopePrefixComponent`s to process. It may be empty.

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
        firstTypeArgs = map(self.visit, prefix[0].typeArguments)
        ty = self.handlePossiblePrefixSymbol(prefix[0].name, firstTypeArgs, prefix[0].id,
                                             None, None, prefix[0].location)
        self.info.setType(prefix[0], ty)

        i = 1
        while i < len(prefix) and self.info.hasScopePrefixInfo(prefix[i - 1]):
            hasPrefix = True
            scope = self.info.getScope(self.info.getScopePrefixInfo(prefix[i - 1]).scopeId)
            typeArgs = map(self.visit, prefix[i].typeArguments)
            ty = self.handlePossiblePrefixSymbol(prefix[0].name, typeArgs, prefix[0].id,
                                                 scope, ty, prefix[0].location)
            self.info.setType(prefix[i], ty)
            i += 1
        while i < len(prefix):
            scope = self.info.getScope(ir_t.getClassFromType(ty))
            typeArgs = map(self.visit, prefix[i].typeArguments)
            ty = self.handleMethodCall(prefix[i].name, receiverType, typeArgs, [],
                                       prefix[i].id, prefix[i].location)
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
        if nameInfo.isClass():
            raise TypeException(loc, "%s: class name can't be used as a value" % name)
        if nameInfo.isPackagePrefix():
            raise TypeException(loc, "package prefix can't be used as a value")
        defnInfo, allTypeArgs, _ = findDefnInfoWithArgTypes(
            nameInfo.overloads, receiverType, False,
            [], [], nameInfo.name, loc)
        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))
        scope.use(defnInfo, useAstId, USE_AS_VALUE, loc)
        return self.getDefnType(receiverType, False, defnInfo.irDefn, allTypeArgs)

    def handlePossiblePrefixSymbol(self, name, typeArgs, useAstId, scope, scopeType, loc):
        """Processes a symbol reference which may be part of a scope prefix.

        If the symbol does turn out to be part of a prefix, `ScopePrefixInfo` will be saved
        at `useAstId`. The normal `UseInfo` and `CallInfo` will be saved in any case.

        Args:
            name: the symbol being referenced.
            typeArgs: a list of type arguments that are part of the reference. Pass the empty
                list if there are none.
            useAstId: the AST id of the reference. This will be used to save info.
            scope: the `Scope` where the reference occurs (optional). If `None`, the reference
                is assumed to be made in the current scope, and a self-lookup will be performed.
                Otherwise, an external lookup will be performed.
            scopeType: the `Type` of the scope where the reference occurs (optional). This
                should be `None` iff `scope` is `None`. The receiver type will be taken from
                the current scope, if it has one.
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
            defnInfo, allTypeArgs, _ = findDefnInfoWithArgTypes(
                nameInfo.overloads, receiverType, receiverIsExplicit,
                typeArgs, [], nameInfo.name, loc)

        callInfo = CallInfo(allTypeArgs)
        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))

        self.scope().use(defnInfo, useAstId, USE_AS_PROPERTY, loc)
        return self.getDefnType(receiverType, True, defnInfo.irDefn, allTypeArgs)

    def handleMethodCall(self, name, receiverType, typeArgs, argTypes, useAstId, loc):
        """Handles a reference to a symbol inside an object.

        This can be used for methods, fields, and inner-class constructors. The call must
        be prefixed with an expression that produces an object (scope prefix is not sufficient).
        `UseInfo` and `CallInfo` will be saved.

        Args:
            name: the name of the symbol being referenced.
            receiverType: the `Type` of the object containing the symbol.
            typeArgs: a list of `Type` arguments passed as part of the call. May be empty.
            argTypes: a list of `Type`s of regular arguments passed as part of the call. May
                be empty.
            useAstId: the AST id where the symbol is referenced. Used to save info.
            loc: the location of the reference in source code. Used in errors.

        Returns:
            The `Type` of the definition that was referenced (with type substitution performed).

        Raises:
            ScopeException: if a definition with this name can't be found or used.
            TypeException: if the definition can't be used because of a type mismatch.
        """
        irClass = ir_t.getClassFromType(receiverType)
        scope = self.info.getScope(irClass)
        nameInfo = scope.lookupFromExternal(name, loc)
        useKind = USE_AS_PROPERTY

        if nameInfo.isClass():
            defnInfo = nameInfo.getDefnInfo()
            irClass = defnInfo.irClass
            receiverType = self.getReceiverTypeForClass(irClass, typeArgs, loc)
            allTypeArgs = receiverType.typeArgs
            if irClass is getNothingClass():
                raise TypeException(loc, "cannot instantiate Nothing")
            nameInfo = nameInfo.getInfoForConstructors(self.info)
            useKind = USE_AS_CONSTRUCTOR
            defnInfo, allTypeargs, _ = findDefnInfoWithArgTypes(
                nameInfo.overloads, receiverType,
                True,  # receiverIsExplicit
                [],  # typeArgs
                argTypes, nameInfo.name, loc)
        else:
            defnInfo, allTypeArgs, _ = findDefnInfoWithArgTypes(
                nameInfo.overloads, receiverType,
                True,  # receiverIsExplicit
                typeArgs, argTypes, nameInfo.name, loc)

        self.info.setCallInfo(useAstId, CallInfo(allTypeArgs))
        self.scope().use(defnInfo, useAstId, useKind, loc)
        return self.getDefnType(receiverType, True, defnInfo.irDefn, allTypeArgs)

    def getReceiverTypeForClass(self, irClass, typeArgs, loc):
        explicitTypeParams = ir.getExplicitTypeParameters(irClass)
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

    def getDefnType(self, receiverType, receiverIsExplicit, irDefn, typeArgs):
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

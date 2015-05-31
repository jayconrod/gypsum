# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import ast
import compile_info
from errors import TypeException
import ir
import ir_types as ir_t
from builtins import getExceptionClass, getPackageClass, getNothingClass
from utils import COMPILE_FOR_VALUE, COMPILE_FOR_MATCH, each
from compile_info import USE_AS_VALUE, USE_AS_TYPE, USE_AS_PROPERTY, USE_AS_CONSTRUCTOR, NORMAL_MODE, STD_MODE, NOSTD_MODE, CallInfo, PackageInfo
from flags import COVARIANT, CONTRAVARIANT, PROTECTED, PUBLIC
import scope_analysis


def analyzeTypes(info):
    """Analyzes a syntax, determines a type for each node, and reports any inconsistencies."""
    # Establish type information for class supertypes, type parameter upper/lower bounds,
    # and function parameter types. This is needed for `Type.isSubtypeOf` and for typing
    # function calls in expressions.
    declarationVisitor = DeclarationTypeVisitor(info)
    declarationVisitor.visit(info.ast)
    declarationVisitor.checkTypes()

    # Add type annotations for AST nodes which need them, and add type information to
    # the package.
    analysis = DefinitionTypeVisitor(info)
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
                func.getReceiverClass(), func.override.getReceiverClass())
            if not func.returnType.isSubtypeOf(overridenReturnType):
                raise TypeException(func.getLocation(),
                                    "%s: return type is not subtype of overriden function" %
                                    func.name)


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

    def preVisit(self, node, *unused):
        if self.info.hasScope(node.id):
            scope = self.info.getScope(node.id)
            self.scopeStack.append(scope)
            enclosingClass = scope.findEnclosingClass()
            if enclosingClass is not None:
                receiverType = ir_t.ClassType.forReceiver(enclosingClass)
                self.receiverTypeStack.append(receiverType)
            else:
                self.receiverTypeStack.append(None)

    def postVisit(self, node, *unused):
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
        nameInfo = self.scope().lookup(node.name, node.location)
        if nameInfo.isOverloaded() or not nameInfo.getDefnInfo().irDefn.isTypeDefn():
            raise TypeException(node.location, "%s: does not refer to a type" % node.name)
        defnInfo = nameInfo.getDefnInfo()
        self.scope().use(defnInfo, node.id, USE_AS_TYPE, node.location)
        irDefn = nameInfo.getDefnInfo().irDefn

        flags = frozenset(map(astTypeFlagToIrTypeFlag, node.flags))

        if isinstance(irDefn, ir.Class):
            explicitTypeParams = ir.getExplicitTypeParameters(irDefn)
            if len(node.typeArguments) != len(explicitTypeParams):
                raise TypeException(node.location,
                                    "%s: wrong number of type arguments; expected %d but have %d\n" %
                                    (node.name,
                                     len(explicitTypeParams),
                                     len(node.typeArguments)))
            explicitTypeArgs = self.handleAstClassTypeArgs(irDefn, node.typeArguments)
            implicitTypeArgs = tuple(map(ir_t.VariableType, ir.getImplicitTypeParameters(irDefn)))
            allTypeArgs = explicitTypeArgs + implicitTypeArgs
            return ir_t.ClassType(irDefn, allTypeArgs, flags)
        else:
            assert isinstance(irDefn, ir.TypeParameter)
            if len(node.typeArguments) > 0:
                raise TypeException(node.location,
                                    "%s: type parameter cannot accept type arguments" %
                                    irDefn.name)
            return ir_t.VariableType(irDefn)

    def visitAstProjectedType(self, node):
        components = []
        def flatten(node):
            if isinstance(node, ast.AstClassType):
                components.append(node)
            else:
                assert isinstance(node, ast.AstProjectedType)
                flatten(node.left)
                flatten(node.right)
        flatten(node)

        scope = self.scope()
        defnInfo = None  # don't cache irDefn, since Scope.use may externalize it.
        isProjected = False
        typeArgs = []
        for component in components:
            nameInfo = scope.lookup(component.name, component.location, localOnly=isProjected)
            if nameInfo.isOverloaded():
                raise TypeException(component.location,
                                    "%s: cannot project from overloaded symbol" %
                                    component.name)
            defnInfo = nameInfo.getDefnInfo()
            if not isinstance(defnInfo.irDefn, ir.Package) and \
               not isinstance(defnInfo.irDefn, ir.PackagePrefix) and \
               not isinstance(defnInfo.irDefn, ir.Class):
                raise TypeException(component.location,
                                    "%s: cannot project from non-class type" %
                                    component.name)

            if len(component.typeArguments) > 0:
                if not isinstance(defnInfo.irDefn, ir.IrDefinition):
                    raise TypeException(component.location,
                                        "%s: non-type definition does not accept type arguments" %
                                        component.name)
                typeArgs.extend(self.handleAstClassTypeArgs(defnInfo.irDefn,
                                                            component.typeArguments))

            if isinstance(defnInfo.irDefn, ir.Package) or \
               isinstance(defnInfo.irDefn, ir.IrDefinition):
                self.scope().use(defnInfo, component.id, USE_AS_TYPE, component.location)
                scope = self.info.getScope(defnInfo.irDefn)
            else:
                assert isinstance(defnInfo.irDefn, ir.PackagePrefix)
                scope = self.info.getScope(defnInfo.scopeId).scopeForPrefix(component.name)
            isProjected = True

        irDefn = defnInfo.irDefn
        if isinstance(irDefn, ir.Package) or not irDefn.isTypeDefn():
            raise TypeException(node.location, "cannot project a non-type definition")
        if isinstance(irDefn, ir.Class):
            assert len(typeArgs) == len(irDefn.typeParameters)
            return ir_t.ClassType(irDefn, tuple(typeArgs))
        else:
            raise NotImplementedError

    def visitAstTupleType(self, node):
        clas = self.info.getTupleClass(len(node.types), node.location)
        def checkType(astType):
            irType = self.visit(astType)
            if irType.isPrimitive():
                raise TypeException(astType.location, "primitive type cannot be part of tuple")
            return irType
        types = tuple(map(checkType, node.types))
        flags = frozenset(map(astTypeFlagToIrTypeFlag, node.flags))
        return ir_t.ClassType(clas, types, flags)

    def handleAstClassTypeArgs(self, irClass, nodes):
        raise NotImplementedError

    def handleResult(self, node, result, *unused):
        if result is not None:
            self.info.setType(node, result)
        return result


class DeclarationTypeVisitor(TypeVisitorBase):
    """Analyzes functions, classes, and type parameters and saves supertypes, upper bounds,
    lower bounds, and parameter types. The analysis proceeds in lexical order over the ASt.
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
        return self.visit(node.pattern, True)

    def visitAstVariablePattern(self, node, isParam=False):
        if not isParam:
            return None
        if node.ty is None:
            raise TypeException(node.location, "%s: type not specified" % node.name)
        return self.visit(node.ty)

    def visitDefault(self, node):
        self.visitChildren(node)

    def handleAstClassTypeArgs(self, irClass, nodes):
        typeArgs = tuple(map(self.visit, nodes))
        typeParams = ir.getExplicitTypeParameters(irClass)
        assert len(typeParams) == len(typeArgs)
        for tp, ta, node in zip(typeParams, typeArgs, nodes):
            self.typeArgsToCheck.append((ta, tp, node.location))
        return typeArgs

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

    def preVisit(self, node, *unused):
        super(DefinitionTypeVisitor, self).preVisit(node, *unused)
        if not self.info.hasDefnInfo(node):
            return
        irDefn = self.info.getDefnInfo(node).irDefn
        if isinstance(irDefn, ir.Class):
            self.functionStack.append(None)
        elif isinstance(irDefn, ir.Function):
            self.functionStack.append(FunctionState(irDefn))

    def postVisit(self, node, *unused):
        super(DefinitionTypeVisitor, self).postVisit(node, *unused)
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
        else:
            exprTy = None
        self.visit(node.pattern, exprTy)

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
        if node.superArgs is not None and \
           (len(node.superArgs) > 0 or hasPrimaryOrDefaultCtor):
            supertype = irClass.supertypes[0]
            superArgTypes = map(self.visit, node.superArgs)
            self.handleMethodCall(ir.CONSTRUCTOR_SUFFIX, node.id, supertype,
                                  [], superArgTypes, False, node.location)

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

    def visitAstTypeParameter(self, node):
        # SubtypeVisitor does all the work, including finding the types of the bounds.
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
        if self.isExternallyVisible(irDefn):
            self.checkPublicType(patTy, node.name, node.location)
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

    def visitAstPropertyExpression(self, node):
        # We handle sequences of property expressions all at once without recursing. This is
        # because some part of the sequence may be a package name. Prefixes of package names
        # are invalid expressions and cannot be typed. We also don't want to add dependencies
        # on packages whose names are prefixes of other package names.
        nodeNames = [(node, node.propertyName)]
        n = node.receiver
        while isinstance(n, ast.AstPropertyExpression):
            nodeNames.append((n, n.propertyName))
            n = n.receiver
        nodeNames.append((n, n.name if isinstance(n, ast.AstVariableExpression) else None))
        nodeNames.reverse()

        packageNameLength = 0
        package = None
        scope = self.scope()
        if nodeNames[0][1] is not None:
            # The expression starts with a variable expression, which could be the start
            # of a package name.
            packageNameInfo = None
            while packageNameLength < len(nodeNames):
                node, name = nodeNames[packageNameLength]
                nextNameInfo = scope.lookup(name, node.location)
                if not (nextNameInfo.isPackagePrefix() or nextNameInfo.isPackage()):
                    break
                packageNameInfo = nextNameInfo
                packageNameLength += 1
                prefixScopeId = packageNameInfo.getDefnInfo().scopeId
                scope = self.info.getScope(prefixScopeId).scopeForPrefix(name)

            if packageNameLength > 0:
                defnInfo = packageNameInfo.getDefnInfo()
                package = defnInfo.irDefn
                if not isinstance(package, ir.Package):
                    assert isinstance(package, ir.PackagePrefix)
                    raise TypeException(node.location,
                                        "%s is not the full name of a package" %
                                        str(package.name))
                packageNode, _ = nodeNames[packageNameLength - 1]
                self.scope().use(defnInfo, packageNode.id, USE_AS_VALUE, packageNode.location)
                packageInfo = PackageInfo(package, scope.scopeId)
                self.info.setPackageInfo(packageNode, packageInfo)
                packageType = ir_t.getPackageType()
                self.info.setType(packageNode, packageType)

        # Now we know the package prefix (if there was one), we can deal with the rest of
        # the expression.
        if packageNameLength == 0:
            receiverType = self.visit(nodeNames[0][0])
            start = 1
        elif packageNameLength == len(nodeNames):
            receiverType = packageType
            start = len(nodeNames)
        else:
            self.info.package.ensureDependency(package)
            n, name = nodeNames[packageNameLength]
            receiverType = self.handlePossibleCall(scope, name, node.id, None,
                                                   [], [], False, node.location)
            start = packageNameLength + 1

        for i in xrange(start, len(nodeNames)):
            n, name = nodeNames[i]
            assert isinstance(n, ast.AstPropertyExpression)
            receiverType = self.handleMethodCall(name, n.id, receiverType,
                                                 [], [], False, n.location)
            self.info.setType(n, receiverType)

        return receiverType

    def visitAstCallExpression(self, node):
        typeArgs = map(self.visit, node.typeArguments)
        argTypes = map(self.visit, node.arguments)
        if isinstance(node.callee, ast.AstVariableExpression):
            ty = self.handlePossibleCall(self.scope(), node.callee.name, node.id,
                                         None, typeArgs, argTypes, False, node.location)
        elif isinstance(node.callee, ast.AstPropertyExpression):
            receiverType = self.visit(node.callee.receiver)
            receiverUseInfo = self.info.getUseInfo(node.callee.receiver)
            if isinstance(receiverUseInfo.defnInfo.irDefn, ir.Package):
                packageInfo = self.info.getPackageInfo(node.callee.receiver)
                packageScope = self.info.getScope(packageInfo.scopeId)
                ty = self.handlePossibleCall(packageScope, node.callee.propertyName, node.id,
                                             None, typeArgs, argTypes, False, node.location)
            else:
                ty = self.handleMethodCall(node.callee.propertyName, node.id,
                                           receiverType, typeArgs, argTypes,
                                           False, node.location)
        elif isinstance(node.callee, ast.AstThisExpression) or \
             isinstance(node.callee, ast.AstSuperExpression):
            receiverType = self.visit(node.callee)
            self.handleMethodCall(ir.CONSTRUCTOR_SUFFIX, node.id,
                                  receiverType, typeArgs, argTypes, False, node.location)
            ty = ir_t.UnitType
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
            ty = ir_t.BooleanType
        else:
            ty = self.handleMethodCall(node.operator, node.id, leftTy,
                                       [], [rightTy], True, node.location)
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

    def handleAstClassTypeArgs(self, irClass, typeArgs):
        typeParams = ir.getExplicitTypeParameters(irClass)
        assert len(typeParams) == len(typeArgs)
        types = []
        for tp, ta in zip(typeParams, typeArgs):
            with VarianceScope.forArgument(self, tp.variance()):
                ty = self.visit(ta)
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
             isinstance(irDefn, ir.Package):
            pass   # already done in previous pass.
        else:
            raise NotImplementedError

    def handleMethodCall(self, name, useAstId, receiverType,
                         typeArgs, argTypes, mayAssign, loc):
        irClass = ir_t.getClassFromType(receiverType)
        scope = self.info.getScope(irClass)
        return self.handlePossibleCall(scope, name, useAstId,
                                       receiverType, typeArgs, argTypes,
                                       mayAssign, loc)

    def handlePossibleCall(self, scope, name, useAstId,
                           receiverType, typeArgs, argTypes,
                           mayAssign, loc):
        receiverIsExplicit = receiverType is not None
        if not receiverIsExplicit and self.hasReceiverType():
            receiverType = self.getReceiverType()

        assert name != ir.CONSTRUCTOR_SUFFIX or receiverIsExplicit
        if name == ir.CONSTRUCTOR_SUFFIX or receiverIsExplicit:
            useKind = USE_AS_PROPERTY
        else:
            useKind = USE_AS_VALUE

        nameInfo = scope.lookup(name, loc,
                                localOnly=receiverIsExplicit, mayBeAssignment=mayAssign)
        if nameInfo.isPackagePrefix():
            raise TypeException(loc,
                                "%s is part of a package name prefix and is not valid here" %
                                name)

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
            typeArgs = []
            nameInfo = nameInfo.getInfoForConstructors(self.info)
            receiverIsExplicit = True
            useKind = USE_AS_CONSTRUCTOR

        (defnInfo, allTypeArgs, allArgTypes) = \
            nameInfo.findDefnInfoWithArgTypes(receiverType, receiverIsExplicit,
                                              typeArgs, argTypes, loc)
        irDefn = defnInfo.irDefn
        receiverExprNeeded = receiverIsExplicit and \
                             (isinstance(irDefn, ir.Field) or \
                              (isinstance(irDefn, ir.Function) and \
                               irDefn.isMethod() and \
                               not irDefn.isConstructor()))
        callInfo = CallInfo(allTypeArgs, receiverExprNeeded)
        self.info.setCallInfo(useAstId, callInfo)
        if isinstance(irDefn, ir.Package):
            defnScope = self.info.getScope(defnInfo.scopeId)
            packageInfo = PackageInfo(irDefn, defnScope.scopeForPrefix(name).scopeId)
            self.info.setPackageInfo(useAstId, packageInfo)

        self.scope().use(defnInfo, useAstId, useKind, loc)
        self.ensureTypeInfoForDefn(irDefn)
        if isinstance(irDefn, ir.Function):
            if irDefn.isConstructor():
                return receiverType
            else:
                assert len(allTypeArgs) == len(irDefn.typeParameters)
                ty = irDefn.returnType.substitute(irDefn.typeParameters, allTypeArgs)
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
        else:
            assert isinstance(irDefn, ir.Package)
            return ir_t.getPackageType()

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

__all__ = ["analyzeTypes"]

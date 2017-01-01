# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import copy
import StringIO

import builtins
import bytecode
import data
import errors
import flags
import ir_values
import utils

NULLABLE_TYPE_FLAG = "nullable"

class Type(data.Data):
    propertyNames = ("flags",)

    def __init__(self, flags=None):
        if flags is None:
            flags = frozenset()
        if isinstance(flags, str):
            flags = frozenset([flags])
        assert isinstance(flags, frozenset)
        self.flags = flags

    def withFlag(self, flag):
        ty = copy.copy(self)
        ty.flags |= frozenset((flag,))
        return ty

    def withoutFlag(self, flag):
        ty = copy.copy(self)
        ty.flags -= frozenset((flag,))
        return ty

    def withFlags(self, flags):
        ty = copy.copy(self)
        ty.flags = flags
        return ty

    def isSubtypeOf(self, other):
        return self.isSubtypeOf_(other, SubstitutionEnvironment())

    def isSubtypeOf_(self, other, subEnv):
        subEnv.beginTransaction()
        result = self.isSubtypeOfRules_(other, subEnv)
        if result:
            subEnv.commitTransaction()
        else:
            subEnv.rollbackTransaction()
        return result

    def isSubtypeOfRules_(self, other, subEnv):
        left = self
        right = other

        # Basic rules that apply to all types.
        if left.isEquivalent(right):
            return True
        if left is AnyType or right is NoType:
            return False
        if right is AnyType or left is NoType:
            return True

        # Primitive types.
        if left.isPrimitive() or right.isPrimitive():
            return False
        assert left.isObject() and right.isObject()

        # Existential types.
        if isinstance(right, ExistentialType):
            utils.each(subEnv.addVariable, right.variables)
            return left.isSubtypeOf_(right.ty, subEnv)
        if subEnv.isExistentialVar(right):
            return subEnv.trySubstitute(left, right)
        if isinstance(left, ExistentialType):
            return left.ty.isSubtypeOf_(right, subEnv)

        # A nullable type cannot be a subtype of a non-nullable type. Note that existential
        # types cannot be nullable (but their inner types can).
        if left.isNullable() and not right.isNullable():
            return False

        # Variable types.
        if isinstance(left, VariableType) and isinstance(right, VariableType):
            # If both are variable types, try to find a common bound.
            leftBound = left
            rightFirstBound = right
            while isinstance(leftBound, VariableType):
                rightBound = rightFirstBound
                while isinstance(rightBound, VariableType):
                    if leftBound.typeParameter is rightBound.typeParameter and \
                       (rightBound.isNullable() or not leftBound.isNullable()):
                        return True
                    rightBound = rightBound.lowerBound()
                leftBound = leftBound.upperBound()
        while isinstance(left, VariableType):
            left = left.upperBound()
        while isinstance(right, VariableType):
            right = right.lowerBound()

        # Class types.
        assert isinstance(left, ClassType) and isinstance(right, ClassType)

        # Special cases for `Nothing`.
        if left.clas is builtins.getNothingClass():
            return True
        if right.clas is builtins.getNothingClass():
            return False

        # Check that left is derived from right, and substitute for the same definition.
        leftBase = left.clas.findBaseType(right.clas)
        if leftBase is None:
            return False
        left = leftBase.substitute(left.clas.typeParameters, left.typeArguments)

        # Compare type arguments, based on variance.
        for lty, rty, tp in \
                zip(left.typeArguments, right.typeArguments, left.clas.typeParameters):
            # This could be more compact, but it's better for debugging to have the cases split.
            variance = tp.variance()
            if variance is flags.COVARIANT:
                if not lty.isSubtypeOf_(rty, subEnv):
                    return False
            elif variance is flags.CONTRAVARIANT:
                if not rty.isSubtypeOf_(lty, subEnv):
                    return False
            else:
                assert variance is INVARIANT
                if not (subEnv.isExistentialVar(rty) and subEnv.trySubstitute(lty, rty)) and \
                   not lty.isEquivalent(rty):
                    return False
        return True

    def isDisjoint(self, other):
        lub = self.lub(other)
        return not self.isEquivalent(lub) and not other.isEquivalent(lub)

    def isPrimitive(self):
        raise NotImplementedError()

    def isObject(self):
        raise NotImplementedError()

    def combine(self, ty, loc):
        combined = self.lub(ty)
        if combined is AnyType:
            raise errors.TypeException(loc, "could not combine")
        return combined

    def isEquivalent(self, other):
        """Returns whether two types correspond to the same set of values.

        This is a slightly more relaxed relation than == because two types may have different
        forms but contain the same values."""
        if isinstance(self, ExistentialType) and isinstance(other, ExistentialType):
            if len(self.variables) != len(other.variables):
                return False
            if all(s.id is t.id for s, t in zip(self.variables, other.variables)) and \
               self.ty.isEquivalent(other.ty):
                return True
            if any(not s.isEquivalent(t) for s, t in zip(self.variables, other.variables)):
                return False
            subArgs = [VariableType(v) for v in self.variables]
            otherSubType = other.ty.substitute(other.variables, subArgs)
            return self.ty.isEquivalent(otherSubType)

        return self == other

    def lub(self, other):
        """Computes the least upper bound of two types on the type lattice. Note that since
        AnyType is not a valid type, this function returns AnyType to indicate that the
        two types couldn't be combined."""
        return self.lub_(other, [], SubstitutionEnvironment())

    def lub_(self, other, stack, subEnv):
        subEnv.beginTransaction()
        result = self.lubRules_(other, stack, subEnv)
        if result is not AnyType:
            subEnv.commitTransaction()
        else:
            subEnv.rollbackTransaction()
        return result

    def lubRules_(self, other, stack, subEnv):
        # We need to be able to detect infinite recursion in order to ensure termination.
        # Consider the case below:
        #   class A[+T]
        #   class B <: A[B]
        #   class C <: A[C]
        # Suppose we want to find B lub C.
        # The correct answer is A[B lub C] = A[A[B lub C]] = A[A[A[B lub C]]] ...
        # Since we have no way to correctly express the least upper bound in that case, we
        # settle for returning a close upper bound: A[Object].
        left = self
        right = other

        if (left, right) in stack:
            if left.isObject() and right.isObject():
                return getRootClassType()
            else:
                return AnyType

        # Basic rules for all types.
        if left.isEquivalent(right):
            return left
        if left is AnyType or right is AnyType:
            return AnyType
        if left is NoType:
            return right
        if right is NoType:
            return left

        # Primitive types.
        if left.isPrimitive() or right.isPrimitive():
            return AnyType

        # Existential types (always recursive).
        if isinstance(left, ExistentialType) or isinstance(right, ExistentialType):
            stack.append((left, right))

            # Add the type parameters from both existential types to the substitution
            # environment. These will be treated a little differently than normal
            # type parameters.
            if isinstance(left, ExistentialType):
                leftInnerType = left.ty
                utils.each(subEnv.addVariable, left.variables)
            else:
                leftInnerType = left
            if isinstance(right, ExistentialType):
                rightInnerType = right.ty
                utils.each(subEnv.addVariable, right.variables)
            else:
                rightInnerType = right

            # Find the lub of the inner types.
            innerLubType = leftInnerType.lub_(rightInnerType, stack, subEnv)
            stack.pop()

            # Find which variables are still being used in the lub. If some of the variables
            # from `left` and `right` are still present, return an existential with those.
            # Note that unused variables can be removed from an existential type. For example:
            #   forsome [X] Option[String] == Option[String]
            # We must be careful the returned type has variables in a deterministic order.
            # Technically, the order shouldn't matter, but it's infeasible to test whether two
            # types are equivalent with arbitrary ordered variables, since they cannot easily
            # be sorted.
            return ExistentialType.close(subEnv.getVariables(), innerLubType)

        # If either side is nullable, the result is nullable.
        if self.isNullable() or other.isNullable():
            combinedFlags = frozenset([NULLABLE_TYPE_FLAG])
        else:
            combinedFlags = frozenset()
        leftWithFlags = left.withFlags(combinedFlags)
        rightWithFlags = right.withFlags(combinedFlags)

        # Rules for subtypes.
        if leftWithFlags.isSubtypeOf_(rightWithFlags, subEnv):
            return rightWithFlags
        if rightWithFlags.isSubtypeOf_(leftWithFlags, subEnv):
            return leftWithFlags

        # Variable types.
        if isinstance(left, VariableType) and isinstance(right, VariableType):
            sharedBound = left.typeParameter.findCommonUpperBound(right.typeParameter)
            if sharedBound is not None:
                return VariableType(sharedBound, combinedFlags)

        # TODO: beyond this point, we can't find an accurate least upper bound because of the
        # presence of multiple shared traits. We will find a common base class instead, which
        # may be less specific. When union types are supported, we should just return
        # left | right.
        while isinstance(left, VariableType):
            left = left.upperBound()
        while isinstance(right, VariableType):
            right = right.upperBound()
        assert isinstance(left, ClassType) and isinstance(right, ClassType)

        # Find a common base class. We don't assume that there is a single root class
        # (even though there is), so this can fail.
        baseClass = left.clas.findCommonBaseClass(right.clas)
        while baseClass is not None:
            left = left.substituteForBase(baseClass)
            right = right.substituteForBase(baseClass)

            # We need to combine the type arguments, according to the variance of the
            # corresponding type parameters. This is not necessarily possible. If we get
            # stuck, we'll try again with the superclass.
            leftArgs = left.typeArguments
            rightArgs = right.typeArguments

            combinedArgs = []
            combineSuccess = True
            for param, leftArg, rightArg in zip(baseClass.typeParameters,
                                                leftArgs, rightArgs):
                variance = param.variance()
                if variance is INVARIANT:
                    if leftArg == rightArg:
                        combined = leftArg
                    elif subEnv.isExistentialVar(leftArg) and \
                         subEnv.trySubstitute(rightArg, leftArg):
                        combined = leftArg
                    elif subEnv.isExistentialVar(rightArg) and \
                         subEnv.trySubstitute(leftArg, rightArg):
                        combined = rightArg
                    else:
                        combined = AnyType
                else:
                    stack.append((self, other))
                    if variance is flags.COVARIANT:
                        combined = leftArg.lub_(rightArg, stack, subEnv)
                    else:
                        assert variance is flags.CONTRAVARIANT
                        combined = leftArg.glb_(rightArg, stack)
                    stack.pop()
                if combined is AnyType:
                    combineSuccess = False
                    break
                combinedArgs.append(combined)

            if combineSuccess:
                return ClassType(baseClass, tuple(combinedArgs), combinedFlags)

            baseClass = baseClass.superclass()

        # If we get here, then we ran out of superclasses. Fall through.
        return AnyType

    def glb(self, ty):
        """Computes the greatest lower bound of two types on the type lattice. Note that since
        this is not a true lattice with a bottom, there may be no shared lower bound (e.g.,
        for i64 and String). This function returns None in that case."""
        return self.glb_(ty, [])

    def glb_(self, ty, stack):
        if (self, ty) in stack:
            if self.isObject() and ty.isObject():
                return getNothingClassType()
            else:
                return NoType

        if self == ty:
            return self
        elif self is NoType:
            return ty
        elif ty is NoType:
            return self
        elif self.isSubtypeOf(ty):
            return self
        elif ty.isSubtypeOf(self):
            return ty
        elif self.isObject() and ty.isObject():
            # Ok, this is kind of a cop-out. Don't judge me.
            # TODO: once there are intersection types, use them instead.
            if self.isNullable() and ty.isNullable():
                flags = frozenset([NULLABLE_TYPE_FLAG])
            else:
                flags = frozenset()
            return ClassType(builtins.getNothingClass(), (), flags)
        else:
            return NoType

    def substitute(self, parameters, replacements):
        raise NotImplementedError()

    def substituteForInheritance(self, clas, base):
        """Substitutes the type of something that was inherited from a class or trait.

        Replaces references to type parameters in the base definition using type arguments from
        the deriving definition.
        """
        assert clas.isDerivedFrom(base)
        if clas is base:
            return self
        else:
            supertype = next(sty for sty in clas.supertypes if sty.clas is base)
            return self.substitute(base.typeParameters, supertype.typeArguments)

    def getTypeArguments(self):
        raise NotImplementedError()

    def findVariables(self):
        """Returns a set of `DefnId`s for `TypeParameters` referenced through `VariableType`s
        in this type."""
        raise NotImplementedError()

    def size(self):
        raise NotImplementedError()

    def alignment(self):
        return self.size()

    def defaultValue(self):
        return None

    def isNullable(self):
        return NULLABLE_TYPE_FLAG in self.flags

    def uninitializedType(self):
        raise NotImplementedError()

    def mayUseAsBound(self):
        """Returns whether this type may be used as an upper or lower bound for a type
        parameter. Class and variable types return true; existential types return false."""
        return False


class SimpleType(Type):
    propertyNames = Type.propertyNames + ("name", "width")

    def __init__(self, name, width, defaultValue=None):
        super(SimpleType, self).__init__(frozenset())
        self.name = name
        self.width = width
        self.defaultValue_ = defaultValue

    def __repr__(self):
        return self.name

    def __str__(self):
        return self.name

    def substitute(self, parameters, replacements):
        return self

    def isPrimitive(self):
        return True

    def isObject(self):
        return False

    def findVariables(self):
        return set()

    def size(self):
        widthSizes = { bytecode.W8: 1, bytecode.W16: 2, bytecode.W32: 4, bytecode.W64: 8 }
        return widthSizes[self.width]

    def defaultValue(self):
        return self.defaultValue_

    def getTypeArguments(self):
        return ()

    def isNullable(self):
        assert NULLABLE_TYPE_FLAG not in self.flags
        return False

    def uninitializedType(self):
        return self


# NoType is the bottom of the type lattice. No values have this type. Expressions which don't
# produce any value (such as return or throw) have this type.
NoType = SimpleType("_", None)

# AnyType is the top of the type lattice. All values have this type, but this type doesn't
# contain any values that aren't also part of another value. This type doesn't have a
# well-defined size, so this is not a valid type for fields, variables, or really anything else.
# This is mostly used as an error value, e.g. for Type.lub.
AnyType = SimpleType("*", None)

UnitType = SimpleType("unit", bytecode.W8, ir_values.UnitValue())
I8Type = SimpleType("i8", bytecode.W8, ir_values.I8Value(0))
I16Type = SimpleType("i16", bytecode.W16, ir_values.I16Value(0))
I32Type = SimpleType("i32", bytecode.W32, ir_values.I32Value(0))
I64Type = SimpleType("i64", bytecode.W64, ir_values.I64Value(0))
F32Type = SimpleType("f32", bytecode.W32, ir_values.F32Value(0.))
F64Type = SimpleType("f64", bytecode.W64, ir_values.F64Value(0.))
BooleanType = SimpleType("boolean", bytecode.W8, ir_values.BooleanValue(False))


class ObjectType(Type):
    def __init__(self, flags):
        super(ObjectType, self).__init__(flags)

    def isPrimitive(self):
        return False

    def isObject(self):
        return True

    def size(self):
        return bytecode.WORDSIZE

    def getBaseClassType(self):
        """Returns the `ClassType` this type is derived from. For root class types, this
        returns `None`."""
        raise NotImplementedError()

    def effectiveClass(self):
        """Returns an `ObjectTypeDefn` that this type can be treated as.

        For `ClassType`s, this returns the `clas` property.

        For `VariableType`s, this returns the effective class of the upper bound.

        For `ExistentialType`s, this returns the effective class of the inner type.
        """
        raise NotImplementedError()

    def effectiveClassType(self):
        """Returns a `ClassType` that this type can be treated as.

        For `ClassType`s, this returns the type itself.

        For `VariableType`s, this returns the effective class type of the upper bound.

        For `ExistentialType`s, this returns the effective class type of the inner type.
        The existential type parameters are also returned.

        Returns:
            (ClassType, [TypeParameter]): the effective class type and a list of existential
            type parameters that were opened.
        """
        raise NotImplentedError()

    def substituteForBase(self, base):
        """Returns the `ClassType` for `base`, which is a supertype of this type, with type
        arguments substituted appropriately.

        For example, if we have class `A[T]` and class `B <: A[C]`, then if we call this method
        on type `B` with `base` `A`, we will get `A[C]`. Note that this goes in the reverse
        direction from `substituteForInheritance`. If `self` is a `ClassType`, and `base` is
        the definition `self` refers to, `self` is returned without substitution.

        This method may not be called until inheritance analysis is complete for the class
        in the `ClassType` returned by `getBaseClassType`.

        Arguments:
            base (ObjectTypeDefn): a type definition which form a base for this type. So if
                this is a `ClassType`, `base` must be a class or trait that `clas` is
                derives from.
        """
        raise NotImplementedError()

    def uninitializedType(self):
        return getNullType() if self.isNullable() else getNothingClassType()


class ClassType(ObjectType,):
    propertyNames = Type.propertyNames + ("clas", "typeArguments")
    width = bytecode.WORD

    def __init__(self, clas, typeArguments=(), flags=None):
        super(ClassType, self).__init__(flags)
        self.clas = clas
        self.typeArguments = typeArguments

    @staticmethod
    def forReceiver(clas):
        typeArgs = tuple(VariableType(t) for t in clas.typeParameters)
        return ClassType(clas, typeArgs, None)

    def __str__(self):
        if len(self.typeArguments) == 0:
            return str(self.clas.name)
        else:
            return "%s[%s]%s" % \
                   (self.clas.name,
                    ",".join(map(str, self.typeArguments)),
                    "?" if self.isNullable() else "")

    def __repr__(self):
        typeArgsStr = (", (" + ", ".join(map(repr, self.typeArguments)) + ")") \
                      if len(self.typeArguments) > 0 \
                      else ""
        flagsStr = (", " + ", ".join(self.flags)) if len(self.flags) > 0 else ""
        return "ClassType(%s%s%s)" % (self.clas.name, typeArgsStr, flagsStr)

    def __hash__(self):
        return utils.hashList([getattr(self, name) for name in Type.propertyNames] + \
                        [self.clas.name, self.typeArguments])

    def __eq__(self, other):
        return self.__class__ is other.__class__ and \
               self.flags == other.flags and \
               self.clas is other.clas and \
               self.typeArguments == other.typeArguments

    def getBaseClassType(self):
        if len(self.clas.supertypes) == 0:
            return None
        return self.substituteForBase(self.clas.supertypes[0].clas)

    def effectiveClass(self):
        return self.clas

    def effectiveClassType(self):
        return self, []

    def substitute(self, parameters, replacements):
        return ClassType(self.clas,
                         tuple(arg.substitute(parameters, replacements)
                               for arg in self.typeArguments),
                         self.flags)

    def substituteForBase(self, base):
        assert base is not builtins.getNothingClass()
        assert self.clas is not builtins.getNothingClass()
        if self.clas is base:
            return self
        baseType = next(sty for sty in self.clas.supertypes if sty.clas is base)
        return baseType.substitute(self.clas.typeParameters, self.typeArguments)

    def mayUseAsBound(self):
        return not self.isNullable()

    def getTypeArguments(self):
        return self.typeArguments

    def findVariables(self):
        variables = set()
        for typeArg in self.typeArguments:
            variables |= typeArg.findVariables()
        return variables


class VariableType(ObjectType):
    propertyNames = Type.propertyNames + ("typeParameter",)
    width = bytecode.WORD

    def __init__(self, typeParameter, flags=frozenset()):
        super(VariableType, self).__init__(flags)
        self.typeParameter = typeParameter

    def __str__(self):
        return str(self.typeParameter.name)

    def __repr__(self):
        return "VariableType(%s)" % self.typeParameter.name

    def __hash__(self):
        return utils.hashList([self.typeParameter.name])

    def __eq__(self, other):
        return self.__class__ is other.__class__ and \
               self.typeParameter is other.typeParameter and \
               self.flags == other.flags

    def getBaseClassType(self):
        baseType = self
        while isinstance(baseType, VariableType):
            baseType = baseType.typeParameter.upperBound
        assert isinstance(baseType, ClassType)
        return baseType

    def effectiveClass(self):
        return self.typeParameter.upperBound.effectiveClass()

    def effectiveClassType(self):
        return self.typeParameter.upperBound.effectiveClassType()

    def substitute(self, parameters, replacements):
        assert len(parameters) == len(replacements)
        for param, repl in zip(parameters, replacements):
            if param is self.typeParameter:
                return repl
        return self

    def getTypeArguments(self):
        if self.typeParameter.upperBound is not None:
            return self.typeParameter.upperBound.getTypeArguments()
        else:
            return ()

    def substituteForBase(self, base):
        return self.typeParameter.upperBound.substituteForBase(base)

    def mayUseAsBound(self):
        return not self.isNullable()

    def findVariables(self):
        return set([self.typeParameter.id])

    def upperBound(self):
        upperBound = self.typeParameter.upperBound
        return upperBound.withFlag(NULLABLE_TYPE_FLAG) \
               if self.isNullable() and not upperBound.isNullable() \
               else upperBound

    def lowerBound(self):
        lowerBound = self.typeParameter.lowerBound
        return lowerBound.withFlag(NULLABLE_TYPE_FLAG) \
               if self.isNullable() and not lowerBound.isNullable() \
               else lowerBound


class ExistentialType(ObjectType):
    propertyNames = Type.propertyNames + ("variables", "ty")
    width = bytecode.WORD

    def __init__(self, variables, ty):
        super(ExistentialType, self).__init__(ty.flags)
        self.variables = variables
        self.ty = ty

    @staticmethod
    def close(variables, ty):
        """Creates an existential type with only the variables that are actually used.

        Args:
            variables (iterable(TypeParameter)): a sequence of variables that may be used
                by `ty`. Duplicates are allowed.
            ty (ObjectType): the inner type of the existential.

        Returns:
            (ObjectType): if one or more of the type parameters in `variables` are used in
            `ty`, an `ExistentialType` is returned using only those variables. The variables
            will be declared in the same order they were passed in. A variable will not be
            declared more than once. If none of the type parameters are used, `ty` is
            returned."""
        variableIdsUsed = ty.findVariables()
        variableIdsIntroduced = set()
        variablesIntroduced = []
        for v in variables:
            if v.id in variableIdsUsed and v.id not in variableIdsIntroduced:
                variablesIntroduced.append(v)
                variableIdsIntroduced.add(v.id)
        if len(variablesIntroduced) == 0:
            return ty
        else:
            return ExistentialType(tuple(variablesIntroduced), ty)

    def __str__(self):
        return "forsome [%s] %s" % (", ".join(map(str, self.variables)), str(self.ty))

    def __repr__(self):
        return "ExistentialType(%s, %s)" % (repr(self.variables), repr(self.ty))

    def __hash__(self):
        return utils.hashList(self.variables + (ty,))

    def __eq__(self, other):
        return self.__class__ is other.__class__ and \
            len(self.variables) == len(other.variables) and \
            all(s.id is t.id for s, t in zip(self.variables, other.variables)) and \
            self.ty == other.ty and \
            self.flags == other.flags

    def getBaseClassType(self):
        innerBaseType = self.ty.getBaseClassType()
        if innerBaseType is None:
            return innerBaseType
        return ExistentialType.close(self.variables, innerBaseType)

    def effectiveClass(self):
        return self.ty.effectiveClass()

    def effectiveClassType(self):
        effectiveType, openedVariables = self.ty.effectiveClassType()
        return effectiveType, list(self.variables) + openedVariables

    def substitute(self, parameters, replacements):
        subTy = self.ty.substitute(parameters, replacements)
        return self if subTy == self.ty else ExistentialType(self.variables, subTy)

    def substituteForBase(self, base):
        subTy = self.ty.substituteForBase(base)
        return self if subTy == self.ty else ExistentialType(self.variables, subTy)

    def getTypeArguments(self):
        return self.ty.getTypeArguments()

    def findVariables(self):
        return self.ty.findVariables()


def getClassFromType(ty):
    if isinstance(ty, ClassType):
        return ty.clas
    elif isinstance(ty, VariableType):
        return getClassFromType(ty.typeParameter.upperBound)
    elif isinstance(ty, ExistentialType):
        return getClassFromType(ty.ty)
    else:
        assert ty.isPrimitive()
        return builtins.getBuiltinClassFromType(ty)


def getRootClassType():
    return ClassType(builtins.getRootClass(), ())


def getNothingClassType():
    return ClassType(builtins.getNothingClass())


def getExceptionClassType():
    return ClassType(builtins.getExceptionClass())


def getNullType():
    return ClassType(builtins.getNothingClass(), (), frozenset([NULLABLE_TYPE_FLAG]))


def getStringType():
    return ClassType(builtins.getStringClass())


def getPackageType():
    return ClassType(builtins.getPackageClass())


# Extra variance symbols
# Invariant means neither covariant nor contravariant.
INVARIANT = "invariant"
# BIVARIANT means either COVARIANT or CONTRAVARIANT.
BIVARIANT = "bivariant"


def changeVariance(old, new):
    assert new is not BIVARIANT
    if old is None:
        old = BIVARIANT

    if old is INVARIANT:
        return INVARIANT
    elif old in [BIVARIANT, flags.COVARIANT]:
        return new
    else:
        if new is flags.CONTRAVARIANT:
            return flags.COVARIANT
        elif new is flags.COVARIANT:
            return flags.CONTRAVARIANT
        else:
            assert new is INVARIANT
            return INVARIANT


class SubstitutionEnvironment(object):
    """Provides an environment for existential substitution in isSubtypeOf, isEquivalent,
    and lub.

    In isSubtypeOf, if the right-side type (the alleged supertype) is an existential type,
    the left-side type is a subtype if there is some type instance of that existential type
    that is a supertype of the left-side type. A type instance of an existential type is any
    legal substitution.

    This environment keeps track of what substitutions have been made and ensures later
    substitutions don't contradict earlier substitutions.
    """

    def __init__(self):
        self.substitutions = []
        self.transactionStack = []

    def __repr__(self):
        ids = set()
        substitutionStrings = []
        for var, ty in reversed(self.substitutions):
            if var.id not in ids:
                ids.add(var.id)
                substitutionStrings.append(str(var.name) + ": " + str(ty))
        return "SubstitutionEnvironment([" + ", ".join(reversed(substitutionStrings)) + "])"

    def beginTransaction(self):
        self.transactionStack.append(len(self.substitutions))

    def commitTransaction(self):
        self.transactionStack.pop()

    def rollbackTransaction(self):
        index = self.transactionStack.pop()
        del self.substitutions[index:]

    def addVariable(self, var):
        if not self.haveVariable_(var):
            self.substitutions.append((var, None))

    def getVariables(self):
        ids = set()
        variables = []
        for v, _ in self.substitutions:
            if v.id not in ids:
                ids.add(v.id)
                variables.append(v)
        return variables

    def isExistentialVar(self, vty):
        return isinstance(vty, VariableType) and self.haveVariable_(vty.typeParameter)

    def trySubstitute(self, ty, vty):
        assert self.isExistentialVar(vty)
        tp = vty.typeParameter
        index = self.indexOf_(tp)
        if self.substitutions[index][1] is None:
            combinedType = ty
        else:
            combinedType = self.substitutions[index][1].lub_(ty, [], self)
        if combinedType.isSubtypeOf_(tp.upperBound, self) and \
           tp.lowerBound.isSubtypeOf_(combinedType, self):
            self.substitutions.append((tp, combinedType))
            return True
        else:
            return False

    def haveVariable_(self, var):
        return self.indexOf_(var) >= 0

    def indexOf_(self, var):
        i = len(self.substitutions) - 1
        while i >= 0:
            if self.substitutions[i][0] is var:
                break
            i -= 1
        return i


__all__ = ["BIVARIANT","INVARIANT", "UnitType", "BooleanType", "I8Type",
           "I16Type", "I32Type", "I64Type", "F32Type", "F64Type",
           "VariableType", "ClassType",  "ExistentialType", "NoType",
           "getRootClassType", "getStringType", "getPackageType", "getNullType",
           "getClassFromType", "NULLABLE_TYPE_FLAG", "changeVariance",
           "getNothingClassType"]

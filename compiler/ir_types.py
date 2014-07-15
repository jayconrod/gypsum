# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import copy

import builtins
from bytecode import *
from data import *
from errors import *
from ir_values import *

NULLABLE_TYPE_FLAG = "nullable"

class Type(Data):
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

    def isSubtypeOf(self, other):
        if self == other:
            return True
        elif self.isObject() and other.isObject():
            def topBottomClasses(ty):
                if isinstance(ty, ClassType):
                    return ty.clas, ty.clas
                else:
                    assert isinstance(ty, VariableType)
                    return (topBottomClasses(ty.typeParameter.upperBound)[0],
                            topBottomClasses(ty.typeParameter.lowerBound)[1])
            topSelfClass = topBottomClasses(self)[0]
            bottomOtherClass = topBottomClasses(other)[1]
            return topSelfClass.isSubclassOf(bottomOtherClass) and \
                   (not self.isNullable() or other.isNullable())
        else:
            return False

    def isPrimitive(self):
        raise NotImplementedError

    def isObject(self):
        raise NotImplementedError

    def combine(self, ty):
        if self == ty:
            return self
        elif self is NoType:
            return ty
        elif ty is NoType:
            return self
        elif isinstance(self, ClassType) and isinstance(ty, ClassType):
            commonBase = self.clas.findCommonBaseClass(ty.clas)
            commonFlags = self.combineFlags(ty)
            return ClassType(commonBase, (), commonFlags)
        else:
            raise TypeException("type error: could not combine")

    def combineFlags(self, other):
        return self.flags.union(other.flags)

    def size(self):
        raise NotImplementedError

    def alignment(self):
        return self.size()

    def defaultValue(self):
        return None

    def isNullable(self):
        return NULLABLE_TYPE_FLAG in self.flags


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

    def isPrimitive(self):
        return True

    def isObject(self):
        return False

    def size(self):
        widthSizes = { W8: 1, W16: 2, W32: 4, W64: 8 }
        return widthSizes[self.width]

    def defaultValue(self):
        return self.defaultValue_


NoType = SimpleType("_", None)
UnitType = SimpleType("unit", W8, UnitValue())
I8Type = SimpleType("i8", W8, I8Value(0))
I16Type = SimpleType("i16", W16, I16Value(0))
I32Type = SimpleType("i32", W32, I32Value(0))
I64Type = SimpleType("i64", W64, I64Value(0))
F32Type = SimpleType("f32", W32, F32Value(0.))
F64Type = SimpleType("f64", W64, F64Value(0.))
BooleanType = SimpleType("boolean", W8, BooleanValue(False))


class ObjectType(Type):
    def __init__(self, flags):
        super(ObjectType, self).__init__(flags)

    def isPrimitive(self):
        return False

    def isObject(self):
        return True

    def size(self):
        return WORDSIZE


class ClassType(ObjectType):
    propertyNames = Type.propertyNames + ("clas", "typeArguments")
    width = WORD

    def __init__(self, clas, typeArguments=(), flags=None):
        super(ClassType, self).__init__(flags)
        self.clas = clas
        self.typeArguments = typeArguments

    def __str__(self):
        if len(self.typeArguments) == 0:
            return self.clas.name
        else:
            return "%s[%s]%s" % \
                   (self.clas.name,
                    ",".join(self.typeArguments),
                    "?" if self.isNullable() else "")

    def __repr__(self):
        return "ClassType(%s, (%s), %s)" % \
               (self.clas.name,
                ", ".join(repr(ta) for ta in self.typeArguments),
                ", ".join(self.flags))

    def __hash__(self):
        return hashList([getattr(self, name) for name in Type.propertyNames] + \
                        [self.clas.name, self.typeArguments])

    def __eq__(self, other):
        return self.__class__ is other.__class__ and \
               self.flags == other.flags and \
               self.clas is other.clas and \
               self.typeArguments == other.typeArguments

    def isNullable(self):
        return NULLABLE_TYPE_FLAG in self.flags



class VariableType(ObjectType):
    propertyNames = Type.propertyNames + ("typeParameter",)
    width = WORD

    def __init__(self, typeParameter):
        super(VariableType, self).__init__(frozenset())
        self.typeParameter = typeParameter

    def __str__(self):
        return self.typeParameter.name

    def __repr__(self):
        return "TypeParameter(%s)" % self.typeParameter.name

    def __hash__(self):
        return hashList([self.typeParameter.name])

    def __eq__(self, other):
        return self.__class__ is other.__class__ and \
               self.typeParameter is other.typeParameter

    def isNullable(self):
        return self.typeParameter.upperBound is not None and \
               self.typeParameter.upperBound.isNullable()


def getClassFromType(ty):
    if isinstance(ty, ClassType):
        return ty.clas
    else:
        assert ty.isPrimitive()
        return builtins.getBuiltinClassFromType(ty)


def getRootClassType():
    return ClassType(builtins.getRootClass(), ())


def getNothingClassType():
    return ClassType(builtins.getNothingClass())


def getNullType():
    return ClassType(builtins.getNothingClass(), (), frozenset([NULLABLE_TYPE_FLAG]))


def getStringType():
    return ClassType(builtins.getStringClass())

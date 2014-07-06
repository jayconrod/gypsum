# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from builtins import *
from data import *
from ir_types import *

class Value(Data):
    propertyNames = ()

    def type(self):
        raise NotImplementedError


class UnitValue(Value):
    def type(self):
        return UnitType


class BooleanValue(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return BooleanType


class I8Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return I8Type


class I16Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return I16Type


class I32Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return I32Type


class I64Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return I64Type


class F32Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return F32Type


class F64Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return F64Type


class NullValue(Value):
    def type(self):
        return getNullType()

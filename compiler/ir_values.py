# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import data
import ir_types

class Value(data.Data):
    propertyNames = ()

    def type(self):
        raise NotImplementedError


class UnitValue(Value):
    def type(self):
        return ir_types.UnitType


class BooleanValue(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return ir_types.BooleanType


class I8Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return ir_types.I8Type


class I16Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return ir_types.I16Type


class I32Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return ir_types.I32Type


class I64Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return ir_types.I64Type


class F32Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return ir_types.F32Type


class F64Value(Value):
    propertyNames = Value.propertyNames + ("value",)

    def type(self):
        return ir_types.F64Type


class NullValue(Value):
    def type(self):
        return ir_types.getNullType()

# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from collections import namedtuple
import yaml

import ids
import utils

InstInfo = namedtuple("InstInfo", ["name", "opcode", "operandCount", "isTerminator"])

_c = utils.Counter()
instInfoByName = {}
instInfoByCode = []

with utils.openCommonFile("opcodes.yaml") as _opcodesFile:
    for _opc in yaml.load(_opcodesFile.read()):
        _info = InstInfo(_opc["name"], _c(), _opc["iops"], _opc["term"])
        instInfoByName[_opc["name"]] = _info
        instInfoByCode.append(_info)

# Instructions and types may have one of the widths below. This number can be added to a base
# instruction like "addi8" to get the appropriate variant.
W8 = 0
W16 = 1
W32 = 2
W64 = 3

WORD = W64
WORDSIZE = 8

# Builtin definitions are accessible to the compiler, but are not included in packages. They
# are referenced by negative ids. The VM implements these.
_nextClassId = utils.Counter(-1, -1)
_classIds = []
_nextFunctionId = utils.Counter(-1, -1)
_functionIds = []

def _assignFunctionId(name):
    id = ids.DefnId(None, ids.DefnId.FUNCTION, _nextFunctionId())
    globals()[name] = id
    _functionIds.append(id)


def _assignClassId(name):
    id = ids.DefnId(None, ids.DefnId.CLASS, _nextClassId())
    globals()[name] = id
    _classIds.append(id)


def getBuiltinFunctionId(index):
    if index < 0:
        index = ~index
    return _functionIds[index]


def getBuiltinClassId(index):
    if index < 0:
        index = ~index
    return _classIds[index]


with utils.openCommonFile("builtins.yaml") as _builtinsFile:
    _classes, _functions = yaml.load_all(_builtinsFile.read())
    for _ty in _classes:
        _assignClassId(_ty["id"])
        if not _ty["isPrimitive"]:
            for _ctor in _ty["constructors"]:
                _assignFunctionId(_ctor["id"])
        for _method in _ty["methods"]:
            _assignFunctionId(_method["id"])
    for _fn in _functions:
        _assignFunctionId(_fn["id"])

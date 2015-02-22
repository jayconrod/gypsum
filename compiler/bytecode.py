# Copyright 2014-2015, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from collections import namedtuple
import os.path
import yaml

import ids
import utils

InstInfo = namedtuple("InstInfo", ["name", "opcode", "operandCount", "isTerminator"])

_c = utils.Counter()
instInfoByName = {}
instInfoByCode = []

_opcodesPath = os.path.join(os.path.dirname(__file__), "..", "common", "opcodes.yaml")
with open(_opcodesPath) as _opcodesFile:
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
_nextFunctionId = utils.Counter(-1, -1)

_builtinsPath = os.path.join(os.path.dirname(__file__), "..", "common", "builtins.yaml")
with open(_builtinsPath) as _builtinsFile:
    _classes, _functions = yaml.load_all(_builtinsFile.read())
    for _ty in _classes:
        globals()[_ty["id"]] = ids.DefnId(None, ids.DefnId.CLASS, _nextClassId())
        if not _ty["isPrimitive"]:
            for _ctor in _ty["constructors"]:
                globals()[_ctor["id"]] = ids.DefnId(None, ids.DefnId.FUNCTION, _nextFunctionId())
        for _method in _ty["methods"]:
            globals()[_method["id"]] = ids.DefnId(None, ids.DefnId.FUNCTION, _nextFunctionId())
    for _fn in _functions:
        globals()[_fn["id"]] = ids.DefnId(None, ids.DefnId.FUNCTION, _nextFunctionId())

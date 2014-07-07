# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import os.path
import yaml

_flagCodes = {}

_initialized = False

def _initialize():
    if _initialized:
        return
    _initialized = True

    flagsPath = os.path.join(os.path.dirname(__file__), "..", "common", "flags.yaml")
    with open(flagsPath) as flagsFile:
        flagList = yaml.load(flagsFile.read())
    code = 1
    for flagName in flagList:
        globals()[flagName] = flagName
        _flagCodes[flagName] = code
        code <<= 1
_initialize()

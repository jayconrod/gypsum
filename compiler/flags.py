# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import os.path
import yaml


def getFlagByName(name):
    return _flagNames[name]


def checkFlagConflicts(flags):
    for group in _flagGroups:
        conflict = group & flags
        if len(conflict) > 1:
            return conflict
    return None


def flagSetToFlagBits(flagSet):
    return reduce(lambda bits, flag: bits | _flagCodes[flag], flagSet, 0)


_flagCodes = {}
_flagNames = {}
_flagGroups = []

_initialized = False

def _initialize():
    global _initialized
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
        _flagNames[flagName.lower()] = flagName
        code <<= 1
    _flagGroups.append(frozenset([PUBLIC, PROTECTED, PRIVATE]))
_initialize()

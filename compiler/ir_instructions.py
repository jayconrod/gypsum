# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from data import *
from bytecode import *
from ir_types import *

BasicBlockBase = Data.makeClass("BasicBlockBase", ("id", "instructions"))
class BasicBlock(BasicBlockBase):
    def __init__(self, id, instructions):
        super(BasicBlock, self).__init__(id, instructions)

    def successorIds(self):
        return self.instructions[-1].successorIds()


class Instruction(object):
    def __init__(self, *operands):
        assert len(operands) == self.info.operandCount
        self.operands = tuple(operands)

    @staticmethod
    def forOpcode(opcode, *operands):
        info = getInstInfoForOpcode(opcode)
        return globals()[info.name](*operands)

    def op(self, i):
        return self.operands[i]

    def opcode(self):
        return self.info.opcode

    def operandCount(self):
        return self.info.operandCount

    def isTerminator(self):
        return self.info.isTerminator

    def successorIds(self):
        assert self.isTerminator()
        return self.operands

    def setSuccessorIds(self, newSuccessorIds):
        assert self.isTerminator() and len(self.operands) == len(newSuccessorIds)
        self.operands = tuple(newSuccessorIds)

    def __repr__(self):
        return "%s(%s)" % (self.info.name, ", ".join(map(str, self.operands)))

    def __str__(self):
        return "%s %s" % (self.info.name, ", ".join(map(str, self.operands)))

    def __eq__(self, other):
        return self.__class__ is other.__class__ and \
               self.operands == other.operands

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hashList([self.info.opcode] + self.operands)


for info in instInfoByCode:
    globals()[info.name] = type(info.name,
                                (Instruction,),
                                {"info": info})

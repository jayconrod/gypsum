# Copyright 2014-2016, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import data
import bytecode
import ir
import utils

BasicBlockBase = data.Data.makeClass("BasicBlockBase", ("id", "instructions"))
class BasicBlock(BasicBlockBase):
    def __init__(self, id, instructions):
        super(BasicBlock, self).__init__(id, instructions)

    def successorIds(self):
        return self.instructions[-1].successorIds()


class Instruction(object):
    def __init__(self, *operands):
        assert len(operands) == self.info.operandCount
        assert all(type(op) in (int, float) for op in operands)
        self.operands = tuple(operands)

    @staticmethod
    def forOpcode(opcode, *operands):
        #TODO: this code is unreachable and seems broken.
        info = getInstInfoForOpcode(opcode)
        return globals()[info.name](*operands)

    def op(self, i):
        return self.operands[i]

    def opcode(self):
        return self.info.opcode

    def operandCount(self):
        return len(self.operands)

    def isTerminator(self):
        return self.info.isTerminator

    def successorIds(self):
        assert self.isTerminator()
        return self.operands

    def setSuccessorIds(self, newSuccessorIds):
        assert self.isTerminator() and len(self.operands) == len(newSuccessorIds)
        self.operands = tuple(newSuccessorIds)

    def pushCount(self):
        if self.info.pushCount is None:
            raise NotImplementedError()
        return self.info.pushCount

    def popCount(self):
        if self.info.popCount is None:
            raise NotImplementedError()
        return self.info.popCount

    def stackDelta(self):
        return self.pushCount() - self.popCount()

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
        return utils.hashList([self.info.opcode] + self.operands)


class label(Instruction):
    def blockId(self):
        return self.operands[0]

    def setBlockId(self, id):
        self.operands = (id,)


class branchl(Instruction):
    def __init__(self, *operands):
        # Do not call super __init__, since it checks for a specific number of operands,
        # and that's not defined for this instruction.
        assert all(type(op) is int for op in operands)
        self.operands = tuple(operands)


class dropi(Instruction):
    def popCount(self):
        return self.operands[0]


class ldg(Instruction):
    def __init__(self, globl):
        assert isinstance(globl, ir.Global) and (globl.isLocal() or globl.isBuiltin())
        super(ldg, self).__init__(globl.id.index)


class ldgf(Instruction):
    def __init__(self, globl):
        assert isinstance(globl, ir.Global) and globl.isForeign()
        super(ldgf, self).__init__(globl.id.packageId.index, globl.id.externIndex)


class stg(Instruction):
    def __init__(self, globl):
        assert isinstance(globl, ir.Global) and (globl.isLocal() or globl.isBuiltin())
        super(stg, self).__init__(globl.id.index)


class stgf(Instruction):
    def __init__(self, globl):
        assert isinstance(globl, ir.Global) and globl.isForeign()
        super(stgf, self).__init__(globl.id.packageId.index, globl.id.externIndex)


class ldf(Instruction):
    def __init__(self, field):
        clas = field.definingClass
        assert isinstance(clas, ir.Class) and not clas.isForeign()
        assert isinstance(field, ir.Field)
        super(ldf, self).__init__(clas.id.index, field.index)


class ldff(Instruction):
    def __init__(self, field):
        clas = field.definingClass
        assert isinstance(clas, ir.Class) and clas.isForeign()
        assert isinstance(field, ir.Field)
        super(ldff, self).__init__(clas.id.packageId.index,
                                   clas.id.externIndex,
                                   field.index)


class stf(Instruction):
    def __init__(self, field):
        clas = field.definingClass
        assert isinstance(clas, ir.Class) and not clas.isForeign()
        assert isinstance(field, ir.Field)
        super(stf, self).__init__(clas.id.index, field.index)


class stff(Instruction):
    def __init__(self, field):
        clas = field.definingClass
        assert isinstance(clas, ir.Class) and clas.isForeign()
        assert isinstance(field, ir.Field)
        super(stff, self).__init__(clas.id.packageId.index,
                                   clas.id.externIndex,
                                   field.index)


class allocobj(Instruction):
    def __init__(self, clas):
        assert isinstance(clas, ir.Class) and (clas.isLocal() or clas.isBuiltin())
        super(allocobj, self).__init__(clas.id.index)


class allocobjf(Instruction):
    def __init__(self, clas):
        assert isinstance(clas, ir.Class) and clas.isForeign()
        super(allocobjf, self).__init__(clas.id.packageId.index, clas.id.externIndex)


class allocarr(Instruction):
    def __init__(self, clas):
        assert isinstance(clas, ir.Class) and (clas.isLocal() or clas.isBuiltin())
        super(allocarr, self).__init__(clas.id.index)


class allocarrf(Instruction):
    def __init__(self, clas):
        assert isinstance(clas, ir.Class) and clas.isForeign()
        super(allocarrf, self).__init__(clas.id.packageId.index, clas.id.externIndex)


class pkg(Instruction):
    def __init__(self, package):
        assert isinstance(package, ir.Package)
        super(pkg, self).__init__(package.id.index)


class tycs(Instruction):
    def __init__(self, clas):
        assert isinstance(clas, ir.Class) and (clas.isLocal() or clas.isBuiltin())
        super(tycs, self).__init__(clas.id.index)


class tycsf(Instruction):
    def __init__(self, clas):
        assert isinstance(clas, ir.Class) and clas.isForeign()
        super(tycsf, self).__init__(clas.id.packageId.index, clas.id.externIndex)


class tycd(Instruction):
    def __init__(self, clas):
        assert isinstance(clas, ir.Class) and (clas.isLocal() or clas.isBuiltin())
        super(tycd, self).__init__(clas.id.index)
        self.popCount_ = len(clas.typeParameters)

    def popCount(self):
        return self.popCount_


class tycdf(Instruction):
    def __init__(self, clas):
        assert isinstance(clas, ir.Class) and clas.isForeign()
        super(tycdf, self).__init__(clas.id.packageId.index, clas.id.externIndex)
        self.popCount_ = len(clas.typeParameters)

    def popCount(self):
        return self.popCount_


class tyts(Instruction):
    def __init__(self, trait):
        assert isinstance(trait, ir.Trait) and (trait.isLocal() or trait.isBuiltin())
        super(tyts, self).__init__(trait.id.index)


class tytsf(Instruction):
    def __init__(self, trait):
        assert isinstance(trait, ir.Trait) and trait.isForeign()
        super(tytsf, self).__init__(trait.id.packageId.index, trait.id.externIndex)


class tytd(Instruction):
    def __init__(self, trait):
        assert isinstance(trait, ir.Trait) and (trait.isLocal() or trait.isBuiltin())
        super(tytd, self).__init__(trait.id.index)
        self.popCount_ = len(trait.typeParameters)

    def popCount(self):
        return self.popCount_


class tytdf(Instruction):
    def __init__(self, trait):
        assert isinstance(trait, ir.Trait) and trait.isForeign()
        super(tytdf, self).__init__(trait.id.packageId.index, trait.id.externIndex)
        self.popCount_ = len(trait.typeParameters)

    def popCount(self):
        return self.popCount_


class tyvs(Instruction):
    def __init__(self, param):
        assert isinstance(param, ir.TypeParameter) and (param.isLocal() or param.isBuiltin())
        super(tyvs, self).__init__(param.id.index)


class tyvd(Instruction):
    def __init__(self, param):
        assert isinstance(param, ir.TypeParameter) and (param.isLocal() or param.isBuiltin())
        super(tyvd, self).__init__(param.id.index)


class tyvdf(Instruction):
    def __init__(self, param):
        assert isinstance(param, ir.TypeParameter) and param.isForeign()
        super(tyvdf, self).__init__(param.id.packageId.index, param.id.externIndex)


class tyxs(Instruction):
    def popCount(self):
        return self.operands[0]


class tyxd(Instruction):
    def popCount(self):
        return self.operands[0]


class callg(Instruction):
    def __init__(self, function):
        assert isinstance(function, ir.Function) and \
            (function.isLocal() or function.isBuiltin())
        super(callg, self).__init__(function.id.index)
        self.popCount_ = len(function.parameterTypes)

    def popCount(self):
        return self.popCount_


class callgf(Instruction):
    def __init__(self, function):
        assert isinstance(function, ir.Function) and function.isForeign()
        super(callgf, self).__init__(function.id.packageId.index, function.id.externIndex)
        self.popCount_ = len(function.parameterTypes)

    def popCount(self):
        return self.popCount_


class callv(Instruction):
    def __init__(self, method):
        assert isinstance(method, ir.Function) and not method.isForeign()
        super(callv, self).__init__(method.id.index)
        self.popCount_ = len(method.parameterTypes)

    def popCount(self):
        return self.popCount_


class callvf(Instruction):
    def __init__(self, method):
        assert isinstance(method, ir.Function) and method.isForeign()
        super(callvf, self).__init__(method.id.packageId.index, method.id.externIndex)
        self.popCount_ = len(method.parameterTypes)

    def popCount(self):
        return self.popCount_


for info in bytecode.instInfoByCode:
    if info.name not in globals():
        globals()[info.name] = type(info.name, (Instruction,), {})
    globals()[info.name].info = info


__all__ = ["BasicBlock", "Instruction"] + [info.name for info in bytecode.instInfoByCode]

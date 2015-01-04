# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


class Visitor(object):
    def visit(self, obj, *args):
        self.preVisit(obj, *args)
        className = obj.__class__.__name__
        methodName = self.getMethodName(className)
        if hasattr(self, methodName):
            result = getattr(self, methodName)(obj, *args)
        else:
            result = self.visitDefault(obj, *args)

        result = self.handleResult(obj, result, *args)
        self.postVisit(obj, *args)
        return result

    def getMethodName(self, className):
        return "visit" + className

    def visitDefault(self, obj, *args):
        raise NotImplementedError

    def preVisit(self, obj, *args):
        pass

    def postVisit(self, obj, *args):
        pass

    def handleResult(self, obj, result, *args):
        return result

__all__ = ["Visitor"]

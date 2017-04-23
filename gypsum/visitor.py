# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


class Visitor(object):
    def visit(self, obj, *args, **kwargs):
        self.preVisit(obj, *args, **kwargs)
        className = obj.__class__.__name__
        methodName = self.getMethodName(className)
        if hasattr(self, methodName):
            method = getattr(self, methodName)
            result = method(obj, *args, **kwargs)
        else:
            result = self.visitDefault(obj, *args, **kwargs)

        result = self.handleResult(obj, result, *args, **kwargs)
        self.postVisit(obj, *args, **kwargs)
        return result

    def getMethodName(self, className):
        return "visit" + className

    def visitDefault(self, obj, *args, **kwargs):
        raise NotImplementedError(obj.__class__.__name__)

    def preVisit(self, obj, *args, **kwargs):
        pass

    def postVisit(self, obj, *args, **kwargs):
        pass

    def handleResult(self, obj, result, *args, **kwargs):
        return result

__all__ = ["Visitor"]

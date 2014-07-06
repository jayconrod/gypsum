# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from utils import *

class Data(object):
    @staticmethod
    def makeClass(name, propertyNames):
        return type(name, (Data,), {"propertyNames": propertyNames})

    def __init__(self, *args, **extra):
        assert len(args) == len(self.propertyNames)
        for key, value in zip(self.propertyNames, args):
            setattr(self, key, value)
        for key in extra:
            setattr(self, key, extra[key])

    def __repr__(self):
        if len(self.propertyNames) == 0:
            return self.__class__.__name__
        else:
            props = ", ".join([repr(getattr(self, name)) for name in self.propertyNames])
            return "%s(%s)" % (self.__class__.__name__, props)

    def __str__(self):
        return repr(self)

    def __eq__(self, other):
        return self.__class__ is other.__class__ and \
               all([getattr(self, name) == getattr(other, name)
                    for name in self.propertyNames])

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hashList(getattr(self, name) for name in self.propertyNames)

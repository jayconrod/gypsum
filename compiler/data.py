# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import utils

class Data(object):
    @staticmethod
    def makeClass(name, propertyNames):
        return type(name, (Data,), {"propertyNames": propertyNames})

    def __init__(self, *args, **extra):
        assert len(args) + len(extra) == len(self.propertyNames)
        assert set(self.propertyNames[len(args):]) == set(extra.keys())
        for key, value in zip(self.propertyNames, args):
            setattr(self, key, value)
        for key, value in extra.iteritems():
            setattr(self, key, value)

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
               all(getattr(self, name) == getattr(other, name)
                   for name in self.propertyNames
                   if not hasattr(self, "skipCompareNames") or
                      name not in self.skipCompareNames)

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return utils.hashList(getattr(self, name) for name in self.propertyNames)

__all__ = ["Data"]

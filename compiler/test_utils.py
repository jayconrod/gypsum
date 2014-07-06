# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


import unittest

from utils import *


class TestUtils(unittest.TestCase):
    def testTryDecodeString(self):
        # import pdb; pdb.set_trace()
        pairs = [("", '""'),
                 ("foo", '"foo"'),
                 (None, r'"foo\"'),
                 ("foo\n", '"foo\n"'),
                 ("foo\nbar", '"foo\nbar"'),
                 ('\a', '"\a"'),
                 ('\b', '"\b"'),
                 ('\f', '"\f"'),
                 ('\n', '"\n"'),
                 ('\r', '"\r"'),
                 ('\t', '"\t"'),
                 ('\v', '"\v"'),
                 ('e', '"\e"'),
                 ('"', '"\\""'),
                 ("'", '"\'"'),
                 ("\\", '"\\\\"'),
                 (None, r'"\x"'),
                 (None, r'"\x4"'),
                 ("A", r'"\x41"'),
                 ("A1", r'"\x411"'),
                 ("J", r'"\x4a"'),
                 ("J", r'"\x4A"'),
                 (None, r'"\u041"'),
                 ("A", r'"\u0041"'),
                 ("A", r'"\U00000041"'),
                 (None, r'"\x4G"')]
        for expected, str in pairs:
            self.assertEquals(expected, tryDecodeString(str))

    def testEncodeString(self):
        pairs = [('""', ""),
                 ('"foo"', "foo"),
                 (r'"foo\nbar"', "foo\nbar"),
                 (r'"\a"', "\a"),
                 (r'"\b"', "\b"),
                 (r'"\f"', "\f"),
                 (r'"\n"', "\n"),
                 (r'"\r"', "\r"),
                 (r'"\t"', "\t"),
                 (r'"\v"', "\v"),
                 (r'"\""', '"'),
                 (r'"\x00"', '\0'),
                 (r'"\u1000"', '\u1000'),
                 (r'"\U10000"', '\U10000')]
        for expected, str in pairs:
            self.assertEquals(expected, encodeString(str))

#!/usr/bin/env python

# Copyright 2014-2015 Jay Conrod. All rights reserved.

# This file is part of CodeSwitch. Use of this source code is governed by
# the 3-clause BSD license that can be found in the LICENSE.txt file.


import os.path
import re
import sys

if len(sys.argv) != 3:
    sys.stderr.write("usage: %s testname in.gy out.cpp\n" % sys.argv[0])
    sys.exit(1)

inFileName = sys.argv[1]
outFileName = sys.argv[2]

with open(inFileName) as inFile:
    contents = inFile.read()

testName = os.path.splitext(os.path.basename(inFileName))[0]
testName = re.sub(r"(?:^|(-))([a-zA-Z0-9])", lambda m: m.group(2).upper(), testName)

with open(outFileName, "w") as outFile:
    outFile.write("""// DO NOT MODIFY
// This file automatically generated by {scriptName}

#include "test.h"

#include "function.h"
#include "handle.h"
#include "interpreter.h"
#include "package.h"
#include "stack.h"
#include "utils.h"
#include "vm.h"

using namespace codeswitch::internal;

TEST({testName}) {{
  u8 bytes[] = {{ {bytes} }};
  VM vm;
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);
  auto package = Package::loadFromBytes(&vm, bytes, sizeof(bytes));
  vm.addPackage(package);

  Local<Stack> stack(vm.stack());
  Interpreter interpreter(&vm, stack);
  Local<Function> init(package->initFunction());
  Local<Function> entry(package->entryFunction());
  try {{
    if (*init)
      interpreter.call(init);
    interpreter.call(entry);
  }} catch (Error exn) {{
    // Test will throw an exception on failure.
    throw TestException(exn.message());
  }}
}}
""".format(scriptName=os.path.basename(sys.argv[0]),
           testName=testName,
           bytes=", ".join("0x%02x" % b for b in bytearray(contents))))

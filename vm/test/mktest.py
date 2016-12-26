#!/usr/bin/env python

# Copyright 2014-2016 Jay Conrod. All rights reserved.

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

#include <sstream>
#include <string>
#include <vector>
#include <cstdint>
#include "codeswitch.h"

using codeswitch::Error;
using codeswitch::Exception;
using codeswitch::VM;
using codeswitch::VMOptions;
using std::ios;
using std::string;
using std::stringstream;
using std::vector;


static vector<string> split(const string& str, char delim) {{
  vector<string> pieces;
  size_t begin = 0;
  auto end = str.find(delim);
  while (end != string::npos) {{
    auto len = end - begin;
    pieces.push_back(str.substr(begin, len));
    begin = end + 1;
    end = str.find(delim, begin);
  }}
  auto len = str.size() - begin;
  pieces.push_back(str.substr(begin, len));
  return pieces;
}}


TEST({testName}) {{
  uint8_t bytes[] = {{ {bytes} }};
  stringstream stream(string(reinterpret_cast<const char*>(bytes), sizeof(bytes)));
  stream.exceptions(ios::failbit | ios::badbit | ios::eofbit);

  VMOptions vmOptions;
  for (auto& path : split(getenv("CS_PACKAGE_PATH"), ':'))
    vmOptions.packageSearchPaths.push_back(path);
  VM vm(vmOptions);
  auto package = vm.loadPackageFromStream(stream);
  auto function = package.entryFunction();
  if (!function)
    throw TestException("main function not found");
  try {{
    function.call();
  }} catch (Error& exn) {{
    // Test will throw an exception on failure.
    throw TestException(exn.message());
  }} catch (Exception& exn) {{
    throw TestException("exception thrown from interpreted code");
  }}
}}
""".format(scriptName=os.path.basename(sys.argv[0]),
           testName=testName,
           bytes=", ".join("0x%02x" % b for b in bytearray(contents))))

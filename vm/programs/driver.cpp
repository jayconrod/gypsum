// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <iostream>
#include <cstdlib>
#include <codeswitch.h>

static void usage(const char* programName);

int main(int argc, char** argv) {
  if (argc != 2) {
    usage(argv[0]);
  }

  try {
    codeswitch::VM vm;
    codeswitch::Package package(vm.loadPackage(argv[1]));
    codeswitch::Function function(package.entryFunction());
    codeswitch::Arguments args(function);
    function.call(args);
  } catch (codeswitch::Error& err) {
    std::cerr << "error: " << err.message() << std::endl;
    exit(1);
  }

  return 0;
}


static void usage(const char* programName) {
  std::cerr << "usage: " << programName << " package" << std::endl;
  exit(1);
}

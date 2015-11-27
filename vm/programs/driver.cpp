// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <iostream>
#include <memory>
#include <string>

#include <codeswitch.h>

using std::cerr;
using std::move;
using std::endl;
using std::string;

using codeswitch::Error;
using codeswitch::Function;
using codeswitch::Package;
using codeswitch::VM;


int main(int argc, char** argv) {
  if (argc != 2) {
    cerr << "usage: " << argv[0] << " package-file" << endl;
    return 1;
  }
  string packageFileName(argv[1]);

  try {
    VM vm;
    auto package = vm.loadPackageFromFile(packageFileName);
    auto function = package.entryFunction();
    function.call();
  } catch (Error& err) {
    cerr << "error: " << err.message() << endl;
    return 1;
  }

  return 0;
}

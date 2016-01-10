// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <iostream>
#include <string>
#include <tuple>
#include <vector>

#include <codeswitch.h>

#include "test.h"

using std::cout;
using std::endl;
using std::make_tuple;
using std::string;
using std::vector;

using codeswitch::Function;
using codeswitch::Package;
using codeswitch::Name;
using codeswitch::NativeFunctionSearch;
using codeswitch::SEARCH_REGISTERED_FUNCTIONS;
using codeswitch::SEARCH_LIBRARY_FUNCTIONS;
using codeswitch::SEARCH_LINKED_FUNCTIONS;
using codeswitch::String;
using codeswitch::VMOptions;
using codeswitch::VM;


static string dirName(const string& path) {
  auto pos = path.find_last_of('/');
  return pos != string::npos ? path.substr(0, pos) : path;
}


static int64_t registered_f(VM* vm) {
  return 56;
}


int main(int argc, char* argv[]) {
  string programPath(argv[0]);
  auto programDir = dirName(programPath);

  VMOptions vmOptions;
  vmOptions.packageSearchPaths.push_back(programDir);
  vmOptions.nativeFunctions.push_back(
      make_tuple("NativeCalls.registered", "f", reinterpret_cast<void(*)()>(registered_f)));
  VM vm(vmOptions);
  auto fName = Name::fromStringForDefn(String(vm, "f"));
  auto gName = Name::fromStringForDefn(String(vm, "g"));

  // Check that we can call a native function in a shared library directly.
  auto sharedlibPath = programDir + "/NativeCalls.sharedlib-1.csp";
  auto sharedlib = vm.loadPackageFromFile(sharedlibPath,
      vector<NativeFunctionSearch>{SEARCH_LIBRARY_FUNCTIONS});
  auto sharedlib_f = sharedlib.getFunction(fName);
  auto result = sharedlib_f.callForI64();
  ASSERT_EQ(12, result);

  // Check that we can call a native function in a shared library through another function.
  auto sharedlib_g = sharedlib.getFunction(gName);
  result = sharedlib_g.callForI64();
  ASSERT_EQ(12, result);

  // Check that we can call a native function in a static library.
  auto staticlibPath = programDir + "/NativeCalls.staticlib-1.csp";
  auto staticlib = vm.loadPackageFromFile(staticlibPath,
      vector<NativeFunctionSearch>{SEARCH_LINKED_FUNCTIONS});
  auto staticlib_f = staticlib.getFunction(fName);
  result = staticlib_f.callForI64();
  ASSERT_EQ(34, result);

  // Check that we can call a native function in a static library through another function.
  auto staticlib_g = staticlib.getFunction(gName);
  result = staticlib_g.callForI64();
  ASSERT_EQ(34, result);

  // Check that we can call a native function registered with the VM.
  auto registeredPath = programDir + "/NativeCalls.registered-1.csp";
  auto registered = vm.loadPackageFromFile(registeredPath,
      vector<NativeFunctionSearch>{SEARCH_REGISTERED_FUNCTIONS});
  auto registered_f = registered.getFunction(fName);
  result = registered_f.callForI64();
  ASSERT_EQ(56, result);

  // Check that we can call a native function registered with the VM through another function.
  auto registered_g = registered.getFunction(gName);
  result = registered_g.callForI64();
  ASSERT_EQ(56, result);

  return 0;
}


// Hack to force static lib to get linked.
extern "C" int64_t NativeCalls__staticlib___f(VM* vm);


void unused() {
  NativeCalls__staticlib___f(nullptr);
}

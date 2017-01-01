// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <string>

#include <codeswitch.h>

using std::string;

using codeswitch::Name;
using codeswitch::String;
using codeswitch::VM;
using codeswitch::VMOptions;


static string dirName(const string& path) {
  auto pos = path.find_last_of('/');
  return pos != string::npos ? path.substr(0, pos) : path;
}


int main(int argc, char* argv[]) {
  VMOptions vmOptions;
  auto testDir = dirName(argv[0]);
  auto newDir = testDir + "/binarycompatibility-new";
  vmOptions.packageSearchPaths.push_back(testDir);
  vmOptions.packageSearchPaths.push_back(newDir);
  VM vm(vmOptions);
  auto packageName = Name::fromStringForPackage(String(&vm, "binarycompatibility.foo"));
  auto package = vm.loadPackage(packageName);
  auto entryFunction = package.entryFunction();
  entryFunction.call();
  return 0;
}

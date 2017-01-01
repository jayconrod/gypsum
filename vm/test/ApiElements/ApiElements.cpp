// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <string>
#include <cstdint>

#include <codeswitch.h>

#include "test.h"

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
  vmOptions.packageSearchPaths.push_back(dirName(argv[0]));
  VM vm(vmOptions);
  auto packageName = Name::fromStringForPackage(String(&vm, "ApiElements"));
  auto package = vm.loadPackage(packageName);
  ASSERT_TRUE(package);

  // TODO: should be able to create objects and arrays from C++.

  // Check that we can get elements from a mutable array.
  auto createMutI8Array = package.findFunction("create-mut-i8-array", "(I)");
  auto array = createMutI8Array.call(static_cast<int32_t>(5)).asObject();
  ASSERT_TRUE(array.hasElements());
  ASSERT_EQ(5, array.length());
  ASSERT_FALSE(array.elementsAreConstant());
  ASSERT_EQ(2, array.getElement(2).asI8());
  array.setElement(2, static_cast<int8_t>(3));
  ASSERT_EQ(3, array.getElement(2).asI8());

  int8_t buffer[3];
  array.copyElementsTo(2, buffer, 3);
  ASSERT_EQ(buffer[0], 3);
  ASSERT_EQ(buffer[1], 3);
  ASSERT_EQ(buffer[2], 4);
  buffer[2] = 99;
  array.copyElementsFrom(0, buffer, 3);
  ASSERT_EQ(99, array.getElement(2).asI8());

  return 0;
}

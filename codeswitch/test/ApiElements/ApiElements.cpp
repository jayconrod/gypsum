// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <string>
#include <cstdint>

#include <codeswitch.h>

#include "error.h"
#include "test.h"

using std::cerr;
using std::endl;
using std::string;

using codeswitch::Error;
using codeswitch::Exception;
using codeswitch::Name;
using codeswitch::String;
using codeswitch::VM;
using codeswitch::VMOptions;


int main(int argc, char* argv[]) {
  try {
    VMOptions vmOptions;
    for (int i = 1; i < argc; i++) {
      vmOptions.packageSearchPaths.push_back(argv[i]);
    }
    VM vm(vmOptions);
    auto packageName = Name::fromStringForPackage(String(&vm, "test.ApiElements"));
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
  } catch (TestException& exn) {
    cerr << "test failure: " << exn.message() << endl;
    return 1;
  } catch (Exception& exn) {
    cerr << "test exception: " << exn.message() << endl;
    return 2;
  } catch (Error& exn) {
    cerr << "test error: " << exn.message() << endl;
    return 2;
  } catch (codeswitch::internal::Error& exn) {
    cerr << "internal error: " << exn.message() << endl;
    return 2;
  }

  return 0;
}

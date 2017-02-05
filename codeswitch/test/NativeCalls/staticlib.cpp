// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <cstdint>

#include <codeswitch.h>

extern "C" __attribute__((visibility("default")))
int64_t test__NativeCalls__staticlib___f(codeswitch::VM* vm) {
  return 34;
}

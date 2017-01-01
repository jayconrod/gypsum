// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "bytecode.h"

namespace codeswitch {
namespace internal {

i64 readVbn(u8** pc) {
  i64 n = 0;
  i64 shift = 0;
  bool more;
  do {
    u8 b = *reinterpret_cast<u8*>((*pc)++);
    more = bit(b, 7);
    n |= (b & 0x7FLL) << shift;
    shift += 7;
  } while (more && shift < 64);
  ASSERT(!more);
  if (shift < 64) {
    int signExtend = 64 - shift;
    n = (n << signExtend) >> signExtend;
  }
  return n;
}

}
}

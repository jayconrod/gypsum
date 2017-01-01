// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef bytecode_h
#define bytecode_h

#include "opcodes.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

enum Width { W8, W16, W32, W64 };

#if WORDSIZE == 64
const Width WORD = W64;
#else
const Width WORD = W32;
#endif


inline word_t sizeFromWidth(Width width) {
  switch (width) {
    case W8:  return 1;
    case W16: return 2;
    case W32: return 4;
    case W64: return 8;
    default:
      UNREACHABLE();
  }
  return 0;
}


// Variable byte numbers are compressed signed integers embedded in byte code. They may be
// between 1 and 9 bytes long. The high bit of each byte except for the last is set; this
// indicates there are more bytes. The other bits are combined in little-endian order and
// sign-extended to 64-bits.
i64 readVbn(u8** pc);

inline i64 readVbn(u8* basePc, length_t* offset) {
  u8* pc = basePc + *offset;
  i64 value = readVbn(&pc);
  *offset = pc - basePc;
  return value;
}

inline i64 readVbn(u8* pc, length_t offset) {
  return readVbn(pc, &offset);
}


}
}

#endif

// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include "bytecode.h"
#include "utils.h"

using namespace codeswitch::internal;

static void checkVbn(u8* data, int length, i64 expected) {
  u8* oldPc = data;
  u8* newPc = oldPc;
  i64 value = readVbn(&newPc);
  ASSERT_EQ(expected, value);
  ASSERT_EQ(length, newPc - oldPc);
}


TEST(ReadVbn) {
  u8 zero[] = { 0 };
  checkVbn(zero, sizeof(zero), 0LL);

  u8 one[] = { 1 };
  checkVbn(one, sizeof(one), 1LL);

  u8 minusOne[] = { 0x7f };
  checkVbn(minusOne, sizeof(minusOne), -1LL);

  u8 oneThousand[] = { 0xe8, 0x07 };
  checkVbn(oneThousand, sizeof(oneThousand), 1000LL);

  u8 minusOneThousand[] = { 0x98, 0x78 };
  checkVbn(minusOneThousand, sizeof(minusOneThousand), -1000LL);

  u8 maxInt[] = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00 };
  checkVbn(maxInt, sizeof(maxInt), 0x7FFFFFFFFFFFFFFFLL);

  u8 minInt[] = { 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01 };
  checkVbn(minInt, sizeof(minInt), 0x8000000000000000LL);
}

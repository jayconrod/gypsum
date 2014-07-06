// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include "utils.h"

using namespace codeswitch::internal;

TEST(Alignment) {
  ASSERT_EQ(0, align(0, 4));
  ASSERT_EQ(4, align(1, 4));
  ASSERT_EQ(4, align(3, 4));
  ASSERT_TRUE(isAligned(0, 4));
  ASSERT_FALSE(isAligned(1, 4));
  ASSERT_FALSE(isAligned(3, 4));
  ASSERT_TRUE(isAligned(4, 4));
}


TEST(Bits) {
  ASSERT_TRUE(bit(0x10, 4));
  ASSERT_FALSE(bit(0x10, 3));

  ASSERT_EQ(0x33, bitExtract(0xF33F00, 8, 12));
  ASSERT_EQ(0xFFFF00, bitInsert(0xF33F00, 0xFF, 8, 12));
}


TEST(Utf8Decode) {
  u32 snowman32 = 0x2603;
  const u8 snowman8[3] { 0xe2, 0x98, 0x83 };
  ASSERT_EQ(UTF8_DECODE_ERROR, utf8Decode(snowman8 + 3, snowman8 + 3));
  ASSERT_EQ(UTF8_DECODE_ERROR, utf8Decode(snowman8 + 2, snowman8 + 3));
  ASSERT_EQ(UTF8_DECODE_ERROR, utf8Decode(snowman8, snowman8 + 1));
  ASSERT_EQ(UTF8_DECODE_ERROR, utf8Decode(snowman8, snowman8 + 2));
  auto p = snowman8;
  ASSERT_EQ(snowman32, utf8Decode(&p, snowman8 + 3));
  ASSERT_EQ(snowman8 + 3, p);

  u32 a32 = 0x41;
  u8 a8[1] = { 0x41 };
  p = a8;
  ASSERT_EQ(a32, utf8Decode(&p, a8 + 1));
  ASSERT_EQ(a8 + 1, p);

  u32 pi32 = 0x3c0;
  u8 pi8[2] = { 0xcf, 0x80 };
  p = pi8;
  ASSERT_EQ(pi32, utf8Decode(&p, pi8 + 2));
  ASSERT_EQ(pi8 + 2, p);

  u32 bug32 = 0x10059;
  u8 bug8[4] = { 0xf0, 0x90, 0x81, 0x99 };
  p = bug8;
  ASSERT_EQ(bug32, utf8Decode(&p, bug8 + 4));
  ASSERT_EQ(bug8 + 4, p);

  u8 tooLong[6] = { 0xfd, 0x80, 0x80, 0x80, 0x80, 0x80 };
  ASSERT_EQ(UTF8_DECODE_ERROR, utf8Decode(tooLong, tooLong + 6));
}


TEST(Utf8EncodeSize) {
  ASSERT_EQ(1, utf8EncodeSize(0x41));
  ASSERT_EQ(2, utf8EncodeSize(0x3c0));
  ASSERT_EQ(3, utf8EncodeSize(0x2603));
  ASSERT_EQ(4, utf8EncodeSize(0x10059));
}


static void testUtf8Encode(u32 char32, u8* expected, word_t size) {
  u8 scratch[4];
  u8* p = scratch;
  utf8Encode(char32, &p);
  ASSERT_EQ(size, static_cast<word_t>(p - scratch));
  for (word_t i = 0; i < size; i++) {
    ASSERT_EQ(expected[i], scratch[i]);
  }
}


TEST(Utf8Encode) {
  u8 a8[] = { 0x41 };
  testUtf8Encode(0x41, a8, 1);
  u8 pi8[] = { 0xcf, 0x80 };
  testUtf8Encode(0x3c0, pi8, 2);
  u8 snowman8[] = { 0xe2, 0x98, 0x83 };
  testUtf8Encode(0x2603, snowman8, 3);
  u8 bug8[] = { 0xf0, 0x90, 0x81, 0x99 };
  testUtf8Encode(0x10059, bug8, 4);
}

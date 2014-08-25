// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <string>
#include <vector>
#include "block.h"
#include "handle.h"
#include "string-inl.h"
#include "vm-inl.h"

using namespace std;
using namespace codeswitch::internal;

TEST(StringFromUtf8) {
  VM vm;
  HandleScope handleScope(&vm);
  const u8 chars[] = { 0x66, 0x6f, 0x6f, 0xe2, 0x98, 0x83, 0x00 };
  word_t size = sizeof(chars) - 1;
  word_t length = 4;
  const u32 expected[] = { 0x66, 0x6f, 0x6f, 0x2603 };
  auto str1 = String::fromUtf8CString(vm.heap(),
                                                reinterpret_cast<const char*>(chars));
  auto str2 = String::fromUtf8String(vm.heap(), chars, size);
  auto str3 = String::fromUtf8String(vm.heap(), chars, length, size);
  ASSERT_EQ(length, str1->length());
  ASSERT_EQ(length, str2->length());
  ASSERT_EQ(length, str3->length());
  for (word_t i = 0; i < length; i++) {
    ASSERT_EQ(expected[i], str1->get(i));
    ASSERT_EQ(expected[i], str2->get(i));
    ASSERT_EQ(expected[i], str3->get(i));
  }
}


TEST(StringToStl) {
  VM vm;
  HandleScope handleScope(&vm);
  const u32 chars[] = { 0x66, 0x6f, 0x6f, 0x2603 };
  const u8 expected[] = { 0x66, 0x6f, 0x6f, 0xe2, 0x98, 0x83 };
  auto str = String::allocate(vm.heap(), 4);
  str->initialize(chars);
  ASSERT_EQ(4, str->length());
  ASSERT_EQ(6, str->utf8EncodedSize());
  auto stlVec = str->toUtf8StlVector();
  auto stlStr = str->toUtf8StlString();
  ASSERT_EQ(6, stlVec.size());
  ASSERT_EQ(6, stlStr.length());
  for (int i = 0; i < 6; i++) {
    ASSERT_EQ(expected[i], stlVec[i]);
    ASSERT_EQ(expected[i], static_cast<u8>(stlStr[i]));
  }
}


TEST(StringCompare) {
  VM vm;
  HandleScope handleScope(&vm);
  auto foo = String::fromUtf8CString(vm.heap(), "foo");
  auto foob = String::fromUtf8CString(vm.heap(), "foob");
  auto bar = String::fromUtf8CString(vm.heap(), "bar");
  ASSERT_TRUE(foo->equals(*foo));
  ASSERT_FALSE(foo->equals(*foob));
  ASSERT_FALSE(foo->equals(*bar));

  ASSERT_TRUE(foo->compare(*foo) == 0);
  ASSERT_TRUE(foo->compare(*foob) < 0);
  ASSERT_TRUE(foob->compare(*foo) > 0);
  ASSERT_TRUE(foo->compare(*bar) > 0);
  ASSERT_TRUE(bar->compare(*foo) < 0);
}


TEST(StringConcat) {
  VM vm;
  HandleScope handleScope(&vm);
  auto empty = String::fromUtf8CString(vm.heap(), "");
  auto foo = String::fromUtf8CString(vm.heap(), "foo");
  auto bar = String::fromUtf8CString(vm.heap(), "bar");
  auto expected = String::fromUtf8CString(vm.heap(), "foobar");
  ASSERT_EQ(*foo, *String::concat(vm.heap(), foo, empty));
  ASSERT_EQ(*foo, *String::concat(vm.heap(), empty, foo));
  ASSERT_TRUE(expected->equals(*String::concat(vm.heap(), foo, bar)));
}

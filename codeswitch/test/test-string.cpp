// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <string>
#include <vector>
#include <cstdint>
#include "array.h"
#include "block.h"
#include "handle.h"
#include "string.h"
#include "vm.h"

using namespace std;
using namespace codeswitch::internal;


TEST(StringFromUtf8) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);
  const u8 chars[] = { 0x66, 0x6f, 0x6f, 0xe2, 0x98, 0x83, 0x00 };
  length_t length = sizeof(chars) - 1;
  auto str1 = STR(reinterpret_cast<const char*>(chars));
  auto str2 = String::fromUtf8String(vm.heap(), chars, length);
  ASSERT_EQ(length, str1->length());
  ASSERT_EQ(length, str2->length());
  for (word_t i = 0; i < length; i++) {
    ASSERT_EQ(chars[i], str1->get(i));
    ASSERT_EQ(chars[i], str2->get(i));
  }
}


TEST(StringToStl) {
  VM vm;
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);
  const u8 chars[] = { 0x66, 0x6f, 0x6f, 0xe2, 0x98, 0x83 };
  length_t length = sizeof(chars);
  auto str = new(vm.heap(), length) String(reinterpret_cast<const char*>(chars));
  ASSERT_EQ(length, str->length());
  auto stlVec = str->toUtf8StlVector();
  auto stlStr = str->toUtf8StlString();
  ASSERT_EQ(length, stlVec.size());
  ASSERT_EQ(length, stlStr.length());
  for (length_t i = 0; i < length; i++) {
    ASSERT_EQ(chars[i], stlVec[i]);
    ASSERT_EQ(chars[i], static_cast<u8>(stlStr[i]));
  }
}


TEST(StringCompare) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);
  auto foo = STR("foo");
  auto foob = STR("foob");
  auto bar = STR("bar");
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
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);
  auto empty = STR("");
  auto foo = STR("foo");
  auto bar = STR("bar");
  auto expected = STR("foobar");
  ASSERT_EQ(*foo, *String::concat(foo, empty));
  ASSERT_EQ(*foo, *String::concat(empty, foo));
  ASSERT_TRUE(expected->equals(*String::concat(foo, bar)));
}


TEST(StringSubstring) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);

  auto hello = STR("hello");
  auto empty = STR("");
  ASSERT_TRUE(empty->equals(*String::substring(hello, 0, 0)));
  ASSERT_TRUE(empty->equals(*String::substring(hello, 5, 5)));
  auto hel = STR("hel");
  ASSERT_TRUE(hel->equals(*String::substring(hello, 0, 3)));
  auto llo = STR("llo");
  ASSERT_TRUE(llo->equals(*String::substring(hello, 2, 5)));
}


TEST(StringFindChar) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);

  auto empty = STR("");
  ASSERT_EQ(kIndexNotSet, empty->find(static_cast<u32>('x')));

  auto hello = STR("hello");
  ASSERT_EQ(0, hello->find(static_cast<u32>('h')));
  ASSERT_EQ(2, hello->find(static_cast<u32>('l')));
  ASSERT_EQ(3, hello->find(static_cast<u32>('l'), 3));
  ASSERT_EQ(kIndexNotSet, hello->find(static_cast<u32>('x')));
}


TEST(StringFindString) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);

  auto empty = STR("");
  auto hello = STR("hello");
  auto helloHello = STR("hello hello");
  ASSERT_EQ(kIndexNotSet, empty->find(*helloHello));
  ASSERT_EQ(0, helloHello->find(*empty));
  ASSERT_EQ(0, helloHello->find(*hello));
  ASSERT_EQ(6, helloHello->find(*hello, 1));
}


TEST(StringCountChar) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);

  u32 sep = ',';
  auto empty = STR("");
  ASSERT_EQ(0, empty->count(sep));

  auto test = STR(",a,bb,,c,");
  ASSERT_EQ(5, test->count(sep));
}


TEST(StringCountString) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);

  auto empty = STR("");
  ASSERT_EQ(1, empty->count(*empty));
  auto sep = STR("||");
  ASSERT_EQ(0, empty->count(*sep));
  ASSERT_EQ(3, sep->count(*empty));

  auto test = STR("||a||bb||||c|||");
  ASSERT_EQ(5, test->count(*sep));
}


TEST(StringSplitChar) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);

  auto empty = STR("");
  auto sep = static_cast<u32>(',');
  auto split = String::split(heap, empty, sep);
  ASSERT_EQ(1, split->length());
  ASSERT_TRUE(split->get(0)->equals(*empty));

  auto obvious = STR("foo,bar,baz");
  split = String::split(heap, obvious, sep);
  ASSERT_EQ(3, split->length());
  ASSERT_TRUE(split->get(0)->equals("foo"));
  ASSERT_TRUE(split->get(1)->equals("bar"));
  ASSERT_TRUE(split->get(2)->equals("baz"));

  auto surrounded = STR(",bar,");
  split = String::split(heap, surrounded, sep);
  ASSERT_EQ(3, split->length());
  ASSERT_TRUE(split->get(0)->equals(""));
  ASSERT_TRUE(split->get(1)->equals("bar"));
  ASSERT_TRUE(split->get(2)->equals(""));

  auto dub = STR("foo,,baz");
  split = String::split(heap, dub, sep);
  ASSERT_EQ(3, split->length());
  ASSERT_TRUE(split->get(0)->equals("foo"));
  ASSERT_TRUE(split->get(1)->equals(""));
  ASSERT_TRUE(split->get(2)->equals("baz"));
}


TEST(StringSplitString) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);

  auto empty = STR("");
  auto sep = STR("||");
  auto split = String::split(heap, empty, sep);
  ASSERT_EQ(1, split->length());
  ASSERT_TRUE(split->get(0)->isEmpty());
  split = String::split(heap, sep, empty);
  ASSERT_EQ(2, split->length());
  ASSERT_TRUE(split->get(0)->equals("|"));
  ASSERT_TRUE(split->get(1)->equals("|"));

  auto obvious = STR("foo||bar||baz");
  split = String::split(heap, obvious, sep);
  ASSERT_EQ(3, split->length());
  ASSERT_TRUE(split->get(0)->equals("foo"));
  ASSERT_TRUE(split->get(1)->equals("bar"));
  ASSERT_TRUE(split->get(2)->equals("baz"));

  auto surrounded = STR("||bar||");
  split = String::split(heap, surrounded, sep);
  ASSERT_EQ(3, split->length());
  ASSERT_TRUE(split->get(0)->equals(""));
  ASSERT_TRUE(split->get(1)->equals("bar"));
  ASSERT_TRUE(split->get(2)->equals(""));

  auto dub = STR("foo||||baz");
  split = String::split(heap, dub, sep);
  ASSERT_EQ(3, split->length());
  ASSERT_TRUE(split->get(0)->equals("foo"));
  ASSERT_TRUE(split->get(1)->equals(""));
  ASSERT_TRUE(split->get(2)->equals("baz"));

  auto almost = STR("foo|||baz");
  split = String::split(heap, almost, sep);
  ASSERT_EQ(2, split->length());
  ASSERT_TRUE(split->get(0)->equals("foo"));
  ASSERT_TRUE(split->get(1)->equals("|baz"));
}


TEST(StringJoin) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);

  auto list = handle(reinterpret_cast<BlockArray<String>*>(vm.roots()->emptyBlockArray()));
  auto empty = STR("");
  auto sep = STR(",");
  ASSERT_TRUE(empty->equals(*String::join(heap, list, empty)));
  ASSERT_TRUE(empty->equals(*String::join(heap, list, sep)));

  auto foo = STR("foo");
  list = BlockArray<String>::create(heap, 1);
  list->set(0, *foo);
  ASSERT_TRUE(foo->equals(*String::join(heap, list, empty)));
  ASSERT_TRUE(foo->equals(*String::join(heap, list, sep)));

  auto bar = STR("bar");
  list = BlockArray<String>::create(heap, 2);
  list->set(0, *foo);
  list->set(1, *bar);
  ASSERT_TRUE(STR("foobar")->equals(*String::join(heap, list, empty)));
  ASSERT_TRUE(STR("foo,bar")->equals(*String::join(heap, list, sep)));
  ASSERT_TRUE(STR("foo||bar")->equals(*String::join(heap, list, STR("||"))));
}


TEST(StringToI32) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(&vm);

  i32 n;
  ASSERT_FALSE(STR("")->tryToI32(&n));
  ASSERT_FALSE(STR("a")->tryToI32(&n));
  ASSERT_FALSE(STR("0a")->tryToI32(&n));
  ASSERT_FALSE(STR("a0")->tryToI32(&n));
  ASSERT_FALSE(STR("+")->tryToI32(&n));
  ASSERT_FALSE(STR("-")->tryToI32(&n));

  ASSERT_TRUE(STR("0")->tryToI32(&n));
  ASSERT_EQ(0, n);
  ASSERT_TRUE(STR("0000")->tryToI32(&n));
  ASSERT_EQ(0, n);
  ASSERT_TRUE(STR("1234")->tryToI32(&n));
  ASSERT_EQ(1234, n);
  ASSERT_TRUE(STR("+123")->tryToI32(&n));
  ASSERT_EQ(123, n);
  ASSERT_TRUE(STR("-123")->tryToI32(&n));

  ASSERT_TRUE(STR("2147483647")->tryToI32(&n));
  ASSERT_EQ(INT32_MAX, n);
  ASSERT_FALSE(STR("2147483648")->tryToI32(&n));
  ASSERT_TRUE(STR("-2147483648")->tryToI32(&n));
  ASSERT_EQ(INT32_MIN, n);
  ASSERT_FALSE(STR("-2147483649")->tryToI32(&n));
}

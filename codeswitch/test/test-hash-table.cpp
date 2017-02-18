// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include "array.h"
#include "hash-table.h"
#include "string.h"

using namespace std;
using namespace codeswitch::internal;

typedef BlockHashMap<String, String> StringHashMap;

TEST(HashMapBasics) {
  TEST_PROLOGUE

  auto map = StringHashMap::create(heap);
  ASSERT_TRUE(map->isEmpty());
  ASSERT_EQ(0, map->length());

  auto foo = STR("foo");
  auto bar = STR("bar");
  auto baz = STR("baz");
  ASSERT_FALSE(map->contains(*foo));

  StringHashMap::add(map, foo, foo);
  ASSERT_EQ(1, map->length());
  ASSERT_FALSE(map->isEmpty());
  ASSERT_TRUE(map->contains(*foo));
  ASSERT_EQ(*foo, map->get(*foo));
  ASSERT_EQ(*foo, map->getOrElse(*foo, nullptr));
  ASSERT_EQ(nullptr, map->getOrElse(*bar, nullptr));

  StringHashMap::add(map, bar, bar);
  StringHashMap::add(map, baz, baz);
  ASSERT_EQ(3, map->length());
  ASSERT_TRUE(map->contains(*bar));
  ASSERT_TRUE(map->contains(*baz));

  StringHashMap::remove(map, foo);
  ASSERT_EQ(2, map->length());
  ASSERT_FALSE(map->contains(*foo));

  StringHashMap::remove(map, bar);
  StringHashMap::remove(map, baz);
  ASSERT_TRUE(map->isEmpty());
}


TEST(HashMapStress) {
  TEST_PROLOGUE

  length_t repetitions = 2000;
  auto strings = BlockArray<String>::create(heap, repetitions);
  {
    HandleScope handleScope(&vm);
    for (length_t i = 0; i < repetitions; i++) {
      auto s = String::fromUtf8String(heap, to_string(i));
      strings->set(i, *s);
    }
  }

  auto map = StringHashMap::create(heap);
  for (auto i = 0; i < 2; i++) {
    {
      HandleScope handleScope(&vm);
      for (length_t j = 0; j < repetitions; j++) {
        auto s = handle(strings->get(j));
        StringHashMap::add(map, s, s);
        ASSERT_EQ(j + 1, map->length());
      }
    }
    {
      HandleScope handleScope(&vm);
      for (length_t j = 0; j < repetitions; j++) {
        auto s = handle(strings->get(j));
        ASSERT_TRUE(map->contains(*s));
      }
    }
    {
      HandleScope handleScope(&vm);
      for (length_t j = 0; j < repetitions; j++) {
        auto s = handle(strings->get(j));
        StringHashMap::remove(map, s);
        ASSERT_EQ(repetitions - j - 1, map->length());
      }
    }
  }
}

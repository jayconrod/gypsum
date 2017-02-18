// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include "array.h"
#include "name.h"
#include "string.h"

using namespace std;
using namespace codeswitch::internal;


TEST(BadPackageNames) {
  TEST_PROLOGUE

  ASSERT_FALSE(Name::fromString(heap, STR(""), Name::PACKAGE_NAME));
  ASSERT_FALSE(Name::fromString(heap, STR("0"), Name::PACKAGE_NAME));
  ASSERT_FALSE(Name::fromString(heap, STR("_"), Name::PACKAGE_NAME));
  ASSERT_FALSE(Name::fromString(heap, STR("-"), Name::PACKAGE_NAME));
  ASSERT_FALSE(Name::fromString(heap, STR("A-"), Name::PACKAGE_NAME));

  ASSERT_FALSE(Name::fromString(heap, STR("."), Name::PACKAGE_NAME));
  ASSERT_FALSE(Name::fromString(heap, STR("a."), Name::PACKAGE_NAME));
  ASSERT_FALSE(Name::fromString(heap, STR(".b"), Name::PACKAGE_NAME));

  auto longLength = Name::kMaxComponentLength + 1;
  vector<char> longNameChars(longLength, 'a');
  auto longNameStr = String::fromUtf8String(heap, longNameChars.data(), longLength);
  ASSERT_FALSE(Name::fromString(heap, longNameStr, Name::PACKAGE_NAME));

  auto longCount = Name::kMaxComponentCount + 1;
  vector<char> longCountChars(longCount * 2 - 1);
  for (size_t i = 0; i + 1 < longCountChars.size(); i += 2) {
    longCountChars[i] = 'a';
    longCountChars[i + 1] = '.';
  }
  longCountChars.back() = 'a';
  auto longCountStr =
      String::fromUtf8String(heap, longCountChars.data(), longCountChars.size());
  ASSERT_FALSE(Name::fromString(heap, longCountStr, Name::PACKAGE_NAME));
}


TEST(GoodPackageNames) {
  TEST_PROLOGUE

  auto name = Name::fromString(heap, STR("foo"), Name::PACKAGE_NAME);
  auto components = BlockArray<String>::create(heap, 1);
  components->set(0, *STR("foo"));
  auto expected = Name::create(heap, components);
  ASSERT_TRUE(expected->equals(*name));

  name = Name::fromString(heap, STR("f_oo1.bar34.baz"), Name::PACKAGE_NAME);
  components = BlockArray<String>::create(heap, 3);
  components->set(0, *STR("f_oo1"));
  components->set(1, *STR("bar34"));
  components->set(2, *STR("baz"));
  expected = Name::create(heap, components);
  ASSERT_TRUE(expected->equals(*name));
}

// Copyright 2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <string>
#include "array.h"
#include "package.h"
#include "string.h"

#define STR(s) String::fromUtf8CString(heap, s)

using namespace std;
using namespace codeswitch::internal;


TEST(BadPackageNames) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);

  ASSERT_FALSE(PackageName::fromString(heap, STR("")));
  ASSERT_FALSE(PackageName::fromString(heap, STR("0")));
  ASSERT_FALSE(PackageName::fromString(heap, STR("_")));
  ASSERT_FALSE(PackageName::fromString(heap, STR("-")));
  ASSERT_FALSE(PackageName::fromString(heap, STR("A-")));

  ASSERT_FALSE(PackageName::fromString(heap, STR(".")));
  ASSERT_FALSE(PackageName::fromString(heap, STR("a.")));
  ASSERT_FALSE(PackageName::fromString(heap, STR(".b")));

  auto longLength = PackageName::kMaxComponentLength + 1;
  vector<u8> longNameChars(longLength, 'a');
  auto longNameStr = String::fromUtf8String(heap, longNameChars.data(), longLength);
  ASSERT_FALSE(PackageName::fromString(heap, longNameStr));

  auto longCount = PackageName::kMaxComponentCount + 1;
  vector<u8> longCountChars(longCount * 2 - 1);
  for (size_t i = 0; i + 1 < longCountChars.size(); i += 2) {
    longCountChars[i] = 'a';
    longCountChars[i + 1] = '.';
  }
  longCountChars.back() = 'a';
  auto longCountStr =
      String::fromUtf8String(heap, longCountChars.data(), longCountChars.size());
  ASSERT_FALSE(PackageName::fromString(heap, longCountStr));
}


TEST(GoodPackageNames) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);

  auto name = PackageName::fromString(heap, STR("foo"));
  auto components = BlockArray<String>::create(heap, 1);
  components->set(0, *STR("foo"));
  auto expected = PackageName::create(heap, components);
  ASSERT_TRUE(expected->equals(*name));

  name = PackageName::fromString(heap, STR("f_oo1.bar34.baz"));
  components = BlockArray<String>::create(heap, 3);
  components->set(0, *STR("f_oo1"));
  components->set(1, *STR("bar34"));
  components->set(2, *STR("baz"));
  expected = PackageName::create(heap, components);
  ASSERT_TRUE(expected->equals(*name));
}


TEST(BadPackageVersions) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);

  ASSERT_FALSE(PackageVersion::fromString(heap, STR("")));
  ASSERT_FALSE(PackageVersion::fromString(heap, STR("A")));
  ASSERT_FALSE(PackageVersion::fromString(heap, STR("-1")));

  ASSERT_FALSE(PackageVersion::fromString(heap, STR(".")));
  ASSERT_FALSE(PackageVersion::fromString(heap, STR("1.")));
  ASSERT_FALSE(PackageVersion::fromString(heap, STR(".1")));

  int bigVersion = static_cast<int>(PackageVersion::kMaxComponent + 1);
  auto bigVersionStr = STR(to_string(bigVersion).c_str());
  ASSERT_FALSE(PackageVersion::fromString(heap, bigVersionStr));

  auto longCount = PackageVersion::kMaxComponentCount + 1;
  vector<u8> longCountChars(2 * longCount - 1);
  for (size_t i = 0; i + 1 < longCountChars.size(); i += 2) {
    longCountChars[i] = '1';
    longCountChars[i + 1] = '.';
  }
  longCountChars.back() = '1';
  auto longCountStr =
      String::fromUtf8String(heap, longCountChars.data(), longCountChars.size());
  ASSERT_FALSE(PackageVersion::fromString(heap, longCountStr));
}


TEST(GoodPackageVersions) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);

  auto version = PackageVersion::fromString(heap, STR("1"));
  auto components = I32Array::create(heap, 1);
  components->set(0, 1);
  auto expected = PackageVersion::create(heap, components);
  ASSERT_TRUE(expected->equals(*version));

  version = PackageVersion::fromString(heap, STR("12.0.34"));
  components = I32Array::create(heap, 3);
  components->set(0, 12);
  components->set(1, 0);
  components->set(2, 34);
  expected = PackageVersion::create(heap, components);
  ASSERT_TRUE(expected->equals(*version));
}


TEST(PackageDependencyFromString) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);

  auto dependency = PackageDependency::fromString(heap, STR("foo.bar"));
  auto name = PackageName::fromString(heap, STR("foo.bar"));
  Local<PackageVersion> minVersion, maxVersion;
  auto expected = PackageDependency::create(heap, name, minVersion, maxVersion);
  ASSERT_TRUE(expected->equals(*dependency));

  dependency = PackageDependency::fromString(heap, STR("foo.bar:"));
  ASSERT_TRUE(expected->equals(*dependency));

  dependency = PackageDependency::fromString(heap, STR("foo.bar::"));
  ASSERT_TRUE(expected->equals(*dependency));

  dependency = PackageDependency::fromString(heap, STR("foo.bar:1.2"));
  minVersion = PackageVersion::fromString(heap, STR("1.2"));
  expected = PackageDependency::create(heap, name, minVersion, maxVersion);
  ASSERT_TRUE(expected->equals(*dependency));

  dependency = PackageDependency::fromString(heap, STR("foo.bar:1.2:"));
  ASSERT_TRUE(expected->equals(*dependency));

  dependency = PackageDependency::fromString(heap, STR("foo.bar:1.2:3.4"));
  maxVersion = PackageVersion::fromString(heap, STR("3.4"));
  expected = PackageDependency::create(heap, name, minVersion, maxVersion);
  ASSERT_TRUE(expected->equals(*dependency));

  dependency = PackageDependency::fromString(heap, STR("foo.bar::3.4"));
  minVersion = Local<PackageVersion>();
  expected = PackageDependency::create(heap, name, minVersion, maxVersion);
  ASSERT_TRUE(expected->equals(*dependency));
}


TEST(PackageDependencySatisfied) {
  VM vm;
  Heap* heap = vm.heap();
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);

  auto dependency = PackageDependency::fromString(heap, STR("foo:1:3"));
  auto name = PackageName::fromString(heap, STR("bar"));
  auto v1 = PackageVersion::fromString(heap, STR("1"));
  ASSERT_FALSE(dependency->isSatisfiedBy(*name, *v1));

  name = PackageName::fromString(heap, STR("foo"));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v1));

  auto v0 = PackageVersion::fromString(heap, STR("0"));
  ASSERT_FALSE(dependency->isSatisfiedBy(*name, *v0));

  auto v3 = PackageVersion::fromString(heap, STR("3"));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v3));

  auto v4 = PackageVersion::fromString(heap, STR("4"));
  ASSERT_FALSE(dependency->isSatisfiedBy(*name, *v4));

  dependency = PackageDependency::fromString(heap, STR("foo::3"));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v0));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v3));
  ASSERT_FALSE(dependency->isSatisfiedBy(*name, *v4));

  dependency = PackageDependency::fromString(heap, STR("foo:1"));
  ASSERT_FALSE(dependency->isSatisfiedBy(*name, *v0));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v1));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v4));
}

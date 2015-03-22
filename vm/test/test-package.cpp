// Copyright 2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <string>
#include "array.h"
#include "flags.h"
#include "global.h"
#include "hash-table.h"
#include "package.h"
#include "string.h"
#include "type.h"

#define STR(s) String::fromUtf8CString(heap, s)

using namespace std;
using namespace codeswitch::internal;


TEST(BadPackageNames) {
  TEST_PROLOGUE

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
  TEST_PROLOGUE

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
  TEST_PROLOGUE

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
  TEST_PROLOGUE

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


TEST(PackageDependencyParseNameAndVersion) {
  TEST_PROLOGUE

  Local<PackageName> name;
  Local<PackageVersion> minVersion, maxVersion;
  bool result;

  auto expectedName = PackageName::fromString(heap, STR("foo.bar"));
  result = PackageDependency::parseNameAndVersion(heap, STR("foo.bar"),
                                                  &name, &minVersion, &maxVersion);
  ASSERT_TRUE(result);
  ASSERT_TRUE(expectedName->equals(*name));
  ASSERT_FALSE(minVersion);
  ASSERT_FALSE(maxVersion);

  result = PackageDependency::parseNameAndVersion(heap, STR("foo.bar:"),
                                                  &name, &minVersion, &maxVersion);
  ASSERT_FALSE(result);

  result = PackageDependency::parseNameAndVersion(heap, STR("foo.bar:-"),
                                                  &name, &minVersion, &maxVersion);
  ASSERT_FALSE(result);

  auto expectedMinVersion = PackageVersion::fromString(heap, STR("1.2"));
  result = PackageDependency::parseNameAndVersion(heap, STR("foo.bar:1.2"),
                                                  &name, &minVersion, &maxVersion);
  ASSERT_TRUE(result);
  ASSERT_TRUE(expectedName->equals(*name));
  ASSERT_TRUE(expectedMinVersion->equals(*minVersion));
  ASSERT_TRUE(expectedMinVersion->equals(*maxVersion));

  result = PackageDependency::parseNameAndVersion(heap, STR("foo.bar:1.2-"),
                                                  &name, &minVersion, &maxVersion);
  ASSERT_TRUE(result);
  ASSERT_TRUE(expectedName->equals(*name));
  ASSERT_TRUE(expectedMinVersion->equals(*minVersion));
  ASSERT_FALSE(maxVersion);

  auto expectedMaxVersion = PackageVersion::fromString(heap, STR("3.4"));
  result = PackageDependency::parseNameAndVersion(heap, STR("foo.bar:-3.4"),
                                                  &name, &minVersion, &maxVersion);
  ASSERT_TRUE(result);
  ASSERT_TRUE(expectedName->equals(*name));
  ASSERT_FALSE(minVersion);
  ASSERT_TRUE(expectedMaxVersion->equals(*expectedMaxVersion));

  result = PackageDependency::parseNameAndVersion(heap, STR("foo.bar:1.2-3.4"),
                                                  &name, &minVersion, &maxVersion);
  ASSERT_TRUE(result);
  ASSERT_TRUE(expectedName->equals(*name));
  ASSERT_TRUE(expectedMinVersion->equals(*expectedMinVersion));
  ASSERT_TRUE(expectedMaxVersion->equals(*expectedMaxVersion));

  result = PackageDependency::parseNameAndVersion(heap, STR("foo.bar:3.4-1.2"),
                                                  &name, &minVersion, &maxVersion);
  ASSERT_FALSE(result);
}


static Local<PackageDependency> packageDependencyFromString(Heap* heap,
                                                            const Handle<String>& str) {
  Local<PackageName> name;
  Local<PackageVersion> minVersion, maxVersion;
  bool result = PackageDependency::parseNameAndVersion(heap, str,
                                                       &name, &minVersion, &maxVersion);
  ASSERT_TRUE(result);

  return PackageDependency::create(heap, name, minVersion, maxVersion, 0, 0, 0, 0);
}


TEST(PackageDependencySatisfied) {
  TEST_PROLOGUE

  auto dependency = packageDependencyFromString(heap, STR("foo:1-3"));
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

  dependency = packageDependencyFromString(heap, STR("foo:-3"));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v0));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v3));
  ASSERT_FALSE(dependency->isSatisfiedBy(*name, *v4));

  dependency = packageDependencyFromString(heap, STR("foo:1-"));
  ASSERT_FALSE(dependency->isSatisfiedBy(*name, *v0));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v1));
  ASSERT_TRUE(dependency->isSatisfiedBy(*name, *v4));
}


TEST(PackageExports) {
  TEST_PROLOGUE

  auto package = Package::create(heap);
  auto globals = BlockArray<Global>::create(heap, 2);
  auto fooName = STR("foo");
  auto foo = Global::create(heap, fooName, NO_FLAGS, handle(Type::unitType(roots)));
  globals->set(0, *foo);
  auto barName = STR("bar");
  auto bar = Global::create(heap, barName, PUBLIC_FLAG, handle(Type::unitType(roots)));
  globals->set(1, *bar);
  package->setGlobals(*globals);

  Package::ensureExports(heap, package);
  auto exports = handle(package->exports());
  ASSERT_FALSE(exports->contains(*fooName));
  ASSERT_EQ(*bar, exports->get(*barName));
}


TEST(PackageLink) {
  TEST_PROLOGUE

  auto package = Package::create(heap);

  auto fooName = PackageName::fromString(heap, STR("foo"));
  Local<PackageVersion> nullVersion;
  auto fooDep = PackageDependency::create(heap, fooName, nullVersion, nullVersion, 1, 0, 0, 0);
  ASSERT_EQ(nullptr, fooDep->linkedGlobals());
  auto externBar = Global::create(heap, STR("bar"),
                                  EXTERN_FLAG | PUBLIC_FLAG,
                                  handle(Type::unitType(roots)));
  fooDep->externGlobals()->set(0, *externBar);
  auto deps = BlockArray<PackageDependency>::create(heap, 1);
  deps->set(0, *fooDep);
  package->setDependencies(*deps);

  auto fooPackage = Package::create(heap);
  auto fooGlobals = BlockArray<Global>::create(heap, 1);
  auto bar = Global::create(heap, STR("bar"), PUBLIC_FLAG, handle(Type::unitType(roots)));
  fooGlobals->set(0, *bar);
  fooPackage->setGlobals(*fooGlobals);
  fooDep->setPackage(*fooPackage);

  auto emptyExternTypes = BlockArray<ExternTypeInfo>::create(heap, 0);
  package->setExternTypes(*emptyExternTypes);

  Package::link(heap, package);

  ASSERT_EQ(*bar, fooDep->linkedGlobals()->get(0));
}


TEST(PackageLinkMissingGlobal) {
  TEST_PROLOGUE

  auto package = Package::create(heap);

  auto fooName = PackageName::fromString(heap, STR("foo"));
  Local<PackageVersion> nullVersion;
  auto fooDep = PackageDependency::create(heap, fooName, nullVersion, nullVersion, 1, 0, 0, 0);
  ASSERT_EQ(nullptr, fooDep->linkedGlobals());
  auto externBar = Global::create(heap, STR("bar"),
                                  EXTERN_FLAG | PUBLIC_FLAG,
                                  handle(Type::unitType(roots)));
  fooDep->externGlobals()->set(0, *externBar);
  auto deps = BlockArray<PackageDependency>::create(heap, 1);
  deps->set(0, *fooDep);
  package->setDependencies(*deps);

  auto fooPackage = Package::create(heap);
  fooDep->setPackage(*fooPackage);

  ASSERT_THROWS(Error, Package::link(heap, package));
}

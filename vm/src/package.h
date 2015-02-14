// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef package_h
#define package_h

#include <iostream>
#include <string>
#include "block.h"
#include "handle.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class BlockArray;
class Function;
class Global;
class Heap;
class PackageDependency;
class PackageName;
class PackageVersion;
class String;
class TypeParameter;


class Package: public Block {
 public:
  static const BlockType kBlockType = PACKAGE_BLOCK_TYPE;

  DEFINE_NEW(Package, PACKAGE_BLOCK_TYPE)
  explicit Package(VM* vm);
  static Local<Package> create(Heap* heap);

  static Local<Package> loadFromFile(VM* vm, const std::string& fileName);
  static Local<Package> loadFromBytes(VM* vm, const u8* bytes, word_t size);
  static Local<Package> loadFromStream(VM* vm, std::istream& stream);

  DEFINE_INL_ACCESSORS2(u64, flags, setFlags)
  PackageName* name() const { return name_.get(); }
  void setName(PackageName* newName) { name_.set(this, newName); }
  PackageVersion* version() const { return version_.get(); }
  void setVersion(PackageVersion* newVersion) { version_.set(this, newVersion); }
  BlockArray<PackageDependency>* dependencySpecs() const { return dependencySpecs_.get(); }
  void setDependencySpecs(BlockArray<PackageDependency>* newDependencySpecs) {
    dependencySpecs_.set(this, newDependencySpecs);
  }
  BlockArray<Package>* dependencies() const { return dependencies_.get(); }
  void setDependencies(BlockArray<Package>* newDependencies) {
    dependencies_.set(this, newDependencies);
  }
  BlockArray<String>* strings() const { return strings_.get(); }
  void setStrings(BlockArray<String>* newStrings) { strings_.set(this, newStrings); }
  String* getString(length_t index);
  BlockArray<Global>* globals() const { return globals_.get(); }
  void setGlobals(BlockArray<Global>* newGlobals) { globals_.set(this, newGlobals); }
  Global* getGlobal(length_t index);
  BlockArray<Function>* functions() const { return functions_.get(); }
  void setFunctions(BlockArray<Function>* newFunctions) { functions_.set(this, newFunctions); }
  Function* getFunction(length_t index);
  BlockArray<Class>* classes() const { return classes_.get(); }
  void setClasses(BlockArray<Class>* newClasses) { classes_.set(this, newClasses); }
  Class* getClass(length_t index);
  BlockArray<TypeParameter>* typeParameters() const { return typeParameters_.get(); }
  void setTypeParameters(BlockArray<TypeParameter>* newTypeParameters) {
    typeParameters_.set(this, newTypeParameters);
  }
  TypeParameter* getTypeParameter(length_t index);
  DEFINE_INL_ACCESSORS2(length_t, entryFunctionIndex, setEntryFunctionIndex)
  Function* entryFunction();
  DEFINE_INL_ACCESSORS2(length_t, initFunctionIndex, setInitFunctionIndex)
  Function* initFunction();

 private:
  DECLARE_POINTER_MAP()

  u64 flags_;
  Ptr<PackageName> name_;
  Ptr<PackageVersion> version_;
  Ptr<BlockArray<PackageDependency>> dependencySpecs_;
  Ptr<BlockArray<Package>> dependencies_;
  Ptr<BlockArray<String>> strings_;
  Ptr<BlockArray<Global>> globals_;
  Ptr<BlockArray<Function>> functions_;
  Ptr<BlockArray<Class>> classes_;
  Ptr<BlockArray<TypeParameter>> typeParameters_;
  length_t entryFunctionIndex_;
  length_t initFunctionIndex_;
  // Update PACKAGE_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const Package* pkg);


class PackageName: public Block {
 public:
  static const BlockType kBlockType = PACKAGE_NAME_BLOCK_TYPE;

  static const length_t kMaxComponentLength = 1000;
  static const length_t kMaxComponentCount = 100;

  DEFINE_NEW(PackageName, PACKAGE_NAME_BLOCK_TYPE)
  explicit PackageName(BlockArray<String>* components);
  static Local<PackageName> create(Heap* heap, const Handle<BlockArray<String>>& components);

  static Local<PackageName> fromString(Heap* heap, const Handle<String>& nameString);
  static Local<String> toString(Heap* heap, const Handle<PackageName>& name);

  BlockArray<String>* components() const { return components_.get(); }

  int compare(const PackageName* other) const;
  bool equals(const PackageName* other) const { return compare(other) == 0; }

 private:
  DECLARE_POINTER_MAP()

  Ptr<BlockArray<String>> components_;
  // Update PACKAGE_NAME_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const PackageName* packageName);


class PackageVersion: public Block {
 public:
  static const BlockType kBlockType = PACKAGE_VERSION_BLOCK_TYPE;

  static const length_t kMaxComponent = 999999;
  static const length_t kMaxComponentCount = 100;

  DEFINE_NEW(PackageVersion, PACKAGE_VERSION_BLOCK_TYPE)
  explicit PackageVersion(I32Array* components);
  static Local<PackageVersion> create(Heap* heap, const Handle<I32Array>& components);

  static Local<PackageVersion> fromString(Heap* heap, const Handle<String>& versionString);

  I32Array* components() const { return components_.get(); }

  int compare(const PackageVersion* other) const;
  bool equals(const PackageVersion* other) const { return compare(other) == 0; }

 private:
  DECLARE_POINTER_MAP()

  Ptr<I32Array> components_;
  // Update PACKAGE_VERSION_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const PackageVersion& packageVersion);


class PackageDependency: public Block {
 public:
  static const BlockType kBlockType = PACKAGE_DEPENDENCY_BLOCK_TYPE;

  DEFINE_NEW(PackageDependency, PACKAGE_DEPENDENCY_BLOCK_TYPE)
  PackageDependency(PackageName* name, PackageVersion* minVersion, PackageVersion* maxVersion);
  static Local<PackageDependency> create(Heap* heap,
                                         const Handle<PackageName>& name,
                                         const Handle<PackageVersion>& minVersion,
                                         const Handle<PackageVersion>& maxVersion);

  static Local<PackageDependency> fromString(Heap* heap, const Handle<String>& depString);

  PackageName* name() const { return name_.get(); }
  PackageVersion* minVersion() const { return minVersion_.get(); }
  PackageVersion* maxVersion() const { return maxVersion_.get(); }

  bool equals(const PackageDependency* dep) const;
  bool isSatisfiedBy(const Package* package) const {
    return isSatisfiedBy(package->name(), package->version());
  }
  bool isSatisfiedBy(const PackageName* name, const PackageVersion* version) const;

 private:
  DECLARE_POINTER_MAP()

  Ptr<PackageName> name_;
  Ptr<PackageVersion> minVersion_;
  Ptr<PackageVersion> maxVersion_;
  // Update PACKAGE_DEPENDENCY_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const PackageDependency* dep);

}
}

#endif

// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef package_h
#define package_h

#include <iostream>
#include <string>
#include "block.h"
#include "handle.h"
#include "object.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class BlockArray;
class Function;
class Global;
template <class K, class V>
class BlockHashMap;
class Heap;
class PackageDependency;
class PackageName;
class PackageVersion;
class String;
class TypeParameter;

typedef BlockHashMap<String, Block> ExportMap;

class Package: public Object {
 public:
  static const BlockType kBlockType = PACKAGE_BLOCK_TYPE;

  DEFINE_NEW(Package)
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
  BlockArray<PackageDependency>* dependencies() const { return dependencies_.get(); }
  void setDependencies(BlockArray<PackageDependency>* newDependencies) {
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

  ExportMap* exports() const { return exports_.get(); }
  void setExports(ExportMap* exports) { exports_.set(this, exports); }
  static void ensureExports(Heap* heap, const Handle<Package>& package);

  static void link(Heap* heap, const Handle<Package>& package);

 private:
  DECLARE_POINTER_MAP()

  u64 flags_;
  Ptr<PackageName> name_;
  Ptr<PackageVersion> version_;
  Ptr<BlockArray<PackageDependency>> dependencies_;
  Ptr<BlockArray<String>> strings_;
  Ptr<BlockArray<Global>> globals_;
  Ptr<BlockArray<Function>> functions_;
  Ptr<BlockArray<Class>> classes_;
  Ptr<BlockArray<TypeParameter>> typeParameters_;
  length_t entryFunctionIndex_;
  length_t initFunctionIndex_;
  Ptr<ExportMap> exports_;
  // Update PACKAGE_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const Package* pkg);


class PackageName: public Block {
 public:
  static const BlockType kBlockType = PACKAGE_NAME_BLOCK_TYPE;

  static const length_t kMaxComponentLength = 1000;
  static const length_t kMaxComponentCount = 100;

  DEFINE_NEW(PackageName)
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

  DEFINE_NEW(PackageVersion)
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

  DEFINE_NEW(PackageDependency)
  PackageDependency(PackageName* name,
                    PackageVersion* minVersion,
                    PackageVersion* maxVersion,
                    BlockArray<Global>* externGlobals,
                    BlockArray<Global>* linkedGlobals,
                    BlockArray<Function>* externFunctions,
                    BlockArray<Function>* linkedFunctions,
                    BlockArray<Class>* externClasses,
                    BlockArray<Class>* linkedClasses);
  static Local<PackageDependency> create(Heap* heap,
                                         const Handle<PackageName>& name,
                                         const Handle<PackageVersion>& minVersion,
                                         const Handle<PackageVersion>& maxVersion,
                                         const Handle<BlockArray<Global>>& externGlobals,
                                         const Handle<BlockArray<Global>>& linkedGlobals,
                                         const Handle<BlockArray<Function>>& externFunctions,
                                         const Handle<BlockArray<Function>>& linkedFunctions,
                                         const Handle<BlockArray<Class>>& externClasses,
                                         const Handle<BlockArray<Class>>& linkedClasses);
  static Local<PackageDependency> create(Heap* heap,
                                         const Handle<PackageName>& name,
                                         const Handle<PackageVersion>& minVersion,
                                         const Handle<PackageVersion>& maxVersion,
                                         length_t globalCount,
                                         length_t functionCount,
                                         length_t classCount);

  static bool parseNameAndVersion(Heap* heap,
                                  const Handle<String>& depString,
                                  Local<PackageName>* outName,
                                  Local<PackageVersion>* outMinVersion,
                                  Local<PackageVersion>* outMaxVersion);

  PackageName* name() const { return name_.get(); }
  PackageVersion* minVersion() const { return minVersion_.get(); }
  PackageVersion* maxVersion() const { return maxVersion_.get(); }
  Package* package() const { return package_.get(); }
  void setPackage(Package* package) { package_.set(this, package); }
  BlockArray<Global>* externGlobals() const { return externGlobals_.get(); }
  BlockArray<Global>* linkedGlobals() const { return linkedGlobals_.get(); }
  BlockArray<Function>* externFunctions() const { return externFunctions_.get(); }
  BlockArray<Function>* linkedFunctions() const { return linkedFunctions_.get(); }
  BlockArray<Class>* externClasses() const { return externClasses_.get(); }
  BlockArray<Class>* linkedClasses() const { return linkedClasses_.get(); }

  bool isSatisfiedBy(const Package* package) const {
    return isSatisfiedBy(package->name(), package->version());
  }
  bool isSatisfiedBy(const PackageName* name, const PackageVersion* version) const;

 private:
  DECLARE_POINTER_MAP()

  Ptr<PackageName> name_;
  Ptr<PackageVersion> minVersion_;
  Ptr<PackageVersion> maxVersion_;
  Ptr<Package> package_;
  Ptr<BlockArray<Global>> externGlobals_;
  Ptr<BlockArray<Global>> linkedGlobals_;
  Ptr<BlockArray<Function>> externFunctions_;
  Ptr<BlockArray<Function>> linkedFunctions_;
  Ptr<BlockArray<Class>> externClasses_;
  Ptr<BlockArray<Class>> linkedClasses_;
  // Update PACKAGE_DEPENDENCY_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const PackageDependency* dep);

}
}

#endif

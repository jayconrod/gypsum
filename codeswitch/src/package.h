// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef package_h
#define package_h

#include <iostream>
#include <string>
#include "block.h"
#include "handle.h"
#include "object.h"
#include "platform.h"
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
class Name;
class PackageDependency;
class PackageVersion;
class String;
class Trait;

typedef BlockHashMap<Name, Block> ExportMap;

class Package: public Object {
 public:
  static const BlockType kBlockType = PACKAGE_BLOCK_TYPE;

  DEFINE_NEW(Package)
  Package(VM* vm, id_t id);
  static Local<Package> create(Heap* heap, id_t id);

  static std::vector<Persistent<Package>> load(
      VM* vm,
      Counter* idCounter,
      const std::string& fileName,
      const std::vector<NativeFunctionSearch>& nativeFunctionSearchOrder
          = std::vector<NativeFunctionSearch>());
  static std::vector<Persistent<Package>> load(
      VM* vm,
      Counter* idCounter,
      std::istream& stream,
      const std::string& dirName,
      const std::vector<NativeFunctionSearch>& nativeFunctionSearchOrder
          = std::vector<NativeFunctionSearch>());

  id_t id() const { return id_; }
  DEFINE_INL_ACCESSORS2(u64, flags, setFlags)
  Name* name() const { return name_.get(); }
  void setName(Name* newName) { name_.set(this, newName); }
  PackageVersion* version() const { return version_.get(); }
  void setVersion(PackageVersion* newVersion) { version_.set(this, newVersion); }
  BlockArray<PackageDependency>* dependencies() const { return dependencies_.get(); }
  void setDependencies(BlockArray<PackageDependency>* newDependencies) {
    dependencies_.set(this, newDependencies);
  }
  BlockArray<String>* strings() const { return strings_.get(); }
  void setStrings(BlockArray<String>* newStrings) { strings_.set(this, newStrings); }
  String* getString(length_t index) const;
  BlockArray<Name>* names() const { return names_.get(); }
  void setNames(BlockArray<Name>* newNames) { names_.set(this, newNames); }
  Name* getName(length_t index) const;
  BlockArray<Global>* globals() const { return globals_.get(); }
  void setGlobals(BlockArray<Global>* newGlobals) { globals_.set(this, newGlobals); }
  Global* getGlobal(length_t index) const;
  BlockArray<Function>* functions() const { return functions_.get(); }
  void setFunctions(BlockArray<Function>* newFunctions) { functions_.set(this, newFunctions); }
  Function* getFunction(length_t index) const;
  BlockArray<Class>* classes() const { return classes_.get(); }
  void setClasses(BlockArray<Class>* newClasses) { classes_.set(this, newClasses); }
  Class* getClass(length_t index) const;
  BlockArray<Trait>* traits() const { return traits_.get(); }
  void setTraits(BlockArray<Trait>* newTraits) { traits_.set(this, newTraits); }
  Trait* getTrait(length_t index) const;
  DEFINE_INL_ACCESSORS2(length_t, entryFunctionIndex, setEntryFunctionIndex)
  Function* entryFunction() const;
  DEFINE_INL_ACCESSORS2(length_t, initFunctionIndex, setInitFunctionIndex)
  Function* initFunction() const;

  ExportMap* exports() const { return exports_.get(); }
  void setExports(ExportMap* exports) { exports_.set(this, exports); }
  static void ensureExports(const Handle<Package>& package);

  BlockHashMap<Name, Global>* globalNameIndex() const { return globalNameIndex_.get(); }
  void setGlobalNameIndex(BlockHashMap<Name, Global>* index) {
    globalNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<Name, Global>> ensureAndGetGlobalNameIndex(
      const Handle<Package>& package);
  BlockHashMap<String, Global>* globalSourceNameIndex() const {
    return globalSourceNameIndex_.get();
  }
  void setGlobalSourceNameIndex(BlockHashMap<String, Global>* index) {
    globalSourceNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<String, Global>> ensureAndGetGlobalSourceNameIndex(
      const Handle<Package>& package);

  BlockHashMap<Name, Function>* functionNameIndex() const { return functionNameIndex_.get(); }
  void setFunctionNameIndex(BlockHashMap<Name, Function>* index) {
    functionNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<Name, Function>> ensureAndGetFunctionNameIndex(
      const Handle<Package>& package);
  BlockHashMap<String, Function>* functionSourceNameIndex() const {
    return functionSourceNameIndex_.get();
  }
  void setFunctionSourceNameIndex(BlockHashMap<String, Function>* index) {
    functionSourceNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<String, Function>> ensureAndGetFunctionSourceNameIndex(
      const Handle<Package>& package);

  BlockHashMap<Name, Class>* classNameIndex() const { return classNameIndex_.get(); }
  void setClassNameIndex(BlockHashMap<Name, Class>* index) {
    classNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<Name, Class>> ensureAndGetClassNameIndex(
      const Handle<Package>& package);
  BlockHashMap<String, Class>* classSourceNameIndex() const {
    return classSourceNameIndex_.get();
  }
  void setClassSourceNameIndex(BlockHashMap<String, Class>* index) {
    classSourceNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<String, Class>> ensureAndGetClassSourceNameIndex(
      const Handle<Package>& package);

  BlockHashMap<Name, Trait>* traitNameIndex() const { return traitNameIndex_.get(); }
  void setTraitNameIndex(BlockHashMap<Name, Trait>* index) {
    traitNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<Name, Trait>> ensureAndGetTraitNameIndex(
      const Handle<Package>& package);
  BlockHashMap<String, Trait>* traitSourceNameIndex() const {
    return traitSourceNameIndex_.get();
  }
  void setTraitSourceNameIndex(BlockHashMap<String, Trait>* index) {
    traitSourceNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<String, Trait>> ensureAndGetTraitSourceNameIndex(
      const Handle<Package>& package);

  NativeLibrary nativeLibrary() const { return nativeLibrary_; }
  void setNativeLibrary(NativeLibrary nativeLibrary) { nativeLibrary_ = nativeLibrary; }
  u32 encodedNativeFunctionSearchOrder() const { return encodedNativeFunctionSearchOrder_; }
  void setEncodedNativeFunctionSearchOrder(u32 encodedNativeFunctionSearchOrder) {
    encodedNativeFunctionSearchOrder_ = encodedNativeFunctionSearchOrder;
  }
  std::vector<NativeFunctionSearch> nativeFunctionSearchOrder() const {
    return decodeNativeFunctionSearchOrder(encodedNativeFunctionSearchOrder());
  }
  void setNativeFunctionSearchOrder(
      const std::vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
    setEncodedNativeFunctionSearchOrder(
        encodeNativeFunctionSearchOrder(nativeFunctionSearchOrder));
  }
  static std::vector<NativeFunctionSearch> decodeNativeFunctionSearchOrder(
      u32 encodedNativeFunctionSearchOrder);
  static u32 encodeNativeFunctionSearchOrder(
      const std::vector<NativeFunctionSearch>& nativeFunctionSearchOrder);
  bool mayLoadFunctionsFromNativeLibrary() const;
  NativeFunction loadNativeFunction(Name* functionName);

  static void link(const Handle<Package>& package);

 private:
  DECLARE_POINTER_MAP()

  id_t id_;
  u64 flags_;
  Ptr<Name> name_;
  Ptr<PackageVersion> version_;
  Ptr<BlockArray<PackageDependency>> dependencies_;
  Ptr<BlockArray<String>> strings_;
  Ptr<BlockArray<Name>> names_;
  Ptr<BlockArray<Global>> globals_;
  Ptr<BlockArray<Function>> functions_;
  Ptr<BlockArray<Class>> classes_;
  Ptr<BlockArray<Trait>> traits_;
  length_t entryFunctionIndex_;
  length_t initFunctionIndex_;
  Ptr<ExportMap> exports_;
  Ptr<BlockHashMap<Name, Global>> globalNameIndex_;
  Ptr<BlockHashMap<String, Global>> globalSourceNameIndex_;
  Ptr<BlockHashMap<Name, Function>> functionNameIndex_;
  Ptr<BlockHashMap<String, Function>> functionSourceNameIndex_;
  Ptr<BlockHashMap<Name, Class>> classNameIndex_;
  Ptr<BlockHashMap<String, Class>> classSourceNameIndex_;
  Ptr<BlockHashMap<Name, Trait>> traitNameIndex_;
  Ptr<BlockHashMap<String, Trait>> traitSourceNameIndex_;
  NativeLibrary nativeLibrary_;
  u32 encodedNativeFunctionSearchOrder_;
  // Update PACKAGE_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const Package* pkg);


class PackageVersion: public Block {
 public:
  static const BlockType kBlockType = PACKAGE_VERSION_BLOCK_TYPE;

  static const length_t kMaxComponent = 999999;
  static const length_t kMaxComponentCount = 100;

  DEFINE_NEW(PackageVersion)
  explicit PackageVersion(I32Array* components);
  static Local<PackageVersion> create(Heap* heap, const Handle<I32Array>& components);

  static Local<PackageVersion> fromString(Heap* heap, const Handle<String>& versionString);
  std::string toStlString();

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
  PackageDependency(Name* name,
                    PackageVersion* minVersion,
                    PackageVersion* maxVersion,
                    BlockArray<Global>* externGlobals,
                    BlockArray<Global>* linkedGlobals,
                    BlockArray<Function>* externFunctions,
                    BlockArray<Function>* linkedFunctions,
                    BlockArray<Class>* externClasses,
                    BlockArray<Class>* linkedClasses,
                    BlockArray<Trait>* externTraits,
                    BlockArray<Trait>* linkedTraits,
                    BlockArray<Function>* externMethods);
  static Local<PackageDependency> create(
      Heap* heap,
      const Handle<Name>& name,
      const Handle<PackageVersion>& minVersion,
      const Handle<PackageVersion>& maxVersion,
      const Handle<BlockArray<Global>>& externGlobals,
      const Handle<BlockArray<Global>>& linkedGlobals,
      const Handle<BlockArray<Function>>& externFunctions,
      const Handle<BlockArray<Function>>& linkedFunctions,
      const Handle<BlockArray<Class>>& externClasses,
      const Handle<BlockArray<Class>>& linkedClasses,
      const Handle<BlockArray<Trait>>& externTraits,
      const Handle<BlockArray<Trait>>& linkedTraits,
      const Handle<BlockArray<Function>>& externMethods);
  static Local<PackageDependency> create(Heap* heap,
                                         const Handle<Name>& name,
                                         const Handle<PackageVersion>& minVersion,
                                         const Handle<PackageVersion>& maxVersion,
                                         length_t globalCount,
                                         length_t functionCount,
                                         length_t classCount,
                                         length_t traitCount,
                                         length_t methodCount);

  static bool parseNameAndVersion(Heap* heap,
                                  const Handle<String>& depString,
                                  Local<Name>* outName,
                                  Local<PackageVersion>* outMinVersion,
                                  Local<PackageVersion>* outMaxVersion);

  Name* name() const { return name_.get(); }
  PackageVersion* minVersion() const { return minVersion_.get(); }
  PackageVersion* maxVersion() const { return maxVersion_.get(); }
  Package* package() const { return package_.get(); }
  void setPackage(Package* package) { package_.set(this, package); }
  BlockArray<Global>* externGlobals() const { return externGlobals_.get(); }
  BlockArray<Global>* linkedGlobals() const { return linkedGlobals_.get(); }
  void setLinkedGlobals(BlockArray<Global>* linkedGlobals);
  BlockArray<Function>* externFunctions() const { return externFunctions_.get(); }
  BlockArray<Function>* linkedFunctions() const { return linkedFunctions_.get(); }
  void setLinkedFunctions(BlockArray<Function>* linkedFunctions);
  BlockArray<Class>* externClasses() const { return externClasses_.get(); }
  BlockArray<Class>* linkedClasses() const { return linkedClasses_.get(); }
  void setLinkedClasses(BlockArray<Class>* linkedClasses);
  BlockArray<Trait>* externTraits() const { return externTraits_.get(); }
  BlockArray<Trait>* linkedTraits() const { return linkedTraits_.get(); }
  void setLinkedTraits(BlockArray<Trait>* linkedTraits);
  BlockArray<Function>* externMethods() const { return externMethods_.get(); }

  bool isSatisfiedBy(const Package* package) const {
    return isSatisfiedBy(package->name(), package->version());
  }
  bool isSatisfiedBy(const Name* name, const PackageVersion* version) const;

 private:
  DECLARE_POINTER_MAP()

  Ptr<Name> name_;
  Ptr<PackageVersion> minVersion_;
  Ptr<PackageVersion> maxVersion_;
  Ptr<Package> package_;
  Ptr<BlockArray<Global>> externGlobals_;
  Ptr<BlockArray<Global>> linkedGlobals_;
  Ptr<BlockArray<Function>> externFunctions_;
  Ptr<BlockArray<Function>> linkedFunctions_;
  Ptr<BlockArray<Class>> externClasses_;
  Ptr<BlockArray<Class>> linkedClasses_;
  Ptr<BlockArray<Trait>> externTraits_;
  Ptr<BlockArray<Trait>> linkedTraits_;
  Ptr<BlockArray<Function>> externMethods_;
  // Update PACKAGE_DEPENDENCY_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const PackageDependency* dep);

}
}

#endif

// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "package.h"

#include <algorithm>
#include <fstream>
#include <memory>
#include <string>
#include <sstream>
#include <vector>
#include "array.h"
#include "block.h"
#include "builtins.h"
#include "bytecode.h"
#include "error.h"
#include "field.h"
#include "flags.h"
#include "function.h"
#include "global.h"
#include "handle.h"
#include "hash-table.h"
#include "heap.h"
#include "name.h"
#include "roots.h"
#include "string.h"
#include "type.h"
#include "type-parameter.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define PACKAGE_POINTER_LIST(F) \
  F(Package, name_)             \
  F(Package, version_)          \
  F(Package, dependencies_)     \
  F(Package, strings_)          \
  F(Package, globals_)          \
  F(Package, functions_)        \
  F(Package, classes_)          \
  F(Package, typeParameters_)   \
  F(Package, exports_)          \
  F(Package, externTypes_)      \

DEFINE_POINTER_MAP(Package, PACKAGE_POINTER_LIST)

#undef PACKAGE_POINTER_LIST


class DependencyLoader;

class Loader {
 public:
  Loader(VM* vm, istream& stream)
      : vm_(vm), stream_(stream) {}
  virtual ~Loader() {}

 protected:
  VM* vm() const { return vm_; }
  Heap* heap() const { return vm_->heap(); }
  Roots* roots() const { return vm_->roots(); }
  Local<Package> package() const { return package_; }

  Local<String> readString();
  Local<Global> readGlobal();
  Local<Function> readFunction();
  void readFunctionHeader(Local<Name>* name,
                          u32* flags,
                          Local<BlockArray<TypeParameter>>* typeParameters,
                          Local<Type>* returnType,
                          Local<BlockArray<Type>>* parameterTypes,
                          Local<Class>* definingClass);
  void readClass(const Local<Class>& clas);
  Local<Field> readField();
  void readTypeParameter(const Local<TypeParameter>& typeParam);
  Local<Type> readType();

  template <typename T>
  T readValue();
  Local<Name> readName();
  length_t readLength();
  vector<u8> readData(word_t size);
  DefnId readDefnIdVbns();
  i64 readVbn();
  word_t readWordVbn();
  length_t readLengthVbn();
  id_t readIdVbn();

  DependencyLoader createDependencyLoader(const Handle<PackageDependency>& dep);

  virtual Local<TypeParameter> getTypeParameter(length_t index) const = 0;
  virtual Local<Function> readIdAndGetMethod() = 0;
  virtual Local<Class> readIdAndGetDefiningClass() = 0;

  Local<Package> package_;
  vector<Local<ExternTypeInfo>> externTypes_;

 private:
  VM* vm_;
  istream& stream_;
};


class PackageLoader: public Loader {
 public:
  PackageLoader(VM* vm, istream& stream)
      : Loader(vm, stream) { }

  Local<Package> load();

 protected:
  virtual Local<TypeParameter> getTypeParameter(length_t index) const;
  virtual Local<Function> readIdAndGetMethod();
  virtual Local<Class> readIdAndGetDefiningClass();

 private:
  static const u32 kMagic = 0x676b7073;

  Local<PackageDependency> readDependencyHeader();
};


class DependencyLoader: public Loader {
 public:
  DependencyLoader(VM* vm,
                   istream& stream,
                   const Handle<Package>& package,
                   const Handle<PackageDependency>& dep)
      : Loader(vm, stream),
        dep_(dep) {
    package_ = package;
  }

 public:
  void readDependencyBody();

 protected:
  void readExternFunction(const Handle<Function>& func);

  virtual Local<TypeParameter> getTypeParameter(length_t index) const;
  virtual Local<Function> readIdAndGetMethod();
  virtual Local<Class> readIdAndGetDefiningClass();

 private:
  Local<PackageDependency> dep_;
};


Package::Package(VM* vm)
    : Object(PACKAGE_BLOCK_TYPE),
      flags_(0),
      dependencies_(this, reinterpret_cast<BlockArray<PackageDependency>*>(
          vm->roots()->emptyBlockArray())),
      strings_(this, reinterpret_cast<BlockArray<String>*>(vm->roots()->emptyBlockArray())),
      globals_(this, reinterpret_cast<BlockArray<Global>*>(vm->roots()->emptyBlockArray())),
      functions_(this, reinterpret_cast<BlockArray<Function>*>(vm->roots()->emptyBlockArray())),
      classes_(this, reinterpret_cast<BlockArray<Class>*>(vm->roots()->emptyBlockArray())),
      typeParameters_(this,
                      reinterpret_cast<BlockArray<TypeParameter>*>(
                          vm->roots()->emptyBlockArray())),
      entryFunctionIndex_(kIndexNotSet),
      initFunctionIndex_(kIndexNotSet) { }


Local<Package> Package::create(Heap* heap) {
  RETRY_WITH_GC(heap, return Local<Package>(new(heap) Package(heap->vm())));
}


Local<Package> Package::loadFromFile(VM* vm, const string& fileName) {
  ifstream file(fileName.c_str(), ios::binary);
  if (!file.good())
    throw Error("could not open package file");
  file.exceptions(ios::failbit | ios::badbit | ios::eofbit);
  auto package = loadFromStream(vm, file);
  auto pos = file.tellg();
  file.seekg(0, ios::end);
  if (pos != file.tellg())
    throw Error("garbage found at end of file");
  return package;
}


Local<Package> Package::loadFromBytes(VM* vm, const u8* bytes, word_t size) {
  stringstream stream(string(reinterpret_cast<const char*>(bytes), size));
  stream.exceptions(ios::failbit | ios::badbit | ios::eofbit);
  auto package = loadFromStream(vm, stream);
  auto pos = stream.tellg();
  stream.seekg(0, ios::end);
  if (pos != stream.tellg())
    throw Error("garbage found at end of bytes");
  return package;
}



Local<Package> Package::loadFromStream(VM* vm, istream& stream) {
  PackageLoader loader(vm, stream);
  return loader.load();
}


String* Package::getString(length_t index) const {
  return block_cast<String>(strings()->get(index));
}


Global* Package::getGlobal(length_t index) const {
  return block_cast<Global>(globals()->get(index));
}


Function* Package::getFunction(length_t index) const {
  return block_cast<Function>(functions()->get(index));
}


Function* Package::getFunction(DefnId id) const {
  if (id.packageId == kBuiltinPackageId) {
    auto builtinId = indexToBuiltinId(id.defnIndex);
    return getVM()->roots()->getBuiltinFunction(builtinId);
  } else if (id.packageId == kLocalPackageId) {
    return getFunction(static_cast<length_t>(id.defnIndex));
  } else {
    return dependencies()->get(static_cast<length_t>(id.packageId))
        ->linkedFunctions()->get(static_cast<length_t>(id.defnIndex));
  }
}


Class* Package::getClass(length_t index) const {
  return block_cast<Class>(classes()->get(index));
}


TypeParameter* Package::getTypeParameter(length_t index) const {
  return block_cast<TypeParameter>(typeParameters()->get(index));
}


Function* Package::entryFunction() const {
  auto index = entryFunctionIndex();
  if (index == kLengthNotSet)
    return nullptr;
  return getFunction(index);
}


Function* Package::initFunction() const {
  auto index = initFunctionIndex();
  if (index == kLengthNotSet)
    return nullptr;
  return getFunction(index);
}


void Package::ensureExports(Heap* heap, const Handle<Package>& package) {
  if (package->exports_)
    return;

  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(heap->vm());
  auto exports = ExportMap::create(heap);

  auto globals = handle(package->globals());
  for (length_t i = 0; i < globals->length(); i++) {
    auto global = handle(globals->get(i));
    if ((global->flags() & PUBLIC_FLAG) != 0) {
      auto name = handle(global->name());
      ASSERT(!exports->contains(*name));
      ExportMap::add(heap, exports, name, global);
    }
  }

  auto functions = handle(package->functions());
  for (length_t i = 0; i < functions->length(); i++) {
    auto function = handle(functions->get(i));
    if ((function->flags() & PUBLIC_FLAG) != 0 &&
        ((function->flags() & METHOD_FLAG) == 0 ||
         (function->flags() & (STATIC_FLAG | METHOD_FLAG)) != 0)) {
      auto name = handle(function->name());
      ASSERT(!exports->contains(*name));
      ExportMap::add(heap, exports, name, function);
    }
  }

  auto classes = handle(package->classes());
  for (length_t i = 0; i < classes->length(); i++) {
    auto clas = handle(classes->get(i));
    if ((clas->flags() & PUBLIC_FLAG) != 0) {
      auto name = handle(clas->name());
      ASSERT(!exports->contains(*name));
      ExportMap::add(heap, exports, name, clas);
    }
  }

  auto typeParameters = handle(package->typeParameters());
  for (length_t i = 0; i < typeParameters->length(); i++) {
    auto typeParameter = handle(typeParameters->get(i));
    if ((typeParameter->flags() & PUBLIC_FLAG) != 0) {
      auto name = handle(typeParameter->name());
      ASSERT(!exports->contains(*name));
      ExportMap::add(heap, exports, name, typeParameter);
    }
  }

  package->setExports(*exports);
}


void Package::link(Heap* heap, const Handle<Package>& package) {
  auto vm = heap->vm();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(vm);
  auto dependencies = handle(package->dependencies());
  for (length_t i = 0; i < dependencies->length(); i++) {
    auto dependency = handle(dependencies->get(i));
    auto depPackage = handle(dependency->package());
    ensureExports(heap, depPackage);
    auto depExports = handle(depPackage->exports());

    ASSERT(dependency->linkedGlobals() == nullptr);
    auto externGlobals = handle(dependency->externGlobals());
    auto globalCount = externGlobals->length();
    auto linkedGlobals = BlockArray<Global>::create(heap, globalCount);
    {
      AllowAllocationScope noAllocation(heap, false);
      for (length_t j = 0; j < globalCount; j++) {
        auto externGlobal = externGlobals->get(j);
        auto name = externGlobal->name();
        auto linkedGlobal = depExports->getOrElse(name, nullptr);
        if (!linkedGlobal || !isa<Global>(linkedGlobal)) {
          throw Error("link error");
        }
        linkedGlobals->set(j, block_cast<Global>(linkedGlobal));
      }
    }
    dependency->setLinkedGlobals(*linkedGlobals);

    ASSERT(dependency->linkedFunctions() == nullptr);
    auto externFunctions = handle(dependency->externFunctions());
    auto functionCount = externFunctions->length();
    auto linkedFunctions = BlockArray<Function>::create(heap, functionCount);
    {
      AllowAllocationScope noAllocation(heap, false);
      for (length_t j = 0; j < functionCount; j++) {
        auto externFunction = externFunctions->get(j);
        auto name = externFunction->name();
        auto linkedFunction = depExports->getOrElse(name, nullptr);
        if (!linkedFunction || !isa<Function>(linkedFunction)) {
          throw Error("link error");
        }
        linkedFunctions->set(j, block_cast<Function>(linkedFunction));
      }
    }
    dependency->setLinkedFunctions(*linkedFunctions);

    ASSERT(dependency->linkedClasses() == nullptr);
    auto externClasses = handle(dependency->externClasses());
    auto classCount = externClasses->length();
    auto linkedClasses = BlockArray<Class>::create(heap, classCount);
    {
      AllowAllocationScope noAllocation(heap, false);
      for (length_t j = 0; j < classCount; j++) {
        auto externClass = externClasses->get(j);
        auto name = externClass->name();
        auto linkedClass = depExports->getOrElse(name, nullptr);
        if (!linkedClass || !isa<Class>(linkedClass)) {
          throw Error("link error");
        }
        linkedClasses->set(j, block_cast<Class>(linkedClass));
      }
    }
    dependency->setLinkedClasses(*linkedClasses);

    ASSERT(dependency->linkedTypeParameters() == nullptr);
    auto externTypeParameters = handle(dependency->externTypeParameters());
    auto typeParameterCount = externTypeParameters->length();
    auto linkedTypeParameters = BlockArray<TypeParameter>::create(heap, typeParameterCount);
    {
      AllowAllocationScope noAllocation(heap, false);
      for (length_t j = 0; j < typeParameterCount; j++) {
        auto externTypeParameter = externTypeParameters->get(j);
        auto name = externTypeParameter->name();
        auto linkedTypeParameter = depExports->getOrElse(name, nullptr);
        if (!linkedTypeParameter || !isa<TypeParameter>(linkedTypeParameter)) {
          throw Error("link error");
        }
        linkedTypeParameters->set(j, block_cast<TypeParameter>(linkedTypeParameter));
      }
    }
    dependency->setLinkedTypeParameters(*linkedTypeParameters);
  }

  auto externTypes = handle(package->externTypes());
  for (length_t i = 0; i < externTypes->length(); i++) {
    externTypes->get(i)->linkType();
  }
  package->setExternTypes(nullptr);
}


ostream& operator << (ostream& os, const Package* pkg) {
  os << brief(pkg)
     << "\n  name: " << brief(pkg->name())
     << "\n  version: " << brief(pkg->version())
     << "\n  dependencies: " << brief(pkg->dependencies())
     << "\n  strings: " << brief(pkg->strings())
     << "\n  functions: " << brief(pkg->functions())
     << "\n  globals: " << brief(pkg->globals())
     << "\n  classes: " << brief(pkg->classes())
     << "\n  type parameters: " << brief(pkg->typeParameters())
     << "\n  entry function index: " << pkg->entryFunctionIndex()
     << "\n  init function index: " << pkg->initFunctionIndex()
     << "\n  exports: " << brief(pkg->exports())
     << "\n  externTypes: " << brief(pkg->externTypes());
  return os;
}


#define PACKAGE_VERSION_POINTER_LIST(F) \
  F(PackageVersion, components_)        \

DEFINE_POINTER_MAP(PackageVersion, PACKAGE_VERSION_POINTER_LIST)

#undef PACKAGE_VERSION_POINTER_LIST


PackageVersion::PackageVersion(I32Array* components)
    : Block(PACKAGE_VERSION_BLOCK_TYPE),
      components_(this, components) {
  ASSERT(components->length() <= kMaxComponentCount);
  #ifdef DEBUG
  for (auto component : *components) {
    ASSERT(inRange<i32>(component, 0, kMaxComponent));
  }
  #endif
}


Local<PackageVersion> PackageVersion::create(Heap* heap, const Handle<I32Array>& components) {
  RETRY_WITH_GC(heap, return Local<PackageVersion>(new(heap) PackageVersion(*components)));
}


Local<PackageVersion> PackageVersion::fromString(Heap* heap,
                                                 const Handle<String>& versionString) {
  auto componentStrings = String::split(heap, versionString, '.');
  if (componentStrings->length() == 0 || componentStrings->length() > kMaxComponentCount)
    return Local<PackageVersion>();

  auto components = I32Array::create(heap, componentStrings->length());
  for (length_t i = 0; i < componentStrings->length(); i++) {
    auto componentString = handle(componentStrings->get(i));
    i32 component;
    if (!componentString->tryToI32(&component) ||
        !inRange<i32>(component, 0, kMaxComponent)) {
      return Local<PackageVersion>();
    }
    components->set(i, component);
  }

  return create(heap, components);
}


int PackageVersion::compare(const PackageVersion* other) const {
  auto len = min(components()->length(), other->components()->length());
  for (length_t i = 0; i < len; i++) {
    int cmp = static_cast<int>(components()->get(i) - other->components()->get(i));
    if (cmp != 0)
      return cmp;
  }
  return static_cast<int>(components()->length()) -
         static_cast<int>(other->components()->length());
}


ostream& operator << (ostream& os, const PackageVersion* version) {
  os << brief(version) << "\n  ";
  auto components = version->components();
  auto length = components->length();
  for (length_t i = 0; i < length - 1; i++) {
    os << components->get(i) << '.';
  }
  os << components->get(length - 1);
  return os;
}


#define PACKAGE_DEPENDENCY_POINTER_LIST(F)    \
  F(PackageDependency, name_)                 \
  F(PackageDependency, minVersion_)           \
  F(PackageDependency, maxVersion_)           \
  F(PackageDependency, package_)              \
  F(PackageDependency, externGlobals_)        \
  F(PackageDependency, linkedGlobals_)        \
  F(PackageDependency, externFunctions_)      \
  F(PackageDependency, linkedFunctions_)      \
  F(PackageDependency, externClasses_)        \
  F(PackageDependency, linkedClasses_)        \
  F(PackageDependency, externTypeParameters_) \
  F(PackageDependency, linkedTypeParameters_) \
  F(PackageDependency, externMethods_)        \

DEFINE_POINTER_MAP(PackageDependency, PACKAGE_DEPENDENCY_POINTER_LIST)

#undef PACKAGE_DEPENDENCY_POINTER_LIST


PackageDependency::PackageDependency(Name* name,
                                     PackageVersion* minVersion,
                                     PackageVersion* maxVersion,
                                     BlockArray<Global>* externGlobals,
                                     BlockArray<Global>* linkedGlobals,
                                     BlockArray<Function>* externFunctions,
                                     BlockArray<Function>* linkedFunctions,
                                     BlockArray<Class>* externClasses,
                                     BlockArray<Class>* linkedClasses,
                                     BlockArray<TypeParameter>* externTypeParameters,
                                     BlockArray<TypeParameter>* linkedTypeParameters,
                                     BlockArray<Function>* externMethods)
    : Block(PACKAGE_DEPENDENCY_BLOCK_TYPE),
      name_(this, name),
      minVersion_(this, minVersion),
      maxVersion_(this, maxVersion),
      externGlobals_(this, externGlobals),
      linkedGlobals_(this, linkedGlobals),
      externFunctions_(this, externFunctions),
      linkedFunctions_(this, linkedFunctions),
      externClasses_(this, externClasses),
      linkedClasses_(this, linkedClasses),
      externTypeParameters_(this, externTypeParameters),
      linkedTypeParameters_(this, linkedTypeParameters),
      externMethods_(this, externMethods) {
  ASSERT(minVersion == nullptr || maxVersion == nullptr ||
         minVersion->compare(maxVersion) <= 0);
  ASSERT(linkedGlobals == nullptr || externGlobals->length() == linkedGlobals->length());
  ASSERT(linkedFunctions == nullptr || externFunctions->length() == linkedFunctions->length());
  ASSERT(linkedClasses == nullptr || externClasses->length() == linkedClasses->length());
  ASSERT(linkedTypeParameters == nullptr ||
         externTypeParameters->length() == linkedTypeParameters->length());
}


Local<PackageDependency> PackageDependency::create(
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
    const Handle<BlockArray<TypeParameter>>& externTypeParameters,
    const Handle<BlockArray<TypeParameter>>& linkedTypeParameters,
    const Handle<BlockArray<Function>>& externMethods) {
  RETRY_WITH_GC(heap, return Local<PackageDependency>(new(heap) PackageDependency(
      *name, minVersion.getOrNull(), maxVersion.getOrNull(),
      *externGlobals, linkedGlobals.getOrNull(),
      *externFunctions, linkedFunctions.getOrNull(),
      *externClasses, linkedClasses.getOrNull(),
      *externTypeParameters, linkedTypeParameters.getOrNull(),
      *externMethods)));
}


Local<PackageDependency> PackageDependency::create(Heap* heap,
                                                   const Handle<Name>& name,
                                                   const Handle<PackageVersion>& minVersion,
                                                   const Handle<PackageVersion>& maxVersion,
                                                   length_t globalCount,
                                                   length_t functionCount,
                                                   length_t classCount,
                                                   length_t typeParameterCount,
                                                   length_t methodCount) {
  auto externGlobals = BlockArray<Global>::create(heap, globalCount);
  auto externFunctions = BlockArray<Function>::create(heap, functionCount);
  auto externClasses = BlockArray<Class>::create(heap, classCount);
  auto externTypeParameters = BlockArray<TypeParameter>::create(heap, typeParameterCount);
  auto externMethods = BlockArray<Function>::create(heap, methodCount);
  RETRY_WITH_GC(heap, return Local<PackageDependency>(new(heap) PackageDependency(
      *name, minVersion.getOrNull(), maxVersion.getOrNull(),
      *externGlobals, nullptr,
      *externFunctions, nullptr,
      *externClasses, nullptr,
      *externTypeParameters, nullptr,
      *externMethods)));
}


bool PackageDependency::parseNameAndVersion(Heap* heap,
                                            const Handle<String>& depString,
                                            Local<Name>* outName,
                                            Local<PackageVersion>* outMinVersion,
                                            Local<PackageVersion>* outMaxVersion) {
  auto colonPos = depString->find(':');

  if (colonPos == kIndexNotSet) {
    auto name = Name::fromString(heap, depString, Name::PACKAGE_NAME);
    if (!name)
      return false;
    *outName = name;
    outMinVersion->clear();
    outMaxVersion->clear();
    return true;
  }

  if (colonPos == depString->length() - 1)
    return false;

  auto nameStr = String::substring(heap, depString, 0, colonPos);
  auto name = Name::fromString(heap, nameStr, Name::PACKAGE_NAME);
  if (!name)
    return false;

  auto dashPos = depString->find('-', colonPos);
  auto minEnd = dashPos != kIndexNotSet ? dashPos : depString->length();
  Local<PackageVersion> minVersion;
  if (minEnd - colonPos > 1) {
    auto minVersionStr = String::substring(heap, depString, colonPos + 1, minEnd);
    minVersion = PackageVersion::fromString(heap, minVersionStr);
    if (!minVersion)
      return false;
  }

  Local<PackageVersion> maxVersion;
  if (dashPos == kIndexNotSet) {
    maxVersion = minVersion;
  } else if (dashPos < depString->length() - 1) {
    auto maxVersionStr = String::substring(heap, depString, dashPos + 1, depString->length());
    maxVersion = PackageVersion::fromString(heap, maxVersionStr);
    if (!maxVersion)
      return false;
  }

  if ((!minVersion && !maxVersion) ||
      (minVersion && maxVersion && minVersion->compare(*maxVersion) > 0)) {
    return false;
  }

  *outName = name;
  *outMinVersion = minVersion;
  *outMaxVersion = maxVersion;
  return true;
}


void PackageDependency::setLinkedGlobals(BlockArray<Global>* linkedGlobals) {
  ASSERT(externGlobals_.get()->length() == linkedGlobals->length());
  linkedGlobals_.set(this, linkedGlobals);
}


void PackageDependency::setLinkedFunctions(BlockArray<Function>* linkedFunctions) {
  ASSERT(externFunctions_.get()->length() == linkedFunctions->length());
  linkedFunctions_.set(this, linkedFunctions);
}


void PackageDependency::setLinkedClasses(BlockArray<Class>* linkedClasses) {
  ASSERT(externClasses_.get()->length() == linkedClasses->length());
  linkedClasses_.set(this, linkedClasses);
}


void PackageDependency::setLinkedTypeParameters(
    BlockArray<TypeParameter>* linkedTypeParameters) {
  ASSERT(externTypeParameters_.get()->length() == linkedTypeParameters->length());
  linkedTypeParameters_.set(this, linkedTypeParameters);
}


bool PackageDependency::isSatisfiedBy(const Name* name,
                                      const PackageVersion* version) const {
  return this->name()->equals(name) &&
         (minVersion() == nullptr || minVersion()->compare(version) <= 0) &&
         (maxVersion() == nullptr || maxVersion()->compare(version) >= 0);
}


ostream& operator << (ostream& os, const PackageDependency* dep) {
  os << brief(dep)
     << "\n  name: " << brief(dep->name())
     << "\n  minVersion: " << brief(dep->minVersion())
     << "\n  maxVersion: " << brief(dep->maxVersion())
     << "\n  package: " << brief(dep->package())
     << "\n  externGlobals: " << brief(dep->externGlobals())
     << "\n  linkedGlobals: " << brief(dep->linkedGlobals())
     << "\n  externFunctions: " << brief(dep->externFunctions())
     << "\n  linkedfunctions: " << brief(dep->linkedFunctions())
     << "\n  externClasses: " << brief(dep->externClasses())
     << "\n  linkedClasses: " << brief(dep->linkedClasses())
     << "\n  externTypeParameters: " << brief(dep->externTypeParameters())
     << "\n  linkedTypeParameters: " << brief(dep->linkedTypeParameters())
     << "\n  externMethods: " << brief(dep->externMethods());
  return os;
}


Local<String> Loader::readString() {
  auto length = readLengthVbn();
  auto size = readLengthVbn();
  vector<u8> utf8Chars = readData(size);
  return String::fromUtf8String(heap(), utf8Chars.data(), length, size);
}


Local<Global> Loader::readGlobal() {
  auto name = readName();
  auto flags = readValue<u32>();
  auto type = readType();
  return Global::create(heap(), name, flags, type);
}


Local<Function> Loader::readFunction() {
  Local<Name> name;
  u32 flags;
  Local<BlockArray<TypeParameter>> typeParameters;
  Local<Type> returnType;
  Local<BlockArray<Type>> parameterTypes;
  Local<Class> definingClass;
  readFunctionHeader(
      &name, &flags, &typeParameters, &returnType, &parameterTypes, &definingClass);

  word_t localsSize = kNotSet;
  Local<LengthArray> blockOffsets;
  vector<u8> instructions;
  if ((flags & (ABSTRACT_FLAG | EXTERN_FLAG)) == 0) {
    localsSize = readWordVbn();
    auto instructionsSize = readLengthVbn();
    instructions = readData(instructionsSize);

    auto blockOffsetCount = readLengthVbn();
    blockOffsets = LengthArray::create(heap(), blockOffsetCount);
    for (length_t i = 0; i < blockOffsetCount; i++) {
      auto offset = readLengthVbn();
      blockOffsets->set(i, offset);
    }
  }

  auto func = Function::create(heap(), name, flags, typeParameters,
                               returnType, parameterTypes, definingClass,
                               localsSize, instructions, blockOffsets,
                               package_);
  return func;
}


void Loader::readFunctionHeader(Local<Name>* name,
                                u32* flags,
                                Local<BlockArray<TypeParameter>>* typeParameters,
                                Local<Type>* returnType,
                                Local<BlockArray<Type>>* parameterTypes,
                                Local<Class>* definingClass) {
  *name = readName();
  *flags = readValue<u32>();

  auto typeParameterCount = readLengthVbn();
  *typeParameters = BlockArray<TypeParameter>::create(heap(), typeParameterCount);
  for (length_t i = 0; i < typeParameterCount; i++) {
    auto index = readLengthVbn();
    auto param = getTypeParameter(index);
    (*typeParameters)->set(i, *param);
  }

  *returnType = readType();
  auto parameterCount = readLengthVbn();
  *parameterTypes = BlockArray<Type>::create(heap(), parameterCount);
  for (length_t i = 0; i < parameterCount; i++) {
    (*parameterTypes)->set(i, *readType());
  }

  *definingClass = readIdAndGetDefiningClass();
}


void Loader::readClass(const Local<Class>& clas) {
  auto name = readName();
  auto flags = readValue<u32>();

  auto typeParamCount = readLengthVbn();
  auto typeParameters = BlockArray<TypeParameter>::create(heap(), typeParamCount);
  for (length_t i = 0; i < typeParamCount; i++) {
    auto index = readLengthVbn();
    auto typeParam = getTypeParameter(index);
    typeParameters->set(i, *typeParam);
  }

  auto supertype = readType();

  auto fieldCount = readLengthVbn();
  auto fields = BlockArray<Field>::create(heap(), fieldCount);
  for (length_t i = 0; i < fieldCount; i++) {
    auto field = readField();
    fields->set(i, *field);
  }

  auto constructorCount = readLengthVbn();
  auto constructors = BlockArray<Function>::create(heap(), constructorCount);
  for (length_t i = 0; i < constructorCount; i++) {
    auto method = readIdAndGetMethod();
    constructors->set(i, *method);
  }

  auto methodCount = readLengthVbn();
  auto methods = BlockArray<Function>::create(heap(), methodCount);
  for (length_t i = 0; i < methodCount; i++) {
    auto method = readIdAndGetMethod();
    methods->set(i, *method);
  }

  auto elementTypeOpt = readLengthVbn();
  Local<Type> elementType;
  length_t lengthFieldIndex = kIndexNotSet;
  if (elementTypeOpt == 1) {
    elementType = readType();
    for (length_t i = 0; i < fields->length(); i++) {
      if ((fields->get(i)->flags() & ARRAY_FLAG) != 0) {
        lengthFieldIndex = i;
        break;
      }
    }
    if (lengthFieldIndex == kIndexNotSet) {
      throw Error("no length field found in array");
    }
  } else if (elementTypeOpt != 0) {
    throw Error("invalid option");
  }

  clas->setName(*name);
  clas->setFlags(flags);
  clas->setTypeParameters(*typeParameters);
  clas->setSupertype(*supertype);
  clas->setFields(*fields);
  clas->setConstructors(*constructors);
  clas->setMethods(*methods);
  clas->setElementType(elementType.getOrNull());
  clas->setLengthFieldIndex(lengthFieldIndex);
  clas->setPackage(*package_);
}


Local<Field> Loader::readField() {
  auto name = readName();
  auto flags = readValue<u32>();
  auto type = readType();
  auto field = Field::create(heap(), name, flags, type);
  return field;
}


void Loader::readTypeParameter(const Local<TypeParameter>& param) {
  auto name = readName();
  auto flags = readValue<u32>();
  auto upperBound = readType();
  auto lowerBound = readType();
  param->setName(*name);
  param->setFlags(flags);
  param->setUpperBound(*upperBound);
  param->setLowerBound(*lowerBound);
}


Local<Type> Loader::readType() {
  const i64 kBuiltinPackageIndex = -2;
  const i64 kLocalPackageIndex = -1;

  auto bits = readWordVbn();
  auto form = static_cast<Type::Form>(bits & 0xf);
  auto flags = static_cast<Type::Flags>(bits >> 4);
  auto isPrimitive = Type::FIRST_PRIMITIVE_TYPE <= form && form <= Type::LAST_PRIMITIVE_TYPE;
  if (form > Type::LAST_TYPE ||
      flags > Type::LAST_FLAG ||
      (isPrimitive && flags != Type::NO_FLAGS)) {
    throw Error("invalid type flags");
  }

  if (isPrimitive) {
    return handle(Type::primitiveTypeFromForm(roots(), form));
  } else if (form == Type::CLASS_TYPE) {
    auto depIndex = readVbn();
    auto defnIndex = readVbn();

    Local<Class> clas;
    bool isExtern = false;
    if (depIndex == kBuiltinPackageIndex) {
      clas = handle(roots()->getBuiltinClass(static_cast<BuiltinId>(defnIndex)));
    } else if (depIndex == kLocalPackageIndex) {
      if (defnIndex < 0 || defnIndex >= package_->classes()->length()) {
        throw Error("invalid definition index");
      }
      clas = handle(package_->classes()->get(defnIndex));
    } else {
      if (depIndex < 0 || depIndex >= package_->dependencies()->length()) {
        throw Error("invalid package index");
      }
      auto dep = handle(package_->dependencies()->get(depIndex));
      if (defnIndex < 0 || defnIndex >= dep->externClasses()->length()) {
        throw Error("invalid extern class index");
      }
      clas = handle(dep->externClasses()->get(defnIndex));
      isExtern = true;
    }

    auto typeArgCount = readLengthVbn();
    if (flags == Type::NULLABLE_FLAG &&
        *clas == roots()->getBuiltinClass(BUILTIN_NOTHING_CLASS_ID) &&
        typeArgCount == 0) {
      return handle(roots()->nullType());
    }

    vector<Local<Type>> typeArgs;
    typeArgs.reserve(typeArgCount);
    for (length_t i = 0; i < typeArgCount; i++) {
      auto typeArg = readType();
      typeArgs.push_back(typeArg);
    }

    Local<Type> ty;
    if (isExtern) {
      ty = Type::createExtern(heap(), clas, typeArgs, flags);
      auto info = ExternTypeInfo::create(heap(), ty, package_, depIndex, defnIndex);
      externTypes_.push_back(info);
    } else {
      ty = Type::create(heap(), clas, typeArgs, flags);
    }

    return ty;
  } else {
    ASSERT(form == Type::VARIABLE_TYPE);
    auto depIndex = readVbn();
    auto defnIndex = readVbn();

    Local<TypeParameter> param;
    bool isExtern = false;
    if (depIndex == kLocalPackageIndex) {
      if (defnIndex < 0 || defnIndex >= package_->typeParameters()->length()) {
        throw Error("invalid definition index");
      }
      param = handle(package_->typeParameters()->get(defnIndex));
    } else {
      if (depIndex < 0 || depIndex >= package_->dependencies()->length()) {
        throw Error("invalid package index");
      }
      auto dep = handle(package_->dependencies()->get(depIndex));
      if (defnIndex < 0 || defnIndex >= dep->externTypeParameters()->length()) {
        throw Error("invalid definition index");
      }
      param = handle(dep->externTypeParameters()->get(defnIndex));
      isExtern = true;
    }

    Local<Type> ty;
    if (isExtern) {
      ty = Type::createExtern(heap(), param, flags);
      auto info = ExternTypeInfo::create(heap(), ty, package_, depIndex, defnIndex);
      externTypes_.push_back(info);
    } else {
      ty = Type::create(heap(), param, flags);
    }

    return ty;
  }
}


template <typename T>
T Loader::readValue() {
  T value;
  stream_.read(reinterpret_cast<char*>(&value), sizeof(value));
  return value;
}


Local<Name> Loader::readName() {
  auto length = readLengthVbn();
  auto components = BlockArray<String>::create(heap(), length);
  for (length_t i = 0; i < length; i++) {
    auto index = readLengthVbn();
    if (index >= package_->strings()->length()) {
      throw Error("name index out of bounds");
    }
    components->set(i, package_->strings()->get(index));
  }
  return Name::create(heap(), components);
}


length_t Loader::readLength() {
  auto len = readValue<length_t>();
  if (len != kLengthNotSet && len > kMaxLength)
    throw Error("could not read length");
  return len;
}


vector<u8> Loader::readData(word_t size) {
  vector<u8> buffer(size);
  stream_.read(reinterpret_cast<char*>(buffer.data()), size);
  return buffer;
}


DefnId Loader::readDefnIdVbns() {
  auto packageId = readIdVbn();
  auto index = readLengthVbn();
  DefnId id = { packageId, index };
  return id;
}


i64 Loader::readVbn() {
  i64 n = 0;
  i64 shift = 0;
  bool more;
  do {
    u8 b;
    stream_.read(reinterpret_cast<char*>(&b), 1);
    more = bit(b, 7);
    n |= (b & 0x7FLL) << shift;
    shift += 7;
  } while (more && shift < 64);
  ASSERT(!more);
  if (shift < 64) {
    int signExtend = 64 - shift;
    n = (n << signExtend) >> signExtend;
  }
  return n;
}


word_t Loader::readWordVbn() {
  i64 n = readVbn();
  word_t w = static_cast<word_t>(n);
  if (static_cast<i64>(w) != n) {
    throw Error("could not read number from stream");
  }
  return w;
}


length_t Loader::readLengthVbn() {
  i64 n = readVbn();
  auto len = static_cast<length_t>(n);
  if (static_cast<i64>(len) != n || len > kMaxLength) {
    throw Error("could not read length from stream");
  }
  return len;
}


id_t Loader::readIdVbn() {
  i64 n = readVbn();
  auto id = static_cast<id_t>(n);
  if (static_cast<i64>(id) != n) {
    throw Error("could not read id from stream");
  }
  return id;
}


DependencyLoader Loader::createDependencyLoader(const Handle<PackageDependency>& dep) {
  return DependencyLoader(vm_, stream_, package_, dep);
}


Local<Package> PackageLoader::load() {
  AllowAllocationScope allowAllocation(heap(), true);
  HandleScope handleScope(vm());
  try {
    auto magic = readValue<u32>();
    if (magic != kMagic)
      throw Error("package file is corrupt");
    auto majorVersion = readValue<u16>();
    auto minorVersion = readValue<u16>();
    if (majorVersion != 0 || minorVersion != 18)
      throw Error("package file has wrong format version");

    package_ = handleScope.escape(*Package::create(heap()));

    auto flags = readValue<u64>();
    package_->setFlags(flags);

    auto stringCount = readLength();
    auto stringArray = BlockArray<String>::create(heap(), stringCount);
    package_->setStrings(*stringArray);

    auto globalCount = readLength();
    auto globalArray = BlockArray<Global>::create(heap(), globalCount);
    package_->setGlobals(*globalArray);

    auto functionCount = readLength();
    auto functionArray = BlockArray<Function>::create(heap(), functionCount);
    package_->setFunctions(*functionArray);

    auto classCount = readLength();
    auto classArray = BlockArray<Class>::create(heap(), classCount);
    for (length_t i = 0; i < classCount; i++) {
      // We pre-allocate classes so Types we read can refer to them.
      auto clas = Class::create(heap());
      clas->setPackage(*package_);
      classArray->set(i, *clas);
    }
    package_->setClasses(*classArray);

    auto typeParameterCount = readLength();
    auto typeParametersArray =
        BlockArray<TypeParameter>::create(heap(), typeParameterCount);
    for (length_t i = 0; i < typeParameterCount; i++) {
      // Type parameters are also pre-allocated.
      auto param = TypeParameter::create(heap());
      typeParametersArray->set(i, *param);
    }
    package_->setTypeParameters(*typeParametersArray);

    auto depCount = readLength();
    auto depArray = BlockArray<PackageDependency>::create(heap(), depCount);
    package_->setDependencies(*depArray);

    auto entryFunctionIndex = readLength();
    package_->setEntryFunctionIndex(entryFunctionIndex);

    auto initFunctionIndex = readLength();
    package_->setInitFunctionIndex(initFunctionIndex);

    auto nameStr = readString();
    auto name = Name::fromString(heap(), nameStr, Name::PACKAGE_NAME);
    if (!name)
      throw Error("invalid package name");
    package_->setName(*name);

    auto versionStr = readString();
    auto version = PackageVersion::fromString(heap(), versionStr);
    if (!version)
      throw Error("invalid package version");
    package_->setVersion(*version);

    for (length_t i = 0; i < depCount; i++) {
      auto dep = readDependencyHeader();
      depArray->set(i, *dep);
    }

    for (length_t i = 0; i < stringCount; i++) {
      auto string = readString();
      stringArray->set(i, *string);
    }
    for (length_t i = 0; i < globalCount; i++) {
      auto global = readGlobal();
      globalArray->set(i, *global);
    }
    for (length_t i = 0; i < functionCount; i++) {
      auto function = readFunction();
      functionArray->set(i, *function);
    }
    for (length_t i = 0; i < classCount; i++) {
      auto clas = handle(classArray->get(i));
      readClass(clas);
    }
    for (length_t i = 0; i < typeParameterCount; i++) {
      auto param = handle(typeParametersArray->get(i));
      readTypeParameter(param);
    }

    for (length_t i = 0; i < depCount; i++) {
      auto dep = handle(depArray->get(i));
      DependencyLoader loader = createDependencyLoader(dep);
      loader.readDependencyBody();
    }

    auto externTypesArray = BlockArray<ExternTypeInfo>::create(heap(), externTypes_.size());
    for (length_t i = 0; i < externTypes_.size(); i++) {
      externTypesArray->set(i, *externTypes_[i]);
    }
    package_->setExternTypes(*externTypesArray);
  } catch (istream::failure exn) {
    throw Error("error reading package");
  }

  return package_;
}


Local<TypeParameter> PackageLoader::getTypeParameter(length_t index) const {
  if (index >= package()->typeParameters()->length()) {
    throw Error("type parameter index out of range");
  }
  return handle(package()->typeParameters()->get(index));
}


Local<Function> PackageLoader::readIdAndGetMethod() {
  DefnId id = readDefnIdVbns();
  if (id.packageId == kBuiltinPackageId) {
    if (id.defnIndex >= BUILTIN_FUNCTION_COUNT) {
      throw Error("invalid builtin function id");
    }
    return handle(roots()->getBuiltinFunction(indexToBuiltinId(id.defnIndex)));
  } else if (id.packageId == kLocalPackageId) {
    if (id.defnIndex >= package()->functions()->length()) {
      throw Error("invalid method index");
    }
    return handle(package()->functions()->get(id.defnIndex));
  } else {
    if (static_cast<length_t>(id.packageId) >= package()->dependencies()->length()) {
      throw Error("invalid dependency index");
    }
    return handle(package()->dependencies()->get(id.packageId)
        ->externFunctions()->get(id.defnIndex));
  }
}


Local<Class> PackageLoader::readIdAndGetDefiningClass() {
  auto opt = readLengthVbn();
  if (opt == 0) {
    return Local<Class>();
  } else if (opt == 1) {
    auto index = readLengthVbn();
    if (index >= package_->classes()->length()) {
      throw Error("invalid index for defining class");
    }
    return handle(package_->classes()->get(index));
  } else {
    throw Error("invalid option");
  }
}


Local<PackageDependency> PackageLoader::readDependencyHeader() {
  auto depStr = readString();
  Local<Name> name;
  Local<PackageVersion> minVersion, maxVersion;
  if (!PackageDependency::parseNameAndVersion(heap(), depStr,
                                              &name, &minVersion, &maxVersion)) {
    throw Error("invalid dependency string");
  }

  auto globalCount = readLengthVbn();
  auto functionCount = readLengthVbn();
  auto classCount = readLengthVbn();
  auto methodCount = readLengthVbn();
  auto typeParameterCount = readLengthVbn();

  auto dep = PackageDependency::create(heap(), name, minVersion, maxVersion,
                                       globalCount, functionCount,
                                       classCount, typeParameterCount,
                                       methodCount);

  // We need to pre-allocate methods, classes, type parameters so that other definitions
  // and types can refer to them. Normally, functions can't be preallocated since they have
  // variable size, but these are extern and don't have code.
  auto externClasses = handle(dep->externClasses());
  for (length_t i = 0; i < classCount; i++) {
    auto c = Class::create(heap());
    externClasses->set(i, *c);
  }

  auto externMethods = handle(dep->externMethods());
  for (length_t i = 0; i < methodCount; i++) {
    auto f = Function::create(heap());
    externMethods->set(i, *f);
  }

  auto externTypeParameters = handle(dep->externTypeParameters());
  for (length_t i = 0; i < typeParameterCount; i++) {
    auto p = TypeParameter::create(heap());
    externTypeParameters->set(i, *p);
  }

  return dep;
}


void DependencyLoader::readExternFunction(const Handle<Function>& func) {
  Local<Name> name;
  u32 flags;
  Local<BlockArray<TypeParameter>> typeParameters;
  Local<Type> returnType;
  Local<BlockArray<Type>> parameterTypes;
  Local<Class> definingClass;

  readFunctionHeader(
      &name, &flags, &typeParameters, &returnType, &parameterTypes, &definingClass);
  func->setName(*name);
  func->setFlags(flags);
  func->setTypeParameters(*typeParameters);
  func->setReturnType(*returnType);
  func->setParameterTypes(*parameterTypes);
  func->setDefiningClass(definingClass.getOrNull());

  if ((flags & EXTERN_FLAG) == 0) {
    throw Error("invalid function");
  }
}


void DependencyLoader::readDependencyBody() {
  auto globals = handle(dep_->externGlobals());
  for (length_t i = 0; i < globals->length(); i++) {
    auto g = readGlobal();
    if ((g->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency global is not extern");
    globals->set(i, *g);
  }

  auto functions = handle(dep_->externFunctions());
  for (length_t i = 0; i < functions->length(); i++) {
    auto f = readFunction();
    if ((f->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency function is not extern");
    functions->set(i, *f);
  }

  auto classes = handle(dep_->externClasses());
  for (length_t i = 0; i < classes->length(); i++) {
    auto c = handle(classes->get(i));
    readClass(c);
    if ((c->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency class is not extern");
  }

  auto methods = handle(dep_->externMethods());
  for (length_t i = 0; i < methods->length(); i++) {
    auto m = handle(methods->get(i));
    readExternFunction(m);
    u32 mask = EXTERN_FLAG | METHOD_FLAG;
    if ((m->flags() & mask) != mask)
      throw Error("dependency method is not extern");
  }

  auto typeParameters = handle(dep_->externTypeParameters());
  for (length_t i = 0; i < typeParameters->length(); i++) {
    auto p = handle(typeParameters->get(i));
    readTypeParameter(p);
    if ((p->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency type parameter is not extern");
  }
}


Local<TypeParameter> DependencyLoader::getTypeParameter(length_t index) const {
  if (index >= dep_->externTypeParameters()->length()) {
    throw Error("type parameter index out of range");
  }
  return handle(dep_->externTypeParameters()->get(index));
}


Local<Function> DependencyLoader::readIdAndGetMethod() {
  auto index = readLengthVbn();
  if (index >= dep_->externMethods()->length()) {
    throw Error("invalid method index");
  }
  return handle(dep_->externMethods()->get(index));
}

Local<Class> DependencyLoader::readIdAndGetDefiningClass() {
  auto opt = readLengthVbn();
  if (opt == 0) {
    return Local<Class>();
  } else if (opt == 1) {
    auto index = readLengthVbn();
    if (index >= dep_->externClasses()->length()) {
      throw Error("invalid defining class");
    }
    return handle(dep_->externClasses()->get(index));
  } else {
    throw Error("invalid option");
  }
}

}
}

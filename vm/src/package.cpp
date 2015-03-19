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
#include "bytecode.h"
#include "error.h"
#include "field.h"
#include "flags.h"
#include "function.h"
#include "global.h"
#include "handle.h"
#include "hash-table.h"
#include "heap.h"
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

DEFINE_POINTER_MAP(Package, PACKAGE_POINTER_LIST)

#undef PACKAGE_POINTER_LIST


class PackageLoader {
 public:
  PackageLoader(VM* vm, istream& stream)
      : vm_(vm),
        stream_(stream) { }

  Local<Package> load();

 private:
  Heap* heap() { return vm_->heap(); }
  Roots* roots() { return vm_->roots(); }

  Local<String> readString();
  Local<Global> readGlobal();
  Local<Function> readFunction();
  void readClass(const Local<Class>& clas);
  Local<Field> readField();
  void readTypeParameter(const Local<TypeParameter>& typeParam);
  Local<PackageDependency> readDependencyHeader();
  void readDependency(const Local<PackageDependency>& dep);
  Local<Type> readType();
  template <typename T>
  T readValue();
  Local<String> readName();
  length_t readLength();
  vector<u8> readData(word_t size);
  i64 readVbn();
  word_t readWordVbn();
  length_t readLengthVbn();
  id_t readIdVbn();

  static const u32 kMagic = 0x676b7073;

  VM* vm_;
  istream& stream_;
  Local<Package> package_;
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
  PackageLoader builder(vm, stream);
  return builder.load();
}


String* Package::getString(length_t index) {
  return block_cast<String>(strings()->get(index));
}


Global* Package::getGlobal(length_t index) {
  return block_cast<Global>(globals()->get(index));
}


Function* Package::getFunction(length_t index) {
  return block_cast<Function>(functions()->get(index));
}


Class* Package::getClass(length_t index) {
  return block_cast<Class>(classes()->get(index));
}


TypeParameter* Package::getTypeParameter(length_t index) {
  return block_cast<TypeParameter>(typeParameters()->get(index));
}


Function* Package::entryFunction() {
  auto index = entryFunctionIndex();
  if (index == kLengthNotSet)
    return nullptr;
  return getFunction(index);
}


Function* Package::initFunction() {
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
    if ((function->flags() & (PUBLIC_FLAG | METHOD_FLAG)) == PUBLIC_FLAG) {
      auto name = handle(function->name());
      ASSERT(!exports->contains(*name));
      ExportMap::add(heap, exports, name, function);
    }
  }

  // TODO: support other kinds of definitions.

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
        auto externGlobal = externGlobals->get(i);
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
        auto externFunction = externFunctions->get(i);
        auto name = externFunction->name();
        auto linkedFunction = depExports->getOrElse(name, nullptr);
        if (!linkedFunction || !isa<Function>(linkedFunction)) {
          throw Error("link error");
        }
        linkedFunctions->set(j, block_cast<Function>(linkedFunction));
      }
    }
    dependency->setLinkedFunctions(*linkedFunctions);
  }
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
     << "\n  exports: " << pkg->exports();
  return os;
}


#define PACKAGE_NAME_POINTER_LIST(F) \
  F(PackageName, components_)        \

DEFINE_POINTER_MAP(PackageName, PACKAGE_NAME_POINTER_LIST)

#undef PACKAGE_NAME_POINTER_LIST


PackageName::PackageName(BlockArray<String>* components)
    : Block(PACKAGE_NAME_BLOCK_TYPE),
      components_(this, components) {
  ASSERT(components->length() <= kMaxComponentCount);
  #ifdef DEBUG
  for (auto component : *components) {
    ASSERT(component->length() <= kMaxComponentLength);
  }
  #endif
}


Local<PackageName> PackageName::create(Heap* heap,
                                       const Handle<BlockArray<String>>& components) {
  RETRY_WITH_GC(heap, return Local<PackageName>(new(heap) PackageName(*components)));
}


Local<PackageName> PackageName::fromString(Heap* heap, const Handle<String>& nameString) {
  auto components = String::split(heap, nameString, '.');
  if (components->length() == 0 || components->length() > kMaxComponentCount)
    return Local<PackageName>();

  for (auto component : **components) {
    if (component->length() == 0 || component->length() > kMaxComponentLength)
      return Local<PackageName>();
    auto first = component->get(0);
    if (!inRange<u32>(first, 'A', 'Z') && !inRange<u32>(first, 'a', 'z'))
      return Local<PackageName>();
    for (auto it = component->begin() + 1; it != component->end(); ++it) {
      auto ch = *it;
      if (!inRange<u32>(ch, 'A', 'Z') &&
          !inRange<u32>(ch, 'a', 'z') &&
          !inRange<u32>(ch, '0', '9') &&
          ch != '_') {
        return Local<PackageName>();
      }
    }
  }

  return create(heap, components);
}


Local<String> PackageName::toString(Heap* heap, const Handle<PackageName>& name) {
  auto sep = String::fromUtf8CString(heap, ".");
  auto nameStr = String::join(heap, handle(name->components()), sep);
  return nameStr;
}


int PackageName::compare(const PackageName* other) const {
  auto len = min(components()->length(), other->components()->length());
  for (length_t i = 0; i < len; i++) {
    int cmp = components()->get(i)->compare(other->components()->get(i));
    if (cmp != 0)
      return cmp;
  }
  return static_cast<int>(components()->length()) -
         static_cast<int>(other->components()->length());
}


ostream& operator << (ostream& os, const PackageName* packageName) {
  os << brief(packageName) << "\n  ";
  auto components = packageName->components();
  auto length = components->length();
  for (length_t i = 0; i < length - 1; i++) {
    os << components->get(i) << '.';
  }
  os << components->get(length - 1);
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

DEFINE_POINTER_MAP(PackageDependency, PACKAGE_DEPENDENCY_POINTER_LIST)

#undef PACKAGE_DEPENDENCY_POINTER_LIST


PackageDependency::PackageDependency(PackageName* name,
                                     PackageVersion* minVersion,
                                     PackageVersion* maxVersion,
                                     BlockArray<Global>* externGlobals,
                                     BlockArray<Global>* linkedGlobals,
                                     BlockArray<Function>* externFunctions,
                                     BlockArray<Function>* linkedFunctions,
                                     BlockArray<Class>* externClasses,
                                     BlockArray<Class>* linkedClasses,
                                     BlockArray<TypeParameter>* externTypeParameters)
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
      externTypeParameters_(this, externTypeParameters) {
  ASSERT(minVersion == nullptr || maxVersion == nullptr ||
         minVersion->compare(maxVersion) <= 0);
  ASSERT(linkedGlobals == nullptr || externGlobals->length() == linkedGlobals->length());
  ASSERT(linkedFunctions == nullptr || externFunctions->length() == linkedFunctions->length());
  ASSERT(linkedClasses == nullptr || externClasses->length() == linkedClasses->length());
}


Local<PackageDependency> PackageDependency::create(
    Heap* heap,
    const Handle<PackageName>& name,
    const Handle<PackageVersion>& minVersion,
    const Handle<PackageVersion>& maxVersion,
    const Handle<BlockArray<Global>>& externGlobals,
    const Handle<BlockArray<Global>>& linkedGlobals,
    const Handle<BlockArray<Function>>& externFunctions,
    const Handle<BlockArray<Function>>& linkedFunctions,
    const Handle<BlockArray<Class>>& externClasses,
    const Handle<BlockArray<Class>>& linkedClasses,
    const Handle<BlockArray<TypeParameter>>& externTypeParameters) {
  RETRY_WITH_GC(heap, return Local<PackageDependency>(new(heap) PackageDependency(
      *name, minVersion.getOrNull(), maxVersion.getOrNull(),
      *externGlobals, linkedGlobals.getOrNull(),
      *externFunctions, linkedFunctions.getOrNull(),
      *externClasses, linkedClasses.getOrNull(),
      *externTypeParameters)));
}


Local<PackageDependency> PackageDependency::create(Heap* heap,
                                                   const Handle<PackageName>& name,
                                                   const Handle<PackageVersion>& minVersion,
                                                   const Handle<PackageVersion>& maxVersion,
                                                   length_t globalCount,
                                                   length_t functionCount,
                                                   length_t classCount,
                                                   length_t typeParameterCount) {
  auto externGlobals = BlockArray<Global>::create(heap, globalCount);
  auto externFunctions = BlockArray<Function>::create(heap, functionCount);
  auto externClasses = BlockArray<Class>::create(heap, classCount);
  auto externTypeParameters = BlockArray<TypeParameter>::create(heap, typeParameterCount);
  RETRY_WITH_GC(heap, return Local<PackageDependency>(new(heap) PackageDependency(
      *name, minVersion.getOrNull(), maxVersion.getOrNull(),
      *externGlobals, nullptr,
      *externFunctions, nullptr,
      *externClasses, nullptr,
      *externTypeParameters)));
}


bool PackageDependency::parseNameAndVersion(Heap* heap,
                                            const Handle<String>& depString,
                                            Local<PackageName>* outName,
                                            Local<PackageVersion>* outMinVersion,
                                            Local<PackageVersion>* outMaxVersion) {
  auto colonPos = depString->find(':');

  if (colonPos == kIndexNotSet) {
    auto name = PackageName::fromString(heap, depString);
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
  auto name = PackageName::fromString(heap, nameStr);
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


bool PackageDependency::isSatisfiedBy(const PackageName* name,
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
     << "\n  externTypeParameters: " << brief(dep->externTypeParameters());
  return os;
}


Local<Package> PackageLoader::load() {
  AllowAllocationScope allowAllocation(heap(), true);
  HandleScope handleScope(vm_);
  try {
    auto magic = readValue<u32>();
    if (magic != kMagic)
      throw Error("package file is corrupt");
    auto majorVersion = readValue<u16>();
    auto minorVersion = readValue<u16>();
    if (majorVersion != 0 || minorVersion != 16)
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
    auto name = PackageName::fromString(heap(), nameStr);
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
      readDependency(dep);
    }
  } catch (istream::failure exn) {
    throw Error("error reading package");
  }

  return package_;
}


Local<String> PackageLoader::readString() {
  auto length = readLengthVbn();
  auto size = readLengthVbn();
  vector<u8> utf8Chars = readData(size);
  return String::fromUtf8String(heap(), utf8Chars.data(), length, size);
}


Local<Global> PackageLoader::readGlobal() {
  auto name = readName();
  auto flags = readValue<u32>();
  auto type = readType();
  return Global::create(heap(), name, flags, type);
}


Local<Function> PackageLoader::readFunction() {
  auto name = readName();
  auto flags = readValue<u32>();

  auto typeParameterCount = readLengthVbn();
  auto typeParameters = TaggedArray<TypeParameter>::create(heap(), typeParameterCount);
  for (length_t i = 0; i < typeParameterCount; i++) {
    auto id = readIdVbn();
    typeParameters->set(i, Tagged<TypeParameter>(id));
  }

  auto returnType = readType();
  auto parameterCount = readLengthVbn();
  auto types = BlockArray<Type>::create(heap(), parameterCount + 1);
  types->set(0, *returnType);
  for (length_t i = 0; i < parameterCount; i++) {
    types->set(i + 1, *readType());
  }

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

  auto function = Function::create(heap(), name, flags, typeParameters, types,
                                   localsSize, instructions, blockOffsets, package_);
  return function;
}


void PackageLoader::readClass(const Local<Class>& clas) {
  auto name = readName();
  auto flags = readValue<u32>();

  auto typeParamCount = readLengthVbn();
  auto typeParameters = TaggedArray<TypeParameter>::create(heap(), typeParamCount);
  for (length_t i = 0; i < typeParamCount; i++) {
    auto id = readIdVbn();
    typeParameters->set(i, Tagged<TypeParameter>(id));
  }

  auto supertype = readType();

  auto fieldCount = readLengthVbn();
  auto fields = BlockArray<Field>::create(heap(), fieldCount);
  for (length_t i = 0; i < fieldCount; i++) {
    auto field = readField();
    fields->set(i, *field);
  }

  auto constructorCount = readLengthVbn();
  auto constructors = IdArray::create(heap(), constructorCount);
  for (length_t i = 0; i < constructorCount; i++) {
    auto id = readIdVbn();
    constructors->set(i, id);
  }

  auto methodCount = readLengthVbn();
  auto methods = IdArray::create(heap(), methodCount);
  for (length_t i = 0; i < methodCount; i++) {
    auto id = readIdVbn();
    methods->set(i, id);
  }

  clas->setName(*name);
  clas->setFlags(flags);
  clas->setTypeParameters(*typeParameters);
  clas->setSupertype(*supertype);
  clas->setFields(*fields);
  clas->setConstructors(*constructors);
  clas->setMethods(*methods);
  clas->setPackage(*package_);
}


Local<Field> PackageLoader::readField() {
  auto name = readName();
  auto flags = readValue<u32>();
  auto type = readType();
  auto field = Field::create(heap(), name, flags, type);
  return field;
}


void PackageLoader::readTypeParameter(const Local<TypeParameter>& param) {
  auto name = readName();
  auto flags = readValue<u32>();
  auto upperBound = readType();
  auto lowerBound = readType();
  param->setName(*name);
  param->setFlags(flags);
  param->setUpperBound(*upperBound);
  param->setLowerBound(*lowerBound);
}


Local<PackageDependency> PackageLoader::readDependencyHeader() {
  auto depStr = readString();
  Local<PackageName> name;
  Local<PackageVersion> minVersion, maxVersion;
  if (!PackageDependency::parseNameAndVersion(heap(), depStr,
                                              &name, &minVersion, &maxVersion)) {
    throw Error("invalid dependency string");
  }

  auto globalCount = readLengthVbn();
  auto functionCount = readLengthVbn();
  auto classCount = readLengthVbn();
  auto typeParameterCount = readLengthVbn();

  auto dep = PackageDependency::create(heap(), name, minVersion, maxVersion,
                                       globalCount, functionCount,
                                       classCount, typeParameterCount);

  // We need to pre-allocate classes and type parameters so types can refer to them.
  auto externClasses = handle(dep->externClasses());
  for (length_t i = 0; i < classCount; i++) {
    auto c = Class::create(heap());
    externClasses->set(i, *c);
  }

  auto externTypeParameters = handle(dep->externTypeParameters());
  for (length_t i = 0; i < typeParameterCount; i++) {
    auto p = TypeParameter::create(heap());
    externTypeParameters->set(i, *p);
  }

  return dep;
}


void PackageLoader::readDependency(const Local<PackageDependency>& dep) {
  auto globals = handle(dep->externGlobals());
  for (length_t i = 0; i < globals->length(); i++) {
    auto g = readGlobal();
    if ((g->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency global is not extern");
    globals->set(i, *g);
  }

  auto functions = handle(dep->externFunctions());
  for (length_t i = 0; i < functions->length(); i++) {
    auto f = readFunction();
    if ((f->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency function is not extern");
    functions->set(i, *f);
  }

  auto classes = handle(dep->externClasses());
  for (length_t i = 0; i < classes->length(); i++) {
    auto c = handle(classes->get(i));
    readClass(c);
    if ((c->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency class is not extern");
  }

  auto typeParameters = handle(dep->externTypeParameters());
  for (length_t i = 0; i < typeParameters->length(); i++) {
    auto p = handle(typeParameters->get(i));
    readTypeParameter(p);
    if ((p->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency type parameter is not extern");
  }
}


Local<Type> PackageLoader::readType() {
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
  } else {
    auto code = readVbn();
    if (form == Type::CLASS_TYPE) {
      auto typeArgCount = readLengthVbn();
      if (flags == Type::NULLABLE_FLAG &&
          code == BUILTIN_NOTHING_CLASS_ID) {
        ASSERT(typeArgCount == 0);
        return handle(roots()->nullType());
      }
      Local<Class> clas;
      if (isBuiltinId(code)) {
        clas = handle(roots()->getBuiltinClass(static_cast<BuiltinId>(code)));
      } else {
        clas = handle(package_->getClass(code));
      }
      vector<Local<Type>> typeArgs;
      typeArgs.reserve(typeArgCount);
      for (length_t i = 0; i < typeArgCount; i++) {
        auto typeArg = readType();
        typeArgs.push_back(typeArg);
      }
      auto ty = Type::create(heap(), clas, typeArgs, flags);
      return ty;
    } else {
      ASSERT(form == Type::VARIABLE_TYPE);
      if (isBuiltinId(code))
        throw Error("no builtin type parameters");
      auto param = handle(package_->getTypeParameter(code));
      auto ty = Type::create(heap(), param, flags);
      return ty;
    }
  }
}


template <typename T>
T PackageLoader::readValue() {
  T value;
  stream_.read(reinterpret_cast<char*>(&value), sizeof(value));
  return value;
}


length_t PackageLoader::readLength() {
  auto len = readValue<length_t>();
  if (len != kLengthNotSet && len > kMaxLength)
    throw Error("could not read length");
  return len;
}


Local<String> PackageLoader::readName() {
  auto index = readLengthVbn();
  if (index == kIndexNotSet) {
    return handle(roots()->emptyString());
  } else if (index >= package_->strings()->length()) {
    throw Error("name index out of bounds");
  } else {
    return handle(package_->strings()->get(index));
  }
}


vector<u8> PackageLoader::readData(word_t size) {
  vector<u8> buffer(size);
  stream_.read(reinterpret_cast<char*>(buffer.data()), size);
  return buffer;
}


i64 PackageLoader::readVbn() {
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


word_t PackageLoader::readWordVbn() {
  i64 n = readVbn();
  word_t w = static_cast<word_t>(n);
  if (static_cast<i64>(w) != n) {
    throw Error("could not read number from stream");
  }
  return w;
}


length_t PackageLoader::readLengthVbn() {
  i64 n = readVbn();
  auto len = static_cast<length_t>(n);
  if (static_cast<i64>(len) != n || len > kMaxLength) {
    throw Error("could not read length from stream");
  }
  return len;
}


id_t PackageLoader::readIdVbn() {
  i64 n = readVbn();
  auto id = static_cast<id_t>(n);
  if (static_cast<i64>(id) != n) {
    throw Error("could not read id from stream");
  }
  return id;
}

}
}

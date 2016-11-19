// Copyright 2014-2016 Jay Conrod. All rights reserved.

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
#include "index.h"
#include "name.h"
#include "roots.h"
#include "string.h"
#include "trait.h"
#include "type.h"
#include "type-parameter.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define PACKAGE_POINTER_LIST(F) \
  F(Package, name_) \
  F(Package, version_) \
  F(Package, dependencies_) \
  F(Package, strings_) \
  F(Package, globals_) \
  F(Package, functions_) \
  F(Package, classes_) \
  F(Package, traits_) \
  F(Package, typeParameters_) \
  F(Package, exports_) \
  F(Package, globalNameIndex_)  \
  F(Package, globalSourceNameIndex_) \
  F(Package, functionNameIndex_) \
  F(Package, functionSourceNameIndex_) \
  F(Package, classNameIndex_) \
  F(Package, classSourceNameIndex_) \
  F(Package, traitNameIndex_) \
  F(Package, traitSourceNameIndex_)

DEFINE_POINTER_MAP(Package, PACKAGE_POINTER_LIST)

#undef PACKAGE_POINTER_LIST


class DependencyLoader;

/** State used by multiple {@link Loader} objects loading a set of dependent packages. */
struct LoadState {
  LoadState(
      VM* vm,
      Counter* idCounter,
      const vector<NativeFunctionSearch>* nativeFunctionSearchOrder)
      : vm(vm),
        idCounter(idCounter),
        nativeFunctionSearchOrder(nativeFunctionSearchOrder) { }

  VM* vm;
  Counter* idCounter;
  vector<Persistent<Package>> packages;
  vector<Persistent<Package>> loadingPackages;
  const vector<NativeFunctionSearch>* nativeFunctionSearchOrder;
};


/**
 * Identifier used within a package to point to a local, builtin, or foreign definition.
 * The type of the definition is implied by the context.
 */
struct PackageDefnId {
  id_t packageId;
  length_t defnIndex;

  bool operator == (const PackageDefnId& other) const {
    return packageId == other.packageId && defnIndex == other.defnIndex;
  }
  bool operator != (const PackageDefnId& other) const {
    return !(*this == other);
  }
};


class Loader {
 public:
  Loader(LoadState* loadState, istream& stream, Persistent<Package> package)
      : loadState_(loadState),
        stream_(stream),
        package_(package) {}
  virtual ~Loader() {}

 protected:
  VM* vm() const { return loadState_->vm; }
  Heap* heap() const { return vm()->heap(); }
  Roots* roots() const { return vm()->roots(); }
  Local<Package> package() const { return package_; }

  Local<String> readString();
  Local<Global> readGlobal(DefnId id);
  Local<Function> readFunction(DefnId id);
  void readFunctionHeader(Local<Name>* name,
                          Local<String>* sourceName,
                          u32* flags,
                          Local<BlockArray<TypeParameter>>* typeParameters,
                          Local<Type>* returnType,
                          Local<BlockArray<Type>>* parameterTypes,
                          Local<ObjectTypeDefn>* definingClass,
                          Local<BlockArray<Function>>* overrides);
  void readClass(const Local<Class>& clas);
  void readTrait(const Local<Trait>& trait);
  Local<Field> readField();
  void readTypeParameter(const Local<TypeParameter>& typeParam);
  Local<Type> readType();

  template <typename T>
  T readValue();
  Local<Name> readName();
  Local<ObjectTypeDefn> readDefiningClass();
  Local<String> readSourceName();
  Local<String> readStringId();

  template <typename T>
  Local<BlockArray<T>> readBlockList(Local<T> (Loader::*reader)());
  template <typename T, typename Reader>
  Local<BlockArray<T>> readBlockList(Reader reader);
  template <typename T>
  Local<T> readOption(Local<T> (Loader::*reader)());
  template <typename T, typename Reader>
  Local<T> readOption(Reader reader);
  length_t readLength();
  vector<u8> readData(word_t size);
  PackageDefnId readDefnIdVbns();
  i64 readVbn();
  word_t readWordVbn();
  length_t readLengthVbn();
  id_t readIdVbn();

  DependencyLoader createDependencyLoader(const Handle<PackageDependency>& dep);

  virtual Local<TypeParameter> getTypeParameter(length_t index) const = 0;
  virtual Local<Function> readIdAndGetMethod() = 0;
  virtual Local<TraitTable> readTraitTable() = 0;

 protected:
  LoadState* loadState_;
  istream& stream_;
  Persistent<Package> package_;
  bool isLinked_ = false;
};


class PackageLoader: public Loader {
 public:
  PackageLoader(LoadState* loadState,
                istream& stream,
                const string& dirName)
      : Loader(
            loadState,
            stream,
            Package::create(loadState->vm->heap(), loadState->idCounter->next())),
        dirName_(dirName) {
    loadState_->loadingPackages.push_back(package());
  }

  Persistent<Package> load();

 protected:
  virtual Local<TypeParameter> getTypeParameter(length_t index) const;
  virtual Local<Function> readIdAndGetMethod();
  virtual Local<TraitTable> readTraitTable();

 private:
  static const u32 kMagic = 0x676b7073;

  Persistent<Package> loadDependency(const Handle<PackageDependency>& dep);

  Local<PackageDependency> readDependencyHeader();
  void loadNativeLibrary();

  string dirName_;
};


class DependencyLoader: public Loader {
 public:
  DependencyLoader(LoadState* loadState,
                   istream& stream,
                   const Handle<Package>& package,
                   const Handle<PackageDependency>& dep)
      : Loader(loadState, stream, package),
        dep_(dep) { }

 public:
  void readDependencyBody();

 protected:
  void readExternFunction(const Handle<Function>& func);

  virtual Local<TypeParameter> getTypeParameter(length_t index) const;
  virtual Local<Function> readIdAndGetMethod();
  virtual Local<TraitTable> readTraitTable();

 private:
  Local<PackageDependency> dep_;
};


Package::Package(VM* vm, id_t id)
    : Object(PACKAGE_BLOCK_TYPE),
      id_(id),
      flags_(0),
      dependencies_(this, reinterpret_cast<BlockArray<PackageDependency>*>(
          vm->roots()->emptyBlockArray())),
      strings_(this, reinterpret_cast<BlockArray<String>*>(vm->roots()->emptyBlockArray())),
      globals_(this, reinterpret_cast<BlockArray<Global>*>(vm->roots()->emptyBlockArray())),
      functions_(this, reinterpret_cast<BlockArray<Function>*>(vm->roots()->emptyBlockArray())),
      classes_(this, reinterpret_cast<BlockArray<Class>*>(vm->roots()->emptyBlockArray())),
      traits_(this, reinterpret_cast<BlockArray<Trait>*>(vm->roots()->emptyBlockArray())),
      typeParameters_(this,
                      reinterpret_cast<BlockArray<TypeParameter>*>(
                          vm->roots()->emptyBlockArray())),
      entryFunctionIndex_(kIndexNotSet),
      initFunctionIndex_(kIndexNotSet),
      nativeLibrary_(nullptr),
      encodedNativeFunctionSearchOrder_(0) { }


Local<Package> Package::create(Heap* heap, id_t id) {
  RETRY_WITH_GC(heap, return Local<Package>(new(heap) Package(heap->vm(), id)));
}


vector<Persistent<Package>> Package::load(
    VM* vm,
    Counter* idCounter,
    const string& fileName,
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  auto dirName = pathDirName(fileName);
  ifstream file(fileName.c_str(), ios::binary);
  if (!file.good())
    throw Error("could not open package file");
  file.exceptions(ios::failbit | ios::badbit | ios::eofbit);

  auto packages = load(vm, idCounter, file, dirName, nativeFunctionSearchOrder);

  auto pos = file.tellg();
  file.seekg(0, ios::end);
  if (pos != file.tellg())
    throw Error("garbage found at end of file");

  return packages;
}


vector<Persistent<Package>> Package::load(
    VM* vm,
    Counter* idCounter,
    istream& stream,
    const string& dirName,
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  AllowAllocationScope allowAllocation(vm->heap(), true);
  HandleScope handleScope(vm);

  LoadState loadState(vm, idCounter, &nativeFunctionSearchOrder);
  PackageLoader loader(&loadState, stream, dirName);
  loader.load();

  return loadState.packages;
}


String* Package::getString(length_t index) const {
  return strings()->get(index);
}


Global* Package::getGlobal(length_t index) const {
  return globals()->get(index);
}


Function* Package::getFunction(length_t index) const {
  return functions()->get(index);
}


Class* Package::getClass(length_t index) const {
  return classes()->get(index);
}


Trait* Package::getTrait(length_t index) const {
  return traits()->get(index);
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


void Package::ensureExports(const Handle<Package>& package) {
  if (package->exports_)
    return;

  auto heap = package->getHeap();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(heap->vm());
  auto exports = ExportMap::create(heap);

  auto globals = handle(package->globals());
  for (length_t i = 0; i < globals->length(); i++) {
    auto global = handle(globals->get(i));
    if ((global->flags() & PUBLIC_FLAG) != 0) {
      auto name = handle(global->name());
      ASSERT(!exports->contains(*name));
      ExportMap::add(exports, name, global);
    }
  }

  auto functions = handle(package->functions());
  for (length_t i = 0; i < functions->length(); i++) {
    auto function = handle(functions->get(i));
    if ((function->flags() & PUBLIC_FLAG) != 0) {
      auto mangledName = mangleFunctionName(function, package);
      ASSERT(!exports->contains(*mangledName));
      ExportMap::add(exports, mangledName, function);
    }
  }

  auto classes = handle(package->classes());
  for (length_t i = 0; i < classes->length(); i++) {
    auto clas = handle(classes->get(i));
    if ((clas->flags() & PUBLIC_FLAG) != 0) {
      auto name = handle(clas->name());
      ASSERT(!exports->contains(*name));
      ExportMap::add(exports, name, clas);
    }
  }

  auto traits = handle(package->traits());
  for (length_t i = 0; i < traits->length(); i++) {
    auto trait = handle(traits->get(i));
    if ((trait->flags() & PUBLIC_FLAG) != 0) {
      auto name = handle(trait->name());
      ASSERT(!exports->contains(*name));
      ExportMap::add(exports, name, trait);
    }
  }

  auto typeParameters = handle(package->typeParameters());
  for (length_t i = 0; i < typeParameters->length(); i++) {
    auto typeParameter = handle(typeParameters->get(i));
    if ((typeParameter->flags() & PUBLIC_FLAG) != 0) {
      auto name = handle(typeParameter->name());
      ASSERT(!exports->contains(*name));
      ExportMap::add(exports, name, typeParameter);
    }
  }

  package->setExports(*exports);
}


Local<BlockHashMap<Name, Global>> Package::ensureAndGetGlobalNameIndex(
    const Handle<Package>& package) {
  if (package->globalNameIndex()) {
    return handle(package->globalNameIndex());
  }
  auto index = buildNameIndex<Global>(handle(package->globals()), allDefnFilter<Global>);
  package->setGlobalNameIndex(*index);
  return index;
}


Local<BlockHashMap<String, Global>> Package::ensureAndGetGlobalSourceNameIndex(
    const Handle<Package>& package) {
  if (package->globalSourceNameIndex()) {
    return handle(package->globalSourceNameIndex());
  }
  auto index = buildSourceNameIndex<Global>(handle(package->globals()), allDefnFilter<Global>);
  package->setGlobalSourceNameIndex(*index);
  return index;
}


Local<BlockHashMap<Name, Function>> Package::ensureAndGetFunctionNameIndex(
    const Handle<Package>& package) {
  if (package->functionNameIndex()) {
    return handle(package->functionNameIndex());
  }
  Local<Name> (*getKey)(const Handle<Function>&) = mangleFunctionName;
  auto index = buildIndex<Name, Function>(
      handle(package->functions()),
      getKey,
      allDefnFilter<Function>);
  package->setFunctionNameIndex(*index);
  return index;
}


static bool functionSourceNameFilter(const Handle<Function>& function) {
  return (function->flags() & METHOD_FLAG) == 0;
}


Local<BlockHashMap<String, Function>> Package::ensureAndGetFunctionSourceNameIndex(
    const Handle<Package>& package) {
  if (package->functionSourceNameIndex()) {
    return handle(package->functionSourceNameIndex());
  }
  auto index = buildIndex<String, Function>(
      handle(package->functions()),
      mangleFunctionSourceName,
      functionSourceNameFilter);
  package->setFunctionSourceNameIndex(*index);
  return index;
}


Local<BlockHashMap<Name, Class>> Package::ensureAndGetClassNameIndex(
    const Handle<Package>& package) {
  if (package->classNameIndex()) {
    return handle(package->classNameIndex());
  }
  auto index = buildNameIndex<Class>(handle(package->classes()), allDefnFilter<Class>);
  package->setClassNameIndex(*index);
  return index;
}


Local<BlockHashMap<String, Class>> Package::ensureAndGetClassSourceNameIndex(
    const Handle<Package>& package) {
  if (package->classSourceNameIndex()) {
    return handle(package->classSourceNameIndex());
  }
  auto index = buildSourceNameIndex<Class>(handle(package->classes()), allDefnFilter<Class>);
  package->setClassSourceNameIndex(*index);
  return index;
}


Local<BlockHashMap<Name, Trait>> Package::ensureAndGetTraitNameIndex(
    const Handle<Package>& package) {
  if (package->traitNameIndex()) {
    return handle(package->traitNameIndex());
  }
  auto index = buildNameIndex<Trait>(handle(package->traits()), allDefnFilter<Trait>);
  package->setTraitNameIndex(*index);
  return index;
}


Local<BlockHashMap<String, Trait>> Package::ensureAndGetTraitSourceNameIndex(
    const Handle<Package>& package) {
  if (package->traitSourceNameIndex()) {
    return handle(package->traitSourceNameIndex());
  }
  auto index = buildSourceNameIndex<Trait>(handle(package->traits()), allDefnFilter<Trait>);
  package->setTraitSourceNameIndex(*index);
  return index;
}


vector<NativeFunctionSearch> Package::decodeNativeFunctionSearchOrder(
    u32 encodedNativeFunctionSearchOrder) {
  vector<NativeFunctionSearch> decoded;
  decoded.reserve(4);
  for (int i = 0; i < 4; i++) {
    u8 encodedSearch = (encodedNativeFunctionSearchOrder >> (i * 8)) & 0xFF;
    if (encodedSearch == 0) {
      return decoded;
    } else {
      decoded.push_back(static_cast<NativeFunctionSearch>(encodedSearch - 1));
    }
  }
  return decoded;
}


u32 Package::encodeNativeFunctionSearchOrder(
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  u32 encoded = 0;
  for (word_t i = 0; i < nativeFunctionSearchOrder.size(); i++) {
    encoded |= (static_cast<u32>(nativeFunctionSearchOrder[i]) + 1) << (i * 8);
  }
  return encoded;
}


bool Package::mayLoadFunctionsFromNativeLibrary() const {
  auto searchOrder = nativeFunctionSearchOrder();
  return find(searchOrder.begin(), searchOrder.end(), SEARCH_LIBRARY_FUNCTIONS)
      != searchOrder.end();
}


static void buildFunctionName(string* nameStr, Name* name) {
  auto components = name->components();
  for (length_t i = 0; i < components->length(); i++) {
    if (i > 0)
      *nameStr += "__";
    for (auto ch : *components->get(i)) {
      auto valid = ('0' <= ch && ch <= '9') ||
          ('A' <= ch && ch <= 'Z') ||
          ('a' <= ch && ch <= 'z') ||
          ch == '_';
      *nameStr += valid ? static_cast<char>(ch) : '_';
    }
  }
}


NativeFunction Package::loadNativeFunction(Name* functionName) {
  // Determine the name of the function.
  string nameStr;
  buildFunctionName(&nameStr, name());
  nameStr += "___";
  buildFunctionName(&nameStr, functionName);

  // Search for the function according to the search order.
  auto searchOrder = nativeFunctionSearchOrder();
  for (auto search : searchOrder) {
    NativeFunction fn = nullptr;
    if (search == SEARCH_LIBRARY_FUNCTIONS) {
      auto library = nativeLibrary();
      ASSERT(library != nullptr);
      fn = ::codeswitch::internal::loadNativeFunction(library, nameStr);
    } else if (search == SEARCH_LINKED_FUNCTIONS) {
      fn = loadLinkedNativeFunction(nameStr);
    } else {
      ASSERT(search == SEARCH_REGISTERED_FUNCTIONS);
      fn = getVM()->loadRegisteredFunction(name(), functionName);
    }
    if (fn != nullptr)
      return fn;
  }
  throw Error(nameStr + ": could not find native function");
}


void Package::link(const Handle<Package>& package) {
  auto heap = package->getHeap();
  auto vm = heap->vm();
  AllowAllocationScope allowAllocation(heap, true);
  HandleScope handleScope(vm);
  auto dependencies = handle(package->dependencies());
  for (length_t i = 0; i < dependencies->length(); i++) {
    auto dependency = handle(dependencies->get(i));
    auto depPackage = handle(dependency->package());
    ensureExports(depPackage);
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
      for (length_t j = 0; j < functionCount; j++) {
        auto externFunction = handle(externFunctions->get(j));
        auto mangledName = mangleFunctionName(externFunction, handle(dependency->package()));
        Local<Block> linkedFunction(vm, depExports->getOrElse(*mangledName, nullptr));
        if (!linkedFunction || !isa<Function>(*linkedFunction)) {
          throw Error("link error");
        }
        linkedFunctions->set(j, block_cast<Function>(*linkedFunction));
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

    ASSERT(dependency->linkedTraits() == nullptr);
    auto externTraits = handle(dependency->externTraits());
    auto traitCount = externTraits->length();
    auto linkedTraits = BlockArray<Trait>::create(heap, traitCount);
    {
      AllowAllocationScope noAllocation(heap, false);
      for (length_t j = 0; j < traitCount; j++) {
        auto externTrait = externTraits->get(j);
        auto name = externTrait->name();
        auto linkedTrait = depExports->getOrElse(name, nullptr);
        if (!linkedTrait || !isa<Trait>(linkedTrait)) {
          throw Error("link error");
        }
        linkedTraits->set(j, block_cast<Trait>(linkedTrait));
      }
    }
    dependency->setLinkedTraits(*linkedTraits);

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
     << "\n  traits: " << brief(pkg->traits())
     << "\n  type parameters: " << brief(pkg->typeParameters())
     << "\n  entry function index: " << pkg->entryFunctionIndex()
     << "\n  init function index: " << pkg->initFunctionIndex()
     << "\n  exports: " << brief(pkg->exports())
     << "\n  global name index: " << brief(pkg->globalNameIndex())
     << "\n  global source name index: " << brief(pkg->globalSourceNameIndex())
     << "\n  function name index: " << brief(pkg->functionNameIndex())
     << "\n  function source name index: " << brief(pkg->functionSourceNameIndex())
     << "\n  class name index: " << brief(pkg->classNameIndex())
     << "\n  class source name index: " << brief(pkg->classSourceNameIndex())
     << "\n  trait name index: " << brief(pkg->traitNameIndex())
     << "\n  trait source name index: " << brief(pkg->traitSourceNameIndex())
     << "\n  nativeLibrary: " << pkg->nativeLibrary()
     << "\n  encodedNativeFunctionSearchOrder: " << pkg->encodedNativeFunctionSearchOrder();
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


string PackageVersion::toStlString() {
  string versionStr = to_string(components()->get(0));
  for (length_t i = 1; i < components()->length(); i++) {
    versionStr += "." + to_string(components()->get(i));
  }
  return versionStr;
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
  F(PackageDependency, externTraits_)         \
  F(PackageDependency, linkedTraits_)         \
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
                                     BlockArray<Trait>* externTraits,
                                     BlockArray<Trait>* linkedTraits,
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
      externTraits_(this, externTraits),
      linkedTraits_(this, linkedTraits),
      externTypeParameters_(this, externTypeParameters),
      linkedTypeParameters_(this, linkedTypeParameters),
      externMethods_(this, externMethods) {
  ASSERT(minVersion == nullptr || maxVersion == nullptr ||
         minVersion->compare(maxVersion) <= 0);
  ASSERT(linkedGlobals == nullptr || externGlobals->length() == linkedGlobals->length());
  ASSERT(linkedFunctions == nullptr || externFunctions->length() == linkedFunctions->length());
  ASSERT(linkedClasses == nullptr || externClasses->length() == linkedClasses->length());
  ASSERT(linkedTraits == nullptr || externTraits->length() == linkedTraits->length());
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
    const Handle<BlockArray<Trait>>& externTraits,
    const Handle<BlockArray<Trait>>& linkedTraits,
    const Handle<BlockArray<TypeParameter>>& externTypeParameters,
    const Handle<BlockArray<TypeParameter>>& linkedTypeParameters,
    const Handle<BlockArray<Function>>& externMethods) {
  RETRY_WITH_GC(heap, return Local<PackageDependency>(new(heap) PackageDependency(
      *name, minVersion.getOrNull(), maxVersion.getOrNull(),
      *externGlobals, linkedGlobals.getOrNull(),
      *externFunctions, linkedFunctions.getOrNull(),
      *externClasses, linkedClasses.getOrNull(),
      *externTraits, linkedTraits.getOrNull(),
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
                                                   length_t traitCount,
                                                   length_t typeParameterCount,
                                                   length_t methodCount) {
  auto externGlobals = BlockArray<Global>::create(heap, globalCount);
  auto externFunctions = BlockArray<Function>::create(heap, functionCount);
  auto externClasses = BlockArray<Class>::create(heap, classCount);
  auto externTraits = BlockArray<Trait>::create(heap, traitCount);
  auto externTypeParameters = BlockArray<TypeParameter>::create(heap, typeParameterCount);
  auto externMethods = BlockArray<Function>::create(heap, methodCount);
  RETRY_WITH_GC(heap, return Local<PackageDependency>(new(heap) PackageDependency(
      *name, minVersion.getOrNull(), maxVersion.getOrNull(),
      *externGlobals, nullptr,
      *externFunctions, nullptr,
      *externClasses, nullptr,
      *externTraits, nullptr,
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

  auto nameStr = String::substring(depString, 0, colonPos);
  auto name = Name::fromString(heap, nameStr, Name::PACKAGE_NAME);
  if (!name)
    return false;

  auto dashPos = depString->find('-', colonPos);
  auto minEnd = dashPos != kIndexNotSet ? dashPos : depString->length();
  Local<PackageVersion> minVersion;
  if (minEnd - colonPos > 1) {
    auto minVersionStr = String::substring(depString, colonPos + 1, minEnd);
    minVersion = PackageVersion::fromString(heap, minVersionStr);
    if (!minVersion)
      return false;
  }

  Local<PackageVersion> maxVersion;
  if (dashPos == kIndexNotSet) {
    maxVersion = minVersion;
  } else if (dashPos < depString->length() - 1) {
    auto maxVersionStr = String::substring(depString, dashPos + 1, depString->length());
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


void PackageDependency::setLinkedTraits(BlockArray<Trait>* linkedTraits) {
  ASSERT(externTraits_.get()->length() == linkedTraits->length());
  linkedTraits_.set(this, linkedTraits);
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
     << "\n  externTraits: " << brief(dep->externTraits())
     << "\n  linkedTraits: " << brief(dep->linkedTraits())
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


Local<Global> Loader::readGlobal(DefnId id) {
  auto name = readName();
  auto sourceName = readSourceName();
  auto flags = readValue<u32>();
  auto type = readType();
  return Global::create(heap(), id, name, sourceName, flags, type);
}


Local<Function> Loader::readFunction(DefnId id) {
  Local<Name> name;
  Local<String> sourceName;
  u32 flags;
  Local<BlockArray<TypeParameter>> typeParameters;
  Local<Type> returnType;
  Local<BlockArray<Type>> parameterTypes;
  Local<ObjectTypeDefn> definingClass;
  Local<BlockArray<Function>> overrides;
  readFunctionHeader(
      &name, &sourceName, &flags, &typeParameters,
      &returnType, &parameterTypes, &definingClass, &overrides);

  word_t localsSize = kNotSet;
  Local<LengthArray> blockOffsets;
  vector<u8> instructions;
  if ((flags & (ABSTRACT_FLAG | EXTERN_FLAG | NATIVE_FLAG)) == 0) {
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

  auto func = Function::create(heap(), id, name, sourceName, flags, typeParameters,
                               returnType, parameterTypes, definingClass,
                               localsSize, instructions, blockOffsets,
                               package_, overrides, nullptr);
  return func;
}


void Loader::readFunctionHeader(Local<Name>* name,
                                Local<String>* sourceName,
                                u32* flags,
                                Local<BlockArray<TypeParameter>>* typeParameters,
                                Local<Type>* returnType,
                                Local<BlockArray<Type>>* parameterTypes,
                                Local<ObjectTypeDefn>* definingClass,
                                Local<BlockArray<Function>>* overrides) {
  *name = readName();
  *sourceName = readSourceName();
  *flags = readValue<u32>();

  *typeParameters = readBlockList<TypeParameter>([this]() {
    auto index = readLengthVbn();
    return getTypeParameter(index);
  });

  *returnType = readType();
  *parameterTypes = readBlockList<Type>(&Loader::readType);

  if ((EXTERN_FLAG & *flags) == 0) {
    *definingClass = readDefiningClass();
  } else {
    definingClass->clear();
  }

  if ((OVERRIDE_FLAG & *flags) != 0) {
    *overrides = readBlockList<Function>([this]() { return readIdAndGetMethod(); });
  } else {
    overrides->clear();
  }
}


void Loader::readClass(const Local<Class>& clas) {
  auto name = readName();
  auto sourceName = readSourceName();
  auto flags = readValue<u32>();

  auto typeParameters = readBlockList<TypeParameter>([this]() {
    auto index = readLengthVbn();
    return getTypeParameter(index);
  });

  auto supertypes = readBlockList<Type>(&Loader::readType);

  auto fieldCount = readLengthVbn();
  auto fields = BlockArray<Field>::create(heap(), fieldCount);
  for (length_t i = 0; i < fieldCount; i++) {
    auto field = readField();
    fields->set(i, *field);
  }

  auto constructors = readBlockList<Function>([this]() { return readIdAndGetMethod(); });
  auto methods = readBlockList<Function>([this]() { return readIdAndGetMethod(); });

  auto traits = readTraitTable();

  auto elementType = readOption<Type>(&Loader::readType);

  clas->setName(*name);
  clas->setSourceName(sourceName.getOrNull());
  clas->setFlags(flags);
  clas->setTypeParameters(*typeParameters);
  clas->setSupertypes(*supertypes);
  clas->setFields(*fields);
  clas->setConstructors(*constructors);
  clas->setMethods(*methods);
  clas->setTraits(traits.getOrNull());
  clas->setElementType(elementType.getOrNull());
}


void Loader::readTrait(const Local<Trait>& trait) {
  auto name = readName();
  auto sourceName = readSourceName();
  auto flags = readValue<u32>();

  auto typeParams = readBlockList<TypeParameter>([this]() {
    auto index = readLengthVbn();
    return getTypeParameter(index);
  });

  auto supertypes = readBlockList<Type>(&Loader::readType);
  auto methods = readBlockList<Function>([this]() { return readIdAndGetMethod(); });

  trait->setName(*name);
  trait->setSourceName(sourceName.getOrNull());
  trait->setFlags(flags);
  trait->setTypeParameters(*typeParams);
  trait->setSupertypes(*supertypes);
  trait->setMethods(*methods);
}


Local<Field> Loader::readField() {
  auto name = readName();
  auto sourceName = readSourceName();
  auto flags = readValue<u32>();
  auto type = readType();
  auto offset = static_cast<u32>(kNotSet);
  auto field = Field::create(heap(), name, sourceName, flags, type, offset);
  return field;
}


void Loader::readTypeParameter(const Local<TypeParameter>& param) {
  auto name = readName();
  auto sourceName = readSourceName();
  auto flags = readValue<u32>();
  auto upperBound = readType();
  auto lowerBound = readType();
  param->setName(*name);
  param->setSourceName(sourceName.getOrNull());
  param->setFlags(flags);
  param->setUpperBound(*upperBound);
  param->setLowerBound(*lowerBound);
}


Local<Type> Loader::readType() {
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
    if (depIndex == kBuiltinPackageId) {
      if (!roots()->isValidBuiltinClassId(static_cast<BuiltinId>(defnIndex))) {
        throw Error("invalid builtin class id");
      }
      clas = handle(roots()->getBuiltinClass(static_cast<BuiltinId>(defnIndex)));
    } else if (depIndex == kLocalPackageId) {
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
      clas = handle(isLinked_ ?
          dep->linkedClasses()->get(defnIndex) :
          dep->externClasses()->get(defnIndex));
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

    return Type::create(heap(), clas, typeArgs, flags);
  } else if (form == Type::TRAIT_TYPE) {
    auto depIndex = readVbn();
    auto defnIndex = readVbn();

    Local<Trait> trait;
    if (depIndex == kBuiltinPackageId) {
      if (!roots()->isValidBuiltinTraitId(static_cast<BuiltinId>(defnIndex))) {
        throw Error("invalid builtin trait id");
      }
      trait = handle(roots()->getBuiltinTrait(static_cast<BuiltinId>(defnIndex)));
    } else if (depIndex == kLocalPackageId) {
      if (defnIndex < 0 || defnIndex >= package_->traits()->length()) {
        throw Error("invalid definition index");
      }
      trait = handle(package_->traits()->get(defnIndex));
    } else {
      if (depIndex < 0 || depIndex >= package_->dependencies()->length()) {
        throw Error("invalid package index");
      }
      auto dep = handle(package_->dependencies()->get(depIndex));
      if (defnIndex < 0 || defnIndex >= dep->externTraits()->length()) {
        throw Error("invalid extern trait index");
      }
      trait = handle(isLinked_ ?
          dep->linkedTraits()->get(defnIndex) :
          dep->externTraits()->get(defnIndex));
    }

    auto typeArgCount = readLengthVbn();
    vector<Local<Type>> typeArgs;
    typeArgs.reserve(typeArgCount);
    for (length_t i = 0; i < typeArgCount; i++) {
      auto typeArg = readType();
      typeArgs.push_back(typeArg);
    }

    return Type::create(heap(), trait, typeArgs, flags);
  } else if (form == Type::VARIABLE_TYPE) {
    auto depIndex = readVbn();
    auto defnIndex = readVbn();

    Local<TypeParameter> param;
    if (depIndex == kLocalPackageId) {
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
      param = handle(isLinked_ ?
          dep->linkedTypeParameters()->get(defnIndex) :
          dep->externTypeParameters()->get(defnIndex));
    }

    return Type::create(heap(), param, flags);
  } else {
    ASSERT(form == Type::EXISTENTIAL_TYPE);
    auto length = readLengthVbn();
    vector<Local<TypeParameter>> variables;
    variables.reserve(length);
    for (length_t i = 0; i < length; i++) {
      auto depIndex = readVbn();
      auto defnIndex = readVbn();

      Local<TypeParameter> param;
      if (depIndex == kLocalPackageId) {
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
        param = handle(isLinked_ ?
            dep->linkedTypeParameters()->get(defnIndex) :
            dep->externTypeParameters()->get(defnIndex));
      }
      variables.push_back(param);
    }
    auto innerType = readType();
    return Type::create(heap(), variables, innerType);
  }
}


template <typename T>
T Loader::readValue() {
  T value;
  stream_.read(reinterpret_cast<char*>(&value), sizeof(value));
  return value;
}


Local<Name> Loader::readName() {
  auto components = readBlockList<String>(&Loader::readStringId);
  return Name::create(heap(), components);
}


Local<ObjectTypeDefn> Loader::readDefiningClass() {
  auto code = readVbn();
  if (code == 0) {
    return Local<ObjectTypeDefn>();
  } else if (code == 1) {
    auto index = readLengthVbn();
    if (index < 0 || index >= package_->classes()->length()) {
      throw Error("defining class index out of bounds");
    }
    return handle(package_->classes()->get(index));
  } else if (code == 2) {
    auto index = readLengthVbn();
    if (index < 0 || index >= package_->traits()->length()) {
      throw Error("defining trait index out of bounds");
    }
    return handle(package_->traits()->get(index));
  } else {
    throw Error("invalid defining class code");
  }
}


Local<String> Loader::readSourceName() {
  return readOption<String>(&Loader::readStringId);
}


Local<String> Loader::readStringId() {
  auto index = readLengthVbn();
  if (index >= package_->strings()->length()) {
    throw Error("string index out of bounds");
  }
  return handle(package_->strings()->get(index));
}


template <typename T>
Local<BlockArray<T>> Loader::readBlockList(Local<T> (Loader::*reader)(void)) {
  return readBlockList<T>([this, reader]() { return (this->*reader)(); });
}


template <typename T, typename Reader>
Local<BlockArray<T>> Loader::readBlockList(Reader reader) {
  HandleScope handleScope(vm());
  auto length = readLengthVbn();
  auto array = BlockArray<T>::create(heap(), length);
  for (length_t i = 0; i < length; i++) {
    auto elem = reader();
    array->set(i, *elem);
  }
  return handleScope.escape(*array);
}


template <typename T>
Local<T> Loader::readOption(Local<T> (Loader::*reader)(void)) {
  return readOption<T>([this, reader]() { return (this->*reader)(); });
}


template <typename T, typename Reader>
Local<T> Loader::readOption(Reader reader) {
  auto code = readVbn();
  if (code == 0) {
    return Local<T>();
  } else if (code == 1) {
    return reader();
  } else {
    throw Error("invalid option code");
  }
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


PackageDefnId Loader::readDefnIdVbns() {
  auto packageId = readIdVbn();
  auto index = readLengthVbn();
  PackageDefnId id = { packageId, index };
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
  return DependencyLoader(loadState_, stream_, package_, dep);
}


Persistent<Package> PackageLoader::load() {
  AllowAllocationScope allowAllocation(heap(), true);
  HandleScope handleScope(vm());
  try {
    auto magic = readValue<u32>();
    if (magic != kMagic)
      throw Error("package file is corrupt");
    auto majorVersion = readValue<u16>();
    auto minorVersion = readValue<u16>();
    if (majorVersion != 0 || minorVersion != 20)
      throw Error("package file has wrong format version");

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
      DefnId id(DefnId::CLASS, package_->id(), i);
      auto clas = Class::create(heap(), id);
      clas->setPackage(*package_);
      classArray->set(i, *clas);
    }
    package_->setClasses(*classArray);

    auto traitCount = readLength();
    auto traitArray = BlockArray<Trait>::create(heap(), traitCount);
    for (length_t i = 0; i < traitCount; i++) {
      // We pre-allocate traits so Types can refer to them.
      DefnId id(DefnId::TRAIT, package_->id(), i);
      auto trait = Trait::create(heap(), id);
      trait->setPackage(*package_);
      traitArray->set(i, *trait);
    }
    package_->setTraits(*traitArray);

    auto typeParameterCount = readLength();
    auto typeParametersArray =
        BlockArray<TypeParameter>::create(heap(), typeParameterCount);
    for (length_t i = 0; i < typeParameterCount; i++) {
      // Type parameters are also pre-allocated.
      DefnId id(DefnId::TYPE_PARAMETER, package_->id(), i);
      auto param = TypeParameter::create(heap(), id);
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

    for (length_t i = 0; i < depCount; i++) {
      auto dep = handle(depArray->get(i));
      DependencyLoader loader = createDependencyLoader(dep);
      loader.readDependencyBody();
    }

    for (length_t i = 0; i < depCount; i++) {
      auto dep = handle(depArray->get(i));
      auto depPackage = loadDependency(dep);
      dep->setPackage(*depPackage);
      auto depClasses = handle(dep->externClasses());
      for (length_t j = 0; j < depClasses->length(); j++) {
        depClasses->get(j)->setPackage(*depPackage);
      }
      auto depTraits = handle(dep->externTraits());
      for (length_t j = 0; j < depTraits->length(); j++) {
        depTraits->get(j)->setPackage(*depPackage);
      }
    }
    Package::link(package_);
    isLinked_ = true;

    for (length_t i = 0; i < globalCount; i++) {
      DefnId id(DefnId::GLOBAL, package_->id(), i);
      auto global = readGlobal(id);
      globalArray->set(i, *global);
    }

    bool hasNativeFunctions = false;
    for (length_t i = 0; i < functionCount; i++) {
      DefnId id(DefnId::FUNCTION, package_->id(), i);
      auto function = readFunction(id);
      functionArray->set(i, *function);
      hasNativeFunctions |= (NATIVE_FLAG & function->flags()) != 0;
    }

    for (length_t i = 0; i < classCount; i++) {
      auto clas = handle(classArray->get(i));
      readClass(clas);
    }

    for (length_t i = 0; i < traitCount; i++) {
      auto trait = handle(traitArray->get(i));
      readTrait(trait);
    }

    for (length_t i = 0; i < typeParameterCount; i++) {
      auto param = handle(typeParametersArray->get(i));
      readTypeParameter(param);
    }

    package_->setNativeFunctionSearchOrder(*loadState_->nativeFunctionSearchOrder);
    if (hasNativeFunctions && package_->mayLoadFunctionsFromNativeLibrary()) {
      loadNativeLibrary();
    }
  } catch (istream::failure exn) {
    throw Error("error reading package");
  }

  loadState_->packages.push_back(package_);
  ASSERT(*loadState_->loadingPackages.back() == *package_);
  loadState_->loadingPackages.pop_back();

  return package_;
}


Local<TypeParameter> PackageLoader::getTypeParameter(length_t index) const {
  if (index >= package()->typeParameters()->length()) {
    throw Error("type parameter index out of range");
  }
  return handle(package()->typeParameters()->get(index));
}


Local<Function> PackageLoader::readIdAndGetMethod() {
  PackageDefnId id = readDefnIdVbns();
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


Local<TraitTable> PackageLoader::readTraitTable() {
  auto traitCount = readLengthVbn();
  auto capacity = recommendHashTableCapacity(traitCount);
  auto traitTable = TraitTable::create(heap(), capacity);
  for (length_t i = 0; i < traitCount; i++) {
    Local<Trait> trait;
    auto packageId = readIdVbn();
    auto defnIndex = readLengthVbn();
    if (packageId == kBuiltinPackageId) {
      trait = handle(vm()->roots()->getBuiltinTrait(indexToBuiltinId(defnIndex)));
    } else {
      Local<BlockArray<Trait>> traits;
      if (packageId == kLocalPackageId) {
        traits = handle(package()->traits());
      } else if (0 <= packageId &&
                 static_cast<length_t>(packageId) < package()->dependencies()->length()) {
        traits = handle(package()->dependencies()->get(packageId)->linkedTraits());
      } else {
        throw Error("invalid package id");
      }
      if (defnIndex >= traits->length()) {
        throw Error("invalid trait index");
      }
      trait = handle(traits->get(defnIndex));
    }

    auto methodCount = readLengthVbn();
    auto methods = BlockArray<Function>::create(heap(), methodCount);
    for (length_t j = 0; j < methodCount; j++) {
      auto method = readIdAndGetMethod();
      methods->set(j, *method);
    }
    traitTable->add(TraitTableElement(*trait, *methods));
  }
  return traitTable;
}


Persistent<Package> PackageLoader::loadDependency(const Handle<PackageDependency>& dep) {
  auto depName = handle(dep->name());
  Persistent<Package> depPackage = vm()->findPackage(depName);
  if (depPackage && !dep->isSatisfiedBy(*depPackage)) {
    throw Error("package depends on loaded package with wrong version");
  }
  if (!depPackage) {
    for (auto& loadingPackage : loadState_->loadingPackages) {
      if (depName->equals(loadingPackage->name())) {
        throw Error("circular package dependency");
      }
    }
    for (auto& loadedPackage : loadState_->packages) {
      if (depName->equals(loadedPackage->name())) {
        depPackage = loadedPackage;
        break;
      }
    }
    if (depPackage && !dep->isSatisfiedBy(*depPackage)) {
      throw Error("package depends on loaded package with wrong version");
    }
    if (!depPackage) {
      auto fileName = vm()->searchForPackage(dep);
      if (fileName.empty()) {
        throw Error("missing package dependency");
      }
      auto dirName = pathDirName(fileName);
      ifstream file(fileName.c_str(), ios::binary);
      file.exceptions(ios::failbit | ios::badbit | ios::eofbit);
      PackageLoader loader(loadState_, file, dirName);
      depPackage = loader.load();
      auto pos = file.tellg();
      file.seekg(0, ios::end);
      if (pos != file.tellg()) {
        throw Error("garbage at end of package file");
      }
      if (!dep->isSatisfiedBy(*depPackage)) {
        throw Error("invalid package dependency");
      }
    }
  }
  return depPackage;
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
  auto traitCount = readLengthVbn();
  auto methodCount = readLengthVbn();
  auto typeParameterCount = readLengthVbn();

  auto dep = PackageDependency::create(heap(), name, minVersion, maxVersion,
                                       globalCount, functionCount,
                                       classCount, traitCount,
                                       typeParameterCount, methodCount);

  // We need to pre-allocate methods, classes, traits, type parameters so that other definitions
  // and types can refer to them. Normally, functions can't be preallocated since they have
  // variable size, but these are extern and don't have code.
  auto externClasses = handle(dep->externClasses());
  for (length_t i = 0; i < classCount; i++) {
    auto c = Class::create(heap(), DefnId::invalidClass());
    externClasses->set(i, *c);
  }

  auto externTraits = handle(dep->externTraits());
  for (length_t i = 0; i < traitCount; i++) {
    auto t = Trait::create(heap(), DefnId::invalidTrait());
    externTraits->set(i, *t);
  }

  auto externMethods = handle(dep->externMethods());
  for (length_t i = 0; i < methodCount; i++) {
    auto f = Function::create(heap(), DefnId::invalidFunction());
    externMethods->set(i, *f);
  }

  auto externTypeParameters = handle(dep->externTypeParameters());
  for (length_t i = 0; i < typeParameterCount; i++) {
    auto p = TypeParameter::create(heap(), DefnId::invalidTypeParameter());
    externTypeParameters->set(i, *p);
  }

  return dep;
}


void PackageLoader::loadNativeLibrary() {
  if (dirName_.empty()) {
    throw Error("cannot load native library; directory is unknown");
  }
  auto packageNameStr = package_->name()->toStlString();
  auto packageVersionStr = package_->version()->toStlString();
  auto libName = string(kNativeLibraryPrefix) + packageNameStr + "-" +
      packageVersionStr + kNativeLibrarySuffix;
  auto libPath = pathJoin(dirName_, libName);
  auto lib = codeswitch::internal::loadNativeLibrary(libPath);
  package_->setNativeLibrary(lib);
}


void DependencyLoader::readExternFunction(const Handle<Function>& func) {
  Local<Name> name;
  Local<String> sourceName;
  u32 flags;
  Local<BlockArray<TypeParameter>> typeParameters;
  Local<Type> returnType;
  Local<BlockArray<Type>> parameterTypes;
  Local<ObjectTypeDefn> definingClass;
  Local<BlockArray<Function>> overrides;

  readFunctionHeader(
      &name, &sourceName, &flags, &typeParameters,
      &returnType, &parameterTypes, &definingClass, &overrides);
  func->setName(*name);
  func->setSourceName(sourceName.getOrNull());
  func->setFlags(flags);
  func->setTypeParameters(*typeParameters);
  func->setReturnType(*returnType);
  func->setParameterTypes(*parameterTypes);
  func->setDefiningClass(definingClass.getOrNull());
  func->setOverrides(overrides.getOrNull());

  if ((flags & EXTERN_FLAG) == 0) {
    throw Error("invalid function");
  }
}


void DependencyLoader::readDependencyBody() {
  auto globals = handle(dep_->externGlobals());
  for (length_t i = 0; i < globals->length(); i++) {
    auto g = readGlobal(DefnId::invalidGlobal());
    if ((g->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency global is not extern");
    globals->set(i, *g);
  }

  auto functions = handle(dep_->externFunctions());
  for (length_t i = 0; i < functions->length(); i++) {
    auto f = readFunction(DefnId::invalidFunction());
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

  auto traits = handle(dep_->externTraits());
  for (length_t i = 0; i < traits->length(); i++) {
    auto t = handle(traits->get(i));
    readTrait(t);
    if ((t->flags() & EXTERN_FLAG) == 0)
      throw Error("dependency trait is not extern");
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


Local<TraitTable> DependencyLoader::readTraitTable() {
  return Local<TraitTable>();
}

}
}

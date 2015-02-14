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
  F(Package, dependencySpecs_)  \
  F(Package, dependencies_)     \
  F(Package, strings_)          \
  F(Package, globals_)          \
  F(Package, functions_)        \
  F(Package, classes_)          \
  F(Package, typeParameters_)   \

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
    : Block(PACKAGE_BLOCK_TYPE),
      flags_(0),
      dependencySpecs_(this, reinterpret_cast<BlockArray<PackageDependency>*>(
          vm->roots()->emptyBlockArray())),
      dependencies_(this, reinterpret_cast<BlockArray<Package>*>(
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


ostream& operator << (ostream& os, const Package* pkg) {
  os << brief(pkg)
     << "\n  name: " << brief(pkg->name())
     << "\n  version: " << brief(pkg->version())
     << "\n  strings: " << brief(pkg->strings())
     << "\n  functions: " << brief(pkg->functions())
     << "\n  globals: " << brief(pkg->globals())
     << "\n  classes: " << brief(pkg->classes())
     << "\n  type parameters: " << brief(pkg->typeParameters())
     << "\n  entry function index: " << pkg->entryFunctionIndex()
     << "\n  init function index: " << pkg->initFunctionIndex();
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


#define PACKAGE_DEPENDENCY_POINTER_LIST(F) \
  F(PackageDependency, name_)              \
  F(PackageDependency, minVersion_)        \
  F(PackageDependency, maxVersion_)        \

DEFINE_POINTER_MAP(PackageDependency, PACKAGE_DEPENDENCY_POINTER_LIST)

#undef PACKAGE_DEPENDENCY_POINTER_LIST


PackageDependency::PackageDependency(PackageName* name,
                                     PackageVersion* minVersion,
                                     PackageVersion* maxVersion)
    : Block(PACKAGE_DEPENDENCY_BLOCK_TYPE),
      name_(this, name),
      minVersion_(this, minVersion),
      maxVersion_(this, maxVersion) { }


Local<PackageDependency> PackageDependency::create(Heap* heap,
                                                   const Handle<PackageName>& name,
                                                   const Handle<PackageVersion>& minVersion,
                                                   const Handle<PackageVersion>& maxVersion) {
  RETRY_WITH_GC(heap, return Local<PackageDependency>(new(heap) PackageDependency(
      *name, minVersion.getOrNull(), maxVersion.getOrNull())));
}


Local<PackageDependency> PackageDependency::fromString(Heap* heap,
                                                       const Handle<String>& depString) {
  auto components = String::split(heap, depString, ':');
  if (!inRange<length_t>(components->length(), 1, 3))
    return Local<PackageDependency>();

  auto name = PackageName::fromString(heap, handle(components->get(0)));
  if (!name)
    return Local<PackageDependency>();

  Local<PackageVersion> minVersion, maxVersion;
  if (components->length() >= 2 && !components->get(1)->isEmpty()) {
    minVersion = PackageVersion::fromString(heap, handle(components->get(1)));
    if (!minVersion)
      return Local<PackageDependency>();
  }
  if (components->length() >= 3 && !components->get(2)->isEmpty()) {
    maxVersion = PackageVersion::fromString(heap, handle(components->get(2)));
    if (!maxVersion)
      return Local<PackageDependency>();
  }

  return create(heap, name, minVersion, maxVersion);
}


bool PackageDependency::equals(const PackageDependency* other) const {
  return name()->equals(other->name()) &&
         ((!minVersion_ && !other->minVersion_) ||
          (minVersion()->equals(other->minVersion()))) &&
         ((!maxVersion_ && !other->maxVersion_) ||
          (maxVersion()->equals(other->maxVersion())));
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
     << "\n  maxVersion: " << brief(dep->maxVersion());
  return os;
}


Local<Package> PackageLoader::load() {
  HandleScope handleScope(vm_);
  try {
    auto magic = readValue<u32>();
    if (magic != kMagic)
      throw Error("package file is corrupt");
    auto majorVersion = readValue<u16>();
    auto minorVersion = readValue<u16>();
    if (majorVersion != 0 || minorVersion != 14)
      throw Error("package file has wrong format version");

    package_ = handleScope.escape(*Package::create(heap()));

    auto flags = readValue<u64>();
    package_->setFlags(flags);

    auto depCount = readLength();
    auto depSpecArray = BlockArray<PackageDependency>::create(heap(), depCount);
    package_->setDependencySpecs(*depSpecArray);
    auto depArray = BlockArray<Package>::create(heap(), depCount);
    package_->setDependencies(*depArray);

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
      auto depStr = readString();
      auto dep = PackageDependency::fromString(heap(), depStr);
      if (!dep)
        throw Error("invalid package dependency");
      depSpecArray->set(i, *dep);
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
      auto clas = handle(block_cast<Class>(classArray->get(i)));
      readClass(clas);
    }
    for (length_t i = 0; i < typeParameterCount; i++) {
      auto param = handle(block_cast<TypeParameter>(typeParametersArray->get(i)));
      readTypeParameter(param);
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
  if ((flags & ABSTRACT_FLAG) == 0) {
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

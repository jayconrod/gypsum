// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "package-inl.h"

#include <algorithm>
#include <fstream>
#include <memory>
#include <string>
#include <sstream>
#include <vector>
#include <cstdio>
#include "array.h"
#include "block-inl.h"
#include "bytecode.h"
#include "error.h"
#include "field.h"
#include "function-inl.h"
#include "handle-inl.h"
#include "heap-inl.h"
#include "memory-inl.h"
#include "string-inl.h"
#include "type-inl.h"

namespace codeswitch {
namespace internal {

static Handle<String> readString(VM* vm, istream& stream);
static Handle<Function> readFunction(VM* vm, istream& stream, Handle<Package> package);
static void readClass(VM* vm, istream& stream, Handle<Package> package, Handle<Class> clas);
static Handle<Field> readField(VM* vm, istream& stream, Handle<BlockArray> classes);
static Handle<Type> readType(VM* vm, istream& stream, Handle<BlockArray> classes);
template <typename T>
static T readValue(istream& stream);
static vector<u8> readData(istream& stream, word_t size);
static i64 readVbn(istream& stream);
static word_t readWordVbn(istream& stream);

Package* Package::tryAllocate(Heap* heap) {
  Package* pkg = reinterpret_cast<Package*>(heap->allocateRaw(Package::kSize));
  if (pkg == nullptr)
    return nullptr;

  pkg->setMeta(PACKAGE_BLOCK_TYPE);
  return pkg;
}


Handle<Package> Package::allocate(Heap* heap) {
  DEFINE_ALLOCATION(heap, Package, tryAllocate(heap))
}


void Package::printPackage(FILE* out) {
  printf("Package @%p\n", reinterpret_cast<void*>(this));
  printf("  flags: " WFX "\n", flags());
  printf("  entry function: " WFD "\n", entryFunctionIndex());
  printf("  functions:\n");
  for (word_t i = 0, n = functions()->length(); i < n; i++) {
    printf("    %p\n", reinterpret_cast<void*>(getFunction(i)));
  }
}


Handle<Package> Package::loadFromFile(VM* vm, const char* fileName) {
  ifstream file(fileName, ios::binary);
  if (!file.good())
    throw Error("could not open package file");
  file.exceptions(ios::failbit | ios::badbit | ios::eofbit);
  Handle<Package> package = loadFromStream(vm, file);
  auto pos = file.tellg();
  file.seekg(0, ios::end);
  if (pos != file.tellg())
    throw Error("garbage found at end of file");
  return package;
}


Handle<Package> Package::loadFromBytes(VM* vm, const u8* bytes, word_t size) {
  stringstream stream(string(reinterpret_cast<const char*>(bytes), size));
  stream.exceptions(ios::failbit | ios::badbit | ios::eofbit);
  Handle<Package> package = loadFromStream(vm, stream);
  auto pos = stream.tellg();
  stream.seekg(0, ios::end);
  if (pos != stream.tellg())
    throw Error("garbage found at end of bytes");
  return package;
}



Handle<Package> Package::loadFromStream(VM* vm, istream& stream) {
  unique_ptr<Page> page;
  Handle<Package> package;
  try {
    auto magic = readValue<u32>(stream);
    if (magic != kMagic)
      throw Error("package file is corrupt");
    auto majorVersion = readValue<u16>(stream);
    auto minorVersion = readValue<u16>(stream);
    if (majorVersion != 0 || minorVersion != 4)
      throw Error("package file has wrong format version");

    package = Package::allocate(vm->heap());

    auto flags = readValue<u64>(stream);
    package->setFlags(flags);

    auto stringCount = readValue<word_t>(stream);
    auto stringArray = BlockArray::allocate(vm->heap(), stringCount, false, nullptr);
    package->setStrings(*stringArray);

    auto functionCount = readValue<word_t>(stream);
    auto functionArray = BlockArray::allocate(vm->heap(), functionCount,
                                              false, nullptr);
    package->setFunctions(*functionArray);

    auto classCount = readValue<word_t>(stream);
    auto classArray = BlockArray::allocate(vm->heap(), classCount, false, nullptr);
    for (word_t i = 0; i < classCount; i++) {
      // We pre-allocate classes so Types we read can refer to them.
      auto clas = Class::allocate(vm->heap());
      clas->setPackage(*package);
      classArray->set(i, *clas);
    }
    package->setClasses(*classArray);

    auto entryFunctionIndex = readValue<word_t>(stream);
    package->setEntryFunctionIndex(entryFunctionIndex);

    for (word_t i = 0; i < stringCount; i++) {
      auto string = readString(vm, stream);
      stringArray->set(i, *string);
    }
    for (word_t i = 0; i < functionCount; i++) {
      auto function = readFunction(vm, stream, package);
      function->setPackage(*package);
      functionArray->set(i, *function);
    }
    for (word_t i = 0; i < classCount; i++) {
      readClass(vm, stream, package, Handle<Class>(Class::cast(classArray->get(i))));
    }
  } catch (istream::failure exn) {
    throw Error("error reading package");
  }

  return package;
}


static Handle<String> readString(VM* vm, istream& stream) {
  auto length = readWordVbn(stream);
  auto size = readWordVbn(stream);
  vector<u8> utf8Chars = readData(stream, size);
  return String::fromUtf8String(vm->heap(), utf8Chars.data(), length, size);
}


static Handle<Function> readFunction(VM* vm, istream& stream, Handle<Package> package) {
  auto classes = handle(package->classes());
  u32 flags;
  stream.read(reinterpret_cast<char*>(&flags), sizeof(flags));
  auto returnType = readType(vm, stream, classes);
  auto parameterCount = readWordVbn(stream);
  auto types = BlockArray::allocate(vm->heap(), parameterCount + 1);
  types->set(0, *returnType);
  for (word_t i = 0; i < parameterCount; i++) {
    types->set(i + 1, *readType(vm, stream, classes));
  }

  auto localsSize = readWordVbn(stream);

  auto instructionsSize = readWordVbn(stream);
  vector<u8> instructions = readData(stream, instructionsSize);

  auto blockOffsetCount = readWordVbn(stream);
  auto blockOffsets = WordArray::allocate(vm->heap(), blockOffsetCount);
  for (word_t i = 0; i < blockOffsetCount; i++) {
    auto offset = readWordVbn(stream);
    blockOffsets->set(i, offset);
  }

  auto function = Function::allocate(vm->heap(), instructionsSize);
  function->initialize(flags, *types, localsSize, instructions,
                       *blockOffsets, *package, nullptr);
  return function;
}


static void readClass(VM* vm, istream& stream, Handle<Package> package, Handle<Class> clas) {
  auto classes = handle(package->classes());
  u32 flags;
  stream.read(reinterpret_cast<char*>(&flags), sizeof(flags));
  auto supertype = readType(vm, stream, classes);

  auto fieldCount = readWordVbn(stream);
  auto fields = BlockArray::allocate(vm->heap(), fieldCount);
  for (word_t i = 0; i < fieldCount; i++) {
    auto field = readField(vm, stream, classes);
    fields->set(i, *field);
  }

  auto constructorCount = readWordVbn(stream);
  auto constructors = WordArray::allocate(vm->heap(), constructorCount);
  for (word_t i = 0; i < constructorCount; i++) {
    word_t id = readWordVbn(stream);
    constructors->set(i, id);
  }

  auto methodCount = readWordVbn(stream);
  auto methods = WordArray::allocate(vm->heap(), methodCount);
  for (word_t i = 0; i < methodCount; i++) {
    word_t id = readWordVbn(stream);
    methods->set(i, id);
  }

  clas->initialize(flags, *supertype, *fields, nullptr,
                   *constructors, *methods, *package, nullptr);
}


static Handle<Field> readField(VM* vm, istream& stream, Handle<BlockArray> classes) {
  u32 flags;
  stream.read(reinterpret_cast<char*>(&flags), sizeof(flags));
  auto type = readType(vm, stream, classes);
  auto field = Field::allocate(vm->heap());
  field->initialize(flags, *type);
  return field;
}


static Handle<Type> readType(VM* vm, istream& stream, Handle<BlockArray> classes) {
  auto heap = vm->heap();
  auto flags = static_cast<Type::Flags>(readWordVbn(stream));
  if (flags & ~Type::kFlagsMask)
    throw Error("invalid type flags");
  auto code = readVbn(stream);
  if (flags == Type::NO_FLAGS && isBuiltinId(code)) {
    return handle(vm->roots()->getBuiltinType(static_cast<BuiltinId>(code)));
  } else if (flags == Type::NULLABLE_FLAG && code == BUILTIN_NOTHING_CLASS_ID) {
    return handle(vm->roots()->nullType());
  } else {
    Handle<Class> clas;
    if (isBuiltinId(code)) {
      clas = handle(vm->roots()->getBuiltinClass(static_cast<BuiltinId>(code)));
    } else {
      clas = handle(Class::cast(classes->get(code)));
    }
    auto ty = Type::allocate(heap, 1);
    ty->initialize(*clas, flags);
    return ty;
  }
}


template <typename T>
static T readValue(istream& stream) {
  T value;
  stream.read(reinterpret_cast<char*>(&value), sizeof(value));
  return value;
}


static vector<u8> readData(istream& stream, word_t size) {
  vector<u8> buffer(size);
  stream.read(reinterpret_cast<char*>(buffer.data()), size);
  return buffer;
}


static i64 readVbn(istream& stream) {
  i64 n = 0;
  i64 shift = 0;
  bool more;
  do {
    u8 b;
    stream.read(reinterpret_cast<char*>(&b), 1);
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


static word_t readWordVbn(istream& stream) {
  i64 n = readVbn(stream);
  word_t w = static_cast<word_t>(n);
  if (static_cast<i64>(w) != n) {
    throw Error("could not read number from stream");
  }
  return w;
}

}
}

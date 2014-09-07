// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "package.h"

#include <algorithm>
#include <fstream>
#include <memory>
#include <string>
#include <sstream>
#include <vector>
#include <cstdio>
#include "array.h"
#include "block.h"
#include "bytecode.h"
#include "error.h"
#include "field.h"
#include "function.h"
#include "handle.h"
#include "heap.h"
#include "string-inl.h"
#include "type-inl.h"

using namespace std;

namespace codeswitch {
namespace internal {

static Local<String> readString(VM* vm, istream& stream);
static Local<Function> readFunction(VM* vm, istream& stream, const Local<Package>& package);
static void readClass(VM* vm,
                      istream& stream,
                      const Local<Package>& package,
                      const Local<Class>& clas);
static Local<Field> readField(VM* vm, istream& stream, const Local<Package>& package);
static void readTypeParameter(VM* vm,
                              istream& stream,
                              const Local<Package>& package,
                              const Local<TypeParameter>& param);
static Local<Type> readType(VM* vm, istream& stream, const Local<Package>& package);
template <typename T>
static T readValue(istream& stream);
static vector<u8> readData(istream& stream, word_t size);
static i64 readVbn(istream& stream);
static word_t readWordVbn(istream& stream);


Package::Package(VM* vm)
    : Block(PACKAGE_BLOCK_TYPE),
      flags_(0),
      strings_(reinterpret_cast<BlockArray<String>*>(vm->roots()->emptyBlockArray())),
      functions_(reinterpret_cast<BlockArray<Function>*>(vm->roots()->emptyBlockArray())),
      classes_(reinterpret_cast<BlockArray<Class>*>(vm->roots()->emptyBlockArray())),
      typeParameters_(reinterpret_cast<BlockArray<TypeParameter>*>(
                          vm->roots()->emptyBlockArray())),
      entryFunctionIndex_(kNotSet) { }


Local<Package> Package::create(Heap* heap) {
  RETRY_WITH_GC(heap, return Local<Package>(new(heap) Package(heap->vm())));
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


Local<Package> Package::loadFromFile(VM* vm, const char* fileName) {
  ifstream file(fileName, ios::binary);
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
  HandleScope handleScope(vm);
  Local<Package> package;
  try {
    auto magic = readValue<u32>(stream);
    if (magic != kMagic)
      throw Error("package file is corrupt");
    auto majorVersion = readValue<u16>(stream);
    auto minorVersion = readValue<u16>(stream);
    if (majorVersion != 0 || minorVersion != 6)
      throw Error("package file has wrong format version");

    package = handleScope.escape(*Package::create(vm->heap()));

    auto flags = readValue<u64>(stream);
    package->setFlags(flags);

    auto stringCount = readValue<word_t>(stream);
    auto stringArray = BlockArray<String>::create(vm->heap(), stringCount);
    package->setStrings(*stringArray);

    auto functionCount = readValue<word_t>(stream);
    auto functionArray = BlockArray<Function>::create(vm->heap(), functionCount);
    package->setFunctions(*functionArray);

    auto classCount = readValue<word_t>(stream);
    auto classArray = BlockArray<Class>::create(vm->heap(), classCount);
    for (word_t i = 0; i < classCount; i++) {
      // We pre-allocate classes so Types we read can refer to them.
      auto clas = Class::allocate(vm->heap());
      clas->setPackage(*package);
      classArray->set(i, *clas);
    }
    package->setClasses(*classArray);

    auto typeParameterCount = readValue<word_t>(stream);
    auto typeParametersArray =
        BlockArray<TypeParameter>::create(vm->heap(), typeParameterCount);
    for (word_t i = 0; i < typeParameterCount; i++) {
      // Type parameters are also pre-allocated.
      auto param = TypeParameter::allocate(vm->heap());
      typeParametersArray->set(i, *param);
    }
    package->setTypeParameters(*typeParametersArray);

    auto entryFunctionIndex = readValue<word_t>(stream);
    package->setEntryFunctionIndex(entryFunctionIndex);

    for (word_t i = 0; i < stringCount; i++) {
      auto string = readString(vm, stream);
      stringArray->set(i, *string);
    }
    for (word_t i = 0; i < functionCount; i++) {
      auto function = readFunction(vm, stream, package);
      functionArray->set(i, *function);
    }
    for (word_t i = 0; i < classCount; i++) {
      auto clas = handle(Class::cast(classArray->get(i)));
      readClass(vm, stream, package, clas);
    }
    for (word_t i = 0; i < typeParameterCount; i++) {
      auto param = handle(TypeParameter::cast(typeParametersArray->get(i)));
      readTypeParameter(vm, stream, package, param);
    }
  } catch (istream::failure exn) {
    throw Error("error reading package");
  }

  return package;
}


String* Package::getString(word_t index) {
  return String::cast(strings()->get(index));
}


Function* Package::getFunction(word_t index) {
  return Function::cast(functions()->get(index));
}


Class* Package::getClass(word_t index) {
  return Class::cast(classes()->get(index));
}


TypeParameter* Package::getTypeParameter(word_t index) {
  return TypeParameter::cast(typeParameters()->get(index));
}


Function* Package::entryFunction() {
  word_t index = entryFunctionIndex();
  if (index == kNotSet)
    return nullptr;
  return getFunction(index);
}


static Local<String> readString(VM* vm, istream& stream) {
  auto length = readWordVbn(stream);
  auto size = readWordVbn(stream);
  vector<u8> utf8Chars = readData(stream, size);
  return String::fromUtf8String(vm->heap(), utf8Chars.data(), length, size);
}


static Local<Function> readFunction(VM* vm, istream& stream, const Local<Package>& package) {
  u32 flags;
  stream.read(reinterpret_cast<char*>(&flags), sizeof(flags));

  auto typeParameterCount = readWordVbn(stream);
  auto typeParameters = TaggedArray<TypeParameter>::create(vm->heap(), typeParameterCount);
  for (word_t i = 0; i < typeParameterCount; i++) {
    auto id = readWordVbn(stream);
    typeParameters->set(i, Tagged<TypeParameter>(id));
  }

  auto returnType = readType(vm, stream, package);
  auto parameterCount = readWordVbn(stream);
  auto types = BlockArray<Type>::create(vm->heap(), parameterCount + 1);
  types->set(0, *returnType);
  for (word_t i = 0; i < parameterCount; i++) {
    types->set(i + 1, *readType(vm, stream, package));
  }

  auto localsSize = readWordVbn(stream);

  auto instructionsSize = readWordVbn(stream);
  vector<u8> instructions = readData(stream, instructionsSize);

  auto blockOffsetCount = readWordVbn(stream);
  auto blockOffsets = WordArray::create(vm->heap(), blockOffsetCount);
  for (word_t i = 0; i < blockOffsetCount; i++) {
    auto offset = readWordVbn(stream);
    blockOffsets->set(i, offset);
  }

  auto function = Function::create(vm->heap(), flags, typeParameters, types,
                                   localsSize, instructions, blockOffsets, package);
  return function;
}


static void readClass(VM* vm,
                      istream& stream,
                      const Local<Package>& package,
                      const Local<Class>& clas) {
  u32 flags;
  stream.read(reinterpret_cast<char*>(&flags), sizeof(flags));
  auto supertype = readType(vm, stream, package);

  auto fieldCount = readWordVbn(stream);
  auto fields = BlockArray<Field>::create(vm->heap(), fieldCount);
  for (word_t i = 0; i < fieldCount; i++) {
    auto field = readField(vm, stream, package);
    fields->set(i, *field);
  }

  auto constructorCount = readWordVbn(stream);
  auto constructors = WordArray::create(vm->heap(), constructorCount);
  for (word_t i = 0; i < constructorCount; i++) {
    word_t id = readWordVbn(stream);
    constructors->set(i, id);
  }

  auto methodCount = readWordVbn(stream);
  auto methods = WordArray::create(vm->heap(), methodCount);
  for (word_t i = 0; i < methodCount; i++) {
    word_t id = readWordVbn(stream);
    methods->set(i, id);
  }

  clas->initialize(flags, *supertype, *fields, nullptr,
                   *constructors, *methods, *package, nullptr);
}


static Local<Field> readField(VM* vm, istream& stream, const Local<Package>& package) {
  u32 flags;
  stream.read(reinterpret_cast<char*>(&flags), sizeof(flags));
  auto type = readType(vm, stream, package);
  auto field = Field::allocate(vm->heap());
  field->initialize(flags, *type);
  return field;
}


static void readTypeParameter(VM* vm,
                              istream& stream,
                              const Local<Package>& package,
                              const Local<TypeParameter>& param) {
  u32 flags;
  stream.read(reinterpret_cast<char*>(&flags), sizeof(flags));
  auto upperBound = readType(vm, stream, package);
  auto lowerBound = readType(vm, stream, package);
  param->initialize(flags, *upperBound, *lowerBound);
}


static Local<Type> readType(VM* vm,
                            istream& stream,
                            const Local<Package>& package) {
  auto heap = vm->heap();
  auto bits = readWordVbn(stream);
  auto form = static_cast<Type::Form>(bits & Type::kFormMask);
  auto flags = static_cast<Type::Flags>(bits >> Type::kFlagsShift);
  auto isPrimitive = Type::FIRST_PRIMITIVE_TYPE <= form && form <= Type::LAST_PRIMITIVE_TYPE;
  if (form > Type::LAST_TYPE ||
      (flags & ~Type::kFlagsMask) != 0 ||
      (isPrimitive && flags != Type::NO_FLAGS)) {
    throw Error("invalid type flags");
  }

  if (isPrimitive) {
    return handle(Type::primitiveTypeFromForm(vm->roots(), form));
  } else {
    auto code = readVbn(stream);
    if (form == Type::CLASS_TYPE &&
        flags == Type::NULLABLE_FLAG &&
        code == BUILTIN_NOTHING_CLASS_ID) {
      return handle(vm->roots()->nullType());
    } else if (form == Type::CLASS_TYPE) {
      Local<Class> clas;
      if (isBuiltinId(code)) {
        clas = handle(vm->roots()->getBuiltinClass(static_cast<BuiltinId>(code)));
      } else {
        clas = handle(package->getClass(code));
      }
      auto ty = Type::allocate(heap, 1);
      ty->initialize(*clas, flags);
      return ty;
    } else {
      ASSERT(form == Type::VARIABLE_TYPE);
      if (isBuiltinId(code))
        throw Error("no builtin type parameters");
      auto param = handle(package->getTypeParameter(code));
      auto ty = Type::allocate(heap, 1);
      ty->initialize(*param, flags);
      return ty;
    }
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

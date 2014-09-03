// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef function_inl_h
#define function_inl_h

#include "function.h"
#include "bitmap-inl.h"
#include "handle.h"
#include "package-inl.h"
#include "type-inl.h"
#include "type-parameter.h"

namespace codeswitch {
namespace internal {

word_t Function::sizeForFunction(word_t instructionsSize) {
  return kHeaderSize + align(instructionsSize, kWordSize);
}


word_t Function::sizeOfFunction() const {
  // TODO: hack
  auto f = const_cast<Function*>(this);
  return sizeForFunction(f->instructionsSize());
}


bool Function::hasBuiltinId() {
  return mem<i8>(this, kBuiltinIdOffset) != 0;
}


BuiltinId Function::builtinId() {
  ASSERT(hasBuiltinId());
  auto bits = mem<i8>(this, kBuiltinIdOffset);
  auto decoded = static_cast<BuiltinId>(-bits);
  return decoded;
}


void Function::setBuiltinId(BuiltinId id) {
  auto encoded = -static_cast<i8>(id);
  mem<i8>(this, kBuiltinIdOffset) = encoded;
}


word_t Function::typeParameterCount() {
  return typeParameters()->length();
}


TypeParameter* Function::typeParameter(word_t index) {
  auto paramTag = typeParameters()->get(index);
  if (paramTag.isPointer()) {
    return TypeParameter::cast(paramTag.getPointer());
  } else {
    return package()->typeParameters()->get(paramTag.getNumber());
  }
}


Type* Function::returnType() {
  return Type::cast(types()->get(0));
}


word_t Function::parameterCount() {
  return types()->length() - 1;
}


word_t Function::parametersSize() {
  word_t size = 0;
  for (word_t i = 0, n = parameterCount(); i < n; i++) {
    size += align(parameterType(i)->typeSize(), kWordSize);
  }
  return size;
}


ptrdiff_t Function::parameterOffset(word_t index) {
  ptrdiff_t offset = 0;
  for (word_t i = parameterCount() - 1; i > index; i--) {
    offset += align(parameterType(i)->typeSize(), kWordSize);
  }
  return offset;
}


Type* Function::parameterType(word_t index) {
  return Type::cast(types()->get(index + 1));
}


u8* Function::instructionsStart() {
  return reinterpret_cast<u8*>(this) + kHeaderSize;
}


word_t Function::blockOffset(word_t index) {
  return blockOffsets()->get(index);
}


StackPointerMap* StackPointerMap::cast(Block* block) {
  WordArray* array = WordArray::cast(block);
  return reinterpret_cast<StackPointerMap*>(array);
}


Bitmap StackPointerMap::bitmap() {
  word_t* base = reinterpret_cast<word_t*>(
      elements() + kHeaderLength + entryCount() * kEntryLength);
  return Bitmap(base, bitmapLength());
}


void StackPointerMap::getParametersRegion(word_t* paramOffset, word_t* paramCount) {
  *paramOffset = 0;
  // The parameter region is first in the bitmap. We determine its size by checking the offset
  // of the first locals region. If there are no other regions, then it is the size of the
  // whole bitmap.
  if (entryCount() == 0) {
    *paramCount = bitmapLength();
  } else {
    *paramCount = mapOffset(0);
  }
}

}
}

#endif

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "class-inl.h"

#include "block-inl.h"
#include "field.h"
#include "handle.h"
#include "type-inl.h"

namespace codeswitch {
namespace internal {

Class* Class::tryAllocate(Heap* heap) {
  Class* clas = reinterpret_cast<Class*>(heap->allocate(kSize));
  if (clas == nullptr)
    return nullptr;

  clas->setMeta(CLASS_BLOCK_TYPE);
  return clas;
}


Local<Class> Class::allocate(Heap* heap) {
  DEFINE_ALLOCATION(heap, Class, tryAllocate(heap))
}


void Class::initialize(u32 flags,
                       Type* supertype,
                       BlockArray* fields,
                       Type* elementType,
                       WordArray* constructors,
                       WordArray* methods,
                       Package* package,
                       Meta* instanceMeta) {
  setFlags(flags);
  setSupertype(supertype);
  setFields(fields);
  setElementType(elementType);
  setConstructors(constructors);
  setMethods(methods);
  setPackage(package);
  setInstanceMeta(instanceMeta);
}


void Class::printClass(FILE* out) {
  fprintf(out, "Class @%p\n", reinterpret_cast<void*>(this));
}


word_t Class::findFieldIndex(word_t offset) {
  word_t currentOffset = kWordSize;
  for (word_t i = 0, n = fields()->length(); i < n; i++) {
    auto type = Field::cast(fields()->get(i))->type();
    currentOffset = align(currentOffset, type->alignment());
    if (currentOffset == offset)
      return i;
    currentOffset += type->typeSize();
  }
  UNREACHABLE();
  return 0;
}


word_t Class::findFieldOffset(word_t index) {
  ASSERT(index < fields()->length());
  word_t currentOffset = kWordSize;
  for (word_t i = 0; i < index; i++) {
    auto size = Field::cast(fields()->get(i))->type()->typeSize();
    auto nextAlignment = Field::cast(fields()->get(i + 1))->type()->alignment();
    currentOffset = align(currentOffset + size, nextAlignment);
  }
  return currentOffset;
}


Meta* Class::tryBuildInstanceMeta(Heap* heap) {
  ASSERT(instanceMeta() == nullptr);
  u32 objectSize = kWordSize, elementSize = 0;
  bool hasObjectPointers = false, hasElementPointers = false;
  BitSet objectPointerMap(1), elementPointerMap;
  computeSizeAndPointerMap(&objectSize, &hasObjectPointers, &objectPointerMap);
  if (elementType() != nullptr) {
    computeSizeAndPointerMapForType(elementType(), &elementSize,
                                    &hasElementPointers, &elementPointerMap);
  }

  auto methodCount = methods()->length();
  auto m = Meta::tryAllocate(heap, methodCount, objectSize, elementSize);
  if (m == nullptr)
    return nullptr;
  m->initialize(OBJECT_BLOCK_TYPE, this, objectSize, elementSize);
  m->setHasPointers(hasObjectPointers);
  m->setHasElementPointers(hasElementPointers);
  for (word_t i = 0; i < methodCount; i++) {
    auto method = getMethod(i);
    m->setData(i, method);
  }
  m->objectPointerMap().copyFrom(objectPointerMap.bitmap());
  if (elementSize > 0)
    m->elementPointerMap().copyFrom(elementPointerMap.bitmap());
  setInstanceMeta(m);
  return m;
}


bool Class::isSubclassOf(Class* other) {
  Class* super = this;
  while (super != nullptr && super != other) {
    super = supertype()->asClass();
  }
  return super == other;
}


void Class::computeSizeAndPointerMap(u32* size, bool* hasPointers, BitSet* pointerMap) {
  for (word_t i = 0, n = fields()->length(); i < n; i++) {
    auto type = Field::cast(fields()->get(i))->type();
    computeSizeAndPointerMapForType(type, size, hasPointers, pointerMap);
  }
}


void Class::computeSizeAndPointerMapForType(Type* type, u32* size,
                                            bool* hasPointers, BitSet* pointerMap) {
  u32 offset = align(*size, type->alignment());
  if (type->isObject()) {
    pointerMap->add(offset / kWordSize);
    *hasPointers = true;
  }
  *size = offset + type->typeSize();
  pointerMap->expand(align(*size, kWordSize) / kWordSize);
}

}
}

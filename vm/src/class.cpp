// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "class.h"

#include "array.h"
#include "block.h"
#include "field.h"
#include "function.h"
#include "gc.h"
#include "handle.h"
#include "package.h"
#include "roots-inl.h"
#include "type.h"

namespace codeswitch {
namespace internal {

#define CLASS_POINTER_LIST(F) \
  F(Class, supertype_)        \
  F(Class, fields_)           \
  F(Class, elementType_)      \
  F(Class, constructors_)     \
  F(Class, methods_)          \
  F(Class, package_)          \
  F(Class, instanceMeta_)     \

DEFINE_POINTER_MAP(Class, CLASS_POINTER_LIST)

#undef CLASS_POINTER_LIST


void* Class::operator new (size_t, Heap* heap) {
  return reinterpret_cast<void*>(heap->allocate(sizeof(Class)));
}


Class::Class(u32 flags,
             Type* supertype,
             BlockArray<Field>* fields,
             Type* elementType,
             WordArray* constructors,
             WordArray* methods,
             Package* package,
             Meta* instanceMeta)
    : Block(CLASS_BLOCK_TYPE),
      flags_(flags),
      supertype_(supertype),
      fields_(fields),
      elementType_(elementType),
      constructors_(constructors),
      methods_(methods),
      package_(package),
      instanceMeta_(instanceMeta) { }


Local<Class> Class::create(Heap* heap,
                           u32 flags,
                           const Handle<Type>& supertype,
                           const Handle<BlockArray<Field>>& fields,
                           const Handle<Type>& elementType,
                           const Handle<WordArray>& constructors,
                           const Handle<WordArray>& methods,
                           const Handle<Package>& package,
                           const Handle<Meta>& instanceMeta) {
  RETRY_WITH_GC(heap, return Local<Class>(new(heap) Class(
      flags, *supertype, *fields, *elementType, *constructors,
      *methods, package.getOrNull(), instanceMeta.getOrNull())));
}


Local<Class> Class::create(Heap* heap) {
  RETRY_WITH_GC(heap, return Local<Class>(new(heap) Class(
      0, nullptr, nullptr, nullptr, nullptr,
      nullptr, nullptr, nullptr)));
}


void Class::printClass(FILE* out) {
  fprintf(out, "Class @%p\n", reinterpret_cast<void*>(this));
}


word_t Class::findFieldIndex(word_t offset) const {
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


word_t Class::findFieldOffset(word_t index) const {
  ASSERT(index < fields()->length());
  word_t currentOffset = kWordSize;
  for (word_t i = 0; i < index; i++) {
    auto size = Field::cast(fields()->get(i))->type()->typeSize();
    auto nextAlignment = Field::cast(fields()->get(i + 1))->type()->alignment();
    currentOffset = align(currentOffset + size, nextAlignment);
  }
  return currentOffset;
}


Function* Class::getConstructor(word_t index) const {
  word_t id = constructors()->get(index);
  Function* ctor = package()->getFunction(id);
  return ctor;
}


Function* Class::getMethod(word_t index) const {
  intptr_t id = methods()->get(index);
  if (isBuiltinId(id)) {
    return getVM()->roots()->getBuiltinFunction(static_cast<BuiltinId>(id));
  } else {
    return package()->getFunction(id);
  }
}


Meta* Class::buildInstanceMeta() {
  u32 objectSize = kWordSize, elementSize = 0;
  bool hasObjectPointers = false, hasElementPointers = false;
  BitSet objectPointerMap(1), elementPointerMap;
  computeSizeAndPointerMap(&objectSize, &hasObjectPointers, &objectPointerMap);
  if (elementType() != nullptr) {
    computeSizeAndPointerMapForType(elementType(), &elementSize,
                                    &hasElementPointers, &elementPointerMap);
  }

  auto methodCount = methods()->length();
  auto meta = new(getHeap(), methodCount, objectSize, elementSize) Meta(OBJECT_BLOCK_TYPE);
  meta->setClass(this);
  meta->hasPointers_ = hasObjectPointers;
  meta->hasElementPointers_ = hasElementPointers;
  for (word_t i = 0; i < methodCount; i++) {
    auto method = getMethod(i);
    meta->setData(i, method);
  }
  meta->objectPointerMap().copyFrom(objectPointerMap.bitmap());
  if (elementSize > 0)
    meta->elementPointerMap().copyFrom(elementPointerMap.bitmap());
  return meta;
}


Local<Meta> Class::ensureAndGetInstanceMeta(const Handle<Class>& clas) {
  ensureInstanceMeta(clas);
  return Local<Meta>(clas->instanceMeta());
}


void Class::ensureInstanceMeta(const Handle<Class>& clas) {
  if (clas->instanceMeta() != nullptr)
    return;
  RETRY_WITH_GC(clas->getHeap(), clas->setInstanceMeta(clas->buildInstanceMeta()));
}


bool Class::isSubclassOf(Class* other) const {
  auto super = this;
  while (super != nullptr && super != other) {
    super = supertype()->asClass();
  }
  return super == other;
}


void Class::computeSizeAndPointerMap(u32* size, bool* hasPointers, BitSet* pointerMap) const {
  for (word_t i = 0, n = fields()->length(); i < n; i++) {
    auto type = Field::cast(fields()->get(i))->type();
    computeSizeAndPointerMapForType(type, size, hasPointers, pointerMap);
  }
}


void Class::computeSizeAndPointerMapForType(Type* type, u32* size,
                                            bool* hasPointers, BitSet* pointerMap) const {
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

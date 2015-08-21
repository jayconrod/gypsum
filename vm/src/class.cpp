// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "class.h"

#include "array.h"
#include "block.h"
#include "field.h"
#include "flags.h"
#include "function.h"
#include "handle.h"
#include "package.h"
#include "roots.h"
#include "type.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define CLASS_POINTER_LIST(F) \
  F(Class, name_)             \
  F(Class, typeParameters_)   \
  F(Class, supertype_)        \
  F(Class, fields_)           \
  F(Class, constructors_)     \
  F(Class, methods_)          \
  F(Class, package_)          \
  F(Class, instanceMeta_)     \
  F(Class, elementType_)      \

DEFINE_POINTER_MAP(Class, CLASS_POINTER_LIST)

#undef CLASS_POINTER_LIST


void* Class::operator new (size_t, Heap* heap) {
  return reinterpret_cast<void*>(heap->allocate(sizeof(Class)));
}


Class::Class(Name* name,
             u32 flags,
             BlockArray<TypeParameter>* typeParameters,
             Type* supertype,
             BlockArray<Field>* fields,
             BlockArray<Function>* constructors,
             BlockArray<Function>* methods,
             Package* package,
             Meta* instanceMeta,
             Type* elementType,
             length_t lengthFieldIndex)
    : Block(CLASS_BLOCK_TYPE),
      name_(this, name),
      flags_(flags),
      typeParameters_(this, typeParameters),
      supertype_(this, supertype),
      fields_(this, fields),
      constructors_(this, constructors),
      methods_(this, methods),
      package_(this, package),
      instanceMeta_(this, instanceMeta),
      elementType_(this, elementType),
      lengthFieldIndex_(lengthFieldIndex) {
  ASSERT((elementType_ == nullptr) == (lengthFieldIndex_ == kIndexNotSet));
}


Local<Class> Class::create(Heap* heap,
                           const Handle<Name>& name,
                           u32 flags,
                           const Handle<BlockArray<TypeParameter>>& typeParameters,
                           const Handle<Type>& supertype,
                           const Handle<BlockArray<Field>>& fields,
                           const Handle<BlockArray<Function>>& constructors,
                           const Handle<BlockArray<Function>>& methods,
                           const Handle<Package>& package,
                           const Handle<Meta>& instanceMeta,
                           const Handle<Type>& elementType,
                           length_t lengthFieldIndex) {
  RETRY_WITH_GC(heap, return Local<Class>(new(heap) Class(
      *name, flags, *typeParameters, *supertype, *fields, *constructors, *methods,
      package.getOrNull(), instanceMeta.getOrNull(),
      elementType.getOrNull(), lengthFieldIndex)));
}


Local<Class> Class::create(Heap* heap) {
  RETRY_WITH_GC(heap, return Local<Class>(new(heap) Class(
      nullptr, 0, nullptr, nullptr, nullptr, nullptr, nullptr,
      nullptr, nullptr, nullptr, kIndexNotSet)));
}


TypeParameter* Class::typeParameter(length_t index) const {
  return typeParameters()->get(index);
}


length_t Class::typeParameterCount() const {
  return typeParameters()->length();
}


length_t Class::findFieldIndex(word_t offset) const {
  word_t currentOffset = kWordSize;
  for (length_t i = 0, n = fields()->length(); i < n; i++) {
    auto type = block_cast<Field>(fields()->get(i))->type();
    currentOffset = align(currentOffset, type->alignment());
    if (currentOffset == offset)
      return i;
    currentOffset += type->typeSize();
  }
  UNREACHABLE();
  return 0;
}


word_t Class::findFieldOffset(length_t index) const {
  ASSERT(index < fields()->length());
  word_t currentOffset = kWordSize;
  for (length_t i = 0; i < index; i++) {
    auto size = block_cast<Field>(fields()->get(i))->type()->typeSize();
    auto nextAlignment = block_cast<Field>(fields()->get(i + 1))->type()->alignment();
    currentOffset = align(currentOffset + size, nextAlignment);
  }
  return currentOffset;
}


Class* Class::findFieldClass(length_t index) {
  ASSERT(index < fields()->length());
  auto clas = this;
  while (true) {
    if (clas->supertype() == nullptr)
      return clas;
    auto base = clas->supertype()->asClass();
    if (index >= base->fields()->length())
      return clas;
    clas = base;
  }
  UNREACHABLE();
  return nullptr;
}


Meta* Class::buildInstanceMeta() {
  // Compute size of instances of this class.
  u32 objectSize = kWordSize, elementSize = 0;
  u8 lengthOffset = 0;
  bool hasObjectPointers = false, hasElementPointers = false;
  BitSet objectPointerMap(1), elementPointerMap;
  computeSizeAndPointerMap(&objectSize, &hasObjectPointers, &objectPointerMap);
  if (elementType() != nullptr) {
    computeSizeAndPointerMapForType(elementType(), &elementSize,
                                    &hasElementPointers, &elementPointerMap);
    lengthOffset = findFieldOffset(lengthFieldIndex_);
  }

  // Count non-static methods.
  length_t methodCount = 0;
  for (length_t i = 0; i < methods()->length(); i++) {
    if ((methods()->get(i)->flags() & STATIC_FLAG) == 0) {
      methodCount++;
    }
  }

  // Allocate the meta and set the data elements to point to the non-static methods.
  auto meta = new(getHeap(), methodCount, objectSize, elementSize) Meta(OBJECT_BLOCK_TYPE);
  meta->setClass(this);
  meta->hasPointers_ = hasObjectPointers;
  meta->hasElementPointers_ = hasElementPointers;
  meta->lengthOffset_ = lengthOffset;
  for (length_t methodIndex = 0, dataIndex = 0;
       methodIndex < methods()->length();
       methodIndex++) {
    auto method = methods()->get(methodIndex);
    if ((method->flags() & STATIC_FLAG) == 0) {
      meta->setData(dataIndex++, method);
    }
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
  auto current = this;
  while (current->supertype() != nullptr && current != other) {
    current = current->supertype()->asClass();
  }
  return current == other;
}


void Class::computeSizeAndPointerMap(u32* size, bool* hasPointers, BitSet* pointerMap) const {
  for (length_t i = 0, n = fields()->length(); i < n; i++) {
    auto type = block_cast<Field>(fields()->get(i))->type();
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


ostream& operator << (ostream& os, const Class* clas) {
  os << brief(clas)
     << "\n  name: " << brief(clas->name())
     << "\n  supertype: " << brief(clas->supertype())
     << "\n  fields: " << brief(clas->fields())
     << "\n  constructors: " << brief(clas->constructors())
     << "\n  methods: " << brief(clas->methods())
     << "\n  package: " << brief(clas->package())
     << "\n  instance meta: " << brief(clas->instanceMeta())
     << "\n  element type: " << brief(clas->elementType())
     << "\n  length field index: " << clas->lengthFieldIndex();
  return os;
}

}
}

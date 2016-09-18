// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "class.h"

#include "array.h"
#include "block.h"
#include "field.h"
#include "flags.h"
#include "function.h"
#include "handle.h"
#include "index.h"
#include "package.h"
#include "roots.h"
#include "trait.h"
#include "type.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define CLASS_POINTER_LIST(F) \
  F(Class, name_) \
  F(Class, sourceName_) \
  F(Class, typeParameters_) \
  F(Class, supertypes_) \
  F(Class, fields_) \
  F(Class, constructors_) \
  F(Class, methods_) \
  F(Class, traits_) \
  F(Class, package_) \
  F(Class, instanceMeta_) \
  F(Class, elementType_) \
  F(Class, fieldNameIndex_) \
  F(Class, fieldSourceNameIndex_) \
  F(Class, methodNameIndex_) \
  F(Class, methodSourceNameIndex_) \
  F(Class, constructorSignatureIndex_) \

DEFINE_POINTER_MAP(Class, CLASS_POINTER_LIST)

#undef CLASS_POINTER_LIST


void* Class::operator new (size_t, Heap* heap) {
  return reinterpret_cast<void*>(heap->allocate(sizeof(Class)));
}


Class::Class(Name* name,
             String* sourceName,
             u32 flags,
             BlockArray<TypeParameter>* typeParameters,
             BlockArray<Type>* supertypes,
             BlockArray<Field>* fields,
             BlockArray<Function>* constructors,
             BlockArray<Function>* methods,
             TraitTable* traits,
             Package* package,
             Meta* instanceMeta,
             Type* elementType,
             length_t lengthFieldIndex)
    : ObjectTypeDefn(CLASS_BLOCK_TYPE),
      name_(this, name),
      sourceName_(this, sourceName),
      flags_(flags),
      typeParameters_(this, typeParameters),
      supertypes_(this, supertypes),
      fields_(this, fields),
      constructors_(this, constructors),
      methods_(this, methods),
      traits_(this, traits),
      package_(this, package),
      instanceMeta_(this, instanceMeta),
      elementType_(this, elementType),
      lengthFieldIndex_(lengthFieldIndex) {
  ASSERT((elementType_ == nullptr) == (lengthFieldIndex_ == kIndexNotSet));
}


Local<Class> Class::create(Heap* heap,
                           const Handle<Name>& name,
                           const Handle<String>& sourceName,
                           u32 flags,
                           const Handle<BlockArray<TypeParameter>>& typeParameters,
                           const Handle<BlockArray<Type>>& supertypes,
                           const Handle<BlockArray<Field>>& fields,
                           const Handle<BlockArray<Function>>& constructors,
                           const Handle<BlockArray<Function>>& methods,
                           const Handle<TraitTable>& traits,
                           const Handle<Package>& package,
                           const Handle<Meta>& instanceMeta,
                           const Handle<Type>& elementType,
                           length_t lengthFieldIndex) {
  RETRY_WITH_GC(heap, return Local<Class>(new(heap) Class(
      *name, sourceName.getOrNull(), flags, *typeParameters, *supertypes,
      *fields, *constructors, *methods, *traits,
      package.getOrNull(), instanceMeta.getOrNull(),
      elementType.getOrNull(), lengthFieldIndex)));
}


Local<Class> Class::create(Heap* heap) {
  RETRY_WITH_GC(heap, return Local<Class>(new(heap) Class(
      nullptr, nullptr, 0, nullptr, nullptr, nullptr, nullptr, nullptr,
      nullptr, nullptr, nullptr, nullptr, kIndexNotSet)));
}


Type* Class::baseClassType() const {
  auto types = supertypes();
  return types->isEmpty() ? nullptr : types->get(0);
}


Class* Class::baseClass() const {
  auto baseType = baseClassType();
  return baseType ? baseType->asClass() : nullptr;
}


Function* Class::getNonStaticMethod(length_t index) const {
  ASSERT(instanceMeta_);
  return block_cast<Function>(instanceMeta_.get()->getData(index));
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
  return kLengthNotSet;
}


word_t Class::findFieldOffset(length_t index) const {
  ASSERT(index < fields()->length());
  return fields()->get(index)->offset();
}


Class* Class::findFieldClass(length_t index) {
  ASSERT(index < fields()->length());
  auto clas = this;
  while (true) {
    auto base = clas->baseClass();
    if (!base)
      return clas;
    if (index >= base->fields()->length())
      return clas;
    clas = base;
  }
  UNREACHABLE();
  return nullptr;
}


Local<BlockHashMap<Name, Field>> Class::ensureAndGetFieldNameIndex(const Handle<Class>& clas) {
  if (clas->fieldNameIndex()) {
    return handle(clas->fieldNameIndex());
  }
  auto index = buildNameIndex<Field>(handle(clas->fields()), allDefnFilter<Field>);
  clas->setFieldNameIndex(*index);
  return index;
}


Local<BlockHashMap<String, Field>> Class::ensureAndGetFieldSourceNameIndex(
    const Handle<Class>& clas) {
  if (clas->fieldSourceNameIndex()) {
    return handle(clas->fieldSourceNameIndex());
  }
  auto index = buildSourceNameIndex<Field>(handle(clas->fields()), allDefnFilter<Field>);
  clas->setFieldSourceNameIndex(*index);
  return index;
}


Local<BlockHashMap<Name, Function>> Class::ensureAndGetMethodNameIndex(
    const Handle<Class>& clas) {
  if (clas->methodNameIndex()) {
    return handle(clas->methodNameIndex());
  }
  Local<Name> (*getKey)(const Handle<Function>&) = mangleFunctionName;
  auto index = buildIndex<Name, Function>(
      handle(clas->methods()),
      getKey,
      allDefnFilter<Function>);
  clas->setMethodNameIndex(*index);
  return index;
}


Local<BlockHashMap<String, Function>> Class::ensureAndGetMethodSourceNameIndex(
    const Handle<Class>& clas) {
  if (clas->methodSourceNameIndex()) {
    return handle(clas->methodSourceNameIndex());
  }
  auto index = buildIndex<String, Function>(
      handle(clas->methods()),
      mangleFunctionSourceName,
      allDefnFilter<Function>);
  clas->setMethodSourceNameIndex(*index);
  return index;
}


Local<BlockHashMap<String, Function>> Class::ensureAndGetConstructorSignatureIndex(
    const Handle<Class>& clas) {
  if (clas->constructorSignatureIndex()) {
    return handle(clas->constructorSignatureIndex());
  }
  auto index = buildIndex<String, Function>(
      handle(clas->constructors()),
      mangleSignature,
      allDefnFilter<Function>);
  clas->setConstructorSignatureIndex(*index);
  return index;
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


bool Class::isSubclassOf(const Class* other) const {
  auto current = this;
  while (current->baseClass() != nullptr && current != other) {
    current = current->baseClass();
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
     << "\n  sourceName: " << brief(clas->sourceName())
     << "\n  supertypes: " << brief(clas->supertypes())
     << "\n  fields: " << brief(clas->fields())
     << "\n  constructors: " << brief(clas->constructors())
     << "\n  methods: " << brief(clas->methods())
     << "\n  traits: " << brief(clas->traits())
     << "\n  package: " << brief(clas->package())
     << "\n  instance meta: " << brief(clas->instanceMeta())
     << "\n  element type: " << brief(clas->elementType())
     << "\n  length field index: " << clas->lengthFieldIndex()
     << "\n  field name index: " << brief(clas->fieldNameIndex())
     << "\n  field source name index: " << brief(clas->fieldSourceNameIndex())
     << "\n  method name index: " << brief(clas->methodNameIndex())
     << "\n  method source name index: " << brief(clas->methodSourceNameIndex())
     << "\n  constructor signature index: " << brief(clas->constructorSignatureIndex());
  return os;
}

}
}

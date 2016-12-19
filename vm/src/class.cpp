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
  F(Class, flatFields_) \
  F(Class, constructors_) \
  F(Class, methods_) \
  F(Class, flatMethods_) \
  F(Class, methodIdIndex_) \
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


Class::Class(DefnId id,
             Name* name,
             String* sourceName,
             u32 flags,
             BlockArray<TypeParameter>* typeParameters,
             BlockArray<Type>* supertypes,
             BlockArray<Field>* fields,
             BlockArray<Function>* constructors,
             BlockArray<Function>* methods,
             Package* package,
             Meta* instanceMeta,
             Type* elementType)
    : ObjectTypeDefn(CLASS_BLOCK_TYPE),
      id_(id),
      name_(this, name),
      sourceName_(this, sourceName),
      flags_(flags),
      typeParameters_(this, typeParameters),
      supertypes_(this, supertypes),
      fields_(this, fields),
      constructors_(this, constructors),
      methods_(this, methods),
      package_(this, package),
      instanceMeta_(this, instanceMeta),
      elementType_(this, elementType) {}


Local<Class> Class::create(Heap* heap,
                           DefnId id,
                           const Handle<Name>& name,
                           const Handle<String>& sourceName,
                           u32 flags,
                           const Handle<BlockArray<TypeParameter>>& typeParameters,
                           const Handle<BlockArray<Type>>& supertypes,
                           const Handle<BlockArray<Field>>& fields,
                           const Handle<BlockArray<Function>>& constructors,
                           const Handle<BlockArray<Function>>& methods,
                           const Handle<Package>& package,
                           const Handle<Meta>& instanceMeta,
                           const Handle<Type>& elementType) {
  RETRY_WITH_GC(heap, return Local<Class>(new(heap) Class(
      id, *name, sourceName.getOrNull(), flags, *typeParameters, *supertypes,
      *fields, *constructors, *methods,
      package.getOrNull(), instanceMeta.getOrNull(),
      elementType.getOrNull())));
}


Local<Class> Class::create(Heap* heap, DefnId id) {
  RETRY_WITH_GC(heap, return Local<Class>(new(heap) Class(
      id, nullptr, nullptr, 0, nullptr, nullptr, nullptr, nullptr,
      nullptr, nullptr, nullptr, nullptr)));
}


Type* Class::baseClassType() const {
  auto types = supertypes();
  return types->isEmpty() ? nullptr : types->get(0);
}


Class* Class::baseClass() const {
  auto baseType = baseClassType();
  return baseType ? baseType->asClass() : nullptr;
}


Field* Class::findField(Name* name) const {
  for (auto f : *fields()) {
    if (f->name()->equals(name)) {
      return f;
    }
  }
  UNREACHABLE();
  return nullptr;
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


Function* Class::findMethod(DefnId methodId) const {
  return methodIdIndex()->get(methodId);
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
  auto index = buildNameIndex<Function>(handle(clas->methods()), allDefnFilter<Function>);
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


bool Class::isSubclassOf(const Class* other) const {
  auto current = this;
  while (current->baseClass() != nullptr && current != other) {
    current = current->baseClass();
  }
  return current == other;
}


Local<Meta> Class::ensureInstanceMeta(const Handle<Class>& clas) {
  if (clas->instanceMeta() != nullptr) {
    return handle(clas->instanceMeta());
  }

  // Flatten fields and methods if we haven't done so already.
  auto flatFields = ensureFlatFields(clas);
  ensureFlatMethods(Local<ObjectTypeDefn>(*clas));

  // Compute object size from the last field.
  u32 objectSize;
  if (flatFields->isEmpty()) {
    objectSize = kObjectMetaSize;
  } else {
    auto lastField = flatFields->get(flatFields->length() - 1);
    objectSize = lastField->offset() + lastField->type()->typeSize();
  }

  // Build a pointer map for the object.
  BitSet objectPointerMap(align(objectSize, kWordSize) / kWordSize);
  bool hasObjectPointers = false;
  length_t lengthFieldIndex = kIndexNotSet;
  for (length_t i = 0; i < flatFields->length(); i++) {
    auto field = handle(flatFields->get(i));
    if (field->type()->isObject()) {
      hasObjectPointers = true;
      objectPointerMap.add(field->offset() / kWordSize);
    }
    if ((ARRAY_FLAG & field->flags()) != 0) {
      ASSERT(lengthFieldIndex == kIndexNotSet);
      ASSERT(clas->elementType() != nullptr);
      lengthFieldIndex = i;
    }
  }

  // Build a pointer map for the elements, if there are any.
  u32 elementSize = 0;
  u8 lengthOffset = 0;
  BitSet elementPointerMap;
  bool hasElementPointers = false;
  if (clas->elementType() != nullptr) {
    elementSize = clas->elementType()->typeSize();
    elementPointerMap.expand(align(elementSize, kWordSize) / kWordSize);
    ASSERT(lengthFieldIndex != kIndexNotSet);
    lengthOffset = static_cast<u8>(flatFields->get(lengthFieldIndex)->offset());
    ASSERT(lengthOffset == flatFields->get(lengthFieldIndex)->offset());
    if (clas->elementType()->isObject()) {
      hasElementPointers = true;
      elementPointerMap.add(0);
    }
  }

  // Build the meta.
  // TODO: include non-static methods in the meta in order to enable virtual calls. No point
  // in doing this yet because virtual calls aren't optimized.
  auto meta = Meta::create(
      clas->getHeap(), 0 /* dataLength */, objectSize, elementSize, OBJECT_BLOCK_TYPE);
  meta->setClass(*clas);
  meta->hasPointers_ = hasObjectPointers;
  meta->hasElementPointers_ = hasElementPointers;
  meta->lengthOffset_ = lengthOffset;
  meta->objectPointerMap().copyFrom(objectPointerMap.bitmap());
  if (elementSize > 0) {
    meta->elementPointerMap().copyFrom(elementPointerMap.bitmap());
  }
  clas->setInstanceMeta(*meta);

  return meta;
}


Local<BlockArray<Field>> Class::ensureFlatFields(const Handle<Class>& clas) {
  if (clas->flatFields() != nullptr) {
    return handle(clas->flatFields());
  }

  Local<Class> base(clas->getVM(), clas->baseClass());
  Local<BlockArray<Field>> flatFields;
  length_t firstOwnFieldIndex = kIndexNotSet;
  u32 lastFieldEnd;
  if (base.isEmpty()) {
    flatFields = handle(clas->fields());
    firstOwnFieldIndex = 0;
    lastFieldEnd = kObjectMetaSize;
  } else {
    auto baseFields = Class::ensureFlatFields(base);
    auto flatFieldCount = baseFields->length() + clas->fields()->length();
    flatFields = BlockArray<Field>::create(clas->getHeap(), flatFieldCount);
    for (length_t i = 0; i < baseFields->length(); i++) {
      flatFields->set(i, baseFields->get(i));
    }
    firstOwnFieldIndex = baseFields->length();
    if (baseFields->isEmpty()) {
      lastFieldEnd = kObjectMetaSize;
    } else {
      auto lastBaseField = handle(baseFields->get(baseFields->length() - 1));
      lastFieldEnd = lastBaseField->offset() + lastBaseField->type()->typeSize();
    }
  }

  auto ownFields = handle(clas->fields());
  for (length_t i = 0; i < ownFields->length(); i++) {
    auto ownFieldIndex = firstOwnFieldIndex + i;
    auto field = handle(ownFields->get(i));
    auto offset = align(lastFieldEnd, field->type()->alignment());
    field->setOffset(offset);
    flatFields->set(ownFieldIndex, *field);
    lastFieldEnd = offset + field->type()->typeSize();
  }

  clas->setFlatFields(*flatFields);
  return flatFields;
}


ostream& operator << (ostream& os, const Class* clas) {
  os << brief(clas)
     << "\n  id: " << clas->id()
     << "\n  name: " << brief(clas->name())
     << "\n  sourceName: " << brief(clas->sourceName())
     << "\n  supertypes: " << brief(clas->supertypes())
     << "\n  fields: " << brief(clas->fields())
     << "\n  flat fields: " << brief(clas->flatFields())
     << "\n  constructors: " << brief(clas->constructors())
     << "\n  methods: " << brief(clas->methods())
     << "\n  package: " << brief(clas->package())
     << "\n  instance meta: " << brief(clas->instanceMeta())
     << "\n  element type: " << brief(clas->elementType())
     << "\n  field name index: " << brief(clas->fieldNameIndex())
     << "\n  field source name index: " << brief(clas->fieldSourceNameIndex())
     << "\n  method name index: " << brief(clas->methodNameIndex())
     << "\n  method source name index: " << brief(clas->methodSourceNameIndex())
     << "\n  constructor signature index: " << brief(clas->constructorSignatureIndex());
  return os;
}

}
}

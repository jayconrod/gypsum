// Copyright 2014,2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "object.h"

#include <vector>
#include <cstring>
#include "array.h"
#include "block.h"
#include "class.h"
#include "defnid.h"
#include "field.h"
#include "flags.h"
#include "handle.h"
#include "heap.h"
#include "type.h"
#include "type-parameter.h"

using namespace std;

namespace codeswitch {
namespace internal {

void* Object::operator new (size_t, Heap* heap, Meta* meta) {
  ASSERT(meta->elementSize() == 0);
  auto size = meta->objectSize();
  auto obj = reinterpret_cast<Object*>(heap->allocate(size));
  obj->setMeta(meta);
  return obj;
}


void* Object::operator new (size_t, Heap* heap, Meta* meta, length_t length) {
  ASSERT(meta->elementSize() > 0);
  u64 size = elementsOffset(meta->objectSize(), meta->elementSize()) +
      static_cast<u64>(length) * meta->elementSize();
  if (size > Heap::kMaxAllocatableSize) {
    throw AllocationError(false /* shouldRetryAfterGC */);
  }
  ASSERT(length <= kMaxLength);
  auto obj = reinterpret_cast<Object*>(heap->allocate(static_cast<word_t>(size)));
  obj->setMeta(meta);
  obj->setElementsLength(length);
  return obj;
}


Object::Object()
    : Block(meta()) { }


Object::Object(BlockType blockType)
    : Block(blockType) { }


Local<Object> Object::create(Heap* heap, const Handle<Meta>& meta) {
  RETRY_WITH_GC(heap, return Local<Object>(new(heap, *meta) Object));
}


Local<Object> Object::create(Heap* heap, const Handle<Meta>& meta, length_t length) {
  RETRY_WITH_GC(heap, return Local<Object>(new(heap, *meta, length) Object));
}


Local<Type> Object::typeof(const Handle<Object>& object) {
  auto heap = object->getHeap();
  auto clas = handle(object->meta()->clas());
  auto typeParamCount = clas->typeParameterCount();
  Local<Type> type;
  if (typeParamCount == 0) {
    type = Type::create(heap, clas);
  } else {
    vector<Local<TypeParameter>> variables;
    variables.reserve(typeParamCount);
    vector<Local<Type>> typeArgs;
    typeArgs.reserve(typeParamCount);
    for (length_t i = 0; i < typeParamCount; i++) {
      auto typeParam = handle(clas->typeParameter(i));
      ASSERT((typeParam->flags() & STATIC_FLAG) != 0);
      variables.push_back(typeParam);
      auto typeArg = Type::create(heap, typeParam);
      typeArgs.push_back(typeArg);
    }
    auto classType = Type::create(heap, clas, typeArgs);
    type = Type::create(heap, variables, classType);
  }
  return type;
}


Class* Object::clas() const {
  return meta()->clas();
}


Function* Object::findMethod(DefnId id) const {
  return clas()->findMethod(id);
}


u64 Object::getRawField(const Field* field) const {
  // TODO: store offset within the field so we don't have to look it up here.
  auto clas = meta()->clas();
  auto fields = clas->fields();
  length_t index = kIndexNotSet;
  for (length_t i = 0; i < fields->length(); i++) {
    if (field == fields->get(i)) {
      index = i;
      break;
    }
  }
  ASSERT(index != kIndexNotSet);
  auto offset = clas->findFieldOffset(index);
  auto type = field->type();
  auto size = type->typeSize();
  if (size == 1) {
    return static_cast<u64>(mem<i8>(this, offset));
  } else if (size == 2) {
    return static_cast<u64>(mem<i16>(this, offset));
  } else if (size == 4) {
    return static_cast<u64>(mem<i32>(this, offset));
  } else {
    ASSERT(size == 8);
    return static_cast<u64>(mem<i64>(this, offset));
  }
}


void Object::setRawField(const Field* field, u64 bits) {
  // TODO: store offset within the field so we don't have to look it up here.
  auto clas = meta()->clas();
  auto fields = clas->fields();
  length_t index = kIndexNotSet;
  for (length_t i = 0; i < fields->length(); i++) {
    if (field == fields->get(i)) {
      index = i;
      break;
    }
  }
  ASSERT(index != kIndexNotSet);
  auto offset = clas->findFieldOffset(index);
  auto type = field->type();
  if (type->isObject()) {
    auto slot = &mem<Block*>(this, offset);
    auto ptr = reinterpret_cast<Block*>(static_cast<word_t>(bits));
    *slot = ptr;
    Heap::recordWrite(slot, ptr);
  } else {
    auto size = type->typeSize();
    if (size == 1) {
      mem<u8>(this, offset) = static_cast<u8>(bits);
    } else if (size == 2) {
      mem<u16>(this, offset) = static_cast<u16>(bits);
    } else if (size == 4) {
      mem<u32>(this, offset) = static_cast<u32>(bits);
    } else {
      ASSERT(size == 8);
      mem<u64>(this, offset) = bits;
    }
  }
}


u64 Object::getRawElement(length_t index) const {
  ASSERT(meta()->hasElements());
  ASSERT(index < elementsLength());
  auto base = elementsBase();
  auto size = meta()->clas()->elementType()->typeSize();
  auto addr = base + index * size;
  if (size == 1) {
    return static_cast<u64>(mem<u8>(addr));
  } else if (size == 2) {
    return static_cast<u64>(mem<u16>(addr));
  } else if (size == 4) {
    return static_cast<u64>(mem<u32>(addr));
  } else {
    ASSERT(size == 8);
    return mem<u64>(addr);
  }
}


void Object::setRawElement(length_t index, u64 bits) {
  ASSERT(meta()->hasElements());
  ASSERT(index < elementsLength());
  auto base = elementsBase();
  auto size = meta()->clas()->elementType()->typeSize();
  auto addr = base + index * size;
  if (size == 1) {
    mem<u8>(addr) = static_cast<u8>(bits);
  } else if (size == 2) {
    mem<u16>(addr) = static_cast<u16>(bits);
  } else if (size == 4) {
    mem<u32>(addr) = static_cast<u32>(bits);
  } else {
    ASSERT(size == 8);
    mem<u64>(addr) = bits;
  }
}


ostream& operator << (ostream& os, const Object* obj) {
  return os << brief(obj);
}

}
}

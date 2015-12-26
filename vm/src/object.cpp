// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "object.h"

#include <vector>
#include <cstring>
#include "block.h"
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
  ASSERT(length <= kMaxLength);
  auto size = meta->objectSize() + length * meta->elementSize();
  auto obj = reinterpret_cast<Object*>(heap->allocate(size));
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


ostream& operator << (ostream& os, const Object* obj) {
  return os << brief(obj);
}

}
}

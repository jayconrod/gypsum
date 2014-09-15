// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "object.h"

#include <cstring>
#include "block.h"
#include "gc.h"
#include "handle.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

void* Object::operator new (size_t, Heap* heap, Meta* meta) {
  ASSERT(meta->elementSize() == 0);
  auto size = meta->objectSize();
  auto obj = reinterpret_cast<Object*>(heap->allocate(size));
  obj->setMeta(meta);
  return obj;
}


void* Object::operator new (size_t, Heap* heap, Meta* meta, word_t length) {
  ASSERT(meta->elementSize() > 0);
  auto size = meta->objectSize() + length * meta->elementSize();
  auto obj = reinterpret_cast<Object*>(heap->allocate(size));
  obj->setMeta(meta);
  obj->setElementsLength(length);
  return obj;
}


Object::Object()
    : Block(meta()) { }


Local<Object> Object::create(Heap* heap, const Handle<Meta>& meta) {
  RETRY_WITH_GC(heap, return Local<Object>(new(heap, *meta) Object));
}


Local<Object> Object::create(Heap* heap, const Handle<Meta>& meta, word_t length) {
  RETRY_WITH_GC(heap, return Local<Object>(new(heap, *meta, length) Object));
}


void Object::printObject(FILE* out) {
  fprintf(out, "Object @%p\n", reinterpret_cast<void*>(this));
}

}
}

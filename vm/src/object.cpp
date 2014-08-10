// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "object.h"

#include <cstring>
#include "block-inl.h"
#include "handle.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

Object* Object::tryAllocate(Heap* heap, Meta* meta) {
  ASSERT(meta->elementSize() == 0);
  word_t size = meta->objectSize();
  Object* obj = reinterpret_cast<Object*>(heap->allocate(size));
  if (obj == nullptr)
    return obj;

  obj->setMeta(meta);
  memset(reinterpret_cast<char*>(obj) + kWordSize, 0, size - kWordSize);
  return obj;
}


Object* Object::tryAllocateArray(Heap* heap, Meta* meta, word_t length) {
  ASSERT(meta->elementSize() > 0);
  word_t size = meta->objectSize() + length * meta->elementSize();
  Object* obj = reinterpret_cast<Object*>(heap->allocate(size));
  if (obj == nullptr)
    return obj;

  obj->setMeta(meta);
  // TODO: this is a hack.
  mem<word_t>(obj, kWordSize) = length;
  memset(reinterpret_cast<char*>(obj) + 2 * kWordSize, 0, size - 2 * kWordSize);
  return obj;
}


Local<Object> Object::allocate(Heap* heap, Meta* meta) {
  DEFINE_ALLOCATION(heap, Object, tryAllocate(heap, meta))
}


Local<Object> Object::allocateArray(Heap* heap, Meta* meta, word_t length) {
  DEFINE_ALLOCATION(heap, Object, tryAllocateArray(heap, meta, length))
}


void Object::printObject(FILE* out) {
  fprintf(out, "Object @%p\n", reinterpret_cast<void*>(this));
}

}
}

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "field.h"

#include "block-inl.h"
#include "handle.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

Field* Field::tryAllocate(Heap* heap) {
  Field* field = reinterpret_cast<Field*>(heap->allocate(kSize));
  if (field == nullptr)
    return nullptr;

  field->setMeta(FIELD_BLOCK_TYPE);
  return field;
}


Local<Field> Field::allocate(Heap* heap) {
  DEFINE_ALLOCATION(heap, Field, tryAllocate(heap))
}


void Field::initialize(u32 flags, Type* type) {
  setFlags(flags);
  setType(type);
}


void Field::printField(FILE* out) {
  fprintf(out, "Field @%p\n", reinterpret_cast<void*>(this));
}

}
}

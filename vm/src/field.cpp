// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "field.h"

#include "block.h"
#include "gc.h"
#include "handle.h"
#include "heap.h"
#include "type.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define FIELD_POINTERS_LIST(F) \
  F(Field, type_)              \

DEFINE_POINTER_MAP(Field, FIELD_POINTERS_LIST)

#undef FIELD_POINTERS_LIST


void* Field::operator new (size_t, Heap* heap) {
  return reinterpret_cast<Field*>(heap->allocate(sizeof(Field)));
}


Field::Field(u32 flags, Type* type)
    : Block(FIELD_BLOCK_TYPE),
      flags_(flags),
      type_(this, type) { }


Local<Field> Field::create(Heap* heap, u32 flags, const Handle<Type>& type) {
  RETRY_WITH_GC(heap, return Local<Field>(new(heap) Field(flags, *type)));
}


ostream& operator << (ostream& os, const Field* field) {
  os << brief(field)
     << "\n  type: " << brief(field->type());
  return os;
}

}
}

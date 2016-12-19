// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "field.h"

#include "block.h"
#include "handle.h"
#include "heap.h"
#include "type.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define FIELD_POINTERS_LIST(F) \
  F(Field, name_)              \
  F(Field, sourceName_)        \
  F(Field, type_)              \

DEFINE_POINTER_MAP(Field, FIELD_POINTERS_LIST)

#undef FIELD_POINTERS_LIST


void* Field::operator new (size_t, Heap* heap) {
  return reinterpret_cast<Field*>(heap->allocate(sizeof(Field)));
}


Field::Field(Name* name, String* sourceName, u32 flags, Type* type, u32 offset)
    : Block(FIELD_BLOCK_TYPE),
      name_(this, name),
      sourceName_(this, sourceName),
      flags_(flags),
      type_(this, type),
      offset_(offset) { }


Local<Field> Field::create(
    Heap* heap,
    const Handle<Name>& name,
    const Handle<String>& sourceName,
    u32 flags,
    const Handle<Type>& type,
    u32 offset) {
  RETRY_WITH_GC(heap, return Local<Field>(new(heap) Field(
      *name, sourceName.getOrNull(), flags, *type, offset)));
}


ostream& operator << (ostream& os, const Field* field) {
  os << brief(field)
     << "\n  name: " << brief(field->name())
     << "\n  source name: " << brief(field->sourceName())
     << "\n  type: " << brief(field->type())
     << "\n  offset: " << field->offset();
  return os;
}

}
}

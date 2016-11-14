// Copyright 2015-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "global.h"

#include "flags.h"
#include "type.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define GLOBAL_POINTERS_LIST(F) \
  F(Global, name_)              \
  F(Global, sourceName_)        \
  F(Global, type_)              \

DEFINE_POINTER_MAP(Global, GLOBAL_POINTERS_LIST)

#undef GLOBAL_POINTERS_LIST


Global::Global(DefnId id, Name* name, String* sourceName, u32 flags, Type* type)
    : Block(GLOBAL_BLOCK_TYPE),
      id_(id),
      name_(this, name),
      sourceName_(this, sourceName),
      flags_(flags),
      type_(this, type) {
  value_.primitive = 0;
}


Local<Global> Global::create(Heap* heap, DefnId id, const Handle<Name>& name,
                             const Handle<String>& sourceName,
                             u32 flags, const Handle<Type>& type) {
  RETRY_WITH_GC(heap, return Local<Global>(new(heap) Global(
      id, *name, sourceName.getOrNull(), flags, *type)));
}


bool Global::isObject() const {
  return type()->isObject();
}


Object* Global::getObject() const {
  ASSERT(isObject());
  return reinterpret_cast<Object*>(value_.object);
}


void Global::setObject(Object* value) {
  ASSERT(isObject());
  auto slot = objectSlot();
  *slot = value;
  Heap::recordWrite(slot, value);
}


Object** Global::objectSlot() {
  ASSERT(isObject());
  return reinterpret_cast<Object**>(&value_.object);
}


bool Global::isPrimitive() const {
  return type()->isPrimitive();
}


i64 Global::getPrimitive() const {
  ASSERT(isPrimitive());
  return value_.primitive;
}


void Global::setPrimitive(i64 value) {
  ASSERT(isPrimitive());
  value_.primitive = value;
}


i64 Global::getRaw() const {
  if (isPrimitive()) {
    return getPrimitive();
  } else {
    return static_cast<i64>(static_cast<u64>(value_.object));
  }
}


void Global::setRaw(i64 value) {
  if (isPrimitive()) {
    setPrimitive(value);
  } else {
    setObject(reinterpret_cast<Object*>(static_cast<word_t>(value)));
  }
}


ostream& operator << (ostream& os, const Global* global) {
  os << brief(global)
     << "\n  id: " << global->id()
     << "\n  name: " << brief(global->name())
     << "\n  sourceName: " << brief(global->sourceName())
     << "\n  type: " << brief(global->type())
     << "\n  value: ";
  if (global->isPrimitive()) {
    os << global->getPrimitive();
  } else {
    os << brief(global->getObject());
  }
  return os;
}

}
}

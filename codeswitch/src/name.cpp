// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "name.h"

#include <iostream>
#include "array.h"
#include "string.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define NAME_POINTER_LIST(F) \
  F(Name, components_)        \

DEFINE_POINTER_MAP(Name, NAME_POINTER_LIST)

#undef NAME_POINTER_LIST


Name::Name(BlockArray<String>* components)
    : Block(NAME_BLOCK_TYPE),
      components_(this, components) {
  ASSERT(components->length() <= kMaxComponentCount);
  #ifdef DEBUG
  for (auto component : *components) {
    ASSERT(component->length() <= kMaxComponentLength);
  }
  #endif
}


Local<Name> Name::create(Heap* heap, const Handle<BlockArray<String>>& components) {
  RETRY_WITH_GC(heap, return Local<Name>(new(heap) Name(*components)));
}


Local<Name> Name::fromString(Heap* heap, const Handle<String>& nameString, NameUsage usage) {
  auto components = String::split(heap, nameString, '.');
  if (components->length() == 0 || components->length() > kMaxComponentCount)
    return Local<Name>();

  for (auto component : **components) {
    if (component->length() == 0 || component->length() > kMaxComponentLength)
      return Local<Name>();
    if (usage == PACKAGE_NAME) {
      auto first = component->get(0);
      if (!inRange<u32>(first, 'A', 'Z') && !inRange<u32>(first, 'a', 'z'))
        return Local<Name>();
      for (auto it = component->begin() + 1; it != component->end(); ++it) {
        auto ch = *it;
        if (!inRange<u32>(ch, 'A', 'Z') &&
            !inRange<u32>(ch, 'a', 'z') &&
            !inRange<u32>(ch, '0', '9') &&
            ch != '_') {
          return Local<Name>();
        }
      }
    }
  }

  return create(heap, components);
}


Local<String> Name::toString(Heap* heap, const Handle<Name>& name) {
  auto sep = String::fromUtf8CString(heap, ".");
  auto nameStr = String::join(heap, handle(name->components()), sep);
  return nameStr;
}


string Name::toStlString() {
  string nameStr = components()->get(0)->toUtf8StlString();
  for (length_t i = 1; i < components()->length(); i++) {
    nameStr += "." + components()->get(i)->toUtf8StlString();
  }
  return nameStr;
}


int Name::compare(const Name* other) const {
  auto len = min(components()->length(), other->components()->length());
  for (length_t i = 0; i < len; i++) {
    int cmp = components()->get(i)->compare(other->components()->get(i));
    if (cmp != 0)
      return cmp;
  }
  return static_cast<int>(components()->length()) -
         static_cast<int>(other->components()->length());
}


u32 Name::hashCode() const {
  u32 code = 0;
  for (auto component : *components()) {
    code = 31 * code + component->hashCode();
  }
  return code;
}


ostream& operator << (ostream& os, const Name* name) {
  os << brief(name) << "\n  ";
  auto components = name->components();
  auto length = components->length();
  for (length_t i = 0; i < length - 1; i++) {
    os << components->get(i)->toUtf8StlString() << '.';
  }
  os << components->get(length - 1)->toUtf8StlString();
  return os;
}

}
}

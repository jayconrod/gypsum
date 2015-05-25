// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "type-parameter.h"

#include <unordered_set>
#include "block.h"
#include "flags.h"
#include "handle.h"
#include "heap.h"
#include "name.h"
#include "string.h"
#include "type.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define TYPE_PARAMETERS_POINTER_LIST(F) \
  F(TypeParameter, name_)               \
  F(TypeParameter, upperBound_)         \
  F(TypeParameter, lowerBound_)         \

DEFINE_POINTER_MAP(TypeParameter, TYPE_PARAMETERS_POINTER_LIST)

#undef TYPE_PARAMETERS_POINTER_LIST


void* TypeParameter::operator new (size_t, Heap* heap) {
  return reinterpret_cast<TypeParameter*>(heap->allocate(sizeof(TypeParameter)));
}


TypeParameter::TypeParameter(Name* name, u32 flags, Type* upperBound, Type* lowerBound)
    : Block(TYPE_PARAMETER_BLOCK_TYPE),
      name_(this, name),
      flags_(flags),
      upperBound_(this, upperBound),
      lowerBound_(this, lowerBound) { }


Local<TypeParameter> TypeParameter::create(Heap* heap) {
  RETRY_WITH_GC(heap, return Local<TypeParameter>(
      new(heap) TypeParameter(nullptr, 0, nullptr, nullptr)));
}


Local<TypeParameter> TypeParameter::create(Heap* heap,
                                           const Handle<Name>& name,
                                           u32 flags,
                                           const Handle<Type>& upperBound,
                                           const Handle<Type>& lowerBound) {
  RETRY_WITH_GC(heap, return Local<TypeParameter>(
      new(heap) TypeParameter(*name, flags, upperBound.getOrNull(), lowerBound.getOrNull())));
}


bool TypeParameter::isCompatibleWith(const Handle<TypeParameter>& a,
                                     const Handle<TypeParameter>& b) {
  return a->name()->equals(b->name()) &&
         (a->flags() | EXTERN_FLAG) == (b->flags() | EXTERN_FLAG) &&
         a->upperBound()->equals(b->upperBound()) &&
         a->lowerBound()->equals(b->lowerBound());
}


bool TypeParameter::hasCommonBound(TypeParameter* other) const {
  unordered_set<const TypeParameter*> otherLowerBounds{other};
  auto last = other;
  while (last->lowerBound()->isVariable()) {
    last = last->lowerBound()->asVariable();
    otherLowerBounds.insert(last);
  }
  auto current = this;
  while (true) {
    if (otherLowerBounds.find(current) != otherLowerBounds.end())
      return true;
    if (!current->upperBound()->isVariable())
      return false;
    current = current->upperBound()->asVariable();
  }
}


Variance TypeParameter::variance() const {
  if (COVARIANT_FLAG & flags()) {
    return COVARIANT;
  } else if (CONTRAVARIANT_FLAG & flags()) {
    return CONTRAVARIANT;
  } else {
    return INVARIANT;
  }
}


ostream& operator << (ostream& os, const TypeParameter* tp) {
  os << brief(tp)
     << "\n  name: " << brief(tp->name())
     << "\n  upper bound: " << brief(tp->upperBound())
     << "\n  lower bound: " << brief(tp->lowerBound());
  return os;
}

}
}

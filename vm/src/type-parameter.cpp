// Copyright 2014-2016 Jay Conrod. All rights reserved.

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
  F(TypeParameter, sourceName_)         \
  F(TypeParameter, upperBound_)         \
  F(TypeParameter, lowerBound_)         \

DEFINE_POINTER_MAP(TypeParameter, TYPE_PARAMETERS_POINTER_LIST)

#undef TYPE_PARAMETERS_POINTER_LIST


void* TypeParameter::operator new (size_t, Heap* heap) {
  return reinterpret_cast<TypeParameter*>(heap->allocate(sizeof(TypeParameter)));
}


TypeParameter::TypeParameter(
    DefnId id,
    Name* name,
    String* sourceName,
    u32 flags,
    Type* upperBound,
    Type* lowerBound)
    : Block(TYPE_PARAMETER_BLOCK_TYPE),
      id_(id),
      name_(this, name),
      sourceName_(this, sourceName),
      flags_(flags),
      upperBound_(this, upperBound),
      lowerBound_(this, lowerBound) { }


Local<TypeParameter> TypeParameter::create(Heap* heap, DefnId id) {
  RETRY_WITH_GC(heap, return Local<TypeParameter>(
      new(heap) TypeParameter(id, nullptr, nullptr, 0, nullptr, nullptr)));
}


Local<TypeParameter> TypeParameter::create(Heap* heap,
                                           DefnId id,
                                           const Handle<Name>& name,
                                           const Handle<String>& sourceName,
                                           u32 flags,
                                           const Handle<Type>& upperBound,
                                           const Handle<Type>& lowerBound) {
  RETRY_WITH_GC(heap, return Local<TypeParameter>(new(heap) TypeParameter(
      id, *name, sourceName.getOrNull(), flags,
      upperBound.getOrNull(), lowerBound.getOrNull())));
}


bool TypeParameter::isCompatibleWith(const Handle<TypeParameter>& a,
                                     const Handle<TypeParameter>& b) {
  return a->name()->equals(b->name()) &&
         (a->flags() | EXTERN_FLAG) == (b->flags() | EXTERN_FLAG) &&
         a->upperBound()->equals(b->upperBound()) &&
         a->lowerBound()->equals(b->lowerBound());
}


bool TypeParameter::isEquivalent(TypeParameter* other) const {
  return upperBound()->equals(other->upperBound()) &&
         lowerBound()->equals(other->lowerBound());
}


TypeParameter* TypeParameter::findCommonUpperBound(TypeParameter* other) const {
  vector<const TypeParameter*> selfBounds;
  selfBounds.push_back(this);
  for (auto bound = upperBound();
       bound->isVariable();
       bound = bound->asVariable()->upperBound()) {
    selfBounds.push_back(bound->asVariable());
  }

  if (any_of(selfBounds.begin(), selfBounds.end(),
          [&](const TypeParameter* tp) { return tp == other; })) {
    return other;
  }
  for (auto bound = other->upperBound();
       bound->isVariable();
       bound = bound->asVariable()->upperBound()) {
    if (any_of(selfBounds.begin(), selfBounds.end(),
            [&](const TypeParameter* tp) { return tp == other; })) {
      return bound->asVariable();
    }
  }
  return nullptr;
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
     << "\n  id: " << tp->id()
     << "\n  name: " << brief(tp->name())
     << "\n  source name: " << brief(tp->sourceName())
     << "\n  upper bound: " << brief(tp->upperBound())
     << "\n  lower bound: " << brief(tp->lowerBound());
  return os;
}

}
}

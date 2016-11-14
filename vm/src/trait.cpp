// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "trait.h"

#include "function.h"
#include "handle.h"
#include "heap.h"
#include "index.h"

using std::ostream;

namespace codeswitch {
namespace internal {

#define TRAIT_POINTER_LIST(F) \
  F(Trait, name_) \
  F(Trait, sourceName_) \
  F(Trait, typeParameters_) \
  F(Trait, supertypes_) \
  F(Trait, methods_) \
  F(Trait, package_) \
  F(Trait, methodNameIndex_) \
  F(Trait, methodSourceNameIndex_) \

DEFINE_POINTER_MAP(Trait, TRAIT_POINTER_LIST)

#undef TRAIT_POINTER_LIST


void* Trait::operator new (size_t, Heap* heap) {
  return reinterpret_cast<void*>(heap->allocate(sizeof(Trait)));
}


Trait::Trait(DefnId id,
             Name* name,
             String* sourceName,
             u32 flags,
             BlockArray<TypeParameter>* typeParameters,
             BlockArray<Type>* supertypes,
             BlockArray<Function>* methods,
             Package* package)
    : ObjectTypeDefn(TRAIT_BLOCK_TYPE),
      id_(id),
      name_(this, name),
      sourceName_(this, sourceName),
      flags_(flags),
      typeParameters_(this, typeParameters),
      supertypes_(this, supertypes),
      methods_(this, methods) { }


Local<Trait> Trait::create(Heap* heap, DefnId id) {
  RETRY_WITH_GC(heap, return Local<Trait>(new(heap) Trait(
      id, nullptr, nullptr, 0, nullptr, nullptr, nullptr, nullptr)));
}


Local<Trait> Trait::create(Heap* heap,
                           DefnId id,
                           const Handle<Name>& name,
                           const Handle<String>& sourceName,
                           u32 flags,
                           const Handle<BlockArray<TypeParameter>>& typeParameters,
                           const Handle<BlockArray<Type>>& supertypes,
                           const Handle<BlockArray<Function>>& methods,
                           const Handle<Package>& package) {
  RETRY_WITH_GC(heap, return Local<Trait>(new(heap) Trait(
      id, *name, sourceName.getOrNull(), flags, *typeParameters, *supertypes,
      *methods, package.getOrNull())));
}


u32 Trait::hashCode() const {
  if (hashCode_ != kHashNotSet) {
    return hashCode_;
  }
  // Hack: initialize hash code using the address of the trait. The hash code will not
  // change after being initialized. This is neither deterministic, nor random. We do this
  // because neither package nor name may be set at this point.
  hashCode_ = hashMix(static_cast<u32>(reinterpret_cast<word_t>(this)));
  if (hashCode_ == kHashNotSet) {
    hashCode_ += 1;
  }
  return hashCode_;
}


Local<BlockHashMap<Name, Function>> Trait::ensureAndGetMethodNameIndex(
    const Handle<Trait>& trait) {
  if (trait->methodNameIndex()) {
    return handle(trait->methodNameIndex());
  }
  Local<Name> (*getKey)(const Handle<Function>&) = mangleFunctionName;
  auto index = buildIndex<Name, Function>(
      handle(trait->methods()),
      getKey,
      allDefnFilter<Function>);
  trait->setMethodNameIndex(*index);
  return index;
}


Local<BlockHashMap<String, Function>> Trait::ensureAndGetMethodSourceNameIndex(
    const Handle<Trait>& trait) {
  if (trait->methodSourceNameIndex()) {
    return handle(trait->methodSourceNameIndex());
  }
  auto index = buildIndex<String, Function>(
      handle(trait->methods()),
      mangleFunctionSourceName,
      allDefnFilter<Function>);
  trait->setMethodSourceNameIndex(*index);
  return index;
}


ostream& operator << (ostream& os, const Trait* trait) {
  os << brief(trait)
     << "\n  id: " << trait->id()
     << "\n  name: " << brief(trait->name())
     << "\n  source name: " << brief(trait->sourceName())
     << "\n  type parameters: " << brief(trait->typeParameters())
     << "\n  supertypes: " << brief(trait->supertypes())
     << "\n  methods: " << brief(trait->methods())
     << "\n  package: " << brief(trait->package())
     << "\n  method name index: " << brief(trait->methodNameIndex())
     << "\n  method source name index: " << brief(trait->methodSourceNameIndex());
  return os;
}


void TraitTableElement::set(const HashTable<TraitTableElement>* table,
                            const TraitTableElement& elem) {
  key = elem.key;
  table->getHeap()->recordWrite(
      reinterpret_cast<Trait**>(&key), reinterpret_cast<Trait*>(elem.key.getPointer()));
  value = elem.value;
  table->getHeap()->recordWrite(&value, elem.value);
}


Local<TraitTable> TraitTable::create(Heap* heap, length_t capacity) {
  RETRY_WITH_GC(heap, return Local<TraitTable>(new(heap, capacity) TraitTable()));
}

}
}

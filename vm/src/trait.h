// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef trait_h
#define trait_h

#include <iostream>
#include "block.h"
#include "hash-table.h"
#include "ptr.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T> class BlockArray;
template <class K, class V> class BlockHashMap;
class Function;
template <class T> class Handle;
class Heap;
template <class T> class Local;
class Name;
class Package;
class String;
class TypeParameter;

class Trait: public Block {
 public:
  static const BlockType kBlockType = TRAIT_BLOCK_TYPE;

  void* operator new(size_t, Heap* heap);
  Trait(Name* name,
        String* sourceName,
        u32 flags,
        BlockArray<TypeParameter>* typeParameters,
        BlockArray<Type>* supertypes,
        BlockArray<Function>* methods,
        Package* package);
  static Local<Trait> create(Heap* heap);
  static Local<Trait> create(Heap* heap,
                             const Handle<Name>& name,
                             const Handle<String>& sourceName,
                             u32 flags,
                             const Handle<BlockArray<TypeParameter>>& typeParameters,
                             const Handle<BlockArray<Type>>& supertypes,
                             const Handle<BlockArray<Function>>& methods,
                             const Handle<Package>& package);

  Name* name() const { return name_.get(); }
  void setName(Name* name) { name_.set(this, name); }

  String* sourceName() const { return sourceName_.get(); }
  void setSourceName(String* sourceName) { sourceName_.set(this, sourceName); }

  u32 flags() const { return flags_; }
  void setFlags(u32 flags) { flags_ = flags; }

  BlockArray<TypeParameter>* typeParameters() const { return typeParameters_.get(); }
  void setTypeParameters(BlockArray<TypeParameter>* typeParameters) {
    typeParameters_.set(this, typeParameters);
  }

  BlockArray<Type>* supertypes() const { return supertypes_.get(); }
  void setSupertypes(BlockArray<Type>* supertypes) { supertypes_.set(this, supertypes); }

  BlockArray<Function>* methods() const { return methods_.get(); }
  void setMethods(BlockArray<Function>* methods) { methods_.set(this, methods); }

  Package* package() const { return package_.get(); }
  void setPackage(Package* package) { package_.set(this, package); }

  BlockHashMap<Name, Function>* methodNameIndex() const { return methodNameIndex_.get(); }
  void setMethodNameIndex(BlockHashMap<Name, Function>* methodNameIndex) {
    methodNameIndex_.set(this, methodNameIndex);
  }
  static Local<BlockHashMap<Name, Function>> ensureAndGetMethodNameIndex(
      const Handle<Trait>& trait);

  BlockHashMap<String, Function>* methodSourceNameIndex() const {
    return methodSourceNameIndex_.get();
  }
  void setMethodSourceNameIndex(BlockHashMap<String, Function>* methodSourceNameIndex) {
    methodSourceNameIndex_.set(this, methodSourceNameIndex);
  }
  static Local<BlockHashMap<String, Function>> ensureAndGetMethodSourceNameIndex(
      const Handle<Trait>& trait);

 private:
  DECLARE_POINTER_MAP()
  Ptr<Name> name_;
  Ptr<String> sourceName_;
  u32 flags_;
  Ptr<BlockArray<TypeParameter>> typeParameters_;
  Ptr<BlockArray<Type>> supertypes_;
  Ptr<BlockArray<Function>> methods_;
  Ptr<Package> package_;
  Ptr<BlockHashMap<Name, Function>> methodNameIndex_;
  Ptr<BlockHashMap<String, Function>> methodSourceNameIndex_;
  // Update TRAIT_POINTER_LIST if pointer members change.
};

std::ostream& operator << (std::ostream& os, const Trait* trait);


struct TraitId {
  id_t packageId;
  length_t traitIndex;

  bool operator == (const TraitId& other) const {
    return packageId == other.packageId && traitIndex == other.traitIndex;
  }
  bool operator != (const TraitId& other) const {
    return !(*this == other);
  }
};


struct TraitTableElement {
  static const id_t kEmptyPackageId = kBuiltinPackageId - 1;
  static const id_t kDeadPackageId = kEmptyPackageId - 1;

  TraitTableElement()
      : key{kEmptyPackageId, kIndexNotSet},
        value(nullptr) { }
  TraitTableElement(TraitId key, BlockArray<Function>* value)
      : key(key),
        value(value) { }
  TraitTableElement(const TraitTableElement&) = delete;
  TraitTableElement& operator = (const TraitTableElement&) = delete;
  TraitTableElement(TraitTableElement&&) = delete;
  TraitTableElement&& operator = (TraitTableElement&&) = delete;

  bool isEmpty() const { return key.packageId == kEmptyPackageId; }
  void setEmpty() {
    key.packageId = kEmptyPackageId;
    value = nullptr;
  }
  bool isDead() const { return key.packageId == kDeadPackageId; }
  void setDead() {
    key.packageId = kDeadPackageId;
    value = nullptr;
  }
  bool isLive() const { return !isEmpty() && !isDead(); }

  void set(const HashTable<TraitTableElement>* table, const TraitTableElement& elem);

  bool operator == (const TraitTableElement& other) const {
    return key == other.key && value == other.value;
  }
  bool operator != (const TraitTableElement& other) const {
    return !(*this == other);
  }
  bool matches(const TraitTableElement& other) const {
    return isLive() && key == other.key;
  }
  u32 hashCode() const {
    return hashMix(key.packageId) ^ hashMix(key.traitIndex);
  }

  TraitId key;
  BlockArray<Function>* value;
};


class TraitTable: public HashTable<TraitTableElement> {
 public:
  static const BlockType kBlockType = TRAIT_TABLE_BLOCK_TYPE;

  TraitTable()
      : HashTable<TraitTableElement>(kBlockType) { }

  static Local<TraitTable> create(Heap* heap, length_t capacity);

 private:
  friend class Roots;
  static const word_t kElementPointerMap = 2;
};

}
}

#endif

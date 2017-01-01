// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef trait_h
#define trait_h

#include <iostream>
#include "block.h"
#include "defnid.h"
#include "hash-table.h"
#include "object-type-defn.h"
#include "ptr.h"
#include "tagged.h"
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

class Trait: public ObjectTypeDefn {
 public:
  static const BlockType kBlockType = TRAIT_BLOCK_TYPE;

  void* operator new(size_t, Heap* heap);
  Trait(DefnId id,
        Name* name,
        String* sourceName,
        u32 flags,
        BlockArray<TypeParameter>* typeParameters,
        BlockArray<Type>* supertypes,
        BlockArray<Function>* methods,
        Package* package);
  static Local<Trait> create(Heap* heap, DefnId id);
  static Local<Trait> create(Heap* heap,
                             DefnId id,
                             const Handle<Name>& name,
                             const Handle<String>& sourceName,
                             u32 flags,
                             const Handle<BlockArray<TypeParameter>>& typeParameters,
                             const Handle<BlockArray<Type>>& supertypes,
                             const Handle<BlockArray<Function>>& methods,
                             const Handle<Package>& package);

  DefnId id() const { return id_; }

  Name* name() const { return name_.get(); }
  void setName(Name* name) { name_.set(this, name); }

  String* sourceName() const { return sourceName_.get(); }
  void setSourceName(String* sourceName) { sourceName_.set(this, sourceName); }

  u32 flags() const { return flags_; }
  void setFlags(u32 flags) { flags_ = flags; }

  u32 hashCode() const;

  BlockArray<TypeParameter>* typeParameters() const { return typeParameters_.get(); }
  void setTypeParameters(BlockArray<TypeParameter>* typeParameters) {
    typeParameters_.set(this, typeParameters);
  }

  BlockArray<Type>* supertypes() const { return supertypes_.get(); }
  void setSupertypes(BlockArray<Type>* supertypes) { supertypes_.set(this, supertypes); }

  BlockArray<Function>* methods() const { return methods_.get(); }
  void setMethods(BlockArray<Function>* methods) { methods_.set(this, methods); }

  BlockArray<Function>* flatMethods() const { return flatMethods_.get(); }
  void setFlatMethods(BlockArray<Function>* flatMethods) {
    flatMethods_.set(this, flatMethods);
  }

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
  const DefnId id_;
  Ptr<Name> name_;
  Ptr<String> sourceName_;
  u32 flags_;
  mutable u32 hashCode_ = kHashNotSet;
  Ptr<BlockArray<TypeParameter>> typeParameters_;
  Ptr<BlockArray<Type>> supertypes_;
  Ptr<BlockArray<Function>> methods_;
  Ptr<BlockArray<Function>> flatMethods_;
  Ptr<Package> package_;
  Ptr<BlockHashMap<Name, Function>> methodNameIndex_;
  Ptr<BlockHashMap<String, Function>> methodSourceNameIndex_;
  // Update TRAIT_POINTER_LIST if pointer members change.
};

std::ostream& operator << (std::ostream& os, const Trait* trait);

}
}

#endif

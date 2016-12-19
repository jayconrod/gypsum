// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef object_type_defn_h
#define object_type_defn_h

#include <vector>
#include "block.h"

namespace codeswitch {
namespace internal {

template <class T> class BlockArray;
class DefnId;
class Function;
class Type;
class TypeParameter;

/**
 * Base class for `Class` and `Trait`. Garbage collected objects can't have virtual methods,
 * so this doesn't do anything useful.
 */
class ObjectTypeDefn: public Block {
 public:
  explicit ObjectTypeDefn(MetaWord mw)
      : Block(mw) { }

  DefnId id() const;

  BlockArray<Type>* supertypes() const;
  void setSupertypes(BlockArray<Type>* newSupertypes);

  BlockArray<TypeParameter>* typeParameters() const;
  void setTypeParameters(BlockArray<TypeParameter>* newTypeParameters);
  TypeParameter* typeParameter(length_t index) const;
  length_t typeParameterCount() const;

  BlockArray<Function>* methods() const;
  void setMethods(BlockArray<Function>* newMethods);
  BlockArray<Function>* flatMethods() const;
  void setFlatMethods(BlockArray<Function>* newFlatMethods);

  /**
   * Builds a list of methods in this definition if it doesn't exist, then returns it.
   *
   * When serialized into a package file, {@link Class classes} and {@link Trait traits} only
   * include methods they define. Inherited methods are not included because if another
   * package contains a base class or trait, and that package is changed, it will break
   * binary compatibility. Therefore, the full "flat" list of methods is constructed at
   * run-time by this method.
   *
   * Note that this list includes static methods.
   */
  static Local<BlockArray<Function>> ensureFlatMethods(const Handle<ObjectTypeDefn>& defn);

  ObjectTypeDefn* findCommonBase(ObjectTypeDefn* other);

 private:
  std::vector<ObjectTypeDefn*> bases();
};

}
}

#endif

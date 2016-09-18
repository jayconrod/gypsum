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

  BlockArray<Type>* supertypes() const;
  void setSupertypes(BlockArray<Type>* newSupertypes);
  BlockArray<TypeParameter>* typeParameters() const;
  void setTypeParameters(BlockArray<TypeParameter>* newTypeParameters);
  TypeParameter* typeParameter(length_t index) const;
  length_t typeParameterCount() const;

  ObjectTypeDefn* findCommonBase(ObjectTypeDefn* other);

 private:
  std::vector<ObjectTypeDefn*> bases();
};

}
}

#endif

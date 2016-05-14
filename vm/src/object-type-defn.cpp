// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "object-type-defn.h"

#include "array.h"
#include "class.h"
#include "trait.h"
#include "type.h"
#include "type-parameter.h"

namespace codeswitch {
namespace internal {

BlockArray<Type>* ObjectTypeDefn::supertypes() const {
  return isa<Class>(this)
      ? const_block_cast<Class>(this)->supertypes()
      : const_block_cast<Trait>(this)->supertypes();
}


void ObjectTypeDefn::setSupertypes(BlockArray<Type>* newSupertypes) {
  if (isa<Class>(this)) {
    block_cast<Class>(this)->setSupertypes(newSupertypes);
  } else {
    block_cast<Trait>(this)->setSupertypes(newSupertypes);
  }
}


BlockArray<TypeParameter>* ObjectTypeDefn::typeParameters() const {
  return isa<Class>(this)
      ? const_block_cast<Class>(this)->typeParameters()
      : const_block_cast<Trait>(this)->typeParameters();
}


void ObjectTypeDefn::setTypeParameters(BlockArray<TypeParameter>* newTypeParameters) {
  if (isa<Class>(this)) {
    block_cast<Class>(this)->setTypeParameters(newTypeParameters);
  } else {
    block_cast<Trait>(this)->setTypeParameters(newTypeParameters);
  }
}

}
}

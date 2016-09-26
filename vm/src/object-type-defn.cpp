// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "object-type-defn.h"

#include "array.h"
#include "class.h"
#include "trait.h"
#include "type.h"
#include "type-parameter.h"

using std::vector;

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


TypeParameter* ObjectTypeDefn::typeParameter(length_t index) const {
  return typeParameters()->get(index);
}


length_t ObjectTypeDefn::typeParameterCount() const {
  return typeParameters()->length();
}


ObjectTypeDefn* ObjectTypeDefn::findCommonBase(ObjectTypeDefn* other) {
  // TODO: This implementation doesn't fully consider traits. It only finds bases within
  // the class tree. This matches what the compiler does. A proper implementation would find
  // common base definitions in the supertype lists and return the first one.
  auto roots = getVM()->roots();

  if (this == other) {
    return this;
  }
  auto nothingClass = roots->getBuiltinClass(BUILTIN_NOTHING_CLASS_ID);
  if (this == nothingClass) {
    return other;
  }
  if (other == nothingClass) {
    return this;
  }

  vector<ObjectTypeDefn*> selfBases = bases();
  vector<ObjectTypeDefn*> otherBases = other->bases();
  auto si = selfBases.size() - 1;
  auto oi = otherBases.size() - 1;
  if (selfBases[si] != otherBases[oi]) {
    return nullptr;
  }

  while (si > 0 && oi > 0 && selfBases[si] == otherBases[oi]) {
    si--;
    oi--;
  }
  return selfBases[si + 1];
}


vector<ObjectTypeDefn*> ObjectTypeDefn::bases() {
  vector<ObjectTypeDefn*> bases;
  bases.push_back(this);
  auto clas = this;
  while (!clas->supertypes()->isEmpty()) {
    clas = clas->supertypes()->get(0)->asClass();
    bases.push_back(clas);
  }
  return bases;
}

}
}

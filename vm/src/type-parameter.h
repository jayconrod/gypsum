// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef type_parameter_h
#define type_parameter_h

#include <iostream>
#include "block.h"
#include "type.h"

namespace codeswitch {
namespace internal {

class Name;
class Type;

class TypeParameter: public Block {
 public:
  static const BlockType kBlockType = TYPE_PARAMETER_BLOCK_TYPE;

  void* operator new (size_t, Heap* heap);
  TypeParameter(Name* name, String* sourceName, u32 flags, Type* upperBound, Type* lowerBound);
  static Local<TypeParameter> create(Heap* heap);
  static Local<TypeParameter> create(Heap* heap,
                                     const Handle<Name>& name,
                                     const Handle<String>& sourceName,
                                     u32 flags,
                                     const Handle<Type>& upperBound,
                                     const Handle<Type>& lowerBound);

  // The bounds can be set after construction, even though we woud like to consider
  // TypeParameter as immutable. This is necessary since TypeParameter and Type have a cyclic
  // relationship. We may need to allocate TypeParameter objects early, then fill them after
  // other objects which refer to them have been allocated.

  Name* name() const { return name_.get(); }
  void setName(Name* name) { name_.set(this, name); }
  String* sourceName() const { return sourceName_.get(); }
  void setSourceName(String* sourceName) { sourceName_.set(this, sourceName); }
  u32 flags() const { return flags_; }
  void setFlags(u32 newFlags) { flags_ = newFlags; }
  Type* upperBound() const { return upperBound_.get(); }
  void setUpperBound(Type* newUpperBound) { upperBound_.set(this, newUpperBound); }
  Type* lowerBound() const { return lowerBound_.get(); }
  void setLowerBound(Type* newLowerBound) { lowerBound_.set(this, newLowerBound); }

  static bool isCompatibleWith(const Handle<TypeParameter>& a, const Handle<TypeParameter>& b);
  bool isEquivalent(TypeParameter* other) const;
  bool hasCommonBound(TypeParameter* other) const;

  Variance variance() const;

 private:
  DECLARE_POINTER_MAP()

  Ptr<Name> name_;
  Ptr<String> sourceName_;
  u32 flags_;
  Ptr<Type> upperBound_;
  Ptr<Type> lowerBound_;
  // Update TYPE_PARAMETER_POINTER_LIST if pointer members change.
};

std::ostream& operator << (std::ostream& os, const TypeParameter* tp);

}
}

#endif

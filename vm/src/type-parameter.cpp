// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "type-parameter.h"

#include "block-inl.h"
#include "handle.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

TypeParameter* TypeParameter::tryAllocate(Heap* heap) {
  TypeParameter* param = reinterpret_cast<TypeParameter*>(heap->allocate(kSize));
  if (param == nullptr) {
    return nullptr;
  }

  param->setMeta(TYPE_PARAMETER_BLOCK_TYPE);
  return param;
}


Local<TypeParameter> TypeParameter::allocate(Heap* heap) {
  DEFINE_ALLOCATION(heap, TypeParameter, tryAllocate(heap))
}


void TypeParameter::initialize(u32 flags, Type* upperBound, Type* lowerBound) {
  setFlags(flags);
  setUpperBound(upperBound);
  setLowerBound(lowerBound);
}


void TypeParameter::printTypeParameter(FILE* out) {
  fprintf(out, "TypeParameter @%p\n", reinterpret_cast<void*>(this));
}

}
}

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef type_parameter_h
#define type_parameter_h

#include "block.h"

namespace codeswitch {
namespace internal {

class TypeParameter: public Block {
 public:
  static TypeParameter* tryAllocate(Heap* heap);
  static Local<TypeParameter> allocate(Heap* heap);
  void initialize(u32 flags, Type* upperBound, Type* lowerBound);

  void printTypeParameter(FILE* out);

  DEFINE_CAST(TypeParameter)

  DEFINE_INL_ACCESSORS(u32, flags, setFlags, kFlagsOffset)
  DEFINE_INL_PTR_ACCESSORS(Type*, upperBound, setUpperBound, kUpperBoundOffset)
  DEFINE_INL_PTR_ACCESSORS(Type*, lowerBound, setLowerBound, kLowerBoundOffset)

  static const int kFlagsOffset = kBlockHeaderSize;
  static const int kUpperBoundOffset = align(kFlagsOffset + sizeof(u32), kWordSize);
  static const int kLowerBoundOffset = align(kUpperBoundOffset + kWordSize, kWordSize);
  static const int kSize = align(kLowerBoundOffset + kWordSize, kWordSize);

  static const word_t kPointerMap = 0xc;
};

}
}

#endif

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef field_h
#define field_h

#include "block.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Field: public Block {
 public:
  static Field* tryAllocate(Heap* heap);
  static Handle<Field> allocate(Heap* heap);

  void initialize(u32 flags, Type* type);

  void printField(FILE* out);
  DEFINE_CAST(Field)

  DEFINE_INL_ACCESSORS(u32, flags, setFlags, kFlagsOffset)
  DEFINE_INL_PTR_ACCESSORS(Type*, type, setType, kTypeOffset)

  static const int kFlagsOffset = kBlockHeaderSize;
  static const int kTypeOffset = align(kFlagsOffset + sizeof(u32), kWordSize);
  static const int kSize = kTypeOffset + kWordSize;

  static const word_t kPointerMap = 0x4;
};

}
}

#endif

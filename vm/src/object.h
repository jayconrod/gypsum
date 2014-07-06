// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef object_h
#define object_h

#include "block.h"

namespace codeswitch {
namespace internal {

class Object: public Block {
 public:
  static Object* tryAllocate(Heap* heap, Meta* meta);
  static Handle<Object> allocate(Heap* heap, Meta* meta);
  static Object* tryAllocateArray(Heap* heap, Meta* meta, word_t length);
  static Handle<Object> allocateArray(Heap* heap, Meta* meta, word_t length);
  void printObject(FILE* out);
  DEFINE_CAST(Object)
};

}
}

#endif

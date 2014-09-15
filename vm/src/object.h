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
  void* operator new (size_t, Heap* heap, Meta* meta);
  void* operator new (size_t, Heap* heap, Meta* meta, word_t length);
  Object();
  static Local<Object> create(Heap* heap, const Handle<Meta>& meta);
  static Local<Object> create(Heap* heap, const Handle<Meta>& meta, word_t length);

  void printObject(FILE* out);
  DEFINE_CAST(Object)
};

}
}

#endif

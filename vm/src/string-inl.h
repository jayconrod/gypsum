// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef string_inl_h
#define string_inl_h

#include "string.h"

#include "handle-inl.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

String* String::tryAllocate(Heap* heap, word_t length) {
  word_t size = kHeaderSize + length * sizeof(u32);
  String* string = reinterpret_cast<String*>(heap->allocate(size));
  if (string == nullptr)
    return nullptr;

  string->setMeta(STRING_BLOCK_TYPE);
  mem<word_t>(string, kLengthOffset) = length;
  return string;
}


Handle<String> String::allocate(Heap* heap, word_t length) {
  DEFINE_ALLOCATION(heap, String, tryAllocate(heap, length))
}


inline word_t String::length() {
  return mem<word_t>(this, kLengthOffset);
}


inline u32* String::chars() {
  return &mem<u32>(this, kHeaderSize);
}


inline u32 String::get(word_t index) {
  ASSERT(index < length());
  return chars()[index];
}

}
}

#endif

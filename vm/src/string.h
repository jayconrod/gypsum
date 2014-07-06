// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef string_h
#define string_h

#include <string>
#include <vector>
#include "object.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class String: public Object {
 public:
  static inline String* tryAllocate(Heap* heap, word_t length);
  static inline Handle<String> allocate(Heap* heap, word_t length);
  void initialize(const u32* chars);
  static Handle<String> fromUtf8CString(Heap* heap, const char* utf8Chars);
  static Handle<String> fromUtf8String(Heap* heap, const u8* utf8Chars, word_t size);
  static Handle<String> fromUtf8String(Heap* heap, const u8* utf8Chars,
                                       word_t length, word_t size);

  DEFINE_CAST(String)

  inline word_t length();
  inline u32* chars();
  inline u32 get(word_t index);

  word_t utf8EncodedSize();
  std::vector<u8> toUtf8StlVector();
  std::string toUtf8StlString();

  bool equals(String* other);
  int compare(String* other);

  String* tryConcat(Heap* heap, String* other);
  static Handle<String> concat(Heap* heap, Handle<String> left, Handle<String> right);

  static const int kLengthOffset = kBlockHeaderSize;
  static const int kHeaderSize = kLengthOffset + kWordSize;
};

}
}

#endif

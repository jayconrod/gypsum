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
  static const BlockType kBlockType = STRING_BLOCK_TYPE;

  static word_t sizeForLength(length_t length);
  void* operator new (size_t, Heap* heap, length_t length);
  explicit String(const u32* chars);
  static Local<String> create(Heap* heap, length_t length, const u32* chars);
  static Local<String> fromUtf8CString(Heap* heap, const char* utf8Chars);
  static Local<String> fromUtf8String(Heap* heap, const u8* utf8Chars, word_t size);
  static Local<String> fromUtf8String(Heap* heap, const u8* utf8Chars,
                                      length_t length, word_t size);

  length_t length() const { return length_; }
  const u32* chars() const { return chars_; }
  u32 get(length_t index) const {
    ASSERT(index < length_);
    return chars_[index];
  }

  word_t utf8EncodedSize() const;
  std::vector<u8> toUtf8StlVector() const;
  std::string toUtf8StlString() const;

  bool equals(String* other) const;
  int compare(String* other) const;

  String* tryConcat(Heap* heap, String* other);
  static Local<String> concat(Heap* heap,
                              const Handle<String>& left,
                              const Handle<String>& right);

 private:
  String();
  static Local<String> create(Heap* heap, length_t length);

  friend class Roots;
  static const word_t kPointerMap = 0;

  length_t length_;
  u32 chars_[0];
};

}
}

#endif

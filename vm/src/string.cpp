// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "string.h"

#include <algorithm>
#include <cstring>
#include "block.h"
#include "error.h"
#include "gc.h"
#include "handle.h"
#include "heap.h"
#include "utils.h"

using namespace std;

namespace codeswitch {
namespace internal {

word_t String::sizeForLength(length_t length) {
  ASSERT(length <= kMaxLength);
  return sizeof(String) + length * sizeof(u32);
}


void* String::operator new (size_t, Heap* heap, length_t length) {
  ASSERT(length <= kMaxLength);
  auto size = sizeForLength(length);
  auto string = reinterpret_cast<String*>(heap->allocate(size));
  string->length_ = length;
  return string;
}


String::String(const u32* chars)
    : Object(STRING_BLOCK_TYPE) {
  copy_n(chars, length_, chars_);
}


String::String()
    : Object(STRING_BLOCK_TYPE) { }


Local<String> String::create(Heap* heap, length_t length, const u32* chars) {
  RETRY_WITH_GC(heap, return Local<String>(new(heap, length) String(chars)));
}


Local<String> String::create(Heap* heap, length_t length) {
  RETRY_WITH_GC(heap, return Local<String>(new(heap, length) String));
}


Local<String> String::fromUtf8String(Heap* heap, const u8* utf8Chars,
                                      length_t length, word_t size) {
  auto string = String::create(heap, length);
  u32* chars = string->chars_;
  auto end = utf8Chars + size;
  length_t i;
  for (i = 0; i < length; i++) {
    auto ch = utf8Decode(&utf8Chars, end);
    if (ch == UTF8_DECODE_ERROR)
      throw Error("invalid utf8 string");
    chars[i] = ch;
  }
  if (utf8Chars != end)
    throw Error("invalid utf8 string");
  return string;
}


Local<String> String::fromUtf8CString(Heap* heap, const char* utf8Chars) {
  word_t size = strlen(utf8Chars);
  return fromUtf8String(heap, reinterpret_cast<const u8*>(utf8Chars), size);
}


Local<String> String::fromUtf8String(Heap* heap, const u8* utf8Chars, word_t size) {
  length_t length = 0;
  auto p = utf8Chars;
  auto end = p + size;
  while (p != end) {
    auto ch = utf8Decode(&p, end);
    if (ch == UTF8_DECODE_ERROR)
      throw Error("invalid utf8 string");
    length++;
  }
  return fromUtf8String(heap, utf8Chars, length, size);
}


word_t String::utf8EncodedSize() const {
  word_t size = 0;
  for (length_t i = 0; i < length(); i++) {
    size += utf8EncodeSize(get(i));
  }
  return size;
}


vector<u8> String::toUtf8StlVector() const {
  word_t size = utf8EncodedSize();
  vector<u8> utf8Chars(size);
  auto p = utf8Chars.data();
  auto end = p + size;
  for (length_t i = 0; i < length(); i++) {
    utf8Encode(get(i), &p);
  }
  ASSERT(p == end);
  return utf8Chars;
}



string String::toUtf8StlString() const {
  vector<u8> utf8Chars = toUtf8StlVector();
  return string(reinterpret_cast<char*>(utf8Chars.data()), utf8Chars.size());
}


bool String::equals(String* other) const {
  if (length() != other->length())
    return false;
  for (length_t i = 0; i < length(); i++) {
    if (get(i) != other->get(i))
      return false;
  }
  return true;
}


int String::compare(String* other) const {
  auto minLength = min(length(), other->length());
  int cmp;
  for (length_t i = 0; i < minLength; i++) {
    cmp = static_cast<int>(get(i)) - static_cast<int>(other->get(i));
    if (cmp != 0)
      return cmp;
  }
  cmp = static_cast<int>(length()) - static_cast<int>(other->length());
  return cmp;
}


String* String::tryConcat(Heap* heap, String* other) {
  if (other->length_ == 0) {
    return this;
  } else if (length_ == 0) {
    return other;
  }

  auto consLength = length_ + other->length_;
  if (consLength > kMaxLength)
    throw Error("maximum string length exeeded in concatenation");

  auto cons = new(heap, consLength) String;
  copy_n(chars_, length_, cons->chars_);
  copy_n(other->chars_, other->length_, cons->chars_ + length_);
  return cons;
}


Local<String> String::concat(Heap* heap,
                             const Handle<String>& left,
                             const Handle<String>& right) {
  RETRY_WITH_GC(heap, return Local<String>(left->tryConcat(heap, *right)));
}

}
}

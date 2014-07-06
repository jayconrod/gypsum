// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "string-inl.h"

#include <algorithm>
#include <cstring>
#include "error.h"
#include "handle-inl.h"
#include "heap-inl.h"
#include "utils.h"

using namespace std;

namespace codeswitch {
namespace internal {

void String::initialize(const u32* chars) {
  copy_n(chars, length(), this->chars());
}


Handle<String> String::fromUtf8String(Heap* heap, const u8* utf8Chars,
                                      word_t length, word_t size) {
  auto string = String::allocate(heap, length);
  auto chars = string->chars();
  auto end = utf8Chars + size;
  word_t i;
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


Handle<String> String::fromUtf8CString(Heap* heap, const char* utf8Chars) {
  word_t size = strlen(utf8Chars);
  return fromUtf8String(heap, reinterpret_cast<const u8*>(utf8Chars), size);
}


Handle<String> String::fromUtf8String(Heap* heap, const u8* utf8Chars, word_t size) {
  word_t length = 0;
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


word_t String::utf8EncodedSize() {
  word_t size = 0;
  for (word_t i = 0; i < length(); i++) {
    size += utf8EncodeSize(get(i));
  }
  return size;
}


vector<u8> String::toUtf8StlVector() {
  word_t size = utf8EncodedSize();
  vector<u8> utf8Chars(size);
  auto p = utf8Chars.data();
  auto end = p + size;
  for (word_t i = 0; i < length(); i++) {
    utf8Encode(get(i), &p);
  }
  ASSERT(p == end);
  return utf8Chars;
}



string String::toUtf8StlString() {
  vector<u8> utf8Chars = toUtf8StlVector();
  return string(reinterpret_cast<char*>(utf8Chars.data()), utf8Chars.size());
}


bool String::equals(String* other) {
  if (length() != other->length())
    return false;
  for (word_t i = 0; i < length(); i++) {
    if (get(i) != other->get(i))
      return false;
  }
  return true;
}


int String::compare(String* other) {
  auto minLength = min(length(), other->length());
  int cmp;
  for (word_t i = 0; i < minLength; i++) {
    cmp = static_cast<int>(get(i)) - static_cast<int>(other->get(i));
    if (cmp != 0)
      return cmp;
  }
  cmp = static_cast<int>(length()) - static_cast<int>(other->length());
  return cmp;
}


String* String::tryConcat(Heap* heap, String* other) {
  if (other->length() == 0) {
    return this;
  } else if (length() == 0) {
    return other;
  }

  auto consLength = length() + other->length();
  // TODO: check for overflow
  auto cons = tryAllocate(heap, consLength);
  if (cons == nullptr)
    return nullptr;
  copy_n(chars(), length(), cons->chars());
  copy_n(other->chars(), other->length(), cons->chars() + length());
  return cons;
}


Handle<String> String::concat(Heap* heap, Handle<String> left, Handle<String> right) {
  DEFINE_ALLOCATION(heap, String, left->tryConcat(heap, *right))
}

}
}

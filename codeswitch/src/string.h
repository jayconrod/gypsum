// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef string_h
#define string_h

#include <iostream>
#include <iterator>
#include <string>
#include <vector>
#include "object.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class BlockArray;

class String: public Object {
 public:
  static const BlockType kBlockType = STRING_BLOCK_TYPE;

  static word_t sizeForLength(length_t length);
  void* operator new (size_t, Heap* heap, length_t length);
  explicit String(const u8* chars);
  explicit String(const char* chars);
  static Local<String> create(Heap* heap, length_t length, const u8* chars);
  static Local<String> create(Heap* heap, length_t length, const char* chars);
  static String* rawFromUtf8CString(Heap* heap, const u8* utf8Chars);
  static String* rawFromUtf8CString(Heap* heap, const char* chars);
  static Local<String> fromUtf8CString(Heap* heap, const u8* utf8Chars);
  static Local<String> fromUtf8String(Heap* heap, const std::string& stlString);
  static Local<String> fromUtf8String(Heap* heap, const u8* utf8Chars, length_t length);
  static Local<String> fromUtf8CString(Heap* heap, const char* utf8Chars);
  static Local<String> fromUtf8String(Heap* heap, const char* utf8Chars, length_t length);

  length_t length() const { return length_; }
  bool isEmpty() const { return length() == 0; }
  const u8* chars() const { return chars_; }
  u8 get(length_t index) const {
    ASSERT(index < length_);
    return chars_[index];
  }

  std::vector<u8> toUtf8StlVector() const;
  std::string toUtf8StlString() const;

  bool equals(const String* other) const;
  bool equals(const u8* other) const;
  bool equals(const char* other) const;
  int compare(String* other) const;
  u32 hashCode() const;

  String* tryConcat(String* other);
  static Local<String> concat(const Handle<String>& left,
                              const Handle<String>& right);
  String* trySubstring(length_t begin, length_t end) const;
  static Local<String> substring(const Handle<String>& string,
                                 length_t begin, length_t end);

  length_t find(u8 needle, length_t start = 0) const;
  length_t find(String* needle, length_t start = 0) const;

  length_t count(u8 needle) const;
  length_t count(String* needle) const;

  static Local<BlockArray<String>> split(Heap* heap, const Handle<String>& string, u8 sep);
  static Local<BlockArray<String>> split(Heap* heap,
                                         const Handle<String>& string,
                                         const Handle<String>& sep);

  static Local<String> join(Heap* heap,
                            const Handle<BlockArray<String>>& strings,
                            const Handle<String>& sep);

  bool tryToI32(i32* n) const;

  // Note: iterators contain raw pointers. They should not be used across allocations.
  class iterator: public std::iterator<std::random_access_iterator_tag, u8> {
   public:
    iterator(const String* str, length_t index)
        : str_(str), index_(index) { }

    u8 operator * () const;
    bool operator == (const iterator& other) const;
    bool operator != (const iterator& other) const;
    bool operator < (const iterator& other) const;
    bool operator <= (const iterator& other) const;
    bool operator > (const iterator& other) const;
    bool operator >= (const iterator& other) const;
    iterator operator + (ssize_t offset) const;
    iterator& operator += (ssize_t offset);
    iterator& operator ++ ();
    iterator operator - (ssize_t offset) const;
    iterator& operator -= (ssize_t offset);
    iterator& operator -- ();

   private:
    const String* str_;
    length_t index_;

    friend class String;
  };

  iterator begin() const;
  iterator end() const;

  #ifdef DEBUG
  void dump() const;
  #endif

 private:
  String();
  static Local<String> create(Heap* heap, length_t length);

  friend class Roots;
  static const word_t kPointerMap = 0;

  length_t length_;
  u8 chars_[0];
};

std::ostream& operator << (std::ostream& os, const String* str);

}
}

#endif

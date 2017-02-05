// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef tagged_h
#define tagged_h

#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class Tagged {
 public:
  explicit Tagged(word_t number) {
    setNumber(number);
  }
  explicit Tagged(T* pointer) {
    setPointer(pointer);
  }
  Tagged(const Tagged& other) : value_(other.value_) { }
  template <class S>
  Tagged(Tagged<S> other) {
    if (other.isNumber())
      setNumber(other.getNumber());
    else
      setPointer(other.getPointer());
  }

  bool isNumber() const { return (value_ & kTagMask) == kTag; }
  static bool isNumber(void* p) {
    auto ip = reinterpret_cast<word_t>(p);
    return (ip & kTagMask) == kTag;
  }
  word_t getNumber() const {
    ASSERT(isNumber());
    return static_cast<word_t>(static_cast<intptr_t>(value_) >> kTagSize);
  }
  void setNumber(word_t number) {
    value_ = number << kTagSize | kTag;
  }

  bool isPointer() const { return !isNumber(); }
  static bool isPointer(void* p) {
    auto ip = reinterpret_cast<word_t>(p);
    return (ip & kTagMask) == 0;
  }
  T* getPointer() const {
    ASSERT(isPointer());
    return reinterpret_cast<T*>(value_);
  }
  void setPointer(T* pointer) {
    value_ = reinterpret_cast<word_t>(pointer);
    ASSERT(isPointer());
  }

  word_t raw() const { return value_; }

  bool operator == (word_t n) const { return isNumber() && getNumber() == n; }
  bool operator == (T* p) const { return isPointer() && getPointer() == p; }
  bool operator == (Tagged t) const { return value_ == t.value_; }
  bool operator != (word_t n) const { return !(*this == n); }
  bool operator != (T* p) const { return !(*this == p); }
  bool operator != (Tagged t) const { return !(*this == t); }

  static const word_t kTag = 1;
  static const word_t kTagMask = 1;
  static const word_t kTagSize = 1;

 private:
  word_t value_;
};

template <class T>
std::ostream& operator << (std::ostream& os, const Tagged<T>& tagged) {
  return (tagged.isNumber() ? os << tagged.getNumber() : os << brief(tagged.getPointer()));
}


template <class T>
Tagged<T> tag(T* pointer) {
  return Tagged<T>(pointer);
}

}
}

#endif

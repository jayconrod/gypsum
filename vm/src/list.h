// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef list_h
#define list_h

#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class List {
 public:
  List(word_t capacity = kDefaultCapacity)
      : length_(0),
        capacity_(0),
        data_(nullptr) {
    ensureCapacity(capacity);
  }
  List(const List& list)
      : length_(list.length_),
        capacity_(list.capacity_),
        data_(new T[capacity_]) {
    for (word_t i = 0; i < length_; i++)
      data_[i] = list.data_[i];
  }
  ~List() {
    delete [] data_;
  }

  List& operator = (const List& list) {
    delete [] data_;
    length_ = list.length_;
    capacity_ = list.capacity_;
    data_ = new T[capacity_];
    for (word_t i = 0; i < length_; i++)
      data_[i] = list.data_[i];
    return *this;
  }

  word_t length() const { return length_; }
  void setLength(word_t newLength) {
    ensureCapacity(newLength);
    length_ = newLength;
  }
  bool isEmpty() const { return length() == 0; }
  void add(const T& elem) {
    ensureCapacity(length() + 1);
    data_[length_++] = elem;
  }
  void remove(word_t index) {
    for (word_t i = index; i < length_ - 1; i++) {
      data_[i] = data_[i + 1];
    }
    length_--;
  }
  T pop() {
    T elem = data_[length() - 1];
    remove(length() - 1);
    return elem;
  }
  word_t find(const T& value) {
    for (word_t i = 0; i < length_; i++) {
      if (data_[i] == value)
        return i;
    }
    return kNotFound;
  }

  bool findAndRemove(const T& value) {
    for (word_t i = 0; i < length_; i++) {
      if (data_[i] == value) {
        remove(i);
        return true;
      }
    }
    return false;
  }

  T& at(word_t index) {
    ASSERT(0 <= index && index < length_);
    return data_[index];
  }
  const T& at(word_t index) const {
    ASSERT(0 <= index && index < length_);
    return data_[index];
  }

  T& operator [] (word_t index) { return at(index); }
  const T& operator [] (word_t index) const { return at(index); }

  T& first() { return at(0); }
  T& last() { return at(length() - 1); }

  T* data() { return data_; }

  void sort() {
    sort(defaultCompare);
  }

  template <class C>
  void sort(C compare) {
    List<T> scratch(length_);
    scratch.setLength(length_);
    sortRange(0, length_, compare, scratch);
  }

  template <class C>
  void sortRange(word_t lowIndex, word_t highIndex,
                 C compare, List<T>& scratch) {
    word_t elemsInRange = highIndex - lowIndex;
    if (elemsInRange <= 1)
      return;
    word_t midIndex = lowIndex + elemsInRange / 2;
    sortRange(lowIndex, midIndex, compare, scratch);
    sortRange(midIndex, highIndex, compare, scratch);
    for (word_t i = 0, l = lowIndex, r = midIndex; i < elemsInRange; i++) {
      if (compare(at(l), at(r)) <= 0) {
        scratch[i] = at(l++);
      } else {
        scratch[i] = at(r++);
      }
    }
    for (word_t i = 0; i < elemsInRange; i++)
      at(lowIndex + i) = scratch[i];
  }

  static int defaultCompare(const T& l, const T& r) {
    if (l < r)
      return -1;
    else if (l > r)
      return +1;
    else
      return 0;
  }

  void ensureCapacity(word_t requiredCapacity) {
    if (requiredCapacity == 0)
      return;
    word_t newCapacity = capacity_ > 0 ? capacity_ : requiredCapacity;
    while (newCapacity < requiredCapacity)
      newCapacity *= 2;
    if (newCapacity > capacity_) {
      T* newData = new T[newCapacity];
      for (word_t i = 0; i < capacity_; i++)
        newData[i] = data_[i];
      if (data_ != nullptr)
        delete [] data_;
      data_ = newData;
      capacity_ = newCapacity;
    }
  }

 private:
  static const word_t kDefaultCapacity = 4;

  word_t length_;
  word_t capacity_;
  T* data_;
};

}
}

#endif

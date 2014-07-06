// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef map_h
#define map_h

#include "utils.h"

namespace codeswitch {
namespace internal {

// Requirements for template parameter K:
// - static emptyKey() returns a sentinel value for an unused key
// - static deletedKey() returns a sentinel value for a deleted key
// - is immutable
// - == and != operators defined
// - hash() returns a u32 hash code (always the same for the same key).
// - if == returns true for two keys, hash() must return the same value.

template <class K, class V>
class Map {
 public:
  Map(word_t initialCapacity = kDefaultCapacity);
  ~Map();

  bool contains(const K& key);
  V& get(const K& key);
  V& operator [] (const K& key) { return get(key); }
  void put(const K& key, const V& value);
  void remove(const K& key);
  word_t length();

  class Iterator {
   public:
    Iterator(Map* map);

    bool done();
    const K& key();
    V& value();
    void advance();

   private:
    Map* map_;
    word_t index_;
  };
  Iterator iterator() { return Iterator(this); }

  static const word_t kDefaultCapacity = 16;

 private:
  word_t lookupForRead(const K& key);
  word_t lookupForWrite(const K& key);
  void ensureCapacity(word_t newEntryCount);

  struct Entry {
    K key;
    V value;
  };

  Entry* entries_;
  word_t capacity_;
  word_t entryCount_;
  word_t deletedCount_;

  friend class Iterator;
};


struct WordKey {
 public:
  WordKey(word_t key)
      : key_(key) { }

  static WordKey emptyKey() { return kEmptyKey; }
  static WordKey deletedKey() { return kDeletedKey; }

  bool operator == (const WordKey& k) { return key_ == k.key_; }
  bool operator != (const WordKey& k) { return key_ != k.key_; }
  u32 hash() {
    // Thomas Wang's integer hash function.
    // Original site appears to no longer exist:
    // http://www.concentric.net/~ttwang/tech/inthash.htm
#if WORDSIZE == 64
    u32 h = (key_ >> 32) ^ (key_ & (0xFFFFFFFF));
#else
    u32 h = key_;
#endif
    h = (h ^ 61) ^ (h >> 16);
    h = h + (h << 3);
    h = h ^ (h >> 4);
    h = h * 0x27d4eb2d;
    h = h ^ (h >> 15);
    return h;
  }

  static const word_t kEmptyKey = kNotFound;
  static const word_t kDeletedKey = kEmptyKey - 1;

 private:
  word_t key_;
};


template <class K, class V>
Map<K, V>::Map(word_t initialCapacity)
    : entries_(new Entry[initialCapacity]),
      capacity_(initialCapacity),
      entryCount_(0),
      deletedCount_(0) {
  for (word_t i = 0; i < capacity_; i++) {
    entries_[i].key = K::emptyKey();
  }
}


template <class K, class V>
Map<K, V>::~Map() {
  delete [] entries_;
}


template <class K, class V>
bool Map<K, V>::contains(const K& key) {
  word_t index = lookupForRead(key);
  return index != kNotFound;
}


template <class K, class V>
V& Map<K, V>::get(const K& key) {
  word_t index = lookupForRead(key);
  return entries_[index].value;
}


template <class K, class V>
void Map<K, V>::put(const K& key, const V& value) {
  ASSERT(!contains(key));
  ensureCapacity(entryCount_ + 1);
  word_t index = lookupForWrite(key);
  entries_[index].key = key;
  entries_[index].value = value;
  entryCount_++;
}


template <class K, class V>
void Map<K, V>::remove(const K& key) {
  word_t index = lookupForRead(key);
  entries_[index].key = K::deletedKey();
  entryCount_--;
  deletedCount_++;
}


template <class K, class V>
word_t Map<K, V>::length() {
  return entryCount_;
}


template <class K, class V>
Map<K, V>::Iterator::Iterator(Map<K, V>* map)
    : map_(map),
      index_(0) {
  while (index_ < map_->capacity_ &&
         (map_->entries_[index_] == K::emptyKey() || map_->entries_[index_] == K::deletedKey()))
    index_++;
}


template <class K, class V>
bool Map<K, V>::Iterator::done() {
  return index_ < map_->capacity_;
}


template <class K, class V>
const K& Map<K, V>::Iterator::key() {
  return map_->entries_[index_].key;
}


template <class K, class V>
V& Map<K, V>::Iterator::value() {
  return map_->entries_[index_].value;
}


template <class K, class V>
void Map<K, V>::Iterator::advance() {
  do {
    index_++;
  } while (index_ < map_->entryCount_ &&
           (map_->entries_[index_].key == K::emptyKey() ||
            map_->entries_[index_].key == K::deletedKey()));
}


template <class K, class V>
word_t Map<K, V>::lookupForRead(const K& key) {
  word_t hash = key.hash();
  word_t mask = capacity_ - 1;
  word_t index = hash & mask;
  word_t count = 0;
  while (count < capacity_) {
    if (entries_[index].key == key)
      return index;
    else if (entries_[index].key == K::emptyKey())
      return kNotFound;
    index = (index + 1) & mask;
    count++;
  }
  return kNotFound;
}


template <class K, class V>
word_t Map<K, V>::lookupForWrite(const K& key) {
  word_t hash = key.hash();
  word_t mask = capacity_ - 1;
  word_t index = hash & mask;
  word_t count = 0;
  while (count < capacity_ &&
         entries_[index].key != K::emptyKey() &&
         entries_[index].key != K::deletedKey()) {
    index = (index + 1) & mask;
    count++;
  }
  if (count == capacity_)
    return kNotFound;
  return index;
}


// template <class K, class V>
// void Map<K, V>::ensureCapacity(word_t newEntryCount) {
//   word_t usedEntries = newEntryCount + deletedCount_;
//   if (usedEntries * 3 / 4 >= capacity_) {
//     word_t newCapacity = capacity_ * 2;
//     newEntries = new Entry[newCapacity];
//     for (word_t i = 0; i < newCapacity; i++)
//       newEntries[i] = K::emptyKey();
//     word_t mask = newCapacity - 1;
//     for (Iterator it = iterator(); !it.done(); it.advance()) {
//       const K& k = it.key();
//       word_t hash = k.hash();
//       word_t index = hash & mask;
//       while (newEntries[index] != K::emptyKey())
//         index = (index + 1) & mask;
//       newEntries[index].key = k;
//       newEntries[index].value = it.value();
//     }
//     delete entries_;
//     entries_ = newEntries;
//     capacity_ = newCapacity;
//     deletedCount_ = 0;
//   }
// }


}
}

#endif

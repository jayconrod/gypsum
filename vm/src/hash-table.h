// Copyright 2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef hash_table_h
#define hash_table_h

#include <iostream>
#include "block.h"
#include "handle.h"

namespace codeswitch {
namespace internal {

template <class E>
class HashTable: public Block {
 public:
  typedef E Element;

  void* operator new(size_t, Heap* heap, length_t capacity) {
    ASSERT(isPowerOf2(capacity));
    auto size = sizeForCapacity(capacity);
    auto table = reinterpret_cast<HashTable<E>*>(heap->allocate(size));
    table->capacity_ = capacity;
    return table;
  }

  explicit HashTable(BlockType type)
      : Block(type) {
    for (length_t i = 0; i < capacity_; i++)
      elements()[i].setEmpty();
  }

  static word_t sizeForCapacity(length_t length) {
    return sizeof(HashTable<E>) + length * sizeof(E);
  }

  length_t capacity() const { return capacity_; }
  length_t liveCount() const { return liveCount_; }
  bool isEmpty() const { return liveCount_ == 0; }
  length_t deadCount() const { return deadCount_; }

  E* elements() { return reinterpret_cast<E*>(elementsBase()); }
  const E* elements() const { return reinterpret_cast<const E*>(elementsBase()); }

  void rehash(const HashTable<E>* table) {
    ASSERT(table->liveCount_ <= capacity_ - liveCount_);
    const E* tableElems = table->elements();
    for (length_t i = 0; i < table->capacity_; i++) {
      if (tableElems[i].isEmpty() || tableElems[i].isDead())
        continue;
      add(tableElems[i]);
    }
  }

  void add(const E& elem) {
    ASSERT(liveCount_ < capacity_);
    u32 code = elem.hashCode();
    length_t probe = mask() & static_cast<length_t>(code);
    E* elems = elements();
    while (!elems[probe].isEmpty() && !elems[probe].isDead()) {
      ASSERT(!elems[probe].matches(elem));
      probe = (probe + 1) & mask();
    }
    elems[probe].set(this, elem);
    liveCount_++;
  }

  void remove(const E& elem) {
    auto index = findIndex(elem);
    if (index == kIndexNotSet)
      return;
    elements()[index].setDead();
    liveCount_--;
    deadCount_++;
  }

  bool contains(const E& elem) const {
    return findIndex(elem) != kIndexNotSet;
  }

  const E* find(const E& elem) const {
    auto index = findIndex(elem);
    return index != kIndexNotSet
         ? elements() + index
         : nullptr;
  }

  E* find(const E& elem) {
    auto index = findIndex(elem);
    return index != kIndexNotSet
         ? elements() + index
         : nullptr;
  }

 protected:
  length_t mask() const { return capacity_ - 1; }
  length_t findIndex(const E& elem) const {
    u32 code = elem.hashCode();
    length_t start = mask() & static_cast<length_t>(code);
    const E* elems = elements();
    if (elems[start].matches(elem))
      return start;

    length_t probe = (start + 1) & mask();
    while (probe != start && !elems[probe].matches(elem)) {
      probe = (probe + 1) & mask();
    }
    return elems[probe].isDead() ? kIndexNotSet : probe;
  }

  length_t capacity_;
  length_t liveCount_;
  length_t deadCount_;
};


template <class Table>
class HashMap: public Block {
 public:
  explicit HashMap(BlockType type)
      : Block(type) { }

  DEFINE_NEW(HashMap)

  static const length_t kDefaultInitialCapacity = 32;

  length_t length() const { return table() ? table()->liveCount() : 0; }
  bool isEmpty() const { return length() == 0; }

  static void add(Heap* heap, Local<HashMap<Table>> map,
                  const typename Table::Element::SafeKey& key,
                  const typename Table::Element::SafeValue& value) {
    ensureCapacity(heap, map, 1);
    typename Table::Element element(key, value);
    map->table()->add(element);
  }

  static void remove(Heap* heap, Local<HashMap<Table>> map,
                     const typename Table::Element::SafeKey& key) {
    if (!map->table_)
      return;
    typename Table::Element element(key);
    map->table()->remove(element);
  }

  bool contains(const typename Table::Element::Key key) const {
    if (!table_)
      return false;
    typename Table::Element element(key);
    return table()->contains(element);
  }

  typename Table::Element::Value get(const typename Table::Element::Key key) const {
    ASSERT(table_);
    typename Table::Element element(key);
    const typename Table::Element* found = table()->find(element);
    ASSERT(found != nullptr);
    return found->value;
  }

  typename Table::Element::Value getOrElse(const typename Table::Element::Key key,
                                           typename Table::Element::Value defaultValue) const {
    if (!table_)
      return defaultValue;
    typename Table::Element element(key);
    const typename Table::Element* found = table()->find(element);
    return found != nullptr ? found->value : defaultValue;
  }

 private:
  friend class Roots;

  Table* table() const { return table_.get(); }

  length_t capacity() const {
    return table() ? table()->capacity() : 0;
  }

  length_t softCapacity() const {
    return capacity() / 2 + capacity() / 4;
  }

  static void ensureCapacity(Heap* heap, const Handle<HashMap<Table>>& map, length_t count) {
    auto newLength = map->length() + count;
    if (newLength >= map->softCapacity()) {
      auto newCapacity = max(roundUpToPowerOf2(newLength),
                             kDefaultInitialCapacity);
      resize(heap, map, newCapacity);
    }
  }

  static void resize(Heap* heap, const Handle<HashMap<Table>>& map, length_t capacity) {
    ASSERT(isPowerOf2(capacity));
    ASSERT(map->length() <= capacity);
    auto newTable = Table::create(heap, capacity);
    if (map->table_)
      newTable->rehash(map->table_.get());
    map->table_.set(*map, *newTable);
  }

  static const word_t kPointerMap = 2;

  Ptr<Table> table_;
  // Update kPointerMap if any pointers change.
};


template <class K, class V>
struct BlockHashMapElement {
  typedef K* Key;
  typedef Local<K> SafeKey;
  typedef V* Value;
  typedef Local<V> SafeValue;

  static Key keyFromSafeKey(const SafeKey& key) { return *key; }
  static Value valueFromSafeValue(const SafeValue& value) { return value.getOrNull(); }

  BlockHashMapElement()
      : key(reinterpret_cast<Key>(kEmpty)) { }
  explicit BlockHashMapElement(Key key)
      : key(key) { }
  explicit BlockHashMapElement(SafeKey key)
      : key(keyFromSafeKey(key)) { }
  BlockHashMapElement(Key key, Value value)
      : key(key), value(value) { }
  BlockHashMapElement(SafeKey key, SafeValue value)
      : key(keyFromSafeKey(key)),
        value(valueFromSafeValue(value)) { }

  bool isEmpty() const { return key == reinterpret_cast<Key>(kEmpty); }
  void setEmpty() { key = reinterpret_cast<Key>(kEmpty); }
  bool isDead() const { return key == reinterpret_cast<Key>(kDead); }
  void setDead() { key = reinterpret_cast<Key>(kDead); }
  bool isLive() const { return !isEmpty() && !isDead(); }

  void set(const HashTable<BlockHashMapElement>* table, const BlockHashMapElement& elem) {
    key = elem.key;
    table->getHeap()->recordWrite(&key, elem.key);
    value = elem.value;
    table->getHeap()->recordWrite(&value, elem.value);
  }

  bool operator == (const BlockHashMapElement& other) const {
    return key == other.key && value == other.value;
  }
  bool operator != (const BlockHashMapElement& other) const {
    return !(*this == other);
  }
  bool matches(const BlockHashMapElement& other) const {
    return isLive() && key->equals(other.key);
  }
  u32 hashCode() const {
    return key->hashCode();
  }

  Key key;
  Value value;

 private:
  static const word_t kEmpty = 0;
  static const word_t kDead = 1;

  BlockHashMapElement& operator = (const BlockHashMapElement& other) = delete;
};


template <class K, class V>
class BlockHashMapTable: public HashTable<BlockHashMapElement<K, V>> {
 public:
  static const BlockType kBlockType = BLOCK_HASH_MAP_TABLE_BLOCK_TYPE;

  BlockHashMapTable()
      : HashTable<BlockHashMapElement<K, V>>(kBlockType) { }

  static Local<BlockHashMapTable> create(Heap* heap, length_t capacity) {
    RETRY_WITH_GC(heap, return Local<BlockHashMapTable>(
        new(heap, capacity) BlockHashMapTable()));
  }

 private:
  friend class Roots;

  static const word_t kElementPointerMap = 3;
};


template <class K, class V>
class BlockHashMap: public HashMap<BlockHashMapTable<K, V>> {
 public:
  static const BlockType kBlockType = BLOCK_HASH_MAP_BLOCK_TYPE;

  DEFINE_NEW(BlockHashMap)

  BlockHashMap()
      : HashMap<BlockHashMapTable<K, V>>(kBlockType) { }

  static Local<BlockHashMap> create(Heap* heap) {
    RETRY_WITH_GC(heap, return Local<BlockHashMap>(new(heap) BlockHashMap()));
  }
};

}
}

#endif

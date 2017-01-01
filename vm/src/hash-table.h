// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef hash_table_h
#define hash_table_h

#include <functional>
#include <iostream>
#include "block.h"
#include "defnid.h"
#include "handle.h"
#include "utils.h"

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
    return elementsOffset(sizeof(HashTable<E>), sizeof(E)) + length * sizeof(E);
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
    bool match = false;
    while (probe != start &&
           !elems[probe].isEmpty() &&
           !(match = elems[probe].matches(elem))) {
      probe = (probe + 1) & mask();
    }
    return match ? probe : kIndexNotSet;
  }

  length_t capacity_;
  length_t liveCount_;
  length_t deadCount_;
};


template <class E>
std::ostream& operator << (std::ostream& os, const HashTable<E>* table) {
  os << brief(table)
     << "\n  capacity: " << table->capacity()
     << "\n  liveCount: " << table->liveCount()
     << "\n  deadCount: " << table->deadCount();
  auto elements = table->elements();
  for (length_t i = 0; i < table->capacity(); i++) {
    if (elements[i].isLive()) {
      os << "\n  element #" << i << ":" << elements[i];
    }
  }
  return os;
}


inline length_t recommendHashTableCapacity(length_t length) {
  ASSERT(length >= 0);
  if (length == 0) {
    return 1;
  } else if (length < 4) {
    return 4;
  } else {
    return roundUpToPowerOf2(length * 4 / 3);
  }
}


template <class Table>
class HashMap: public Block {
 public:
  explicit HashMap(BlockType type)
      : Block(type) { }

  DEFINE_NEW(HashMap)

  static const length_t kDefaultInitialCapacity = 32;

  length_t length() const { return table() ? table()->liveCount() : 0; }
  bool isEmpty() const { return length() == 0; }

  Table* table() { return table_.get(); }
  const Table* table() const { return table_.get(); }

  static void add(Local<HashMap<Table>> map,
                  const typename Table::Element::SafeKey& key,
                  const typename Table::Element::SafeValue& value) {
    ensureCapacity(map->getHeap(), map, 1);
    typename Table::Element element(key, value);
    map->table()->add(element);
  }

  static void remove(Local<HashMap<Table>> map,
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

  length_t capacity() const {
    return table() ? table()->capacity() : 0;
  }

  length_t softCapacity() const {
    return capacity() / 2 + capacity() / 4;
  }

  static void ensureCapacity(Heap* heap, const Handle<HashMap<Table>>& map, length_t count) {
    auto newLength = map->length() + count;
    if (newLength >= map->softCapacity()) {
      auto newCapacity = max(roundUpToPowerOf2(newLength) * 2,
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


template <class Table>
std::ostream& operator << (std::ostream& os, const HashMap<Table>* map) {
  os << brief(map)
     << "\n  table: " << brief(map->table());
  return os;
}


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

  BlockHashMapElement& operator = (const BlockHashMapElement&) = delete;
};


template <class K, class V>
std::ostream& operator << (std::ostream& os, const BlockHashMapElement<K, V>& elem) {
  os << "\n    key: " << elem.key
     << "\n    value: " << elem.value;
  return os;
}


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


template <class V>
struct DefnIdHashMapElement {
  typedef DefnId Key;
  typedef DefnId SafeKey;
  typedef V* Value;
  typedef Local<V> SafeValue;

  static Key keyFromSafeKey(const SafeKey& key) { return key; }
  static Value valueFromSafeValue(const SafeValue& value) { return value.getOrNull(); }

  DefnIdHashMapElement() { }
  explicit DefnIdHashMapElement(Key key)
      : key(key) { }
  DefnIdHashMapElement(Key key, Value value)
      : key(key), value(value) { }
  DefnIdHashMapElement(SafeKey key, SafeValue value)
      : key(keyFromSafeKey(key)), value(valueFromSafeValue(value)) { }

  bool isEmpty() const { return key == empty(); }
  void setEmpty() { key = empty(); }
  bool isDead() const { return key == dead(); }
  void setDead() { key = dead(); }
  bool isLive() const { return key.isValid(); }

  void set(const HashTable<DefnIdHashMapElement>* table, const DefnIdHashMapElement& elem) {
    key = elem.key;
    value = elem.value;
    table->getHeap()->recordWrite(&value, elem.value);
  }

  bool operator == (const DefnIdHashMapElement& other) const {
    return key == other.key && value == other.value;
  }
  bool operator != (const DefnIdHashMapElement& other) const {
    return !(*this == other);
  }
  bool matches(const DefnIdHashMapElement& other) const {
    return isLive() && key == other.key;
  }
  u32 hashCode() const {
    return static_cast<u32>(std::hash<DefnId>()(key));
  }

  Key key;
  Value value;

 private:
  static Key empty() { return DefnId(); }
  static Key dead() { return DefnId(DefnId::NOT_VALID, kIdNotSet, 0 /* index */); }

  DefnIdHashMapElement& operator = (const DefnIdHashMapElement&) = delete;
};


template <class V>
std::ostream& operator << (std::ostream& os, const DefnIdHashMapElement<V>& elem) {
  os << "\n    key: " << elem.key
     << "\n    value: " << elem.value;
  return os;
}


template <class V>
class DefnIdHashMapTable: public HashTable<DefnIdHashMapElement<V>> {
 public:
  static const BlockType kBlockType = DEFN_ID_HASH_MAP_TABLE_BLOCK_TYPE;

  DefnIdHashMapTable()
      : HashTable<DefnIdHashMapElement<V>>(kBlockType) { }

  static Local<DefnIdHashMapTable> create(Heap* heap, length_t capacity) {
    RETRY_WITH_GC(heap, return Local<DefnIdHashMapTable>(
        new(heap, capacity) DefnIdHashMapTable()));
  }

 private:
  friend class Roots;

  static const word_t kElementPointerMap =
      1 << (offsetof(DefnIdHashMapElement<Block*>, value) / kWordSize);
};


template <class V>
class DefnIdHashMap: public HashMap<DefnIdHashMapTable<V>> {
 public:
  static const BlockType kBlockType = DEFN_ID_HASH_MAP_BLOCK_TYPE;

  DEFINE_NEW(DefnIdHashMap)

  DefnIdHashMap()
      : HashMap<DefnIdHashMapTable<V>>(kBlockType) { }

  static Local<DefnIdHashMap> create(Heap* heap) {
    RETRY_WITH_GC(heap, return Local<DefnIdHashMap>(new(heap) DefnIdHashMap()));
  }
};

}
}

#endif

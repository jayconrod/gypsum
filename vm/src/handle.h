// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef handle_h
#define handle_h

#include <deque>
#include <iterator>
#include <vector>
#include "utils.h"

namespace codeswitch {
namespace internal {

class Block;
class VM;

/** Handles are used in C++ code to indirectly reference blocks on the garbage collected heap.
 *  These are necessary since the GC cannot precisely scan the C++ stack for pointers. It
 *  also can't update pointers on the stack when objects move.
 *
 *  This is a base class for handles. Local and Persistent are subclasses.
 */
template <class T>
class Handle {
 public:
  T* operator * () const;
  T* operator -> () const;
  T* getOrNull() const;

  operator bool () const;
  bool operator ! () const;
  bool isEmpty() const;

 protected:
  Handle()
      : slot_(nullptr) { }
  explicit Handle(T** slot)
      : slot_(slot) { }
  Handle(const Handle& h) = delete;

  T** slot_;
};


/** A Local is a handle with stack lifetime. It must be created between the creation and
 *  destruction of a `HandleScope`. It is no longer valid after the most local `HandleScope` is
 *  destroyed (unless it is created with `HandleScope::escape`).
 */
template <class T>
class Local: public Handle<T> {
 public:
  Local();
  template <class S>
  explicit Local(S* block);
  template <class S>
  Local(VM* vm, S* block);
  Local(const Local<T>& local);
  template <class S>
  Local(const Local<S>& local);
  template <class S>
  Local(const Handle<S>& handle);
  Local& operator = (const Local<T>& local);
  template <class S>
  Local& operator = (const Local<S>& local);
  template <class S>
  Local& operator = (const Handle<S>& handle);
  void clear();

 private:
  Local(T** slot)
      : Handle<T>(slot) { }

  friend class HandleScope;
  template <class S>
  friend class Local;
};


/** A Persistent is a handle with no fixed lifetime. This may be stored in data structures or
 *  passed to a client. It is slower to create and destroy than Local though.
 */
template <class T>
class Persistent: public Handle<T> {
 public:
  Persistent() {}
  template <class S>
  explicit Persistent(S* block);
  template <class S>
  Persistent(VM* vm, S* block);
  Persistent(const Persistent<T>& persistent);
  template <class S>
  Persistent(const Handle<S>& handle);
  Persistent(Persistent<T>&& persistent);
  ~Persistent();
  Persistent& operator = (const Persistent<T>& persistent);
  template <class S>
  Persistent& operator = (const Handle<S>& handle);
  Persistent& operator = (Persistent<T>&& persistent);

  template <class S>
  void set(S* block);

 private:
  void release();

  template <class S>
  friend class Persistent;
};


static_assert(sizeof(Persistent<Block>) == sizeof(void*),
    "Persistent is not size of pointer");


template <class T>
Local<T> handle(T* value) {
  return Local<T>(value);
}


/** HandleStorage contains the pointers Handles actually point to in one big list. The GC
 *  can scan and update this list all at once. Handles are always allocated by adding a new
 *  element to the end of the list. `HandleScope`s are used to free handles at the end of
 *  the list.
 */
class HandleStorage {
 public:
  HandleStorage();

  // These methods help the templated handle constructors avoid a direct dependency on the VM
  // class itself. Since vm.h includes this header, we would have a circular reference.
  static HandleStorage* fromBlock(Block* block);
  static HandleStorage* fromVM(VM* vm);

  Block** createLocal(Block* block);
  template <class T>
  T** createLocal(T* block);
  void createPersistent(Block* block, Block*** out_slot);
  template <class T>
  void createPersistent(T* block, T*** out_slot);
  void destroyPersistent(Block** slot);

  class iterator: public std::iterator<std::input_iterator_tag, Block**> {
   public:
    Block** operator * () { return &*it_; }
    bool operator == (const iterator& other) const { return it_ == other.it_; }
    bool operator != (const iterator& other) const { return !(*this == other); }
    iterator& operator ++ ();

   private:
    iterator(const std::deque<Block*>::iterator it,
             bool isLocal,
             HandleStorage* storage);
    void advance();
    bool isValid() const;
    bool done() const;

    std::deque<Block*>::iterator it_;
    bool isLocal_;
    HandleStorage* storage_;

    friend HandleStorage;
  };
  iterator begin();
  iterator end();

  template <class Callback>
  void visitPointers(Callback callback) {
    for (auto p : *this)
      callback(p);
  }

 private:
  bool canCreateLocal_;

  // We use a deque to guarantee references to elements are stable after insertion at the end.
  // We don't insert at the beginning.
  std::deque<Block*> localSlots_;
  std::deque<Block*> persistentSlots_;
  std::deque<Block**> persistentFreeList_;

  friend class GC;
  friend class HandleScope;
  friend class SealHandleScope;
};


/** HandleScope controls allocation of handles by HandleStorage. When a HandleScope is
 *  destroyed, the handles that were created since the HandleScope was created are also
 *  destroyed. This is the only way for handles to be destroyed.
 */
class HandleScope {
 public:
  explicit HandleScope(HandleStorage* storage);
  explicit HandleScope(VM* vm);
  ~HandleScope();

  /** Create a handle that will survive this scope. This can only be done once per scope.
   *  There must be a parent scope.
   */
  template <class T>
  Local<T> escape(T* block);

 private:
  HandleStorage* storage_;
  bool oldCanCreateLocal_;
  size_t oldSize_;

  Block** escapeSlot_;
  bool escapeSlotUsed_;
};


class SealHandleScope {
 public:
  explicit SealHandleScope(HandleStorage* storage);
  explicit SealHandleScope(VM* vm);
  ~SealHandleScope();

 private:
  HandleStorage* storage_;
  bool oldCanCreateLocal_;
};


template <class T>
T* Handle<T>::operator * () const {
  ASSERT(slot_ != nullptr);
  return *slot_;
}


template <class T>
T* Handle<T>::operator -> () const {
  ASSERT(slot_ != nullptr);
  return *slot_;
}


template <class T>
T* Handle<T>::getOrNull() const {
  return slot_ == nullptr ? nullptr : *slot_;
}


template <class T>
Handle<T>::operator bool () const {
  return slot_ != nullptr;
}


template <class T>
bool Handle<T>::operator ! () const {
  return slot_ == nullptr;
}


template <class T>
bool Handle<T>::isEmpty() const {
  return slot_ == nullptr;
}


template <class T>
Local<T>::Local()
    : Handle<T>(nullptr) { }


template <class T>
template <class S>
Local<T>::Local(S* block)
    : Handle<T>(reinterpret_cast<T**>(HandleStorage::fromBlock(block)->createLocal(block))) {
  CHECK_SUBTYPE_VALUE(T*, block);
}


template <class T>
template <class S>
Local<T>::Local(VM* vm, S* block)
    : Handle<T>(block != nullptr
        ? reinterpret_cast<T**>(HandleStorage::fromVM(vm)->createLocal(block))
        : nullptr) {
  CHECK_SUBTYPE_VALUE(T*, block);
}


template <class T>
Local<T>::Local(const Local<T>& local)
    : Handle<T>(local.slot_) { }


template <class T>
template <class S>
Local<T>::Local(const Local<S>& local)
    : Handle<T>(reinterpret_cast<T**>(local.slot_)) {
  CHECK_SUBTYPE_VALUE(T*, *local);
}


template <class T>
template <class S>
Local<T>::Local(const Handle<S>& handle)
    : Handle<T>(handle ?
                reinterpret_cast<T**>(HandleStorage::fromBlock(*handle)->createLocal(*handle))
                : nullptr) {
  CHECK_SUBTYPE_VALUE(T*, *handle);
}


template <class T>
Local<T>& Local<T>::operator = (const Local<T>& local) {
  // g++ complains if we just say `slot_`
  this->slot_ = local.slot_;
  return *this;
}


template <class T>
template <class S>
Local<T>& Local<T>::operator = (const Local<S>& local) {
  this->slot_ = reinterpret_cast<T**>(local.slot_);
  CHECK_SUBTYPE_VALUE(T*, *local);
  return *this;
}


template <class T>
template <class S>
Local<T>& Local<T>::operator = (const Handle<S>& handle) {
  this->slot_ = handle
      ? reinterpret_cast<T**>(HandleStorage::fromBlock(*handle)->createLocal(*handle))
      : nullptr;
  CHECK_SUBTYPE_VALUE(T*, *handle);
  return *this;
}


template <class T>
void Local<T>::clear() {
  this->slot_ = nullptr;
}


template <class T>
template <class S>
Persistent<T>::Persistent(S* block) {
  HandleStorage::fromBlock(block)->createPersistent(block, &this->slot_);
  CHECK_SUBTYPE_VALUE(T*, block);
}


template <class T>
template <class S>
Persistent<T>::Persistent(VM* vm, S* block) {
  HandleStorage::fromVM(vm)->createPersistent(block, &this->slot_);
  CHECK_SUBTYPE_VALUE(T*, block);
}


template <class T>
Persistent<T>::Persistent(const Persistent<T>& persistent) {
  if (persistent) {
    HandleStorage::fromBlock(*persistent)->createPersistent(*persistent, &this->slot_);
  }
}


template <class T>
template <class S>
Persistent<T>::Persistent(const Handle<S>& handle) {
  if (handle) {
    HandleStorage::fromBlock(*handle)->createPersistent(
        reinterpret_cast<Block*>(*handle),
        reinterpret_cast<Block***>(&this->slot_));
  }
  CHECK_SUBTYPE_VALUE(T*, *handle);
}


template <class T>
Persistent<T>::Persistent(Persistent<T>&& persistent)
    : Handle<T>(persistent.slot_) {
  persistent.slot_ = nullptr;
}


template <class T>
Persistent<T>::~Persistent() {
  release();
}


template <class T>
Persistent<T>& Persistent<T>::operator = (const Persistent<T>& persistent) {
  release();
  if (persistent) {
    HandleStorage::fromBlock(*persistent)->createPersistent(*persistent, &this->slot_);
  }
  return *this;
}


template <class T>
template <class S>
Persistent<T>& Persistent<T>::operator = (const Handle<S>& handle) {
  if (this->isEmpty()) {
    if (handle)
      HandleStorage::fromBlock(*handle)->createPersistent(*handle, &this->slot_);
  } else {
    if (handle) {
      *this->slot_ = *handle;
    } else {
      release();
    }
  }
  return *this;
}


template <class T>
Persistent<T>& Persistent<T>::operator = (Persistent<T>&& persistent) {
  release();
  this->slot_ = persistent.slot_;
  persistent.slot_ = nullptr;
  return *this;
}


template <class T>
template <class S>
void Persistent<T>::set(S* block) {
  ASSERT(!this->isEmpty() && block != nullptr);
  *this->slot_ = block;
}


template <class T>
void Persistent<T>::release() {
  auto slot = reinterpret_cast<Block**>(this->slot_);
  if (slot == nullptr)
    return;
  HandleStorage::fromBlock(*slot)->destroyPersistent(slot);
  this->slot_ = nullptr;
}



template <class T>
T** HandleStorage::createLocal(T* block) {
  ASSERT(block != nullptr);
  CHECK_SUBTYPE_VALUE(Block*, block);
  return reinterpret_cast<T**>(createLocal(reinterpret_cast<Block*>(block)));
}


template <class T>
void HandleStorage::createPersistent(T* block, T*** out_slot) {
  ASSERT(block != nullptr);
  CHECK_SUBTYPE_VALUE(Block*, block);
  createPersistent(reinterpret_cast<Block*>(block), reinterpret_cast<Block***>(out_slot));
}


template <class T>
Local<T> HandleScope::escape(T* block) {
  ASSERT(block != nullptr);
  ASSERT(!escapeSlotUsed_);
  T** slot = reinterpret_cast<T**>(escapeSlot_);
  *slot = block;
  escapeSlotUsed_ = true;
  oldSize_++;
  return Local<T>(slot);
}

}
}

#endif

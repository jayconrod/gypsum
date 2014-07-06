// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef handle_inl_h
#define handle_inl_h

#include <memory>
#include <vector>
#include "handle.h"
#include "memory-inl.h"
#include "vm-inl.h"

namespace codeswitch {
namespace internal {

/** HandleData points directly to a heap block. Handles refer to these. HandleData is
 *  reference counted, so handles may be easily copied and stored. Each HandleData is an element
 *  of a HandleDataBlock, which is part of HandleStorage. When not in use, HandleData is part
 *  of a free list within HandleDataBlock.
 */
struct HandleData {
  bool isValid() { return refCount > 0; }
  void invalidate() { refCount = 0; }

  union {
    Block* block;
    word_t freeNext;
  };
  word_t refCount;
};


/** HandleDataBlock is a contiguous, aligned chunk of memory which contains a large number of
 *  HandleData elements. Internally, HandleDataBlock is allocated with mmap to guarantee
 *  alignment. This lets us easily figure out what block any given handle belongs to.
 */
class HandleDataBlock: public MemoryChunk {
 public:
  static HandleDataBlock* allocate();

  inline HandleData* allocateHandle(Block* block);
  inline void freeHandle(HandleData* handle);

  DEFINE_INL_ACCESSORS(word_t, freeCount, setFreeCount, kFreeCountOffset)

  inline HandleData* at(word_t index);

  static inline HandleDataBlock* fromHandle(HandleData* handle);

  static const word_t kFreeListHeadOffset = kMemoryChunkHeaderSize;
  static const word_t kFreeCountOffset = kFreeListHeadOffset + kWordSize;
  static const word_t kHeaderSize = kFreeCountOffset + kWordSize;

  static const word_t kSize = 32 * KB;
  static const word_t kCount = (kSize - kHeaderSize) / sizeof(HandleData);

 private:
  DEFINE_INL_ACCESSORS(word_t, freeListHead, setFreeListHead, kFreeListHeadOffset)

  static const word_t kFreeListEmpty = ~0ULL;
  static const word_t kFreeListFreeAfter = ~0ULL - 1;
};


/** HandleStorage is the top-level class for managing handles. Each VM should have one of these.
 *  It creates or destroys HandleDataBlocks as needed and provides a mechanism to iterate
 *  over all handles.
 */
class HandleStorage {
 public:
  HandleStorage();
  HandleStorage(const HandleStorage&) = delete;
  HandleStorage& operator = (const HandleStorage&) = delete;

  inline HandleData* allocateHandle(Block* block);

  class iterator {
   public:
    inline iterator(std::vector<std::unique_ptr<HandleDataBlock>>::iterator blockIt,
                    word_t dataIndex)
        : blockIt_(blockIt), dataIndex_(dataIndex) { }
    inline iterator& operator ++ ();
    inline HandleData& operator * ();
    inline bool operator == (const iterator& other) const;
    inline bool operator != (const iterator& other) const { return !(*this == other); }

   private:
    std::vector<std::unique_ptr<HandleDataBlock>>::iterator blockIt_;
    word_t dataIndex_;
  };

  inline iterator begin();
  inline iterator end();

 private:
  HandleData* allocateHandleSlow(Block* block);

  std::vector<std::unique_ptr<HandleDataBlock>> blocks_;
  word_t allocationIndex_;
};


template <class T>
Handle<T>::Handle()
    : data_(nullptr) { }


template <class T>
template <class S>
Handle<T>::Handle(S* block)
    : data_(block != nullptr
            ? VM::fromAddress(block)->handleStorage().allocateHandle(block)
            : nullptr) {
  T* t = block;   // ensure S <: T
  USE(t);
}


template <class T>
template <class S>
Handle<T>::Handle(VM* vm, S* block)
    : data_(block != nullptr
            ? vm->handleStorage().allocateHandle(block)
            : nullptr) {
  T* t = block;
  USE(t);
}


template <class T>
Handle<T>::Handle(const Handle<T>& handle)
    : data_(handle.data_) {
  retain();
}


template <class T>
template <class S>
Handle<T>::Handle(const Handle<S>& handle)
    : data_(handle.data()) {
  T* t = *handle;
  USE(t);
  retain();
}


template <class T>
Handle<T>::Handle(Handle<T>&& handle)
    : data_(handle.data_) {
  handle.data_ = nullptr;
}


template <class T>
Handle<T>& Handle<T>::operator = (const Handle<T>& handle) {
  if (data_ != handle.data_) {
    release();
    data_ = handle.data_;
    retain();
  }
  return *this;
}


template <class T>
template <class S>
Handle<T>& Handle<T>::operator = (const Handle<S>& handle) {
  if (data_ != handle.data()) {
    release();
    data_ = handle.data();
    T* t = *handle;
    USE(t);
    retain();
  }
  return *this;
}


template <class T>
Handle<T>& Handle<T>::operator = (Handle<T>&& handle) {
  if (data_ != handle.data_) {
    release();
    data_ = handle.data_;
    handle.data_ = nullptr;
  }
  return *this;
}


template <class T>
Handle<T>::~Handle() {
  release();
}


template <class T>
T* Handle<T>::operator * () const {
  ASSERT(!isEmpty());
  return reinterpret_cast<T*>(data_->block);
}


template <class T>
T* Handle<T>::operator -> () const {
  ASSERT(!isEmpty());
  return reinterpret_cast<T*>(data_->block);
}


template <class T>
Handle<T>::operator bool () const {
  return !isEmpty();
}


template <class T>
bool Handle<T>::operator ! () const {
  return isEmpty();
}


template <class T>
bool Handle<T>::isEmpty() const {
  return data_ == nullptr;
}


template <class T>
void Handle<T>::retain() {
  if (!isEmpty())
    data_->refCount++;
}


template <class T>
void Handle<T>::release() {
  if (isEmpty())
    return;
  if (data_->refCount == 1) {
    HandleDataBlock* dataBlock = HandleDataBlock::fromHandle(data_);
    dataBlock->freeHandle(data_);
    data_ = nullptr;
  } else {
    data_->refCount--;
  }
}


HandleData* HandleDataBlock::allocateHandle(Block* block) {
  ASSERT(freeCount() > 0);
  auto handle = at(freeListHead());
  ASSERT(!handle->isValid());
  setFreeCount(freeCount() - 1);
  if (handle->freeNext == kFreeListFreeAfter) {
    if (handle == at(kCount - 1)) {
      ASSERT(freeCount() == 0);
      setFreeListHead(kFreeListEmpty);
    } else {
      setFreeListHead(freeListHead() + 1);
      at(freeListHead())->freeNext = kFreeListFreeAfter;
    }
  } else {
    setFreeListHead(handle->freeNext);
  }

  handle->refCount = 1;
  handle->block = block;
  return handle;
}


void HandleDataBlock::freeHandle(HandleData* handle) {
  setFreeCount(freeCount() + 1);
  handle->invalidate();
  handle->freeNext = freeListHead();
  word_t index = handle - at(0);
  ASSERT(index < kCount);
  setFreeListHead(index);
}


HandleData* HandleDataBlock::at(word_t index) {
  ASSERT(index < kCount);
  auto handles = reinterpret_cast<HandleData*>(
      reinterpret_cast<Address>(this) + kHeaderSize);
  return handles + index;
}


HandleDataBlock* HandleDataBlock::fromHandle(HandleData* handle) {
  const word_t mask = ~(kSize - 1);
  auto block = reinterpret_cast<HandleDataBlock*>(
      reinterpret_cast<Address>(handle) & mask);
  return block;
}


HandleData* HandleStorage::allocateHandle(Block* block) {
  HandleDataBlock& allocationBlock = *blocks_[allocationIndex_];
  if (allocationBlock.freeCount() > 0) {
    return allocationBlock.allocateHandle(block);
  } else {
    return allocateHandleSlow(block);
  }
}


HandleStorage::iterator& HandleStorage::iterator::operator ++ () {
  dataIndex_++;
  if (dataIndex_ == HandleDataBlock::kCount) {
    blockIt_++;
    dataIndex_ = 0;
  }
  return *this;
}


HandleData& HandleStorage::iterator::operator * () {
  return *((*blockIt_)->at(dataIndex_));
}


bool HandleStorage::iterator::operator == (const HandleStorage::iterator& other) const {
  return blockIt_ == other.blockIt_ && dataIndex_ == other.dataIndex_;
}


HandleStorage::iterator HandleStorage::begin() {
  return iterator(blocks_.begin(), 0);
}


HandleStorage::iterator HandleStorage::end() {
  return iterator(blocks_.end(), 0);
}

}
}

#endif

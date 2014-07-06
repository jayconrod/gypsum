// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef heap_inl_h
#define heap_inl_h

#include "heap.h"

#include "block-inl.h"
#include "handle-inl.h"
#include "remembered-set-inl.h"

namespace codeswitch {
namespace internal {

class Block;


Address Heap::allocateRaw(word_t size) {
  size = align(size, kBlockAlignment);
  Address addr = allocateRawFast(size);
  if (addr != 0) {
    return addr;
  } else {
    return allocateRawSlow(size);
  }
}


template <class T>
void Heap::recordWrite(T** from, T* to) {
  recordWrite(reinterpret_cast<Address>(from), reinterpret_cast<Address>(to));
}


void Heap::recordWrite(Address from, Address to) {
  if (to == 0)
    return;
  auto fromPage = Page::fromAddress(from), toPage = Page::fromAddress(to);
  if (fromPage->identity() == NEW_SPACE || toPage->identity() != NEW_SPACE)
    return;
  toPage->rememberedSet()->add(reinterpret_cast<Block**>(from));
}


Heap::iterator::iterator(Heap* heap, SpaceId id)
    : heap_(heap), id_(id) { }


Space* Heap::iterator::operator * () {
  return heap_->getSpaceById(id_);
}


bool Heap::iterator::operator == (const iterator& other) const {
  return id_ == other.id_;
}


Heap::iterator& Heap::iterator::operator ++ () {
  id_ = static_cast<SpaceId>(static_cast<SpaceId>(id_) + 1);
  return *this;
}


Heap::iterator Heap::begin() {
  return iterator(this, NEW_SPACE);
}


Heap::iterator Heap::end() {
  return iterator(this, SPACE_COUNT);
}


Address Heap::allocateRawFast(word_t size) {
  if (top_ + size > limit_)
    return 0;

  Address addr = top_;
  top_ += size;
  return addr;
}


template <class Callback>
void Heap::visitRoots(Callback callback) {
  // Visit actual heap roots.
  vm()->roots()->visit(callback);

  // Visit handles.
  for (HandleData& handle : vm()->handleStorage()) {
    if (!handle.isValid())
      continue;
    callback(&handle.block);
  }
}

}
}

#endif

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef memory_inl_h
#define memory_inl_h

#include <algorithm>
#include "memory.h"
#include "block.h"

using namespace std;

namespace codeswitch {
namespace internal {

Page* Page::fromAddress(void* addr) {
  return fromAddress(reinterpret_cast<Address>(addr));
}


Page* Page::fromAddress(Address addr) {
  Address base = addr & ~(kSize - 1);
  return reinterpret_cast<Page*>(base);
}


bool Page::contains(Block* block) {
  auto addr = reinterpret_cast<Address>(block);
  return allocationBase() <= addr && addr < allocationPtr();
}


Page::iterator::iterator(Address pos)
    : pos_(pos) { }


Block* Page::iterator::operator * () {
  return reinterpret_cast<Block*>(pos_);
}


bool Page::iterator::operator == (const iterator& other) const {
  return pos_ == other.pos_;
}


Page::iterator& Page::iterator::operator ++ () {
  word_t size = reinterpret_cast<Block*>(pos_)->sizeOfBlock();
  pos_ += align(size, Heap::kBlockAlignment);
  return *this;
}


Page::iterator Page::begin() {
  return iterator(allocationBase());
}


Page::iterator Page::end() {
  return iterator(allocationPtr());
}


bool Space::contains(Block* block) {
  return any_of(begin(), end(), [block](Page* page) { return page->contains(block); });
}


Space::iterator::iterator(vector<unique_ptr<Page>>::iterator it)
    : it_(it) { }


Page* Space::iterator::operator * () {
  return it_->get();
}


bool Space::iterator::operator == (const iterator& other) const {
  return it_ == other.it_;
}


Space::iterator& Space::iterator::operator ++ () {
  ++it_;
  return *this;
}


Space::iterator Space::begin() {
  return iterator(pages_.begin());
}


Space::iterator Space::end() {
  return iterator(pages_.end());
}


bool NewSpace::contains(Block* block) {
  return toSpace_->contains(block);
}

}
}

#endif

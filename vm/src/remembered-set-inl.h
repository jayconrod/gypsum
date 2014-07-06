// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef remembered_set_inl_h
#define remembered_set_inl_h

#include "remembered-set.h"

#include "utils.h"

namespace codeswitch {
namespace internal {

void RememberedSet::add(Slot slot) {
  if (dirty_.get() != nullptr && dirtyCount_ < setSize_)
    addFast(slot);
  else
    addSlow(slot);
}


RememberedSet::iterator::iterator(RememberedSet* set, word_t index)
    : set_(set), index_(index) { }


RememberedSet::Slot RememberedSet::iterator::operator * () {
  ASSERT(index_ < set_->cleanCount_);
  return set_->clean_[index_];
}


bool RememberedSet::iterator::operator == (const iterator& other) const {
  return index_ == other.index_;
}


RememberedSet::iterator& RememberedSet::iterator::operator ++ () {
  ++index_;
  return *this;
}


RememberedSet::iterator RememberedSet::begin() {
  sortAndMerge();
  return iterator(this, 0);
}


RememberedSet::iterator RememberedSet::end() {
  return iterator(this, cleanCount_);
}


void RememberedSet::addFast(Slot slot) {
  dirty_[dirtyCount_++] = slot;
}

}
}

#endif

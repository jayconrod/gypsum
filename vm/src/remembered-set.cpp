// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "remembered-set.h"

#include <algorithm>

using namespace std;

namespace codeswitch {
namespace internal {

RememberedSet::RememberedSet()
    : cleanCount_(0),
      dirtyCount_(0),
      setSize_(kMinimumSetSize) { }


void RememberedSet::add(Slot slot) {
  if (dirty_.get() != nullptr && dirtyCount_ < setSize_)
    addFast(slot);
  else
    addSlow(slot);
}


word_t RememberedSet::length() {
  sortAndMerge();
  return cleanCount_;
}


void RememberedSet::clear() {
  clean_.reset();
  dirty_.reset();
  cleanCount_ = 0;
  dirtyCount_ = 0;
  setSize_ = kMinimumSetSize;
}


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


void RememberedSet::addSlow(Slot slot) {
  sortAndMerge();
  ASSERT(dirty_.get() == nullptr && dirtyCount_ == 0);
  dirty_.reset(new Slot[setSize_]);
  addFast(slot);
}


void RememberedSet::sortAndMerge() {
  if (cleanCount_ == 0 && dirtyCount_ == 0)
    return;

  sort(dirty_.get(), dirty_.get() + dirtyCount_);
  setSize_ = max(cleanCount_ + dirtyCount_, kMinimumSetSize);
  unique_ptr<Slot[]> merged(new Slot[setSize_]);
  word_t mergeIdx = 0;
  for (word_t cleanIdx = 0, dirtyIdx = 0;
       cleanIdx < cleanCount_ || dirtyIdx < dirtyCount_;) {
    Slot slot;
    if (cleanIdx == cleanCount_) {
      slot = dirty_[dirtyIdx++];
    } else if (dirtyIdx == dirtyCount_) {
      slot = clean_[cleanIdx++];
    } else if (clean_[cleanIdx] <= dirty_[dirtyIdx]) {
      slot = clean_[cleanIdx++];
    } else {
      slot = dirty_[dirtyIdx++];
    }
    if (mergeIdx == 0 || slot != merged[mergeIdx - 1]) {
      merged[mergeIdx++] = slot;
    }
  }

  clean_ = move(merged);
  cleanCount_ = mergeIdx;
  dirty_ = nullptr;
  dirtyCount_ = 0;
}

}
}

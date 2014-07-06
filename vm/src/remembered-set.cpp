// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "remembered-set-inl.h"

#include <algorithm>

using namespace std;

namespace codeswitch {
namespace internal {

RememberedSet::RememberedSet()
    : cleanCount_(0),
      dirtyCount_(0),
      setSize_(kMinimumSetSize) { }


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

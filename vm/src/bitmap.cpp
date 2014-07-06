// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "bitmap-inl.h"

namespace codeswitch {
namespace internal {

void Bitmap::clear() {
  for (word_t i = 0, n = wordCount(); i < n; i++)
    base_[i] = 0;
}


void Bitmap::copyFrom(Bitmap bitmap) {
  ASSERT(bitCount_ == bitmap.bitCount_);
  for (word_t i = 0, n = wordCount(); i < n; i++)
    base_[i] = bitmap.base_[i];
}


void BitSet::expand(word_t bitCount) {
  if (bitCount <= bitCount_)
    return;
  bitCount_ = bitCount;
  auto wordCount = align(bitCount, kBitsInWord) / kBitsInWord;
  if (wordCount > words_.size()) {
    words_.resize(wordCount, 0);
  }
  bitmap_ = Bitmap(words_.data(), bitCount_);
}

}
}

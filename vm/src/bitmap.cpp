// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "bitmap.h"

namespace codeswitch {
namespace internal {

word_t Bitmap::sizeFor(word_t bitCount) {
  word_t bits = align(bitCount, kBitsInWord);
  word_t words = bits / kBitsInWord;
  word_t size = words * kWordSize;
  return size;
}


word_t Bitmap::wordCount() const {
  return align(bitCount_, kBitsInWord) / kBitsInWord;
}


bool Bitmap::at(word_t index) const {
  word_t wordIndex = wordIndexForBit(index);
  word_t bitIndex = bitIndexForBit(index);
  bool value = (base_[wordIndex] & (1ULL << bitIndex)) != 0;
  return value;
}


word_t Bitmap::wordAt(word_t wordIndex) const {
  ASSERT(wordIndex * kBitsInWord < bitCount_);
  return base_[wordIndex];
}


void Bitmap::set(word_t index, bool value) {
  word_t wordIndex = wordIndexForBit(index);
  word_t bitIndex = bitIndexForBit(index);
  word_t* wp = base_ + wordIndex;
  *wp = bitInsert(*wp, static_cast<word_t>(value), 1, bitIndex);
}


void Bitmap::setWord(word_t wordIndex, word_t value) {
  ASSERT(wordIndex * kBitsInWord < bitCount_);
  base_[wordIndex] = value;
}


void Bitmap::clear() {
  for (word_t i = 0, n = wordCount(); i < n; i++)
    base_[i] = 0;
}


void Bitmap::copyFrom(Bitmap bitmap) {
  ASSERT(bitCount_ == bitmap.bitCount_);
  for (word_t i = 0, n = wordCount(); i < n; i++)
    base_[i] = bitmap.base_[i];
}


word_t Bitmap::wordIndexForBit(word_t index) const {
  ASSERT(index < bitCount_);
  return index / kBitsInWord;
}


word_t Bitmap::bitIndexForBit(word_t index) const {
  ASSERT(index < bitCount_);
  return index % kBitsInWord;
}


BitSet::BitSet(word_t bitCount)
    : bitCount_(bitCount),
      words_(align(bitCount_, kBitsInWord) / kBitsInWord, 0),
      bitmap_(words_.data(), bitCount_) { }


bool BitSet::contains(word_t key) const {
  if (key >= bitCount_)
    return false;
  else
    return bitmap_.at(key);
}


void BitSet::add(word_t key) {
  ensureCapacity(key);
  bitmap_.set(key, true);
}


void BitSet::remove(word_t key) {
  if (key >= bitCount_)
    return;
  bitmap_.set(key, false);
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


void BitSet::ensureCapacity(word_t key) {
  if (key < bitCount_)
    return;
  expand(key + 1);
}

}
}

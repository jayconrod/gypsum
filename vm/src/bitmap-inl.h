// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef bitmap_inl_h
#define bitmap_inl_h

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


void BitSet::ensureCapacity(word_t key) {
  if (key < bitCount_)
    return;
  expand(key + 1);
}

}
}

#endif

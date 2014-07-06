// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef bitmap_h
#define bitmap_h

#include <vector>
#include "utils.h"

namespace codeswitch {
namespace internal {

class Bitmap {
 public:
  Bitmap(word_t* base, word_t bitCount)
      : base_(base), bitCount_(bitCount) { }

  static inline word_t sizeFor(word_t bitCount);

  word_t* base() { return base_; }
  word_t bitCount() const { return bitCount_; }
  inline word_t wordCount() const;
  inline bool at(word_t index) const;
  inline bool operator [] (word_t index) const { return at(index); }
  inline void set(word_t index, bool value);
  inline void setWord(word_t wordIndex, word_t value);
  void clear();

  void copyFrom(Bitmap bitmap);

 private:
  inline word_t wordIndexForBit(word_t index) const;
  inline word_t bitIndexForBit(word_t index) const;

  word_t* base_;
  word_t bitCount_;
};


class BitSet {
 public:
  explicit inline BitSet(word_t bitCount = 0);

  inline bool contains(word_t key) const;
  inline void add(word_t key);
  inline void remove(word_t key);
  void expand(word_t bitCount);

  Bitmap& bitmap() { return bitmap_; }

 private:
  inline void ensureCapacity(word_t key);

  word_t bitCount_;
  std::vector<word_t> words_;
  Bitmap bitmap_;
};

}
}

#endif

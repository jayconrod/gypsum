// Copyright Jay Conrod. All rights reserved.

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

  static word_t sizeFor(word_t bitCount);

  word_t* base() { return base_; }
  word_t bitCount() const { return bitCount_; }
  word_t wordCount() const;
  bool at(word_t index) const;
  bool operator [] (word_t index) const { return at(index); }
  word_t wordAt(word_t wordIndex) const;
  void set(word_t index, bool value);
  void setWord(word_t wordIndex, word_t value);
  void clear();

  void copyFrom(Bitmap bitmap);

 private:
  word_t wordIndexForBit(word_t index) const;
  word_t bitIndexForBit(word_t index) const;

  word_t* base_;
  word_t bitCount_;
};


class BitSet {
 public:
  explicit BitSet(word_t bitCount = 0);

  bool contains(word_t key) const;
  void add(word_t key);
  void remove(word_t key);
  void expand(word_t bitCount);

  Bitmap& bitmap() { return bitmap_; }

 private:
  void ensureCapacity(word_t key);

  word_t bitCount_;
  std::vector<word_t> words_;
  Bitmap bitmap_;
};

}
}

#endif

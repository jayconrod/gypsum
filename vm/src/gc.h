// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef gc_h
#define gc_h

#include "utils.h"

namespace codeswitch {
namespace internal {

class Block;
class Chunk;
class Free;
class Heap;

class GC {
 public:
  explicit GC(Heap* heap);
  void collectGarbage();

 private:
  static const word_t kMinFreeSize = 64;

  void markLiveObjects();
  static bool isMarked(Block* b);
  static void mark(Block* b);
  static void sweepChunk(Chunk* chunk);
  static Free* maybeFreeRange(Address begin, Address end);

  Heap* heap_;

  friend class PointerMarkingVisitor;

  NON_COPYABLE(GC)
};

}
}

#endif

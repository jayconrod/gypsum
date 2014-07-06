// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef gc_h
#define gc_h

#include "block.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Heap;

class GC {
 public:
  explicit GC(Heap* heap);
  void collectGarbage();

 private:
  static const u8 kIsForwardingAddress = 1;

  void copyAndUpdate(Block** p);

  Heap* heap_;

  friend class PointerUpdatingVisitor;

  NON_COPYABLE(GC)
};

}
}

#endif

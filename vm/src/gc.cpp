// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "gc.h"

#include <cstring>
#include "block-inl.h"
#include "block-visitor.h"
#include "heap.h"
#include "remembered-set.h"

using namespace std;

namespace codeswitch {
namespace internal {

GC::GC(Heap* heap)
    : heap_(heap) { }


class PointerUpdatingVisitor: public BlockVisitorBase<PointerUpdatingVisitor> {
 public:
  explicit PointerUpdatingVisitor(GC& gc)
      : gc_(gc) { }

  void visitPointer(Block** block) {
    // TODO: implement
  }

  void operator () (Block** block) {
    visitPointer(block);
  }

 private:
  GC& gc_;
};


void GC::collectGarbage() {
#ifdef DEBUG
  heap_->verify();
#endif

  // TODO: re-implement simple garbage collector.

#ifdef DEBUG
  heap_->verify();
#endif
}

}
}

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef gc_h
#define gc_h

#include "utils.h"

namespace codeswitch {
namespace internal {

class Heap;

#define RETRY_WITH_GC(heap, stmt)           \
  do {                                      \
    try {                                   \
      stmt;                                 \
      break;                                \
    } catch (const AllocationError& exn) {  \
      if (!exn.shouldRetryAfterGC())        \
        throw;                              \
      GC gc(heap);                          \
      gc.collectGarbage();                  \
    }                                       \
  } while (true)                            \


class GC {
 public:
  explicit GC(Heap* heap);
  void collectGarbage();

 private:
  Heap* heap_;

  friend class PointerUpdatingVisitor;

  NON_COPYABLE(GC)
};

}
}

#endif

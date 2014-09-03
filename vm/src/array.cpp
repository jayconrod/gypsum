// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "array.h"

#include "block.h"
#include "handle.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

void I8Array::printI8Array(FILE* out) {
  UNIMPLEMENTED();
}


void I32Array::printI32Array(FILE* out) {
  fprintf(out, "I32Array @%p\n", reinterpret_cast<void*>(this));
  fprintf(out, "  length: %d\n", static_cast<int>(length()));
  for (word_t i = 0; i < length(); i++) {
    fprintf(out, "  %3d: %d\n", static_cast<int>(i), static_cast<int>(get(i)));
  }
}


void I64Array::printI64Array(FILE* out) {
  fprintf(out, "I64Array @%p\n", reinterpret_cast<void*>(this));
  fprintf(out, "  length: %d\n", static_cast<int>(length()));
  for (word_t i = 0; i < length(); i++) {
    fprintf(out, "  %3d: %ld\n", static_cast<int>(i), static_cast<long>(get(i)));
  }
}

}
}

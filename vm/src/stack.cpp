// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "stack-inl.h"

#include "block-inl.h"
#include "handle.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

void Stack::printStack(FILE* out) {
  fprintf(out, "Stack @%p\n", reinterpret_cast<void*>(this));
  fprintf(out, "  size: %d\n  fp: %d (%p)\n  sp: %d (%p)\n",
          static_cast<int>(stackSize()),
          static_cast<int>(framePointerOffset()),
          reinterpret_cast<void*>(fp()),
          static_cast<int>(stackPointerOffset()),
          reinterpret_cast<void*>(sp()));
}


Stack* Stack::tryAllocate(Heap* heap, word_t size) {
  Stack* stack = reinterpret_cast<Stack*>(heap->allocate(size));
  if (stack == nullptr)
    return stack;

  stack->setMeta(STACK_BLOCK_TYPE);
  stack->setStackSize(size - kHeaderSize);
  stack->resetPointers();
  return stack;
}


Local<Stack> Stack::allocate(Heap* heap, word_t size) {
  DEFINE_ALLOCATION(heap, Stack, tryAllocate(heap, size))
}


void Stack::relocateStack(word_t delta) {
  for (StackFrame frame : *this) {
    frame.setCallerFp(frame.callerFp() + delta);
    ASSERT(base() <= frame.callerFp() && frame.callerFp() <= limit());
  }
}


void Stack::resetPointers() {
  setFp(limit());
  setSp(limit());
}

}
}

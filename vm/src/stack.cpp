// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "stack.h"

#include "block.h"
#include "gc.h"
#include "handle.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

void* Stack::operator new(size_t, Heap* heap, word_t size) {
  auto stack = reinterpret_cast<Stack*>(heap->allocate(size));
  stack->stackSize_ = size - sizeof(Stack);
  return stack;
}


Stack::Stack()
    : Block(STACK_BLOCK_TYPE) {
  resetPointers();
}


Local<Stack> Stack::create(Heap* heap, word_t size) {
  RETRY_WITH_GC(heap, return Local<Stack>(new(heap, size) Stack));
}


void Stack::printStack(FILE* out) const {
  fprintf(out, "Stack @%p\n", reinterpret_cast<const void*>(this));
  fprintf(out, "  size: %d\n  fp: %d (%p)\n  sp: %d (%p)\n",
          static_cast<int>(stackSize()),
          static_cast<int>(framePointerOffset()),
          reinterpret_cast<void*>(fp()),
          static_cast<int>(stackPointerOffset()),
          reinterpret_cast<void*>(sp()));
}


void Stack::relocateStack(word_t delta) {
  fp_ += delta;
  sp_ += delta;
  for (StackFrame frame : *this) {
    frame.setCallerFp(frame.callerFp() + delta);
    ASSERT(base() <= frame.callerFp() && frame.callerFp() <= limit());
  }
}


Address Stack::limit() const {
  return base() + stackSize();
}


Address Stack::base() const {
  return reinterpret_cast<Address>(this) + sizeof(Stack);
}


void Stack::resetPointers() {
  setFp(limit());
  setSp(limit());
}


word_t Stack::framePointerOffset() const {
  return fp() - base();
}


void Stack::setFramePointerOffset(word_t offset) {
  setFp(base() + offset);
}


word_t Stack::stackPointerOffset() const {
  return sp() - base();
}


void Stack::setStackPointerOffset(word_t offset) {
  setSp(base() + offset);
}


bool Stack::isAligned(word_t alignment) const {
  ASSERT(isPowerOf2(alignment));
  return codeswitch::internal::isAligned(sp(), alignment);
}


void Stack::align(word_t alignment) {
  ASSERT(isPowerOf2(alignment));
  setSp(codeswitch::internal::alignDown(sp(), alignment));
}


Stack::iterator Stack::begin() {
  return iterator(this, fp(), *reinterpret_cast<word_t*>(sp()));
}


Stack::iterator Stack::end() {
  return iterator(this, limit(), kNotSet);
}


StackFrame Stack::top(word_t pcOffset) {
  return StackFrame(fp(), pcOffset);
}


Stack::iterator::iterator(Stack* stack, Address fp, word_t pcOffset)
    : stack_(stack),
      frame_(fp, pcOffset) { }


StackFrame Stack::iterator::operator * () {
  return frame_;
}


Stack::iterator& Stack::iterator::operator ++ () {
  frame_ = StackFrame(frame_.callerFp(), frame_.callerPcOffset());
  return *this;
}


bool Stack::iterator::operator != (const Stack::iterator& other) const {
  return frame_.fp() != other.frame_.fp();
}

}
}

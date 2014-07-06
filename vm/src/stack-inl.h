// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef stack_inl_h
#define stack_inl_h

#include "stack.h"

namespace codeswitch {
namespace internal {

Address Stack::limit() {
  return base() + stackSize();
}


Address Stack::base() {
  return reinterpret_cast<Address>(this) + kHeaderSize;
}


word_t Stack::stackPointerOffset() {
  return mem<word_t>(this, kStackPointerOffset);
}


void Stack::setStackPointerOffset(word_t offset) {
  ASSERT(0 <= offset && offset <= stackSize());
  mem<word_t>(this, kStackPointerOffset) = offset;
}


word_t Stack::framePointerOffset() {
  return mem<word_t>(this, kFramePointerOffset);
}


void Stack::setFramePointerOffset(word_t offset) {
  ASSERT(0 <= offset && offset <= stackSize());
  mem<word_t>(this, kFramePointerOffset) = offset;
}


Address Stack::sp() {
  return base() + stackPointerOffset();
}


void Stack::setSp(Address addr) {
  setStackPointerOffset(addr - base());
}


Address Stack::fp() {
  return base() + framePointerOffset();
}


void Stack::setFp(Address addr) {
  setFramePointerOffset(addr - base());
}


template <typename T>
void Stack::push(T x) {
  ASSERT(isAligned(sizeof(x)));
  Address slot = sp() - sizeof(x);
  setSp(slot);
  mem<T>(slot) = x;
}


template <typename T>
T Stack::pop() {
  ASSERT(isAligned(sizeof(T)));
  Address slot = sp();
  setSp(slot + sizeof(T));
  return mem<T>(slot);
}


bool Stack::isAligned(int alignment) {
  return (stackPointerOffset() & (static_cast<word_t>(alignment) - 1)) == 0;
}


void Stack::align(int alignment) {
  word_t offset = stackPointerOffset();
  offset &= ~(static_cast<word_t>(alignment) - 1);
  setStackPointerOffset(offset);
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

#endif

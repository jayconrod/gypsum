// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef stack_h
#define stack_h

#include "block.h"
#include "handle.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class StackFrame {
 public:
  StackFrame(Address fp, length_t pcOffset)
      : fp_(fp), pcOffset_(pcOffset) { }

  Address fp() const { return fp_; }

  Address callerFp() const { return mem<Address>(fp_, kCallerFpOffset); }
  void setCallerFp(Address newFp) { mem<Address>(fp_, kCallerFpOffset) = newFp; }
  Function* function() const { return mem<Function*>(fp_, kFunctionOffset); }
  void setFunction(Function* newFunction) {
    mem<Function*>(fp_, kFunctionOffset) = newFunction;
  }
  length_t pcOffset() const { return pcOffset_; }
  length_t callerPcOffset() const { return toLength(mem<word_t>(fp_, kCallerPcOffsetOffset)); }

  bool isLast() const { return callerPcOffset() == kPcNotSet; }

  Address parameters() const { return fp_ + kParametersOffset; }
  Address locals() const { return fp_ + kLocalsOffset; }

  void relocate(word_t delta);

  static const int kCallerFpOffset = 0;
  static const int kFunctionOffset = kWordSize;
  static const int kCallerPcOffsetOffset = 2 * kWordSize;
  static const int kParametersOffset = 3 * kWordSize;
  static const int kLocalsOffset = 0;

 private:
  Address fp_;
  length_t pcOffset_;
};


class Stack: public Block {
 public:
  void* operator new(size_t, Heap* heap, word_t size);
  Stack();
  static Local<Stack> create(Heap* heap, word_t size);

  DEFINE_CAST(Stack)
  void printStack(FILE* out) const;
  void relocateStack(word_t delta);

  // [stackSize]: size in bytes of the stack itself, not including the header.
  word_t stackSize() const { return stackSize_; }

  // The high address of the stack; sp and fp start from here and grow down.
  Address limit() const;

  // The low address of the stack; sp and fp cannot go past here.
  Address base() const;

  // sp and fp are the stack pointer and frame pointer, respectively. These addresses are
  // stored directly in the stack header. If the GC moves the stack, they are updated by
  // relocateStack.
  Address sp() const { return sp_; }
  void setSp(Address newSp) {
    ASSERT(base() <= newSp && newSp <= limit());
    sp_ = newSp;
  }
  Address fp() const { return fp_; }
  void setFp(Address newFp) {
    ASSERT(base() <= newFp && newFp <= limit());
    fp_ = newFp;
  }
  void resetPointers();

  // [framePointerOffset]: distance from base to fp
  word_t framePointerOffset() const;
  void setFramePointerOffset(word_t offset);

  // [stackPointerOffset]: distance from base to sp
  word_t stackPointerOffset() const;
  void setStackPointerOffset(word_t offset);

  template <typename T>
  void push(T x);
  template <typename T>
  T pop();

  bool isAligned(word_t alignment) const;
  void align(word_t alignment);

  // Stack frame iteration requires the pc offset of the active (top) frame to be pushed on
  // top of the stack. This is normally done when invoking the garbage collector.
  class iterator {
   public:
    iterator(Stack* stack, Address fp, length_t pcOffset);

    StackFrame operator * ();
    iterator& operator ++ ();
    bool operator == (const iterator& other) const {
      return !(*this != other);
    }
    bool operator != (const iterator& other) const;

   private:
    Stack* stack_;
    StackFrame frame_;
  };
  iterator begin();
  iterator end();

  // This method does NOT require the pc offset to be pushed.
  StackFrame top(length_t pcOffset = 0);

  static const int kDefaultSize = 32 * KB;

 private:
  word_t stackSize_;
  Address fp_;
  Address sp_;

  friend class Roots;
};


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


}
}

#endif

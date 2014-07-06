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
  StackFrame(Address fp, word_t pcOffset)
      : fp_(fp), pcOffset_(pcOffset) { }

  Address fp() const { return fp_; }

  Address callerFp() const { return mem<Address>(fp_, kCallerFpOffset); }
  void setCallerFp(Address newFp) { mem<Address>(fp_, kCallerFpOffset) = newFp; }
  Function* function() const { return mem<Function*>(fp_, kFunctionOffset); }
  void setFunction(Function* newFunction) {
    mem<Function*>(fp_, kFunctionOffset) = newFunction;
  }
  word_t pcOffset() const { return pcOffset_; }
  word_t callerPcOffset() const { return mem<word_t>(fp_, kCallerPcOffsetOffset); }

  bool isLast() const { return callerPcOffset() == kNotSet; }

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
  word_t pcOffset_;
};


class Stack: public Block {
 public:
  static Stack* tryAllocate(Heap* heap, word_t size);
  static Handle<Stack> allocate(Heap* heap, word_t size);
  DEFINE_CAST(Stack)
  inline word_t sizeOfStack() { return mem<word_t>(this, kStackSizeOffset); }
  void printStack(FILE* out);
  void relocateStack(word_t delta);

  // [stackSize]: size in bytes of the stack itself, not including the header.
  DEFINE_INL_ACCESSORS(word_t, stackSize, setStackSize, kStackSizeOffset)

  // The high address of the stack; sp and fp start from here and grow down.
  inline Address limit();

  // The low address of the stack; sp and fp cannot go past here.
  inline Address base();

  // [framePointerOffset]: distance from base to fp
  DECLARE_ACCESSORS(word_t, framePointerOffset, setFramePointerOffset)

  // [stackPointerOffset]: distance from base to sp
  DECLARE_ACCESSORS(word_t, stackPointerOffset, setStackPointerOffset)

  // sp and fp are alternative accessors for the corresponding offsets. The actual
  // addresses are not stored anywhere, since the stack object could be moved by the GC.
  DECLARE_ACCESSORS(Address, sp, setSp)
  DECLARE_ACCESSORS(Address, fp, setFp)
  void resetPointers();

  template <typename T>
  inline void push(T x);
  template <typename T>
  inline T pop();

  inline bool isAligned(int alignment);
  inline void align(int alignment);

  // Stack frame iteration requires the pc offset of the active (top) frame to be pushed on
  // top of the stack. This is normally done when invoking the garbage collector.
  class iterator {
   public:
    inline iterator(Stack* stack, Address fp, word_t pcOffset);

    inline StackFrame operator * ();
    inline iterator& operator ++ ();
    inline bool operator == (const iterator& other) const {
      return !(*this != other);
    }
    inline bool operator != (const iterator& other) const;

   private:
    Stack* stack_;
    StackFrame frame_;
  };
  inline iterator begin();
  inline iterator end();

  // This method does NOT require the pc offset to be pushed.
  inline StackFrame top(word_t pcOffset = 0);

  static const int kDefaultSize = 32 * KB;

  static const int kStackSizeOffset = kBlockHeaderSize;
  static const int kFramePointerOffset = kStackSizeOffset + kWordSize;
  static const int kStackPointerOffset = kFramePointerOffset + kWordSize;
  static const int kHeaderSize = kStackPointerOffset;
};

}
}

#endif

// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef interpreter_h
#define interpreter_h

#include <vector>
#include "builtins.h"
#include "handle.h"
#include "utils.h"
#include "vm.h"

namespace codeswitch {
namespace internal {

class Function;
class Meta;
class Object;
class Stack;
class ThreadBindle;
class Type;

class Interpreter {
 public:
  Interpreter(VM* vm, const Handle<Stack>& stack, const Handle<ThreadBindle>& threadBindle);

  i64 call(const Handle<Function>& callee);

  static const int kFpOffset = 0;
  static const int kFunctionOffset = kFpOffset + kWordSize;
  static const int kCallerPcOffsetOffset = kFunctionOffset + kWordSize;
  static const int kFrameControlSize = kCallerPcOffsetOffset + kWordSize;

  static const size_t kSlotSize = 8;

 private:
  static const length_t kDonePcOffset = ~0;

  void ensurePointerMap(const Handle<Function>& function);
  void handleBuiltin(BuiltinId id);
  void handleNative(const Handle<Function>& callee);
  Local<Meta> getMetaForClassId(i64 classId);

  void enter(const Handle<Function>& callee);
  void leave();
  void doThrow(Block* exception);
  void reset();

  template <typename T> void push(T value);
  template <typename T> T pop();
  static const size_t kPrepareForGCSize = kSlotSize;

  ptrdiff_t localOffsetFromIndex(i64 index);
  Address localAddressFromOffset(ptrdiff_t offset);
  Address localAddressFromIndex(i64 index);
  i64 readVbn();

  void prepareForGC();
  void unprepareForGC();
  bool isPreparedForGC() const { return isPreparedForGC_; }
  class GCSafeScope;
  friend GCSafeScope;

  void load(Block* block, word_t offset, Type* type);
  void store(Block* block, word_t offset, Type* type);

  template <typename T> void add();
  template <typename T> void sub();
  template <typename T> void mul();
  template <typename T> void div();
  template <typename T> void mod();
  template <typename T> void lsl();
  template <typename T> void lsr();
  template <typename T> void asr();
  template <typename T> void and_();
  template <typename T> void or_();
  template <typename T> void xor_();
  template <typename T> void eq();
  template <typename T> void ne();
  template <typename T> void lt();
  template <typename T> void le();
  template <typename T> void gt();
  template <typename T> void ge();
  template <typename T> void inv();
  template <typename T> void neg();
  template <typename T> void trunc();
  template <typename W, typename N> void sext();
  template <typename From, typename To> void convert();

  int strcmp();

  template <typename T> void intToString();
  template <typename T> void floatToString();

  struct Handler {
    word_t fpOffset;
    word_t spOffset;
    length_t pcOffset;
  };

  VM* vm_;
  Persistent<Stack> stack_;
  Persistent<ThreadBindle> threadBindle_;

  Persistent<Function> function_;
  length_t pcOffset_;

  std::vector<Handler> handlers_;

  bool isPreparedForGC_;

  NON_COPYABLE(Interpreter)
};


class Exception {
 public:
  explicit Exception(Object* rawException)
      : exception_(rawException) { }
  explicit Exception(const Handle<Object>& exception)
      : exception_(exception) { }

  const Handle<Object>& get() const { return exception_; }

 private:
  Persistent<Object> exception_;
};

}
}

#endif

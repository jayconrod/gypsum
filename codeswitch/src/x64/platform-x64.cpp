// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "platform.h"

#include "api.h"
#include "block.h"
#include "handle.h"
#include "vm.h"

// These functions are implemented in assembly (see glue-x64.s). They actually move arguments
// into appropriate registers and tail-call the native function. Because they don't touch
// the return value, they share the same implementation.
extern "C" uint64_t codeswitch_glue_callNativeFunctionRawForInt(
    codeswitch::internal::NativeFunction function,
    uint64_t intArgCount,
    uint64_t* intArgs,
    uint64_t floatArgCount,
    uint64_t* floatArgs,
    uint64_t stackArgCount,
    uint64_t* stackArgs);

extern "C" double codeswitch_glue_callNativeFunctionRawForFloat(
    codeswitch::internal::NativeFunction function,
    uint64_t intArgCount,
    uint64_t* intArgs,
    uint64_t floatArgCount,
    uint64_t* floatArgs,
    uint64_t stackArgCount,
    uint64_t* stackArgs);

namespace codeswitch {
namespace internal {

// This function could be considered a helper function for the assembly functions above. It
// loads arguments from the VM stack and segregates them into integer, pointer, and stack
// arrays, depending on how they should be passed to the native function.
int64_t callNativeFunctionRaw(
    codeswitch::VM* vm,
    NativeFunction function,
    word_t argCount,
    uint64_t* rawArgs,
    bool* argsAreInt,
    NativeResultType nativeResultType) {
  const int kMaxIntArgs = 6;
  const int kMaxFloatArgs = 8;
  uint64_t intArgs[kMaxIntArgs];
  uint64_t floatArgs[kMaxFloatArgs];
  uint64_t stackArgs[argCount];
  uint64_t resultPtr = kNotSet;
  uint64_t intCount = 0, floatCount = 0, stackCount = 0;

  if (nativeResultType == NATIVE_PTR) {
    intArgs[intCount++] = reinterpret_cast<uint64_t>(&resultPtr);
  }
  intArgs[intCount++] = reinterpret_cast<uint64_t>(vm);

  for (uint64_t i = 0; i < argCount; i++) {
    if (argsAreInt[i]) {
      if (intCount < kMaxIntArgs) {
        intArgs[intCount++] = rawArgs[i];
      } else {
        stackArgs[stackCount++] = rawArgs[i];
      }
    } else {
      if (floatCount < kMaxFloatArgs) {
        floatArgs[floatCount++] = rawArgs[i];
      } else {
        stackArgs[stackCount++] = rawArgs[i];
      }
    }
  }

  if (nativeResultType == NATIVE_INT) {
    auto result = codeswitch_glue_callNativeFunctionRawForInt(
        function, intCount, intArgs, floatCount, floatArgs, stackCount, stackArgs);
    return result;
  } else if (nativeResultType == NATIVE_FLOAT) {
    auto fresult = codeswitch_glue_callNativeFunctionRawForFloat(
        function, intCount, intArgs, floatCount, floatArgs, stackCount, stackArgs);
    auto iresult = f64ToBits(fresult);
    return iresult;
  } else {
    // NATIVE_PTR
    codeswitch_glue_callNativeFunctionRawForInt(
        function, intCount, intArgs, floatCount, floatArgs, stackCount, stackArgs);
    int64_t derefPtr = 0;
    if (resultPtr) {
      derefPtr = *reinterpret_cast<int64_t*>(resultPtr);
      codeswitch::VM::Impl::unwrap(vm)->handleStorage().destroyPersistent(
          reinterpret_cast<Block**>(resultPtr));
    }
    return derefPtr;
  }
}

}
}

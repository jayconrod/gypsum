// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


/**
 * @file
 * Definitions related to the public API. Mostly these are internal implementation classes
 * (following the PIMPL pattern). There are mostly just used in api.cpp, but other modules
 * may need to create these objects and pass them to API clients or native code.
 */

#ifndef api_h
#define api_h

#include "codeswitch.h"

#include <string>
#include "handle.h"
#include "platform.h"
#include "utils.h"
#include "vm.h"

namespace codeswitch {

namespace internal {
  class Function;
  class Name;
  class Object;
  class Package;
  class String;
}


#define API_CHECK(expr, message) \
  do { \
    if (!(expr)) { \
      throw Error(new Error::Impl(message)); \
    } \
  } while (false)


#define API_CHECK_ARG(ref) API_CHECK(ref.impl_, #ref ": not a valid reference")


#define API_CHECK_SELF(type) API_CHECK(impl_, #type ": this is not a valid reference")


class Error::Impl final {
 public:
  explicit Impl(const std::string& message)
      : message(message) { }
  explicit Impl(std::string&& message)
      : message(message) { }
  std::string message;
};


class VM::Impl final {
 public:
  explicit Impl(const VMOptions& vmOptions)
      : vm(vmOptions) { }
  static internal::VM* unwrap(VM* vm) { return &vm->impl_->vm; }
  internal::VM vm;
};


/**
 * Calls a native function using arguments on the VM stack. If the native function pointer
 * has not been loaded already, it will be loaded at this call. Arguments will be moved into
 * registers appropriate for the platform's calling convention, so the call can be made
 * normally. Pointer arguments are wrapped in {@link Object}s and passed by value (we assume
 * an {@link Object} is represented by a single pointer to its impl.
 *
 * @param callee the function to call. It must be native.
 * @param vm a pointer to the internal VM. A pointer to the external VM will be passed to the
 *     native function as the first argument.
 * @param sp a pointer to the last argument on the stack. The calling function and its
 *     pc offset must be pushed below this.
 * @return the result from the native function. The VM stack is not modified.
 */
int64_t callNativeFunction(
    internal::Function* callee,
    internal::VM* vm,
    internal::Address sp);

}

#endif

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.

#ifndef option_h
#define option_h

/** @file
 *  This file contains wrapper classes for "optional" values. These force the programmer to
 *  check whether a value is valid or not before using it. For instance, this can be used to
 *  prevent null pointer dereferences. None of these classes have any data members besides the
 *  value being wrapped, and all their methods are small and inlineable, so they should be
 *  very inexpensive.
 */

#include <functional>
#include <memory>
#include "utils.h"

namespace codeswitch {
namespace internal {

/** GeneralOption is a base class for wrappers around objects which can be in a valid or
 *  invalid state. In the template arguments, `GetNone` returns a canonical invalid value.
 *  `IsNone` is a predicate which checks whether the wrapped value is valid. This class is
 *  cumbersome, so it should probably not be used directly. Use `Option` instead, or define
 *  a similar alias.
 */
template <typename T,
          T (*GetNone)(),
          bool (*IsNone)(const T&)>
class GeneralOption {
 public:
  GeneralOption()
      : value_(GetNone()) { }
  GeneralOption(const T& value)
      : value_(value) { }
  GeneralOption(const T&& value)
      : value_(value) { }

  bool isDefined() const { return IsNone(value_); }
  operator bool () const { return isDefined(); }
  bool operator ! () const { return !isDefined(); }

  T& get() {
    ASSERT(isDefined());
    return value_;
  }
  const T& get() const {
    ASSERT(isDefined());
    return value_;
  }
  const T& getOrElse(const T& other) const { return isDefined() ? value_ : other; }

 private:
  T value_;
};


/** Option should be used to wrap values from classes which implement the `getValue` and
 *  `isValid` static methods.
 */
template <typename T>
using Option = GeneralOption<T, T::getInvalid, T::isValid>;


template <typename T>
inline std::unique_ptr<T> getUniquePtrNone() { return std::unique_ptr<T>(); }

template <typename T>
inline bool isUniquePtrValid(const std::unique_ptr<T>& ptr) { return static_cast<bool>(ptr); }


/** OptUP is used to wrap std::unique_ptr. */
template <typename T>
using OptUP = GeneralOption<std::unique_ptr<T>,
                            &getUniquePtrNone<std::unique_ptr<T>>,
                            &isUniquePtrValid<std::unique_ptr<T>>>;

/** SentinelOption uses a special sentinel value (specified as a template parameter) instead
 *  of a predicate to determine whether a value is valid or not. Unforunately, this cannot
 *  be used with objects, since objects can't be used as non-type template arguments. Again,
 *  this should not be used directly; use an alias like `OptP` instead.
 */
template <class T, T None>
class SentinelOption {
 public:
  SentinelOption()
      : value_(None) { }
  SentinelOption(const T& value)
      : value_(value) { }
  SentinelOption(const T&& value)
      : value_(value) { }

  bool isDefined() const { return value_ != None; }
  operator bool () const { return isDefined(); }
  bool operator ! () const { return !isDefined(); }

  T& get() {
    ASSERT(isDefined());
    return value_;
  }
  const T& get() const {
    ASSERT(isDefined());
    return value_;
  }
  const T& getOrElse(const T& other) const { return isDefined() ? value_ : other; }

 private:
  T value_;
};


/** OptP wraps pointer and pointer-like values (like Address). */
template <typename T>
using OptP = SentinelOption<T, static_cast<T>(0)>;


inline OptP<Address> Some(Address addr) { return OptP<Address>(addr); }


template <typename T>
OptP<T*> Some(T* addr) { return OptP<T*>(addr); }


/** OptI wraps integer values where 0 is valid. 0xFFFF... is the sentinel value. */
template <typename T>
using OptI = SentinelOption<T, ~static_cast<T>(0)>;

}
}

#endif

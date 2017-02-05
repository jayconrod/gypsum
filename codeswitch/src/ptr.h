// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef ptr_h
#define ptr_h

#include "utils.h"

namespace codeswitch {
namespace internal {

/**
 * Wrapper for pointers to Blocks stored within Blocks. This ensures that pointer fields are
 * always properly initialized and writes are always recorded.
 */
template <class T>
class alignas(word_t) Ptr {
 public:
  Ptr() : p_(nullptr) { }
  Ptr(Block* from, T* q) { set(from, q); }
  Ptr(const Ptr&) = delete;
  Ptr(Ptr&&) = delete;
  Ptr& operator = (const Ptr&) = delete;
  Ptr& operator = (Ptr&&) = delete;

  T* get() const { return p_; }
  void set(Block* from, T* q) {
    p_ = q;
    Heap::recordWrite(&p_, q);
  }

  bool operator == (T* q) const { return p_ == q; }
  template <class S>
  bool operator == (const Ptr<S>& q) const { return p_ == q.p_; }
  bool operator != (T* q) const { return p_ != q; }
  template <class S>
  bool operator != (const Ptr<S>& q) const { return p_ != q.p_; }

  operator bool () const { return static_cast<bool>(p_); }
  bool operator ! () const { return !p_; }

 private:
  T* p_;
};

}
}

#endif

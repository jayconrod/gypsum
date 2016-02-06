// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef thread_bindle_h
#define thread_bindle_h

#include "block.h"
#include "ptr.h"

namespace codeswitch {
namespace internal {

class Heap;
template <class T>
class Local;
class Object;

/**
 * A random assortment of things that threads need quick access to. In the future, when we
 * have JITed code, a register will be dedicated to holding a pointer to this.
 *
 * This object holds several pre-allocated built-in exceptions, which can be taken and thrown
 * without being at a function location that has a pointer map. Since nearly every instruction
 * that deals with pointers may throw built-in exceptions, this allows us to reduce the
 * number of pointer maps dramatically. {@link #restoreExceptions} must be called the next
 * time it is safe to allocate memory (during the throw process, before resuming execution).
 */
class ThreadBindle: public Block {
 public:
  static const BlockType kBlockType = THREAD_BINDLE_BLOCK_TYPE;

  ThreadBindle();

  DEFINE_NEW(ThreadBindle)
  static Local<ThreadBindle> create(Heap* heap);

  Object* takeArrayIndexOutOfBoundsException();
  Object* takeCastException();
  Object* takeNullPointerException();
  Object* takeUninitializedException();

  /**
   * Re-allocates any built-in exceptions that were taken and thrown. This should be called
   * after any `take` method is called the next time it is safe to allocate memory.
   */
  static void restoreExceptions(const Handle<ThreadBindle>& bindle);

 private:
  friend std::ostream& operator << (std::ostream& os, const ThreadBindle* bindle);

  Object* takeException(Ptr<Object>* exn);
  static void restoreException(
      const Handle<ThreadBindle>& bindle,
      Ptr<Object>* exn,
      BuiltinId id);

  DECLARE_POINTER_MAP()
  Ptr<Object> arrayIndexOutOfBoundsException_;
  Ptr<Object> castException_;
  Ptr<Object> nullPointerException_;
  Ptr<Object> uninitializedException_;
  // Update FUNCTION_POINTER_LIST if pointer members change.
};

std::ostream& operator << (std::ostream& os, const ThreadBindle* bindle);

}
}

#endif

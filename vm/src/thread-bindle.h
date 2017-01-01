// Copyright Jay Conrod. All rights reserved.

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

  /**
   * Allocates all built-in exceptions. This should be called right after the `ThreadBindle`
   * is created, before any code is executed with it.
   */
  static void createExceptions(const Handle<ThreadBindle>& bindle);

  /**
   * Returns true if it is safe to execute code with this bindle.
   *
   * This checks that all built-in exceptions have been allocated.
   */
  bool isReady() const;

  Object* takeArrayIndexOutOfBoundsException();
  Object* takeCastException();
  Object* takeNullPointerException();
  Object* takeOutOfMemoryException();
  Object* takeUninitializedException();

  /**
   * Allocates a new exception to replace a built-in exception after a `take` method above.
   *
   * This should be called as part of the exception-throwing process, before executing more
   * code. It is safe to call this if no built-in exception was taken.
   */
  static void restoreTakenException(const Handle<ThreadBindle>& bindle);

 private:
  enum TakenException {
    NONE_TAKEN,
    NOT_ALLOCATED,
    ARRAY_INDEX_OUT_OF_BOUNDS_TAKEN,
    CAST_TAKEN,
    NULL_POINTER_TAKEN,
    OUT_OF_MEMORY_TAKEN,
    UNINITIALIZED_TAKEN
  };

  Object* takeException(Ptr<Object>* exn, TakenException taken);
  static void restoreException(
      const Handle<ThreadBindle>& bindle,
      Ptr<Object>* exn,
      BuiltinId id);

  DECLARE_POINTER_MAP()
  Ptr<Object> arrayIndexOutOfBoundsException_;
  Ptr<Object> castException_;
  Ptr<Object> nullPointerException_;
  Ptr<Object> outOfMemoryException_;
  Ptr<Object> uninitializedException_;
  TakenException taken_;
  // Update FUNCTION_POINTER_LIST if pointer members change.

  friend std::ostream& operator << (std::ostream& os, const ThreadBindle* bindle);
};

std::ostream& operator << (std::ostream& os, const ThreadBindle* bindle);

}
}

#endif

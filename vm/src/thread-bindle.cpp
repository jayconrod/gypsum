// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "thread-bindle.h"

#include "builtins.h"
#include "class.h"
#include "handle.h"
#include "heap.h"
#include "object.h"
#include "roots.h"
#include "utils.h"

using std::ostream;

namespace codeswitch {
namespace internal {

#define THREAD_BINDLE_POINTER_LIST(F) \
  F(ThreadBindle, arrayIndexOutOfBoundsException_) \
  F(ThreadBindle, castException_) \
  F(ThreadBindle, nullPointerException_) \
  F(ThreadBindle, outOfMemoryException_) \
  F(ThreadBindle, uninitializedException_) \


DEFINE_POINTER_MAP(ThreadBindle, THREAD_BINDLE_POINTER_LIST)


ThreadBindle::ThreadBindle()
    : Block(THREAD_BINDLE_BLOCK_TYPE),
      taken_(NOT_ALLOCATED) { }


Local<ThreadBindle> ThreadBindle::create(Heap* heap) {
  RETRY_WITH_GC(heap, return Local<ThreadBindle>(new(heap) ThreadBindle()));
}


void ThreadBindle::createExceptions(const Handle<ThreadBindle>& bindle) {
  ASSERT(bindle->taken_ == NOT_ALLOCATED);
  restoreException(bindle, &bindle->arrayIndexOutOfBoundsException_,
      BUILTIN_ARRAY_INDEX_OUT_OF_BOUNDS_EXCEPTION_CLASS_ID);
  restoreException(bindle, &bindle->castException_, BUILTIN_CAST_EXCEPTION_CLASS_ID);
  restoreException(bindle, &bindle->nullPointerException_,
      BUILTIN_NULL_POINTER_EXCEPTION_CLASS_ID);
  restoreException(bindle, &bindle->outOfMemoryException_,
      BUILTIN_OUT_OF_MEMORY_EXCEPTION_CLASS_ID);
  restoreException(bindle, &bindle->uninitializedException_,
      BUILTIN_UNINITIALIZED_EXCEPTION_CLASS_ID);
  bindle->taken_ = NONE_TAKEN;
}


bool ThreadBindle::isReady() const {
  return taken_ == NONE_TAKEN;
}


Object* ThreadBindle::takeArrayIndexOutOfBoundsException() {
  return takeException(&arrayIndexOutOfBoundsException_, ARRAY_INDEX_OUT_OF_BOUNDS_TAKEN);
}


Object* ThreadBindle::takeCastException() {
  return takeException(&castException_, CAST_TAKEN);
}


Object* ThreadBindle::takeNullPointerException() {
  return takeException(&nullPointerException_, NULL_POINTER_TAKEN);
}


Object* ThreadBindle::takeOutOfMemoryException() {
  return takeException(&outOfMemoryException_, OUT_OF_MEMORY_TAKEN);
}


Object* ThreadBindle::takeUninitializedException() {
  return takeException(&uninitializedException_, UNINITIALIZED_TAKEN);
}


void ThreadBindle::restoreTakenException(const Handle<ThreadBindle>& bindle) {
  switch (bindle->taken_) {
    case NONE_TAKEN:
      return;

    case NOT_ALLOCATED:
      UNREACHABLE();
      return;

    case ARRAY_INDEX_OUT_OF_BOUNDS_TAKEN:
      restoreException(bindle, &bindle->arrayIndexOutOfBoundsException_,
          BUILTIN_ARRAY_INDEX_OUT_OF_BOUNDS_EXCEPTION_CLASS_ID);
      break;

    case CAST_TAKEN:
      restoreException(bindle, &bindle->castException_, BUILTIN_CAST_EXCEPTION_CLASS_ID);
      break;

    case NULL_POINTER_TAKEN:
      restoreException(bindle, &bindle->nullPointerException_,
          BUILTIN_NULL_POINTER_EXCEPTION_CLASS_ID);
      break;

    case OUT_OF_MEMORY_TAKEN:
      restoreException(bindle, &bindle->outOfMemoryException_,
          BUILTIN_OUT_OF_MEMORY_EXCEPTION_CLASS_ID);
      break;

    case UNINITIALIZED_TAKEN:
      restoreException(bindle, &bindle->uninitializedException_,
          BUILTIN_UNINITIALIZED_EXCEPTION_CLASS_ID);
      break;
  }
  bindle->taken_ = NONE_TAKEN;
}


Object* ThreadBindle::takeException(Ptr<Object>* exn, TakenException taken) {
  ASSERT(taken_ == NONE_TAKEN);
  auto e = exn->get();
  ASSERT(e);
  exn->set(this, nullptr);
  taken_ = taken;
  return e;
}


void ThreadBindle::restoreException(
    const Handle<ThreadBindle>& bindle,
    Ptr<Object>* exn,
    BuiltinId id) {
  ASSERT(!*exn);
  auto roots = bindle->getVM()->roots();
  auto heap = bindle->getHeap();
  auto clas = handle(roots->getBuiltinClass(id));
  auto meta = Class::ensureInstanceMeta(clas);
  auto e = Object::create(heap, meta);
  exn->set(*bindle, *e);
}


ostream& operator << (ostream& os, const ThreadBindle* bindle) {
  os << brief(bindle)
     << "\n  array index out of bounds exception: "
         << brief(bindle->arrayIndexOutOfBoundsException_.get())
     << "\n  cast exception: " << brief(bindle->castException_.get())
     << "\n  null pointer exception: " << brief(bindle->nullPointerException_.get())
     << "\n  out of memory exception: " << brief(bindle->outOfMemoryException_.get())
     << "\n  uninitialized exception: " << brief(bindle->uninitializedException_.get())
     << "\n  taken: " << bindle->taken_;
  return os;
}

}
}

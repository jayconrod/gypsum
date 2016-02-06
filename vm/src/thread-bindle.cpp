// Copyright 2016 Jay Conrod. All rights reserved.

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
  F(ThreadBindle, uninitializedException_) \


DEFINE_POINTER_MAP(ThreadBindle, THREAD_BINDLE_POINTER_LIST)


ThreadBindle::ThreadBindle()
    : Block(THREAD_BINDLE_BLOCK_TYPE) { }


Local<ThreadBindle> ThreadBindle::create(Heap* heap) {
  RETRY_WITH_GC(heap, return Local<ThreadBindle>(new(heap) ThreadBindle()));
}


Object* ThreadBindle::takeArrayIndexOutOfBoundsException() {
  return takeException(&arrayIndexOutOfBoundsException_);
}


Object* ThreadBindle::takeCastException() {
  return takeException(&castException_);
}


Object* ThreadBindle::takeNullPointerException() {
  return takeException(&nullPointerException_);
}


Object* ThreadBindle::takeUninitializedException() {
  return takeException(&uninitializedException_);
}


void ThreadBindle::restoreExceptions(const Handle<ThreadBindle>& bindle) {
  if (!bindle->arrayIndexOutOfBoundsException_) {
    restoreException(bindle, &bindle->arrayIndexOutOfBoundsException_,
        BUILTIN_ARRAY_INDEX_OUT_OF_BOUNDS_EXCEPTION_CLASS_ID);
  }
  if (!bindle->castException_) {
    restoreException(bindle, &bindle->castException_, BUILTIN_CAST_EXCEPTION_CLASS_ID);
  }
  if (!bindle->nullPointerException_) {
    restoreException(bindle, &bindle->nullPointerException_,
        BUILTIN_NULL_POINTER_EXCEPTION_CLASS_ID);
  }
  if (!bindle->uninitializedException_) {
    restoreException(bindle, &bindle->uninitializedException_,
        BUILTIN_UNINITIALIZED_EXCEPTION_CLASS_ID);
  }
}


Object* ThreadBindle::takeException(Ptr<Object>* exn) {
  auto e = exn->get();
  ASSERT(e);
  exn->set(this, nullptr);
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
  auto meta = Class::ensureAndGetInstanceMeta(clas);
  auto e = Object::create(heap, meta);
  exn->set(*bindle, *e);
}


ostream& operator << (ostream& os, const ThreadBindle* bindle) {
  os << brief(bindle)
     << "\n  array index out of bounds exception: "
         << bindle->arrayIndexOutOfBoundsException_.get()
     << "\n  cast exception: " << bindle->castException_.get()
     << "\n  null pointer exception: " << bindle->nullPointerException_.get()
     << "\n  uninitialized exception: " << bindle->uninitializedException_.get();
  return os;
}

}
}

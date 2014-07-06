// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef handle_h
#define handle_h

#include "list.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Block;
struct HandleData;
class VM;

/** Handles are used to reference heap blocks in C++ code that might trigger garbage collection.
 *  A Handle doesn't point directly to the block it refers to. Instead, HandleData provides
 *  an extra level of indirection.
 */
template <class T>
class Handle {
 public:
  inline Handle();
  template <class S>
  explicit inline Handle(S* block);
  template <class S>
  inline Handle(VM* vm, S* block);
  inline Handle(const Handle<T>& handle);
  template <class S>
  inline Handle(const Handle<S>& handle);
  inline Handle(Handle<T>&& handle);
  inline Handle& operator = (const Handle<T>& handle);
  template <class S>
  inline Handle& operator = (const Handle<S>& handle);
  inline Handle& operator = (Handle<T>&& handle);
  inline ~Handle();

  inline T* operator * () const;
  inline T* operator -> () const;

  inline operator bool () const;
  inline bool operator ! () const;
  inline bool isEmpty() const;

  HandleData* data() const { return data_; }

 private:
  inline void retain();
  inline void release();
  HandleData* data_;
};


template <class T>
Handle<T> handle(T* value) {
  return Handle<T>(value);
}


}
}

#endif

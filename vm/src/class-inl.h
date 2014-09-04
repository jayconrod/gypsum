// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef class_inl_h
#define class_inl_h

#include "class.h"

#include "array.h"
#include "package.h"
#include "roots.h"
#include "type.h"

namespace codeswitch {
namespace internal {

Function* Class::getConstructor(word_t index) {
  word_t id = constructors()->get(index);
  Function* ctor = package()->getFunction(id);
  return ctor;
}


Function* Class::getMethod(word_t index) {
  intptr_t id = methods()->get(index);
  if (isBuiltinId(id)) {
    return getVM()->roots()->getBuiltinFunction(static_cast<BuiltinId>(id));
  } else {
    return package()->getFunction(id);
  }
}


Meta* Class::getOrTryBuildInstanceMeta(Heap* heap) {
  Meta* m = instanceMeta();
  if (m != nullptr)
    return m;
  else
    return tryBuildInstanceMeta(heap);
}


Local<Meta> Class::getOrBuildInstanceMeta(Heap* heap) {
  Local<Meta> meta(instanceMeta());
  if (*meta != nullptr) {
    return meta;
  } else {
    return buildInstanceMeta(heap);
  }
}

}
}

#endif

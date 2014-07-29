// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef package_inl_h
#define package_inl_h

#include "array.h"
#include "function.h"
#include "package.h"
#include "string.h"
#include "type-parameter.h"

namespace codeswitch {
namespace internal {

String* Package::getString(word_t index) {
  return String::cast(strings()->get(index));
}


Function* Package::getFunction(word_t index) {
  return Function::cast(functions()->get(index));
}


Class* Package::getClass(word_t index) {
  return Class::cast(classes()->get(index));
}


TypeParameter* Package::getTypeParameter(word_t index) {
  return TypeParameter::cast(typeParameters()->get(index));
}


Function* Package::entryFunction() {
  word_t index = entryFunctionIndex();
  if (index == kNotSet)
    return nullptr;
  return getFunction(index);
}

}
}

#endif

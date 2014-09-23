// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef vm_inl_h
#define vm_inl_h

#include "vm.h"
#include "memory.h"

namespace codeswitch {
namespace internal {

VM* VM::fromAddress(void* address) {
  Chunk* page = Chunk::fromAddress(address);
  return page->vm();
}

}
}

#endif

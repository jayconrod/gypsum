// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef platform_h
#define platform_h

#include "utils.h"

namespace codeswitch {
namespace internal {

const int kReadable = 1;
const int kWritable = 2;
const int kExecutable = 3;
Address allocateMemory(size_t size, int prot);
void releaseMemory(Address addr, size_t size);

}
}

#endif

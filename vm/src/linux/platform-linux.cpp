// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "platform.h"

#include <dlfcn.h>
#include <sys/mman.h>
#include "error.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

Address allocateMemory(size_t size, int prot) {
  ASSERT(isAligned(size, PAGESIZE));
  void* mmapBase = 0;   // TODO: ASLR
  auto mmapProt = ((prot & kReadable) ? PROT_READ : PROT_NONE) |
                  ((prot & kWritable) ? PROT_WRITE : PROT_NONE) |
                  ((prot & kExecutable) ? PROT_EXEC : PROT_NONE);
  auto mmapFlags = MAP_PRIVATE | MAP_ANONYMOUS;
  int mmapFd = -1;
  off_t mmapOffset = 0;
  auto addr = mmap(mmapBase, size, mmapProt, mmapFlags, mmapFd, mmapOffset);
  if (addr == MAP_FAILED) {
    throw Error("could not allocate memory");
  }
  return reinterpret_cast<Address>(addr);
}


void releaseMemory(Address addr, size_t size) {
  ASSERT(isAligned(addr, PAGESIZE) && isAligned(size, PAGESIZE));
  munmap(reinterpret_cast<void*>(addr), size);
}


const char* kNativeLibrarySuffix = ".so";

}
}

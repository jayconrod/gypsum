// Copyright Jay Conrod. All rights reserved.
//
// This file is part of the Gypsum standard library. Use of this
// source code is governed by the 3-clause BSD license that can be
// found in the LICENSE.txt file.

#include <vector>
#include <cassert>
#include <cstdint>
#include <fcntl.h>
#include <unistd.h>
#include <codeswitch.h>

using std::vector;

using codeswitch::Object;
using codeswitch::String;
using codeswitch::VM;

extern "C" __attribute__((visibility("default")))
int32_t std__io___FileReader__open_fd(VM* vm, String path) {
  int fd = open(path.toStdString().c_str(), O_RDONLY);
  assert(fd >= 0);   // TODO: throw exception
  return static_cast<int32_t>(fd);
}


extern "C" __attribute__((visibility("default")))
void std__io___FileReader__close_fd(VM* vm, int32_t fd) {
  close(static_cast<int>(fd));
}


extern "C" __attribute__((visibility("default")))
int32_t std__io___FileReader__read_fd(
    VM* vm,
    int32_t fd,
    Object buffer,
    int32_t offset,
    int32_t count) {
  assert(offset >= 0);   // TODO: throw exception
  assert(count >= 0);   // TODO: throw exception
  // TODO: check for overflow
  assert(static_cast<uint32_t>(offset + count) <= buffer.length());
  vector<int8_t> nativeBuffer(count);
  int ret = read(fd, nativeBuffer.data(), count);
  assert(ret >= 0);   // TODO: throw exception
  auto actualCount = static_cast<int32_t>(ret);
  buffer.copyElementsFrom(offset, nativeBuffer.data(), actualCount);
  return actualCount;
}

// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef platform_h
#define platform_h

#include <string>
#include <vector>
#include "utils.h"

namespace codeswitch {
namespace internal {

const int kReadable = 1;
const int kWritable = 2;
const int kExecutable = 3;
Address allocateMemory(size_t size, int prot);
void releaseMemory(Address addr, size_t size);

/** Returns a list of file names in a directory. Only base names are returned (for example,
 *  if you are listing the directory `foo`, then `bar` and `baz` would be returned, not
 *  `foo/bar` and `foo/baz`). This includes hidden files and the `.` and `..` entries.
 */
std::vector<std::string> listDirectory(const std::string& path);

/** Joins two components of a path, using a platform-specific separator, like `/`. */
std::string pathJoin(const std::string& parent, const std::string& child);

}
}

#endif

// Copyright 2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "platform.h"

#include <dirent.h>
#include "error.h"

using namespace std;

namespace codeswitch {
namespace internal {

vector<string> listDirectory(const std::string& path) {
  DIR* dir = opendir(path.c_str());
  if (dir == nullptr)
    throw Error("could not open directory");

  vector<string> files;
  struct dirent* entry;
  while ((entry = readdir(dir)) != nullptr) {
    files.push_back(string(entry->d_name));
  }
  closedir(dir);

  return files;
}


string pathJoin(const string& parent, const string& child) {
  return parent + '/' + child;
}

}
}

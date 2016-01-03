// Copyright 2015-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.

#include "platform.h"

#include <dirent.h>
#include <dlfcn.h>
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


const char kPathSeparator = '/';


string pathJoin(const string& parent, const string& child) {
  return parent + kPathSeparator + child;
}


string pathDirName(const string& path) {
  auto sepPos = path.rfind(kPathSeparator);
  if (sepPos == string::npos) {
    return ".";
  } else if (sepPos == 0) {
    return "/";
  } else {
    return path.substr(0, sepPos - 1);
  }
}


const char* kNativeLibraryPrefix = "lib";


NativeLibrary loadNativeLibrary(const string& path) {
  auto handle = dlopen(path.c_str(), RTLD_LAZY | RTLD_LOCAL);
  if (handle == nullptr) {
    throw Error(path + ": could not load library: " + dlerror());
  }
  return handle;
}


void unloadNativeLibrary(NativeLibrary library) {
  dlclose(library);
}


NativeFunction loadNativeFunction(NativeLibrary library, const string& name) {
  auto function = dlsym(library, name.c_str());
  if (function == nullptr) {
    throw Error(name + ": could not load function: " + dlerror());
  }
  return function;
}

}
}

// Copyright 2016, Jay Conrod. All rights reserved.
//
// This file is part of the Gypsum standard library. Use of this
// source code is governed by the 3-clause BSD license that can be
// found in the LICENSE.txt file.

#include <string>
#include <sys/stat.h>
#include <codeswitch.h>

using std::string;

using codeswitch::Name;
using codeswitch::Object;
using codeswitch::String;
using codeswitch::VM;

static string getPath(VM* vm, Object file) {
  auto pathName = Name::fromStringForDefn(vm, "File.path");
  auto field = file.clas().findField(pathName);
  auto path = file.getField(field).asString().toStdString();
  return path;
}


extern "C" __attribute__((visibility("default")))
bool std__io___File__exists(VM* vm, Object self) {
  auto path = getPath(vm, self);
  struct stat st;
  int ret = stat(path.c_str(), &st);
  return ret == 0;
}


extern "C" __attribute__((visibility("default")))
bool std__io___File__is_file(VM* vm, Object self) {
  auto path = getPath(vm, self);
  struct stat st;
  int ret = stat(path.c_str(), &st);
  return ret == 0 && S_ISREG(st.st_mode);
}


extern "C" __attribute__((visibility("default")))
bool std__io___File__is_directory(VM* vm, Object self) {
  auto path = getPath(vm, self);
  struct stat st;
  int ret = stat(path.c_str(), &st);
  return ret == 0 && S_ISDIR(st.st_mode);
}

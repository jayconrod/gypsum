// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <algorithm>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <cstdlib>

#include <codeswitch.h>

using std::cerr;
using std::move;
using std::endl;
using std::string;
using std::vector;

using codeswitch::Error;
using codeswitch::Function;
using codeswitch::Name;
using codeswitch::Package;
using codeswitch::String;
using codeswitch::VM;


vector<string> packagePaths;
vector<string> packageNames;


static void printUsage(const char* programName) {
  cerr << "usage: " << programName << "[-P package-path] package-name..." << endl;
}


static vector<string> split(const string& str, char delim) {
  vector<string> pieces;
  size_t begin = 0;
  auto end = str.find(delim);
  while (end != string::npos) {
    auto len = end - begin;
    pieces.push_back(str.substr(begin, len));
    begin = end + 1;
    end = str.find(delim, begin);
  }
  auto len = str.size() - begin;
  pieces.push_back(str.substr(begin, len));
  return pieces;
}


static bool processArgs(int argc, char* argv[]) {
  char* CS_PACKAGE_PATH = getenv("CS_PACKAGE_PATH");
  if (CS_PACKAGE_PATH != nullptr) {
    auto paths = split(CS_PACKAGE_PATH, ':');
    move(paths.begin(), paths.end(), packagePaths.end());
  }

  for (int i = 1; i < argc; i++) {
    string arg(argv[i]);
    if (arg == "-P") {
      if (i + 1 == argc) {
        return false;
      }
      packagePaths.push_back(argv[++i]);
    } else if (arg == "-h" || arg == "--help") {
      return false;
    } else if (arg.size() > 0 && arg[0] == '-') {
      cerr << "unknown option: " << arg << endl;
      return false;
    } else {
      packageNames.push_back(arg);
    }
  }

  return true;
}


int main(int argc, char* argv[]) {
  if (!processArgs(argc, argv) || packageNames.empty()) {
    printUsage(argv[0]);
    return 1;
  }

  try {
    VM vm;
    for (auto& path : packagePaths) {
      vm.addPackageSearchPath(path);
    }
    bool executedEntry = false;
    for (auto& packageName : packageNames) {
      auto name = Name::fromStringForPackage(String(vm, packageName));
      auto package = vm.loadPackage(name);
      auto entryFunction = package.entryFunction();
      if (entryFunction) {
        executedEntry = true;
        entryFunction.call();
      }
    }
    if (!executedEntry) {
      cerr << "no entry function found in any package" << endl;
    }
  } catch (Error& error) {
    cerr << "error: " << error.message() << endl;
    return 1;
  }

  return 0;
}

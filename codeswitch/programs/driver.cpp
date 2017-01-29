// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <algorithm>
#include <functional>
#include <iostream>
#include <iterator>
#include <memory>
#include <string>
#include <vector>

#include <cstdlib>

#include <codeswitch.h>

namespace placeholders = std::placeholders;

using std::back_inserter;
using std::cerr;
using std::move;
using std::endl;
using std::function;
using std::string;
using std::vector;

using codeswitch::Error;
using codeswitch::Function;
using codeswitch::Name;
using codeswitch::Package;
using codeswitch::String;
using codeswitch::VM;
using codeswitch::VMOptions;


vector<string> packagePaths;
vector<function<Package(VM*)>> packageLoaders;


static void printUsage(const char* programName) {
  cerr << "usage: " << programName << " [options] package-files...\n"
      << "  -h|--help                     displays this help\n"
      << "  -P|--package-path dir-name    adds a path to the list of package directories\n"
      << "  -p|--package package-name     finds and loads a package by its name" << endl;
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


static Package loadPackageByName(const string& packageName, VM* vm) {
  auto name = Name::fromStringForPackage(String(vm, packageName));
  return vm->loadPackage(name);
}


static Package loadPackageFromFile(const string& fileName, VM* vm) {
  return vm->loadPackageFromFile(fileName);
}


static bool processArgs(int argc, char* argv[]) {
  char* CS_PACKAGE_PATH = getenv("CS_PACKAGE_PATH");
  if (CS_PACKAGE_PATH != nullptr) {
    auto paths = split(CS_PACKAGE_PATH, ':');
    move(paths.begin(), paths.end(), back_inserter(packagePaths));
  }

  for (int i = 1; i < argc; i++) {
    string arg(argv[i]);
    if (arg == "-h" || arg == "--help") {
      return false;
    } else if (arg == "-P" || arg == "--package-path") {
      if (i + 1 == argc) {
        return false;
      }
      packagePaths.push_back(argv[++i]);
    } else if (arg == "-p" || arg == "--package") {
      if (i + 1 == argc) {
        return false;
      }
      packageLoaders.push_back(bind(loadPackageByName, argv[++i], placeholders::_1));
    } else if (arg.size() > 0 && arg[0] == '-') {
      cerr << "unknown option: " << arg << endl;
      return false;
    } else {
      packageLoaders.push_back(bind(loadPackageFromFile, argv[i], placeholders::_1));
    }
  }

  return true;
}


int main(int argc, char* argv[]) {
  if (!processArgs(argc, argv) || packageLoaders.empty()) {
    printUsage(argv[0]);
    return 1;
  }

  try {
    VMOptions vmOptions;
    for (auto& path : packagePaths) {
      vmOptions.packageSearchPaths.push_back(path);
    }
    VM vm(vmOptions);
    bool executedEntry = false;
    for (auto& loader : packageLoaders) {
      auto package = loader(&vm);
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

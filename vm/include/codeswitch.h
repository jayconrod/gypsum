// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef codeswitch_h
#define codeswitch_h

#include <cstdio>
#include <stdint.h>   // cstdint is C++11 only

namespace codeswitch {

class Package;
class Function;
class Arguments;

class VM {
 public:
  VM();
  VM(const VM& vm);
  ~VM();

  VM& operator = (const VM& vm);

  Package loadPackage(const char* fileName);

 private:
  class Impl;
  Impl* impl_;
};


class Package {
 public:
  class Impl;
  explicit Package(Impl* impl);
  Package(const Package& package);
  ~Package();

  Package& operator = (const Package& package);

  void print(FILE* out);
  Function entryFunction();

 private:
  Impl* impl_;
};


class Function {
 public:
  class Impl;
  explicit Function(Impl* impl);
  Function(const Function& function);
  ~Function();

  Function& operator = (const Function& function);

  void call(const Arguments& args);
  bool callForBoolean(const Arguments& args);
  int8_t callForI8(const Arguments& args);
  int16_t callForI16(const Arguments& args);
  int32_t callForI32(const Arguments& args);
  int64_t callForI64(const Arguments& args);
  float callForF32(const Arguments& args);
  double callForF64(const Arguments& args);

 private:
  Impl* impl_;
  friend class Arguments;
};


class Arguments {
 public:
  explicit Arguments(const Function& function);
  Arguments(const Arguments& arguments);
  ~Arguments();
  Arguments& operator = (const Arguments& arguments);

  void pushBoolean(bool arg);
  void pushI8(int8_t arg);
  void pushI16(int16_t arg);
  void pushI32(int32_t arg);
  void pushI64(int64_t arg);
  void pushF32(float arg);
  void pushF64(double arg);
  bool isComplete() const;

 private:
  class Impl;
  Impl* impl_;
  friend class Function;
};


class Error {
 public:
  explicit Error(const void* impl);
  ~Error();

  const char* message() const;

 private:
  const void* impl_;
};

}

#endif

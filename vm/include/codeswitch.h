// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef codeswitch_h
#define codeswitch_h

#include <memory>
#include <string>
#include <vector>

#include <cstdint>


namespace codeswitch {

class Error;
class Function;
class Name;
class Package;
class String;


class VM final {
 public:
  class Impl;

  VM();
  VM(const VM&) = delete;
  VM(VM&& vm);
  VM& operator = (const VM&) = delete;
  VM& operator = (VM&&);
  ~VM();

  void addPackageSearchPath(const std::string& path);

  Package loadPackage(const Name& name);
  Package loadPackageFromFile(const std::string& fileName);

 private:
  std::unique_ptr<Impl> impl_;

  friend class String;
};


class Package final {
 public:
  class Impl;

  Package();
  explicit Package(Impl* impl);
  Package(const Package&) = delete;
  Package(Package&& package);
  Package& operator = (const Package&) = delete;
  Package& operator = (Package&& package);
  ~Package();

  operator bool () const;
  bool operator ! () const;

  Function entryFunction();

 private:
  std::unique_ptr<Impl> impl_;
};


class Function final {
 public:
  class Impl;

  Function();
  explicit Function(Impl* impl);
  Function(const Function&) = delete;
  Function(Function&&);
  Function& operator = (const Function&) = delete;
  Function& operator = (Function&&);
  ~Function();

  operator bool () const;
  bool operator ! () const;

  template <class... Ts>
  void call(Ts... args);
  template <class... Ts>
  bool callForBoolean(Ts... args);
  template <class... Ts>
  int8_t callForI8(Ts... args);
  template <class... Ts>
  int16_t callForI16(Ts... args);
  template <class... Ts>
  int32_t callForI32(Ts... args);
  template <class... Ts>
  int64_t callForI64(Ts... args);
  template <class... Ts>
  float callForF32(Ts... args);
  template <class... Ts>
  double callForF64(Ts... args);

 private:
  std::unique_ptr<Impl> impl_;

  friend class CallBuilder;
};


class CallBuilder final {
 public:
  class Impl;
  CallBuilder();
  CallBuilder(const Function& function);
  CallBuilder(const CallBuilder&) = delete;
  CallBuilder(CallBuilder&& builder);
  CallBuilder& operator = (const CallBuilder&) = delete;
  CallBuilder& operator = (CallBuilder&& builder);
  ~CallBuilder();

  operator bool () const;
  bool operator ! () const;

  CallBuilder& argUnit();
  CallBuilder& arg(bool value);
  CallBuilder& arg(int8_t value);
  CallBuilder& arg(int16_t value);
  CallBuilder& arg(int32_t value);
  CallBuilder& arg(int64_t value);
  CallBuilder& arg(float value);
  CallBuilder& arg(double value);

  CallBuilder& args() { return *this; }

  template <class... Ts>
  CallBuilder& args(bool value, Ts... rest);
  template <class... Ts>
  CallBuilder& args(int8_t value, Ts... rest);
  template <class... Ts>
  CallBuilder& args(int16_t value, Ts... rest);
  template <class... Ts>
  CallBuilder& args(int32_t value, Ts... rest);
  template <class... Ts>
  CallBuilder& args(int64_t value, Ts... rest);
  template <class... Ts>
  CallBuilder& args(float value, Ts... rest);
  template <class... Ts>
  CallBuilder& args(double value, Ts... rest);

  void call();
  bool callForBoolean();
  int8_t callForI8();
  int16_t callForI16();
  int32_t callForI32();
  int64_t callForI64();
  float callForF32();
  double callForF64();

 private:
  std::unique_ptr<Impl> impl_;
};


class Name final {
 public:
  class Impl;

  Name();
  explicit Name(Impl* impl);
  Name(const Name&) = delete;
  Name(Name&&);
  Name& operator = (const Name&) = delete;
  Name& operator = (Name&& name);
  ~Name();

  operator bool () const;
  bool operator ! () const;

  static Name fromStringForDefn(const String& str);
  static Name fromStringForPackage(const String& str);

 private:
  std::unique_ptr<Impl> impl_;

  friend class VM;
};


class String final {
 public:
  class Impl;

  String();
  explicit String(Impl* impl);
  String(VM& vm, const std::string& str);
  String(const String&) = delete;
  String(String&& str);
  String& operator = (const String&) = delete;
  String& operator = (String&& str);
  ~String();

  operator bool () const;
  bool operator ! () const;

  std::string toStdString() const;

 private:
  std::unique_ptr<Impl> impl_;

  friend class Name;
};


class Error final {
 public:
  class Impl;

  Error();
  explicit Error(Impl* impl);
  Error(const Error&) = delete;
  Error(Error&& error);
  Error& operator = (const Error&) = delete;
  Error& operator = (Error&& error);
  ~Error();

  operator bool () const;
  bool operator ! () const;

  const char* message() const;

 private:
  std::unique_ptr<Impl> impl_;
};


template <class... Ts>
void Function::call(Ts... args) {
  CallBuilder(*this).args(args...).call();
}


template <class... Ts>
bool Function::callForBoolean(Ts... args) {
  return CallBuilder(*this).args(args...).callForBoolean();
}


template <class... Ts>
int8_t Function::callForI8(Ts... args) {
  return CallBuilder(*this).args(args...).callForI8();
}


template <class... Ts>
int16_t Function::callForI16(Ts... args) {
  return CallBuilder(*this).args(args...).callForI16();
}


template <class... Ts>
int32_t Function::callForI32(Ts... args) {
  return CallBuilder(*this).args(args...).callForI32();
}


template <class... Ts>
int64_t Function::callForI64(Ts... args) {
  return CallBuilder(*this).args(args...).callForI64();
}


template <class... Ts>
float Function::callForF32(Ts... args) {
  return CallBuilder(*this).args(args...).callForF32();
}


template <class... Ts>
double Function::callForF64(Ts... args) {
  return CallBuilder(*this).args(args...).callForF64();
}


template <class... Ts>
CallBuilder& CallBuilder::args(bool value, Ts... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(int8_t value, Ts... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(int16_t value, Ts... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(int32_t value, Ts... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(int64_t value, Ts... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(float value, Ts... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(double value, Ts... rest) {
  arg(value);
  return args(rest...);
}

}

#endif

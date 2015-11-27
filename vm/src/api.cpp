// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "codeswitch.h"

#include <memory>

#include "array.h"
#include "function.h"
#include "handle.h"
#include "heap.h"
#include "interpreter.h"
#include "name.h"
#include "object.h"
#include "package.h"
#include "stack.h"
#include "string.h"
#include "type.h"
#include "utils.h"
#include "vm.h"

using std::move;
using std::string;
using std::unique_ptr;
using std::vector;

namespace codeswitch {

namespace i = internal;


#define API_CHECK(expr, message) \
  do { \
    if (!(expr)) { \
      throw Error(new Error::Impl(message)); \
    } \
  } while (false)


#define API_CHECK_SELF(type) API_CHECK(impl_, #type ": this is not a valid reference")


class Error::Impl final {
 public:
  explicit Impl(const string& message)
      : message(message) { }
  explicit Impl(string&& message)
      : message(message) { }
  string message;
};


class VM::Impl final {
 public:
  i::VM vm;
};


class Package::Impl final {
 public:
  explicit Impl(const i::Handle<i::Package>& package)
      : package(package) {
    API_CHECK(package, "package implementation does not reference a package");
  }
  i::Persistent<i::Package> package;
};


class Function::Impl final {
 public:
  explicit Impl(const i::Handle<i::Function>& function)
      : function(function) {
    API_CHECK(function, "function implementation does not reference a function");
  }
  i::Persistent<i::Function> function;
};


class CallBuilder::Impl final {
 public:
  explicit Impl(const i::Handle<i::Function>& function)
      : function_(function) {
    API_CHECK(function, "call builder does not reference a valid function");
    vm_ = i::VM::fromAddress(*function);
    args_.reserve(function->parameterTypes()->length());
  }

  void argUnit() {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::unitType(vm_->roots())), 0));
  }

  void arg(bool value) {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i64Type(vm_->roots())), value));
  }

  void arg(int8_t value) {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i64Type(vm_->roots())), value));
  }

  void arg(int16_t value) {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i64Type(vm_->roots())), value));
  }

  void arg(int32_t value) {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i64Type(vm_->roots())), value));
  }

  void arg(int64_t value) {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i64Type(vm_->roots())), value));
  }

  void arg(float value) {
    auto bits = i::f32ToBits(value);
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i64Type(vm_->roots())), bits));
  }

  void arg(double value) {
    auto bits = i::f64ToBits(value);
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i64Type(vm_->roots())), bits));
  }

  void callForUnit();
  bool callForBoolean();
  int8_t callForI8();
  int16_t callForI16();
  int32_t callForI32();
  int64_t callForI64();
  float callForF32();
  double callForF64();

 private:
  enum Tag { PRIMITIVE, OBJECT };

  struct Value {
    Value(i::Persistent<i::Type>&& type, i::u64 primitive)
        : tag(PRIMITIVE), type(type), primitive(primitive) { }
    Value(i::Persistent<i::Type>&& type, const i::Handle<i::Object>& object)
        : tag(OBJECT), type(type), object(i::Persistent<i::Object>(object)) { }

    Tag tag;
    i::Persistent<i::Type> type;
    i::u64 primitive;
    i::Persistent<i::Object> object;
  };

  i::i64 call();

  i::VM* vm_;
  i::Persistent<i::Function> function_;
  vector<Value> args_;
};


class Name::Impl final {
 public:
  explicit Impl(const i::Handle<i::Name>& name)
      : name(name) {
    API_CHECK(name, "name implementation does not reference a name");
  }
  i::Persistent<i::Name> name;
};


class String::Impl final {
 public:
  explicit Impl(const i::Handle<i::String>& str)
      : str(str) {
    API_CHECK(str, "string implementation does not reference a string");
  }
  i::Persistent<i::String> str;
};


VM::VM()
    : impl_(new Impl) { }


VM::VM(VM&& vm)
    : impl_(move(vm.impl_)) { }


VM& VM::operator = (VM&& vm) {
  impl_ = move(vm.impl_);
  return *this;
}


VM::~VM() { }


void VM::addPackageSearchPath(const string& path) {
  API_CHECK(!path.empty(), "path is empty");
  impl_->vm.addPackageSearchPath(path);
}


Package VM::loadPackage(const Name& name) {
  API_CHECK(name.impl_, "package name is not valid");
  i::VM* vm = &impl_->vm;
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  try {
    i::Persistent<i::Package> package = vm->loadPackage(name.impl_->name);
    return package ? Package(new Package::Impl(package)) : Package();
  } catch (i::Error& error) {
    throw Error(new Error::Impl(error.message()));
  }
}


Package VM::loadPackageFromFile(const string& fileName) {
  i::Persistent<i::Package> package;
  try {
    package = impl_->vm.loadPackage(fileName);
  } catch (i::Error& error) {
    throw Error(new Error::Impl(error.message()));
  }

  return Package(new Package::Impl(package));
}


Package::Package() { }


Package::Package(Impl* impl)
    : impl_(impl) { }


Package::Package(Package&& package)
    : impl_(move(package.impl_)) { }


Package& Package::operator = (Package&& package) {
  impl_ = move(package.impl_);
  return *this;
}


Package::~Package() { }


Package::operator bool () const {
  return static_cast<bool>(impl_);
}


bool Package::operator ! () const {
  return !impl_;
}


Function Package::entryFunction() {
  API_CHECK_SELF(Package);
  i::Function* function = impl_->package->entryFunction();
  if (function == nullptr) {
    return Function(nullptr);
  }
  return Function(new Function::Impl(i::Persistent<i::Function>(function)));
}


Function::Function() { }


Function::Function(Impl* impl)
    : impl_(impl) { }


Function::Function(Function&& function)
    : impl_(move(function.impl_)) { }


Function& Function::operator = (Function&& function) {
  impl_ = move(function.impl_);
  return *this;
}


Function::~Function() { }


Function::operator bool () const {
  return static_cast<bool>(impl_);
}


bool Function::operator ! () const {
  return !impl_;
}


CallBuilder::CallBuilder() { }


CallBuilder::CallBuilder(const Function& function) {
  API_CHECK(function.impl_, "not a valid function");
  impl_ = unique_ptr<CallBuilder::Impl>(new Impl(function.impl_->function));
}


CallBuilder::CallBuilder(CallBuilder&& builder)
    : impl_(move(builder.impl_)) { }


CallBuilder& CallBuilder::operator = (CallBuilder&& builder) {
  impl_ = move(builder.impl_);
  return *this;
}


CallBuilder::~CallBuilder() { }


CallBuilder::operator bool () const {
  return static_cast<bool>(impl_);
}


bool CallBuilder::operator ! () const {
  return !impl_;
}


CallBuilder& CallBuilder::argUnit() {
  API_CHECK_SELF(CallBuilder);
  impl_->argUnit();
  return *this;
}


CallBuilder& CallBuilder::arg(bool value) {
  API_CHECK_SELF(CallBuilder);
  impl_->arg(value);
  return *this;
}


CallBuilder& CallBuilder::arg(int8_t value) {
  API_CHECK_SELF(CallBuilder);
  impl_->arg(value);
  return *this;
}


CallBuilder& CallBuilder::arg(int16_t value) {
  API_CHECK_SELF(CallBuilder);
  impl_->arg(value);
  return *this;
}


CallBuilder& CallBuilder::arg(int32_t value) {
  API_CHECK_SELF(CallBuilder);
  impl_->arg(value);
  return *this;
}


CallBuilder& CallBuilder::arg(int64_t value) {
  API_CHECK_SELF(CallBuilder);
  impl_->arg(value);
  return *this;
}


CallBuilder& CallBuilder::arg(float value) {
  API_CHECK_SELF(CallBuilder);
  impl_->arg(value);
  return *this;
}


CallBuilder& CallBuilder::arg(double value) {
  API_CHECK_SELF(CallBuilder);
  impl_->arg(value);
  return *this;
}


void CallBuilder::call() {
  API_CHECK_SELF(CallBuilder);
  impl_->callForUnit();
}


bool CallBuilder::callForBoolean() {
  API_CHECK_SELF(CallBuilder);
  return impl_->callForBoolean();
}


int8_t CallBuilder::callForI8() {
  API_CHECK_SELF(CallBuilder);
  return impl_->callForI8();
}


int16_t CallBuilder::callForI16() {
  API_CHECK_SELF(CallBuilder);
  return impl_->callForI16();
}


int32_t CallBuilder::callForI32() {
  API_CHECK_SELF(CallBuilder);
  return impl_->callForI32();
}


int64_t CallBuilder::callForI64() {
  API_CHECK_SELF(CallBuilder);
  return impl_->callForI64();
}


float CallBuilder::callForF32() {
  API_CHECK_SELF(CallBuilder);
  return impl_->callForF32();
}


double CallBuilder::callForF64() {
  API_CHECK_SELF(CallBuilder);
  return impl_->callForF64();
}


void CallBuilder::Impl::callForUnit() {
  API_CHECK(function_->returnType()->form() == i::Type::UNIT_TYPE,
      "wrong function return type");
  call();
}


bool CallBuilder::Impl::callForBoolean() {
  API_CHECK(function_->returnType()->form() == i::Type::BOOLEAN_TYPE,
      "wrong function return type");
  return static_cast<bool>(call());
}


int8_t CallBuilder::Impl::callForI8() {
  API_CHECK(function_->returnType()->form() == i::Type::I8_TYPE,
      "wrong function return type");
  return static_cast<int8_t>(call());
}


int16_t CallBuilder::Impl::callForI16() {
  API_CHECK(function_->returnType()->form() == i::Type::I16_TYPE,
      "wrong function return type");
  return static_cast<int16_t>(call());
}


int32_t CallBuilder::Impl::callForI32() {
  API_CHECK(function_->returnType()->form() == i::Type::I32_TYPE,
      "wrong function return type");
  return static_cast<int32_t>(call());
}


int64_t CallBuilder::Impl::callForI64() {
  API_CHECK(function_->returnType()->form() == i::Type::I64_TYPE,
      "wrong function return type");
  return static_cast<int64_t>(call());
}


float CallBuilder::Impl::callForF32() {
  API_CHECK(function_->returnType()->form() == i::Type::F32_TYPE,
      "wrong function return type");
  return i::f32FromBits(static_cast<i::u32>(call()));
}


double CallBuilder::Impl::callForF64() {
  API_CHECK(function_->returnType()->form() == i::Type::F64_TYPE,
      "wrong function return type");
  return i::f64FromBits(static_cast<i::u64>(call()));
}


i::i64 CallBuilder::Impl::call() {
  // Check arguments.
  i::HandleScope handleScope(vm_);
  API_CHECK(args_.size() == function_->parameterTypes()->length(), "wrong number of arguments");
  for (i::length_t i = 0; i < args_.size(); i++) {
    if (args_[i].tag == OBJECT) {
      args_[i].type = i::Object::typeof(args_[i].object);
    }
    API_CHECK(i::Type::isSubtypeOf(args_[i].type, handle(function_->parameterTypes()->get(i))),
        "type error");
  }

  // Push arguments onto the stack.
  i::AllowAllocationScope denyAlloc(vm_->heap(), false);
  const i::Persistent<i::Stack>& stack = vm_->stack();
  for (auto& arg : args_) {
    if (arg.tag == OBJECT) {
      stack->push(arg.object);
    } else {
      stack->push(arg.primitive);
    }
  }

  // Perform the call.
  i::Interpreter interpreter(vm_, vm_->stack());
  return interpreter.call(function_);
}


Name::Name() { }


Name::Name(Impl* impl)
    : impl_(impl) { }


Name::Name(Name&& name)
    : impl_(move(name.impl_)) { }


Name& Name::operator = (Name&& name) {
  impl_ = move(name.impl_);
  return *this;
}


Name::~Name() { }


Name::operator bool () const {
  return static_cast<bool>(impl_);
}


bool Name::operator ! () const {
  return !impl_;
}


Name Name::fromStringForDefn(const String& str) {
  API_CHECK(str.impl_ != nullptr, "string argument does not reference a string");
  const i::Persistent<i::String>& istr = str.impl_->str;
  i::VM* vm = i::VM::fromAddress(*istr);
  i::HandleScope handleScope(vm);
  i::Local<i::Name> iname = i::Name::fromString(vm->heap(), istr, i::Name::DEFN_NAME);
  API_CHECK(iname, "string argument is not a valid name for definitions");
  return Name(new Name::Impl(i::Persistent<i::Name>(iname)));
}


Name Name::fromStringForPackage(const String& str) {
  API_CHECK(str.impl_ != nullptr, "string argument does not reference a string");
  const i::Persistent<i::String>& istr = str.impl_->str;
  i::VM* vm = i::VM::fromAddress(*istr);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  i::HandleScope handleScope(vm);
  i::Local<i::Name> iname = i::Name::fromString(vm->heap(), istr, i::Name::PACKAGE_NAME);
  API_CHECK(iname, "string argument is not a valid name for packages");
  return Name(new Name::Impl(i::Persistent<i::Name>(iname)));
}


String::String() { }


String::String(Impl* impl)
    : impl_(impl) { }


String::String(VM& vm, const string& str) {
  auto heap = vm.impl_->vm.heap();
  i::AllowAllocationScope allowAlloc(heap, true);
  i::HandleScope handleScope(&vm.impl_->vm);
  i::Local<i::String> istr = i::String::fromUtf8String(heap, str);
  impl_.reset(new Impl(istr));
}


String::String(String&& str)
    : impl_(move(str.impl_)) { }


String& String::operator = (String&& str) {
  impl_ = move(str.impl_);
  return *this;
}


String::~String() { }


String::operator bool () const {
  return static_cast<bool>(impl_);
}


bool String::operator ! () const {
  return !impl_;
}


string String::toStdString() const {
  return impl_->str->toUtf8StlString();
}


Error::Error() { }


Error::Error(Impl* impl)
    : impl_(impl) { }


Error::Error(Error&& error)
    : impl_(move(error.impl_)) { }


Error& Error::operator = (Error&& error) {
  impl_ = move(error.impl_);
  return *this;
}


Error::~Error() { }


Error::operator bool () const {
  return static_cast<bool>(impl_);
}


bool Error::operator ! () const {
  return !impl_;
}


const char* Error::message() const {
  return impl_->message.c_str();
}

}

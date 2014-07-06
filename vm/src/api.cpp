// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "codeswitch.h"

#include <algorithm>
#include <vector>
#include "block-inl.h"
#include "error.h"
#include "function.h"
#include "function-inl.h"
#include "handle-inl.h"
#include "interpreter.h"
#include "package-inl.h"
#include "stack-inl.h"
#include "type-inl.h"
#include "utils.h"
#include "vm.h"

namespace codeswitch {

namespace i = internal;

#define API_CHECK(cond, message) \
do { \
  if (!(cond)) { \
    throw Error(message); \
  } \
} while(false)


#ifdef DEBUG
#define API_ASSERT(cond, message) API_CHECK(cond, message)
#else
#define API_ASSERT(cond, message)
#endif


#define API_UNREACHABLE() i::abort(__FILE__, __LINE__, "unreachable code")


class RefCounted {
 public:
  RefCounted()
      : refCount_(1) {}

  int refCount() { return refCount_; }
  void retain() { refCount_++; }
  void release() {
    refCount_--;
    if (refCount_ == 0)
      delete this;
  }

 private:
  int refCount_;
};


class VM::Impl : public RefCounted {
 public:
  Impl(i::VM* vm)
      : vm_(vm) { }
  ~Impl() {
    delete vm_;
  }

  i::VM* vm() { return vm_; }

 private:
  i::VM* vm_;
};


class Package::Impl : public RefCounted {
 public:
  Impl(i::VM* vm, i::Handle<i::Package> package)
      : vm_(vm),
        package_(package) { }

  i::VM* vm() { return vm_; }
  i::Handle<i::Package> package() { return package_; }

 private:
  i::VM* vm_;
  i::Handle<i::Package> package_;
};


class Function::Impl : public RefCounted {
 public:
  Impl(i::VM* vm, i::Handle<i::Function> function)
      : vm_(vm),
        function_(function) { }

  i::VM* vm() { return vm_; }
  i::Handle<i::Function> function() { return function_; }

  i::i64 call(i::i64* data, i::word_t count) {
    i::Handle<i::Stack> stack = vm()->stack();
    i::word_t size = count * sizeof(i::i64);
    stack->setStackPointerOffset(stack->stackPointerOffset() - size);
    copy_n(data, count, reinterpret_cast<i::word_t*>(stack->sp()));
    i::Interpreter interpreter(vm(), stack);
    i::i64 result = interpreter.call(function());
    return result;
  }

 private:
  i::VM* vm_;
  i::Handle<i::Function> function_;
};


class Arguments::Impl: public RefCounted {
 public:
  Impl(i::VM* vm, i::Handle<i::Function> function)
      : function_(function),
        paramIndex_(0),
        data_(function->parameterCount()) { }

  i::i64* data() { return data_.data(); }

  template <class T>
  void push(T arg);

  i::Type* nextType() {
    API_CHECK(paramIndex_ < function_->parameterCount(), "too many arguments");
    return function_->parameterType(paramIndex_);
  }

  bool isComplete() {
    return paramIndex_ == function_->parameterCount();
  }

 private:
  i::Handle<i::Function> function_;
  i::word_t paramIndex_;
  vector<i::i64> data_;
};


template <class T>
void Arguments::Impl::push(T arg) {
  API_CHECK(paramIndex_ < function_->parameterCount(), "too many arguments");
  data_[paramIndex_++] = static_cast<i::i64>(arg);
}

template <>
void Arguments::Impl::push<i::f32>(i::f32 arg) {
  API_CHECK(paramIndex_ < function_->parameterCount(), "too many arguments");
  auto bits = static_cast<i::u64>(i::f32ToBits(arg));
  data_[paramIndex_++] = bits;
}

template <>
void Arguments::Impl::push<i::f64>(i::f64 arg) {
  API_CHECK(paramIndex_ < function_->parameterCount(), "too many arguments");
  auto bits = i::f64ToBits(arg);
  data_[paramIndex_++] = bits;
}

VM::VM()
    : impl_(new Impl(new i::VM())) {}


VM::VM(const VM& vm)
    : impl_(vm.impl_) {
  impl_->retain();
}


VM& VM::operator = (const VM& vm) {
  impl_->release();
  impl_ = vm.impl_;
  impl_->retain();
  return *this;
}


VM::~VM() {
  impl_->release();
}


Package VM::loadPackage(const char* fileName) {
  i::VM::Scope vmScope(impl_->vm());
  i::Handle<i::Package> package;
  try {
    package = i::Package::loadFromFile(impl_->vm(), fileName);
  } catch (i::Error error) {
    throw Error(error.message());
  }

  impl_->vm()->addPackage(package);
  return Package(new Package::Impl(impl_->vm(), package));
}


Package::Package(Impl* impl)
    : impl_(impl) { }


Package::Package(const Package& package)
    : impl_(package.impl_) {
  impl_->retain();
}


Package& Package::operator = (const Package& package) {
  impl_->release();
  impl_ = package.impl_;
  impl_->retain();
  return *this;
}


Package::~Package() {
  impl_->release();
}


void Package::print(FILE* out) {
  i::VM::Scope vmScope(impl_->vm());
  impl_->package()->print(out);
}


Function Package::entryFunction() {
  i::VM::Scope vmScope(impl_->vm());
  i::Handle<i::Function> function(impl_->vm(), impl_->package()->entryFunction());
  if (!function)
    throw Error("this package does not have an entry function");
  return Function(new Function::Impl(impl_->vm(), function));
}


Function::Function(Impl* impl)
    : impl_(impl) { }


Function::Function(const Function& function)
    : impl_(function.impl_) {
  impl_->retain();
}


Function& Function::operator = (const Function& function) {
  impl_->release();
  impl_ = function.impl_;
  impl_->retain();
  return *this;
}


Function::~Function() {
  impl_->release();
}


void Function::call(const Arguments& args) {
  i::VM::Scope vmScope(impl_->vm());
  API_CHECK(args.isComplete(), "not enough arguments");
  try {
    impl_->call(args.impl_->data(), impl_->function()->parameterCount());
  } catch (i::Error& exn) {
    throw Error(exn.message());
  }
}


#define DEFINE_CALL(ctype, typename, cast)                                                     \
ctype Function::callFor##typename(const Arguments& args) {                                     \
  i::VM::Scope vmScope(impl_->vm());                                                           \
  API_CHECK(impl_->function()->returnType()->is##typename(), "type error");                    \
  API_CHECK(args.isComplete(), "not enough arguments");                                        \
  i::i64 result = i::kNotSet;                                                                  \
  try {                                                                                        \
    result = impl_->call(args.impl_->data(), impl_->function()->parameterCount());             \
  } catch (i::Error& exn) {                                                                    \
    throw Error(exn.message());                                                                \
  }                                                                                            \
  return cast(result);                                                                         \
}                                                                                              \

DEFINE_CALL(bool, Boolean, static_cast<bool>)
DEFINE_CALL(int8_t, I8, static_cast<int8_t>)
DEFINE_CALL(int16_t, I16, static_cast<int16_t>)
DEFINE_CALL(int32_t, I32, static_cast<int32_t>)
DEFINE_CALL(int64_t, I64, static_cast<int64_t>)
DEFINE_CALL(float, F32, i::f32FromBits)
DEFINE_CALL(double, F64, i::f64FromBits)

#undef DEFINE_CALL


Arguments::Arguments(const Function& function)
    : impl_(new Impl(function.impl_->vm(), function.impl_->function())) {}


Arguments::Arguments(const Arguments& arguments)
    : impl_(arguments.impl_) {
  impl_->retain();
}


Arguments::~Arguments() {
  impl_->release();
}


Arguments& Arguments::operator = (const Arguments& arguments) {
  impl_->release();
  impl_ = arguments.impl_;
  impl_->retain();
  return *this;
}


void Arguments::pushBoolean(bool arg) {
  API_CHECK(impl_->nextType()->isBoolean(), "type error");
  impl_->push(arg);
}


void Arguments::pushI8(int8_t arg) {
  API_CHECK(impl_->nextType()->isI8(), "type error");
  impl_->push(arg);
}


void Arguments::pushI16(int16_t arg) {
  API_CHECK(impl_->nextType()->isI16(), "type error");
  impl_->push(arg);
}


void Arguments::pushI32(int32_t arg) {
  API_CHECK(impl_->nextType()->isI32(), "type error");
  impl_->push(arg);
}


void Arguments::pushI64(int64_t arg) {
  API_CHECK(impl_->nextType()->isI64(), "type error");
  impl_->push(arg);
}


void Arguments::pushF32(float arg) {
  API_CHECK(impl_->nextType()->isF32(), "type error");
  impl_->push(arg);
}


void Arguments::pushF64(double arg) {
  API_CHECK(impl_->nextType()->isF64(), "type error");
  impl_->push(arg);
}


bool Arguments::isComplete() const {
  return impl_->isComplete();
}


Error::Error(const void* impl)
    : impl_(impl) { }


Error::~Error() {
}


const char* Error::message() const {
  return reinterpret_cast<const char*>(impl_);
}

}

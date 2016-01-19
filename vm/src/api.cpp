// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "codeswitch.h"
#include "api.h"

#include <memory>
#include <vector>

#include "array.h"
#include "bitmap.h"
#include "builtins.h"
#include "function.h"
#include "handle.h"
#include "heap.h"
#include "interpreter.h"
#include "name.h"
#include "object.h"
#include "package.h"
#include "platform.h"
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
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::booleanType(vm_->roots())), value));
  }

  void arg(int8_t value) {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i8Type(vm_->roots())), value));
  }

  void arg(int16_t value) {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i16Type(vm_->roots())), value));
  }

  void arg(int32_t value) {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i32Type(vm_->roots())), value));
  }

  void arg(int64_t value) {
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::i64Type(vm_->roots())), value));
  }

  void arg(float value) {
    auto bits = i::f32ToBits(value);
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::f32Type(vm_->roots())), bits));
  }

  void arg(double value) {
    auto bits = i::f64ToBits(value);
    args_.push_back(Value(i::Persistent<i::Type>(i::Type::f64Type(vm_->roots())), bits));
  }

  void arg(const String& value);
  void arg(const Object& value);

  void callForAny();
  bool callForBoolean();
  int8_t callForI8();
  int16_t callForI16();
  int32_t callForI32();
  int64_t callForI64();
  float callForF32();
  double callForF64();
  String callForString();
  Object callForObject();

 private:
  enum Tag { PRIMITIVE, OBJECT };

  struct Value {
    Value(i::Persistent<i::Type>&& type, i::u64 primitive)
        : tag(PRIMITIVE), type(type), primitive(primitive) { }
    Value(i::Persistent<i::Type>&& type, const i::Handle<i::Object>& object)
        : tag(OBJECT), type(type), object(i::Persistent<i::Object>(object)) { }
    Value(i::Persistent<i::Type>&& type, const i::Handle<i::String>& string)
        : tag(OBJECT), type(type), object(i::Persistent<i::Object>(string)) { }

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


VM::VM()
    : impl_(new Impl(VMOptions())) { }


static void checkNativeFunctionSearchOrder(
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  i::word_t bits = 0;
  i::Bitmap bitmap(&bits, i::kBitsInWord);
  for (auto search : nativeFunctionSearchOrder) {
    API_CHECK(!bitmap[search], "duplicate entry in native function search order");
    bitmap.set(search, true);
  }
}


VM::VM(const VMOptions& vmOptions) {
  // Check that there were no duplicates in the native function search order.
  checkNativeFunctionSearchOrder(vmOptions.nativeFunctionSearchOrder);
  impl_.reset(new Impl(vmOptions));
  impl_->vm.setApiPtr(this);
}


VM::VM(VM&& vm)
    : impl_(move(vm.impl_)) {
  impl_->vm.setApiPtr(this);
}


VM& VM::operator = (VM&& vm) {
  impl_ = move(vm.impl_);
  impl_->vm.setApiPtr(this);
  return *this;
}


VM::~VM() { }


Package VM::loadPackage(const Name& name,
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  API_CHECK(name.impl_, "package name is not valid");
  checkNativeFunctionSearchOrder(nativeFunctionSearchOrder);
  i::VM* vm = &impl_->vm;
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  try {
    i::Persistent<i::Package> package =
        vm->loadPackage(name.impl_->name, nativeFunctionSearchOrder);
    if (!package) {
      throw Error(new Error::Impl("could not locate package"));
    }
    return Package(new Package::Impl(package));
  } catch (i::Error& error) {
    throw Error(new Error::Impl(error.message()));
  }
}


Package VM::loadPackageFromFile(const string& fileName,
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  checkNativeFunctionSearchOrder(nativeFunctionSearchOrder);
  i::Persistent<i::Package> package;
  try {
    package = impl_->vm.loadPackage(fileName, nativeFunctionSearchOrder);
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


Function Package::entryFunction() const {
  API_CHECK_SELF(Package);
  i::Function* function = impl_->package->entryFunction();
  if (function == nullptr) {
    return Function(nullptr);
  }
  return Function(new Function::Impl(i::Persistent<i::Function>(function)));
}


Function Package::getFunction(const Name& name) const {
  API_CHECK_SELF(Package);
  auto functions = impl_->package->functions();
  for (auto function : *functions) {
    if (function->name()->equals(*name.impl_->name)) {
      return Function(new Function::Impl(i::Persistent<i::Function>(function)));
    }
  }
  return Function(nullptr);
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


CallBuilder& CallBuilder::arg(const String& value) {
  API_CHECK_SELF(CallBuilder);
  impl_->arg(value);
  return *this;
}


void CallBuilder::call() {
  API_CHECK_SELF(CallBuilder);
  impl_->callForAny();
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


String CallBuilder::callForString() {
  API_CHECK_SELF(CallBuilder);
  return impl_->callForString();
}


Object CallBuilder::callForObject() {
  API_CHECK_SELF(CallBuilder);
  return impl_->callForObject();
}


void CallBuilder::Impl::arg(const String& value) {
  API_CHECK(value, "not a valid String reference");
  i::Persistent<i::Type> type(vm_->roots()->getBuiltinType(i::BUILTIN_STRING_CLASS_ID));
  args_.push_back(Value(move(type), value.impl_->str));
}


void CallBuilder::Impl::arg(const Object& value) {
  API_CHECK(value, "not a valid Object reference");
  i::Persistent<i::Type> type(i::Type::rootClassType(vm_->roots()));
  args_.push_back(Value(move(type), value.impl_->obj));
}


void CallBuilder::Impl::callForAny() {
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


String CallBuilder::Impl::callForString() {
  API_CHECK(function_->returnType()->equals(
          vm_->roots()->getBuiltinType(i::BUILTIN_STRING_CLASS_ID)),
      "wrong function return type");
  i::i64 stringPtrBits = call();
  i::AllowAllocationScope allowAlloc(vm_->heap(), false);
  i::String* rawString = reinterpret_cast<i::String*>(static_cast<i::word_t>(stringPtrBits));
  return String(rawString != nullptr
      ? new String::Impl(i::Persistent<i::String>(rawString))
      : nullptr);
}


Object CallBuilder::Impl::callForObject() {
  API_CHECK(function_->returnType()->isRootClass(), "wrong function return type");
  i::i64 objPtrBits = call();
  i::AllowAllocationScope allowAlloc(vm_->heap(), false);
  i::Object* rawObject = reinterpret_cast<i::Object*>(static_cast<i::word_t>(objPtrBits));
  return Object(rawObject != nullptr
      ? new Object::Impl(i::Persistent<i::Object>(rawObject))
      : nullptr);
}


i::i64 CallBuilder::Impl::call() {
  // Check arguments.
  i::HandleScope handleScope(vm_);
  i::AllowAllocationScope allowAlloc(vm_->heap(), true);
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
      stack->push(arg.object.getOrNull());
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
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
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


String String::operator + (const String& other) const {
  API_CHECK_SELF(String);
  API_CHECK_ARG(other);
  auto vm = impl_->str->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAllocation(vm->heap(), true);
  i::Persistent<i::String> result(i::String::concat(impl_->str, other.impl_->str));
  return String(new String::Impl(result));
}


int String::compare(const String& other) const {
  API_CHECK_SELF(String);
  API_CHECK_ARG(other);
  return impl_->str->compare(*other.impl_->str);
}


string String::toStdString() const {
  return impl_->str->toUtf8StlString();
}


Object::Object() { }


Object::Object(Impl* impl)
    : impl_(impl) { }


Object::Object(Object&& obj)
    : impl_(move(obj.impl_)) { }


Object& Object::operator = (Object&& obj) {
  impl_ = move(obj.impl_);
  return *this;
}


Object::~Object() { }


Error::Error() { }


Object::operator bool () const {
  return static_cast<bool>(impl_);
}


bool Object::operator ! () const {
  return !impl_;
}


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


int64_t callNativeFunction(i::Function* callee, i::VM* vm, i::Address sp) {
  // Prepare the arguments.
  auto paramTypes = callee->parameterTypes();
  auto argCount = paramTypes->length();
  unique_ptr<uint64_t[]> rawArgs(new uint64_t[argCount]);
  unique_ptr<bool[]> argsAreInt(new bool[argCount]);
  unique_ptr<Object[]> objects(new Object[argCount]);

  for (i::length_t i = 0; i < argCount; i++) {
    uint64_t value = i::mem<uint64_t>(sp + (argCount - i - 1) * i::Interpreter::kSlotSize);
    auto paramType = paramTypes->get(i);
    if (paramType->isObject()) {
      argsAreInt[i] = true;
      auto rawObject = reinterpret_cast<i::Object*>(static_cast<i::word_t>(value));
      if (rawObject != nullptr) {
        objects[i] = Object(new Object::Impl(i::Persistent<i::Object>(vm, rawObject)));
      }
      rawArgs[i] = static_cast<uint64_t>(reinterpret_cast<i::word_t>(&objects[i]));
    } else if (paramType->isFloat()) {
      argsAreInt[i] = false;
      rawArgs[i] = value;
    } else {
      argsAreInt[i] = true;
      rawArgs[i] = value;
    }
  }

  auto returnType = callee->returnType();
  auto resultType = returnType->isObject() ? i::NATIVE_PTR :
      returnType->isFloat() ? i::NATIVE_FLOAT :
      i::NATIVE_INT;

  // Load the native function, if it's not loaded already.
  auto nativeFunction = callee->ensureAndGetNativeFunction();

  // Call the funtion via an assembly stub.
  auto result = i::callNativeFunctionRaw(
      vm->apiPtr(), nativeFunction, argCount,
      rawArgs.get(), argsAreInt.get(), resultType);
  return result;
}

}

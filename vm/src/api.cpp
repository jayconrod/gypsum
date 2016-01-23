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


static_assert(sizeof(Reference) == sizeof(i::Persistent<i::Block>),
    "Reference and Persistent must be the same size");


template <class T>
static i::Persistent<T>& unwrap(Reference& ref) {
  return *reinterpret_cast<i::Persistent<T>*>(&ref);
}


template <class T>
static const i::Persistent<T>& unwrap(const Reference& ref) {
  return *reinterpret_cast<const i::Persistent<T>*>(&ref);
}


template <class ExternalT, class InternalT>
static ExternalT wrap(const i::Handle<InternalT>& block) {
  if (!block)
    return ExternalT();
  auto& handleStorage = block->getVM()->handleStorage();
  InternalT** slot = nullptr;
  handleStorage.createPersistent(*block, &slot);
  return ExternalT(reinterpret_cast<Impl*>(slot));
}


template <class ExternalT, class InternalT>
static ExternalT wrap(InternalT* block) {
  if (!block)
    return ExternalT();
  auto& handleStorage = block->getVM()->handleStorage();
  InternalT** slot = nullptr;
  handleStorage.createPersistent(block, &slot);
  return ExternalT(reinterpret_cast<Impl*>(slot));
}


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
        vm->loadPackage(unwrap<i::Name>(name), nativeFunctionSearchOrder);
    if (!package) {
      throw Error(new Error::Impl("could not locate package"));
    }
    return wrap<Package, i::Package>(package);
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

  return wrap<Package, i::Package>(package);
}


Reference::Reference()
    : impl_(nullptr) { }


Reference::Reference(Impl* impl)
    : impl_(impl) { }


Reference::Reference(Reference&& ref)
    : impl_(ref.impl_) {
  ref.impl_ = nullptr;
}


Reference& Reference::operator = (Reference&& ref) {
  impl_ = ref.impl_;
  ref.impl_ = nullptr;
  return *this;
}


Reference::~Reference() {
  clear();
}


bool Reference::isValid() const {
  return impl_ != nullptr;
}


void Reference::clear() {
  if (impl_) {
    auto& handleStorage = unwrap<i::Block>(*this)->getVM()->handleStorage();
    handleStorage.destroyPersistent(reinterpret_cast<i::Block**>(impl_));
    impl_ = nullptr;
  }
}


Package::Package() { }


Package::Package(Impl* impl)
    : Reference(impl) { }


Package::Package(Package&& package)
    : Reference(package.impl_) { }


Package::~Package() { }


Function Package::entryFunction() const {
  API_CHECK_SELF(Package);
  i::Function* function = unwrap<i::Package>(*this)->entryFunction();
  if (function == nullptr) {
    return Function();
  }
  return wrap<Function, i::Function>(function);
}


Function Package::getFunction(const Name& name) const {
  API_CHECK_SELF(Package);
  auto functions = unwrap<i::Package>(*this)->functions();
  for (auto function : *functions) {
    if (function->name()->equals(*unwrap<i::Name>(name))) {
      return wrap<Function, i::Function>(function);
    }
  }
  return Function(nullptr);
}


Function::Function() { }


Function::Function(Impl* impl)
    : Reference(impl) { }


Function::Function(Function&& function)
    : Reference(move(function)) { }


Function::~Function() { }


CallBuilder::CallBuilder() { }


CallBuilder::CallBuilder(const Function& function) {
  API_CHECK(function.impl_, "not a valid function");
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(unwrap<i::Function>(function)));
}


CallBuilder::~CallBuilder() { }


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
  args_.push_back(Value(move(type), unwrap<i::String>(value)));
}


void CallBuilder::Impl::arg(const Object& value) {
  API_CHECK(value, "not a valid Object reference");
  i::Persistent<i::Type> type(i::Type::rootClassType(vm_->roots()));
  args_.push_back(Value(move(type), unwrap<i::Object>(value)));
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
  return wrap<String, i::String>(rawString);
}


Object CallBuilder::Impl::callForObject() {
  API_CHECK(function_->returnType()->isRootClass(), "wrong function return type");
  i::i64 objPtrBits = call();
  i::AllowAllocationScope allowAlloc(vm_->heap(), false);
  i::Object* rawObject = reinterpret_cast<i::Object*>(static_cast<i::word_t>(objPtrBits));
  return wrap<Object, i::Object>(rawObject);
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
    : Reference(impl) { }


Name::Name(Name&& name)
    : Reference(move(name)) { }


Name::~Name() { }


Name Name::fromStringForDefn(const String& str) {
  API_CHECK(str.impl_ != nullptr, "string argument does not reference a string");
  auto istr = unwrap<i::String>(str);
  auto vm = i::VM::fromAddress(*istr);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  i::HandleScope handleScope(vm);
  auto iname = i::Name::fromString(vm->heap(), istr, i::Name::DEFN_NAME);
  API_CHECK(iname, "string argument is not a valid name for definitions");
  return wrap<Name, i::Name>(iname);
}


Name Name::fromStringForPackage(const String& str) {
  API_CHECK(str.impl_ != nullptr, "string argument does not reference a string");
  auto istr = unwrap<i::String>(str);
  auto vm = i::VM::fromAddress(*istr);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  i::HandleScope handleScope(vm);
  auto iname = i::Name::fromString(vm->heap(), istr, i::Name::PACKAGE_NAME);
  API_CHECK(iname, "string argument is not a valid name for packages");
  return wrap<Name, i::Name>(iname);
}


String::String() { }


String::String(Impl* impl)
    : Reference(impl) { }


String::String(VM& vm, const string& str) {
  auto heap = vm.impl_->vm.heap();
  i::AllowAllocationScope allowAlloc(heap, true);
  i::HandleScope handleScope(&vm.impl_->vm);
  i::Local<i::String> istr = i::String::fromUtf8String(heap, str);
  *this = move(wrap<String, i::String>(istr));
}


String::String(String&& str)
    : Reference(move(str)) { }


String::~String() { }


String String::operator + (const String& other) const {
  API_CHECK_SELF(String);
  API_CHECK_ARG(other);
  auto str = unwrap<i::String>(*this);
  auto vm = str->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAllocation(vm->heap(), true);
  auto result = i::String::concat(str, unwrap<i::String>(other));
  return wrap<String, i::String>(result);
}


String String::operator + (const std::string& other) const {
  API_CHECK_SELF(String);
  auto str = unwrap<i::String>(*this);
  auto vm = str->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAllocation(vm->heap(), true);
  auto iother = i::String::fromUtf8String(vm->heap(), other);
  auto result = i::String::concat(str, iother);
  return wrap<String, i::String>(result);
}


int String::compare(const String& other) const {
  API_CHECK_SELF(String);
  API_CHECK_ARG(other);
  auto str = unwrap<i::String>(*this);
  return str->compare(*unwrap<i::String>(other));
}


string String::toStdString() const {
  API_CHECK_SELF(String);
  return unwrap<i::String>(*this)->toUtf8StlString();
}


Object::Object() { }


Object::Object(Impl* impl)
    : Reference(impl) { }


Object::Object(Object&& obj)
    : Reference(move(obj)) { }


Object::~Object() { }


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
        objects[i] = wrap<Object, i::Object>(rawObject);
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
  i::SealHandleScope noHandles(vm);
  i::AllowAllocationScope noAlloc(vm->heap(), false);
  auto result = i::callNativeFunctionRaw(
      vm->apiPtr(), nativeFunction, argCount,
      rawArgs.get(), argsAreInt.get(), resultType);
  return result;
}

}

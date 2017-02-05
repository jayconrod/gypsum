// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "codeswitch.h"
#include "api.h"

#include <algorithm>
#include <memory>
#include <vector>

#include "array.h"
#include "bitmap.h"
#include "block.h"
#include "builtins.h"
#include "field.h"
#include "function.h"
#include "global.h"
#include "handle.h"
#include "hash-table.h"
#include "heap.h"
#include "index.h"
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

using std::copy_n;
using std::istream;
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


template <class T>
static T* unwrapRaw(Reference& ref) {
  return **reinterpret_cast<i::Persistent<T>*>(&ref);
}


template <class T>
static const T* unwrapRaw(const Reference& ref) {
  return **reinterpret_cast<const i::Persistent<T>*>(&ref);
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


static VM* refVM(const Reference& ref) {
  return unwrap<i::Block>(ref)->getVM()->apiPtr();
}


static Value valueFromRaw(i::Type* type, i::i64 bits) {
  if (type->isUnit()) {
    return Value();
  } else if (type->isBoolean()) {
    return Value(static_cast<bool>(bits));
  } else if (type->isI8()) {
    return Value(static_cast<int8_t>(bits));
  } else if (type->isI16()) {
    return Value(static_cast<int16_t>(bits));
  } else if (type->isI32()) {
    return Value(static_cast<int32_t>(bits));
  } else if (type->isI64()) {
    return Value(static_cast<int64_t>(bits));
  } else if (type->isF32()) {
    return Value(i::f32FromBits(bits));
  } else if (type->isF64()) {
    return Value(i::f64FromBits(bits));
  } else {
    API_CHECK(type->isObject(), "internal value cannot be returned");
    auto rawObject = reinterpret_cast<i::Object*>(static_cast<intptr_t>(bits));
    return Value(wrap<Object, i::Object>(rawObject));
  }
}


class Value::Impl final {
 public:
  static uint64_t getValueBits(const Value& value) {
    return value.bits_;
  }


  static Object& getValueRef(Value& value) {
    return value.ref_;
  }

  static const Object& getValueRef(const Value& value) {
    return value.ref_;
  }

  static const uint8_t getValueTag(const Value& value) {
    return value.tag_;
  }
};


static i::i64 rawFromValue(const Value& value, const i::Handle<i::Type>& type) {
  auto tag = Value::Impl::getValueTag(value);
  if (tag == i::Type::CLASS_TYPE) {
    const Object& obj = Value::Impl::getValueRef(value);
    if (!obj) {
      API_CHECK(type->isObject() && type->isNullable(), "type error");
      return 0;
    } else {
      i::Local<i::Object> iobj(const_cast<i::Object*>(unwrapRaw<i::Object>(obj)));
      auto objType = i::Object::typeof(iobj);
      API_CHECK(i::Type::isSubtypeOf(objType, type), "type error");
      return static_cast<i::i64>(static_cast<i::u64>(reinterpret_cast<uintptr_t>(*iobj)));
    }
  } else {
    API_CHECK(tag == type->form(), "type error");
    return Value::Impl::getValueBits(value);
  }
}


static i::Local<i::Type> typeFromValue(i::VM* vm, const Value& value) {
  auto form = static_cast<i::Type::Form>(Value::Impl::getValueTag(value));
  if (i::Type::FIRST_PRIMITIVE_TYPE <= form && form <= i::Type::LAST_PRIMITIVE_TYPE) {
    return handle(i::Type::primitiveTypeFromForm(vm->roots(), form));
  } else {
    const Object& obj = Value::Impl::getValueRef(value);
    if (!obj) {
      return handle(i::Type::nullType(vm->roots()));
    } else {
      i::Local<i::Object> iobj(const_cast<i::Object*>(unwrapRaw<i::Object>(obj)));
      return i::Object::typeof(iobj);
    }
  }
}


class CallBuilder::Impl final {
 public:
  explicit Impl(i::VM* vm)
      : vm_(vm) { }

 private:
  i::VM* vm_;
  i::Persistent<i::Package> package_;
  i::Persistent<i::Class> clas_;
  i::Persistent<i::Trait> trait_;
  i::Persistent<i::Function> function_;
  i::Persistent<i::Name> name_;
  i::Persistent<i::String> sourceName_;
  vector<Value> args_;

  friend class CallBuilder;
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


Package VM::loadPackageFromStream(istream& stream,
    const vector<NativeFunctionSearch>& nativeFunctionSearchOrder) {
  checkNativeFunctionSearchOrder(nativeFunctionSearchOrder);
  i::Persistent<i::Package> package;
  try {
    package = impl_->vm.loadPackage(stream, nativeFunctionSearchOrder);
  } catch (i::Error& error) {
    throw Error(new Error::Impl(error.message()));
  }

  return wrap<Package, i::Package>(package);
}


Reference::Reference()
    : impl_(nullptr) { }


Reference::Reference(Impl* impl)
    : impl_(impl) { }


Reference::Reference(const Reference& ref)
    : impl_(nullptr) {
  *this = ref;
}


Reference::Reference(Reference&& ref)
    : impl_(ref.impl_) {
  ref.impl_ = nullptr;
}


Reference& Reference::operator = (const Reference& ref) {
  if (this != &ref) {
    if (!ref) {
      clear();
    } else {
      auto block = *reinterpret_cast<i::Block**>(ref.impl_);
      auto slotPtr = reinterpret_cast<i::Block***>(&impl_);
      auto handleStorage = i::HandleStorage::fromBlock(block);
      handleStorage->createPersistent(block, slotPtr);
    }
  }
  return *this;
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


bool Reference::operator == (const Reference& other) const {
  return unwrapRaw<i::Block>(*this) == unwrapRaw<i::Block>(other);
}


Package::Package(Impl* impl)
    : Reference(impl) { }


Function Package::entryFunction() const {
  API_CHECK_SELF(Package);
  i::Function* function = unwrapRaw<i::Package>(*this)->entryFunction();
  if (function == nullptr) {
    return Function();
  }
  return wrap<Function, i::Function>(function);
}


Global Package::findGlobal(const Name& name) const {
  API_CHECK_SELF(Package);
  API_CHECK_ARG(name);
  auto self = unwrap<i::Package>(*this);
  auto vm = self->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto index = i::Package::ensureAndGetGlobalNameIndex(self);
  auto global = index->getOrElse(*unwrap<i::Name>(name), nullptr);
  return global ? wrap<Global, i::Global>(global) : Global();
}


Global Package::findGlobal(const String& sourceName) const {
  API_CHECK_SELF(Package);
  API_CHECK_ARG(sourceName);
  auto self = unwrap<i::Package>(*this);
  auto vm = self->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto index = i::Package::ensureAndGetGlobalSourceNameIndex(self);
  auto global = index->getOrElse(*unwrap<i::String>(sourceName), nullptr);
  return global ? wrap<Global, i::Global>(global) : Global();
}


Global Package::findGlobal(const string& sourceName) const {
  API_CHECK_SELF(Package);
  String sourceNameStr(refVM(*this), sourceName);
  return findGlobal(sourceNameStr);
}


Function Package::findFunction(const Name& name, const string& signature) const {
  API_CHECK_SELF(Package);
  API_CHECK_ARG(name);
  auto self = unwrap<i::Package>(*this);
  auto vm = self->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto mangled = i::mangleName(unwrap<i::Name>(name), signature);
  auto index = i::Package::ensureAndGetFunctionNameIndex(self);
  auto function = index->getOrElse(*mangled, nullptr);
  return function ? wrap<Function, i::Function>(function) : Function();
}


Function Package::findFunction(const String& sourceName, const string& signature) const {
  API_CHECK_SELF(Package);
  API_CHECK_ARG(sourceName);
  auto self = unwrap<i::Package>(*this);
  auto vm = self->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto index = i::Package::ensureAndGetFunctionSourceNameIndex(self);
  auto mangled = i::mangleSourceName(unwrap<i::String>(sourceName), signature);
  auto function = index->getOrElse(*mangled, nullptr);
  return function ? wrap<Function, i::Function>(function) : Function();
}


Function Package::findFunction(const string& sourceName, const string& signature) const {
  API_CHECK_SELF(Package);
  String sourceNameStr(refVM(*this), sourceName);
  return findFunction(sourceNameStr, signature);
}


Class Package::findClass(const Name& name) const {
  API_CHECK_SELF(Package);
  API_CHECK_ARG(name);
  auto self = unwrap<i::Package>(*this);
  auto vm = self->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto index = i::Package::ensureAndGetClassNameIndex(self);
  auto clas = index->getOrElse(*unwrap<i::Name>(name), nullptr);
  return clas ? wrap<Class, i::Class>(clas) : Class();
}


Class Package::findClass(const String& sourceName) const {
  API_CHECK_SELF(Package);
  API_CHECK_ARG(sourceName);
  auto self = unwrap<i::Package>(*this);
  auto vm = self->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto index = i::Package::ensureAndGetClassSourceNameIndex(self);
  auto clas = index->getOrElse(*unwrap<i::String>(sourceName), nullptr);
  return clas ? wrap<Class, i::Class>(clas) : Class();
}


Class Package::findClass(const string& sourceName) const {
  API_CHECK_SELF(Package);
  String sourceNameStr(refVM(*this), sourceName);
  return findClass(sourceNameStr);
}


Trait Package::findTrait(const Name& name) const {
  API_CHECK_SELF(Package);
  API_CHECK_ARG(name);
  auto self = unwrap<i::Package>(*this);
  auto vm = self->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto index = i::Package::ensureAndGetTraitNameIndex(self);
  auto trait = index->getOrElse(*unwrap<i::Name>(name), nullptr);
  return trait ? wrap<Trait, i::Trait>(trait) : Trait();
}


Trait Package::findTrait(const String& sourceName) const {
  API_CHECK_SELF(Package);
  API_CHECK_ARG(sourceName);
  auto self = unwrap<i::Package>(*this);
  auto vm = self->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto index = i::Package::ensureAndGetTraitSourceNameIndex(self);
  auto trait = index->getOrElse(*unwrap<i::String>(sourceName), nullptr);
  return trait ? wrap<Trait, i::Trait>(trait) : Trait();
}


Trait Package::findTrait(const string& sourceName) const {
  API_CHECK_SELF(Package);
  String sourceNameStr(refVM(*this), sourceName);
  return findTrait(sourceNameStr);
}


Global::Global(Impl* impl)
    : Reference(impl) { }


bool Global::isConstant() const {
  API_CHECK_SELF(Global);
  return (unwrap<i::Global>(*this)->flags() & i::LET_FLAG) != 0;
}


Value Global::value() const {
  API_CHECK_SELF(Global);
  auto self = unwrap<i::Global>(*this);
  return valueFromRaw(self->type(), self->getRaw());
}


void Global::setValue(const Value& value) {
  API_CHECK_SELF(Global);
  API_CHECK(!isConstant(), "global cannot be set because it is constant");
  auto self = unwrap<i::Global>(*this);
  auto vm = self->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);  // for type checking
  self->setRaw(rawFromValue(value, handle(self->type())));
}


Function::Function(Impl* impl)
    : Reference(impl) { }


bool Function::isConstructor() const {
  API_CHECK_SELF(Function);
  return (unwrapRaw<i::Function>(*this)->flags() & i::CONSTRUCTOR_FLAG) != 0;
}


Class Function::clas() const {
  API_CHECK_SELF(Function);
  auto clas = unwrapRaw<i::Function>(*this)->definingClass();
  return (clas && i::isa<i::Class>(clas))
      ? wrap<Class, i::Class>(reinterpret_cast<i::Class*>(clas))
      : Class();
}


Class::Class(Impl* impl)
    : Reference(impl) { }


Function Class::findConstructor(const string& signature) const {
  API_CHECK_SELF(Class);
  auto self = unwrap<i::Class>(*this);
  i::HandleScope handleScope(self->getVM());
  i::AllowAllocationScope allowAlloc(self->getHeap(), true);
  auto index = i::Class::ensureAndGetConstructorSignatureIndex(self);
  auto sigStr = i::String::fromUtf8String(self->getHeap(), signature);
  auto ctor = index->getOrElse(*sigStr, nullptr);
  return ctor ? wrap<Function, i::Function>(ctor) : Function();
}


Function Class::findMethod(const Name& name, const string& signature) const {
  API_CHECK_SELF(Class);
  API_CHECK_ARG(name);
  auto self = unwrap<i::Class>(*this);
  i::HandleScope handleScope(self->getVM());
  i::AllowAllocationScope allowAlloc(self->getHeap(), true);
  auto mangled = i::mangleName(unwrap<i::Name>(name), signature);
  auto index = i::Class::ensureAndGetMethodNameIndex(self);
  auto method = index->getOrElse(*mangled, nullptr);
  return method ? wrap<Function, i::Function>(method) : Function();
}


Function Class::findMethod(const String& sourceName, const string& signature) const {
  API_CHECK_SELF(Class);
  API_CHECK_ARG(sourceName);
  auto self = unwrap<i::Class>(*this);
  i::HandleScope handleScope(self->getVM());
  i::AllowAllocationScope allowAlloc(self->getHeap(), true);
  auto mangled = i::mangleSourceName(unwrap<i::String>(sourceName), signature);
  auto index = i::Class::ensureAndGetMethodSourceNameIndex(self);
  auto method = index->getOrElse(*mangled, nullptr);
  return method ? wrap<Function, i::Function>(method) : Function();
}


Function Class::findMethod(const string& sourceName, const string& signature) const {
  API_CHECK_SELF(Class);
  String sourceNameStr(refVM(*this), sourceName);
  return findMethod(sourceNameStr, signature);
}


Field Class::findField(const Name& name) const {
  API_CHECK_SELF(Class);
  API_CHECK_ARG(name);
  auto self = unwrap<i::Class>(*this);
  i::HandleScope handleScope(self->getVM());
  i::AllowAllocationScope allowAlloc(self->getHeap(), true);
  auto index = i::Class::ensureAndGetFieldNameIndex(self);
  auto field = index->getOrElse(*unwrap<i::Name>(name), nullptr);
  return field ? wrap<Field, i::Field>(field) : Field();
}


Field Class::findField(const String& sourceName) const {
  API_CHECK_SELF(Class);
  API_CHECK_ARG(sourceName);
  auto self = unwrap<i::Class>(*this);
  i::HandleScope handleScope(self->getVM());
  i::AllowAllocationScope allowAlloc(self->getHeap(), true);
  auto index = i::Class::ensureAndGetFieldSourceNameIndex(self);
  auto field = index->getOrElse(*unwrap<i::String>(sourceName), nullptr);
  return field ? wrap<Field, i::Field>(field) : Field();
}


Field Class::findField(const string& sourceName) const {
  API_CHECK_SELF(Class);
  String sourceNameStr(refVM(*this), sourceName);
  return findField(sourceNameStr);
}


Trait::Trait(Impl* impl)
    : Reference(impl) { }


Function Trait::findMethod(const Name& name, const string& signature) const {
  API_CHECK_SELF(Trait);
  API_CHECK_ARG(name);
  auto self = unwrap<i::Trait>(*this);

  i::HandleScope handleScope(self->getVM());
  i::AllowAllocationScope allowAlloc(self->getHeap(), true);
  auto mangled = i::mangleName(unwrap<i::Name>(name), signature);
  auto index = i::Trait::ensureAndGetMethodNameIndex(self);
  auto method = index->getOrElse(*mangled, nullptr);
  return method ? wrap<Function, i::Function>(method) : Function();
}


Function Trait::findMethod(const String& sourceName, const string& signature) const {
  API_CHECK_SELF(Trait);
  API_CHECK_ARG(sourceName);
  auto self = unwrap<i::Trait>(*this);
  i::HandleScope handleScope(self->getVM());
  i::AllowAllocationScope allowAlloc(self->getHeap(), true);
  auto mangled = i::mangleSourceName(unwrap<i::String>(sourceName), signature);
  auto index = i::Trait::ensureAndGetMethodSourceNameIndex(self);
  auto method = index->getOrElse(*mangled, nullptr);
  return method ? wrap<Function, i::Function>(method) : Function();
}


Function Trait::findMethod(const string& sourceName, const string& signature) const {
  API_CHECK_SELF(Trait);
  String sourceNameStr(refVM(*this), sourceName);
  return findMethod(sourceNameStr, signature);
}


Field::Field(Impl* impl)
    : Reference(impl) { }


bool Field::isConstant() const {
  API_CHECK_SELF(Field);
  return (unwrapRaw<i::Field>(*this)->flags() & i::LET_FLAG) != 0;
}


CallBuilder::~CallBuilder() {
  // DO NOT REMOVE.
  // This cannot be defined as `default` in the header since the client doesn't know whether
  // `Impl` has a non-trivial destructor.
}


CallBuilder::CallBuilder(const Function& function) {
  API_CHECK_ARG(function);
  auto ifunction = unwrap<i::Function>(function);
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(ifunction->getVM()));
  impl_->function_ = ifunction;
}


CallBuilder::CallBuilder(const Package& package, const Name& name) {
  API_CHECK_ARG(package);
  API_CHECK_ARG(name);
  auto ipackage = unwrap<i::Package>(package);
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(ipackage->getVM()));
  impl_->package_ = ipackage;
  impl_->name_ = unwrap<i::Name>(name);
}


CallBuilder::CallBuilder(const Package& package, const String& sourceName) {
  API_CHECK_ARG(package);
  API_CHECK_ARG(sourceName);
  auto ipackage = unwrap<i::Package>(package);
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(ipackage->getVM()));
  impl_->package_ = ipackage;
  impl_->sourceName_ = unwrap<i::String>(sourceName);
}


CallBuilder::CallBuilder(const Package& package, const string& sourceName)
    : CallBuilder(package, String(refVM(package), sourceName)) { }


CallBuilder::CallBuilder(const Class& clas, const Function& constructor) {
  API_CHECK_ARG(clas);
  API_CHECK_ARG(constructor);
  API_CHECK(constructor.isConstructor(), "`constructor` is not a constructor");
  auto iclass = unwrap<i::Class>(clas);
  auto vm = iclass->getVM();
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(vm));
  impl_->clas_ = iclass;
  impl_->function_ = unwrap<i::Function>(constructor);
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAllocation(vm->heap(), true);
  auto meta = i::Class::ensureInstanceMeta(iclass);
  auto receiver = i::Object::create(vm->heap(), meta);
  arg(wrap<Object, i::Object>(receiver));
}


CallBuilder::CallBuilder(const Class& clas) {
  API_CHECK_ARG(clas);
  auto iclass = unwrap<i::Class>(clas);
  auto vm = iclass->getVM();
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(vm));
  impl_->clas_ = iclass;
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAllocation(vm->heap(), true);
  auto meta = i::Class::ensureInstanceMeta(iclass);
  auto receiver = i::Object::create(vm->heap(), meta);
  arg(wrap<Object, i::Object>(receiver));
}


CallBuilder::CallBuilder(const Class& clas, const Name& name) {
  API_CHECK_ARG(clas);
  API_CHECK_ARG(name);
  auto iclass = unwrap<i::Class>(clas);
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(iclass->getVM()));
  impl_->clas_ = iclass;
  impl_->name_ = unwrap<i::Name>(name);
}


CallBuilder::CallBuilder(const Class& clas, const String& sourceName) {
  API_CHECK_ARG(clas);
  API_CHECK_ARG(sourceName);
  auto iclass = unwrap<i::Class>(clas);
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(iclass->getVM()));
  impl_->clas_ = iclass;
  impl_->sourceName_ = unwrap<i::String>(sourceName);
}


CallBuilder::CallBuilder(const Class& clas, const string& sourceName)
    : CallBuilder(clas, String(refVM(clas), sourceName)) { }


CallBuilder::CallBuilder(const Trait& trait, const Name& name) {
  API_CHECK_ARG(trait);
  API_CHECK_ARG(name);
  auto itrait = unwrap<i::Trait>(trait);
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(itrait->getVM()));
  impl_->trait_ = itrait;
  impl_->name_ = unwrap<i::Name>(name);
}


CallBuilder::CallBuilder(const Trait& trait, const String& sourceName) {
  API_CHECK_ARG(trait);
  API_CHECK_ARG(sourceName);
  auto itrait = unwrap<i::Trait>(trait);
  impl_ = unique_ptr<CallBuilder::Impl>(new CallBuilder::Impl(itrait->getVM()));
  impl_->trait_ = itrait;
  impl_->sourceName_ = unwrap<i::String>(sourceName);
}


CallBuilder::CallBuilder(const Trait& trait, const string& sourceName)
    : CallBuilder(trait, String(refVM(trait), sourceName)) { }


CallBuilder& CallBuilder::arg(Value&& value) {
  impl_->args_.push_back(value);
  return *this;
}


CallBuilder& CallBuilder::args() {
  return *this;
}


Value CallBuilder::call() {
  auto isConstructor = impl_->clas_ && !impl_->name_ && !impl_->sourceName_;

  i::VM* vm = impl_->vm_;
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  if (impl_->function_) {
    // If we know what function we're going to call, check that the arguments match the
    // function's parameter types.
    API_CHECK(impl_->function_->typeParameters()->length() == 0,
        "function has type parameters and cannot be called");
    API_CHECK(impl_->args_.size() == impl_->function_->parameterTypes()->length(),
        "wrong number of arguments");
    for (i::length_t i = 0; i < impl_->args_.size(); i++) {
      auto argType = typeFromValue(vm, impl_->args_[i]);
      auto paramType = handle(impl_->function_->parameterTypes()->get(i));
      API_CHECK(i::Type::isSubtypeOf(argType, paramType), "type error");
    }
  } else {
    // We don't know what function we're going to call. We need to look it up using the
    // types of the arugments.
    vector<i::Local<i::Type>> argTypes;
    argTypes.reserve(impl_->args_.size());
    for (i::length_t i = 0; i < impl_->args_.size(); i++) {
      argTypes.emplace_back(typeFromValue(vm, impl_->args_[i]));
    }
    auto package = impl_->package_
        ? impl_->package_
        : i::Persistent<i::Package>(impl_->clas_->package());
    auto signature = buildSignature(argTypes, package);
    if (impl_->name_) {
      // Look up function or method by full name.
      auto mangledName = mangleName(impl_->name_, signature);
      i::Local<i::BlockHashMap<i::Name, i::Function>> index;
      if (impl_->package_) {
        index = i::Package::ensureAndGetFunctionNameIndex(impl_->package_);
      } else if (impl_->clas_) {
        index = i::Class::ensureAndGetMethodNameIndex(impl_->clas_);
      } else {
        index = i::Trait::ensureAndGetMethodNameIndex(impl_->trait_);
      }
      impl_->function_ = i::Local<i::Function>(vm, index->getOrElse(*mangledName, nullptr));
    } else if (impl_->sourceName_) {
      // Look up function or method by source name.
      auto mangledSourceName = mangleSourceName(impl_->sourceName_, signature);
      i::Local<i::BlockHashMap<i::String, i::Function>> index;
      if (impl_->package_) {
        index = i::Package::ensureAndGetFunctionSourceNameIndex(impl_->package_);
      } else if (impl_->clas_) {
        index = i::Class::ensureAndGetMethodSourceNameIndex(impl_->clas_);
      } else {
        index = i::Trait::ensureAndGetMethodSourceNameIndex(impl_->trait_);
      }
      impl_->function_ =
          i::Local<i::Function>(vm, index->getOrElse(*mangledSourceName, nullptr));
    } else {
      // Look up constructor.
      auto sigStr = i::String::fromUtf8String(vm->heap(), signature);
      auto index = i::Class::ensureAndGetConstructorSignatureIndex(impl_->clas_);
      impl_->function_ = i::Local<i::Function>(vm, index->getOrElse(*sigStr, nullptr));
    }
    API_CHECK(impl_->function_, "could not find function that matches argument types");
  }

  // Push arguments onto the stack.
  i::AllowAllocationScope denyAlloc(vm->heap(), false);
  const i::Persistent<i::Stack>& stack = vm->stack();
  for (auto& arg : impl_->args_) {
    if (Value::Impl::getValueTag(arg) == i::Type::CLASS_TYPE) {
      stack->push(unwrapRaw<i::Object>(Value::Impl::getValueRef(arg)));
    } else {
      stack->push(Value::Impl::getValueBits(arg));
    }
  }

  // Perform the call.
  i::Interpreter interpreter(vm, vm->stack(), vm->threadBindle());
  i::i64 result;
  try {
    result = interpreter.call(impl_->function_);
  } catch (i::Exception& exception) {
    throw Exception(wrap<Object, i::Object>(exception.get()));
  }

  // Return the result as a value. If this was a constructor call, return the receiver instead
  // of the constructor's return value.
  if (isConstructor) {
    return impl_->args_[0];
  } else {
    return valueFromRaw(impl_->function_->returnType(), result);
  }
}


Name::Name(Impl* impl)
    : Reference(impl) { }


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


Name Name::fromStringForDefn(VM* vm, const string& str) {
  return fromStringForDefn(String(vm, str));
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


Name Name::fromStringForPackage(VM* vm, const string& str) {
  return fromStringForPackage(String(vm, str));
}


String::String(Impl* impl)
    : Object(impl) { }


String::String(VM* vm, const string& str) {
  auto heap = vm->impl_->vm.heap();
  i::AllowAllocationScope allowAlloc(heap, true);
  i::HandleScope handleScope(&vm->impl_->vm);
  i::Local<i::String> istr = i::String::fromUtf8String(heap, str);
  *this = wrap<String, i::String>(istr);
}


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


Object::Object(Impl* impl)
    : Reference(impl) { }


bool Object::isInstanceOf(const Class& clas) const {
  API_CHECK_SELF(Object);
  API_CHECK_ARG(clas);
  auto selfClass = unwrapRaw<i::Object>(*this)->clas();
  auto otherClass = unwrapRaw<i::Class>(clas);
  return selfClass->isSubclassOf(otherClass);
}


bool Object::isInstanceOf(const Trait& trait) const {
  API_CHECK_SELF(Object);
  API_CHECK_ARG(trait);
  auto selfClass = unwrapRaw<i::Object>(*this)->clas();
  auto supertypes = selfClass->supertypes();
  auto otherTrait = unwrapRaw<i::Trait>(trait);
  for (auto type : *supertypes) {
    if (type->isTrait() && type->asTrait() == otherTrait)
      return true;
  }
  return false;
}


Class Object::clas() const {
  API_CHECK_SELF(Object);
  return wrap<Class, i::Class>(unwrapRaw<i::Object>(*this)->clas());
}


String Object::toString() {
  return callMethod("to-string").asString();
}


Value Object::getField(const Field& field) const {
  API_CHECK_SELF(Object);
  API_CHECK_SELF(Field);
  auto iobj = unwrapRaw<i::Object>(*this);
  auto ifield = unwrapRaw<i::Field>(field);
  auto bits = iobj->getRawField(ifield);
  return valueFromRaw(ifield->type(), bits);
}


Value Object::getField(const string& fieldSourceName) const {
  API_CHECK_SELF(Object);
  auto field = clas().findField(fieldSourceName);
  API_CHECK(field, "field could not be found");
  return getField(field);
}


void Object::setField(const Field& field, const Value& value) {
  API_CHECK_SELF(Object);
  API_CHECK_ARG(field);
  auto iobj = unwrap<i::Object>(*this);
  auto ifield = unwrap<i::Field>(field);
  auto vm = iobj->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto bits = rawFromValue(value, handle(ifield->type()));
  iobj->setRawField(*ifield, bits);
}


bool Object::hasElements() const {
  API_CHECK_SELF(Object);
  return unwrapRaw<i::Object>(*this)->meta()->hasElements();
}


bool Object::elementsAreConstant() const {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  return (unwrapRaw<i::Object>(*this)->meta()->clas()->flags() & i::ARRAY_FINAL_FLAG) != 0;
}


uint32_t Object::length() const {
  API_CHECK_SELF(Object);
  return unwrapRaw<i::Object>(*this)->elementsLength();
}


Value Object::getElement(uint32_t index) const {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(index < length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  auto clas = iobj->meta()->clas();
  auto type = clas->elementType();
  return valueFromRaw(type, iobj->getRawElement(index));
}


void Object::setElement(uint32_t index, const Value& value) {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(!elementsAreConstant(), "array elements are not mutable");
  API_CHECK(index < length(), "array element index is out of bounds");
  auto iobj = unwrap<i::Object>(*this);
  auto vm = iobj->getVM();
  i::HandleScope handleScope(vm);
  i::AllowAllocationScope allowAlloc(vm->heap(), true);
  auto type = handle(iobj->meta()->clas()->elementType());
  auto bits = rawFromValue(value, type);
  iobj->setRawElement(index, bits);
}


void Object::copyElementsFrom(uint32_t index, const bool* from, uint32_t count) {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(!elementsAreConstant(), "array elements are not mutable");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isBoolean(), "type error");
  auto to = reinterpret_cast<bool*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsFrom(uint32_t index, const int8_t* from, uint32_t count) {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(!elementsAreConstant(), "array elements are not mutable");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isI8(), "type error");
  auto to = reinterpret_cast<int8_t*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsFrom(uint32_t index, const int16_t* from, uint32_t count) {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(!elementsAreConstant(), "array elements are not mutable");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isI16(), "type error");
  auto to = reinterpret_cast<int16_t*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsFrom(uint32_t index, const int32_t* from, uint32_t count) {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(!elementsAreConstant(), "array elements are not mutable");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isI32(), "type error");
  auto to = reinterpret_cast<int32_t*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsFrom(uint32_t index, const int64_t* from, uint32_t count) {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(!elementsAreConstant(), "array elements are not mutable");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isI64(), "type error");
  auto to = reinterpret_cast<int64_t*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsFrom(uint32_t index, const float* from, uint32_t count) {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(!elementsAreConstant(), "array elements are not mutable");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isF32(), "type error");
  auto to = reinterpret_cast<float*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsFrom(uint32_t index, const double* from, uint32_t count) {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(!elementsAreConstant(), "array elements are not mutable");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isF64(), "type error");
  auto to = reinterpret_cast<double*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsTo(uint32_t index, bool* to, uint32_t count) const {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isBoolean(), "type error");
  auto from = reinterpret_cast<bool*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsTo(uint32_t index, int8_t* to, uint32_t count) const {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isI8(), "type error");
  auto from = reinterpret_cast<int8_t*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsTo(uint32_t index, int16_t* to, uint32_t count) const {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isI16(), "type error");
  auto from = reinterpret_cast<int16_t*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsTo(uint32_t index, int32_t* to, uint32_t count) const {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isI32(), "type error");
  auto from = reinterpret_cast<int32_t*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsTo(uint32_t index, int64_t* to, uint32_t count) const {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isI64(), "type error");
  auto from = reinterpret_cast<int64_t*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsTo(uint32_t index, float* to, uint32_t count) const {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isF32(), "type error");
  auto from = reinterpret_cast<float*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
}


void Object::copyElementsTo(uint32_t index, double* to, uint32_t count) const {
  API_CHECK_SELF(Object);
  API_CHECK(hasElements(), "object does not have array elements");
  API_CHECK(index + count <= length(), "array element index is out of bounds");
  auto iobj = unwrapRaw<i::Object>(*this);
  API_CHECK(iobj->meta()->clas()->elementType()->isF64(), "type error");
  auto from = reinterpret_cast<double*>(iobj->elementsBase()) + index;
  copy_n(from, count, to);
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


Exception::Exception(Object&& exception)
    : exception_(move(exception)) { }


const char* Exception::message() {
  return exception_.toString().toStdString().c_str();
}


Object& Exception::get() {
  return exception_;
}


const Object& Exception::get() const {
  return exception_;
}


Value::Value()
    : bits_(0),
      tag_(i::Type::UNIT_TYPE) { }


Value::Value(bool b)
    : bits_(static_cast<uint64_t>(b)),
      tag_(i::Type::BOOLEAN_TYPE) { }


Value::Value(int8_t n)
    : bits_(static_cast<uint64_t>(n)),
      tag_(i::Type::I8_TYPE) { }


Value::Value(int16_t n)
    : bits_(static_cast<uint64_t>(n)),
      tag_(i::Type::I16_TYPE) { }


Value::Value(int32_t n)
    : bits_(static_cast<uint64_t>(n)),
      tag_(i::Type::I32_TYPE) { }


Value::Value(int64_t n)
    : bits_(static_cast<uint64_t>(n)),
      tag_(i::Type::I64_TYPE) { }


Value::Value(float n)
    : bits_(i::f32ToBits(n)),
      tag_(i::Type::F32_TYPE) { }


Value::Value(double n)
    : bits_(i::f64ToBits(n)),
      tag_(i::Type::F64_TYPE) { }


Value::Value(const Object& o)
    : bits_(0),
      ref_(o),
      tag_(i::Type::CLASS_TYPE) { }


Value::Value(Object&& o)
    : bits_(0),
      ref_(move(o)),
      tag_(i::Type::CLASS_TYPE) { }


void Value::asUnit() const {
  API_CHECK(tag_ == i::Type::UNIT_TYPE, "value is not unit");
}


bool Value::asBoolean() const {
  API_CHECK(tag_ == i::Type::BOOLEAN_TYPE, "value is not Boolean");
  return static_cast<bool>(bits_);
}


int8_t Value::asI8() const {
  API_CHECK(tag_ == i::Type::I8_TYPE, "value is not i8");
  return static_cast<int8_t>(bits_);
}


int16_t Value::asI16() const {
  API_CHECK(tag_ == i::Type::I16_TYPE, "value is not i16");
  return static_cast<int16_t>(bits_);
}


int32_t Value::asI32() const {
  API_CHECK(tag_ == i::Type::I32_TYPE, "value is not i32");
  return static_cast<int32_t>(bits_);
}


int64_t Value::asI64() const {
  API_CHECK(tag_ == i::Type::I64_TYPE, "value is not i64");
  return static_cast<int64_t>(bits_);
}


float Value::asF32() const {
  API_CHECK(tag_ == i::Type::F32_TYPE, "value is not f32");
  return i::f32FromBits(static_cast<i::u32>(bits_));
}


double Value::asF64() const {
  API_CHECK(tag_ == i::Type::F64_TYPE, "value is not f64");
  return i::f64FromBits(bits_);
}


const String& Value::asString() const {
  API_CHECK(tag_ == i::Type::CLASS_TYPE &&
      (!ref_ || i::isa<i::String>(unwrapRaw<i::Object>(ref_))),
      "value is not String");
  return static_cast<const String&>(ref_);
}


const Object& Value::asObject() const {
  API_CHECK(tag_ == i::Type::CLASS_TYPE, "value is not Object");
  return ref_;
}


String&& Value::moveString() {
  API_CHECK(tag_ == i::Type::CLASS_TYPE &&
      (!ref_ || i::isa<i::String>(unwrapRaw<i::Object>(ref_))),
      "value is not String");
  return static_cast<String&&>(move(ref_));
}


Object&& Value::moveObject() {
  API_CHECK(tag_ == i::Type::CLASS_TYPE, "value is not Object");
  return move(ref_);
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
  try {
    auto result = i::callNativeFunctionRaw(
        vm->apiPtr(), nativeFunction, argCount,
        rawArgs.get(), argsAreInt.get(), resultType);
    return result;
  } catch (Exception& e) {
    throw i::Exception(unwrapRaw<i::Object>(e.get()));
  } catch (Object& e) {
    if (e) {
      throw i::Exception(unwrapRaw<i::Object>(e));
    } else {
      throw e;
    }
  }
}

}

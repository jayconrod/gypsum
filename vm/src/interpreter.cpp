// Copyright 2014-2015 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "interpreter.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <vector>
#include "array.h"
#include "block.h"
#include "builtins.h"
#include "bytecode.h"
#include "flags.h"
#include "function.h"
#include "global.h"
#include "handle.h"
#include "name.h"
#include "object.h"
#include "package.h"
#include "roots.h"
#include "stack.h"
#include "string.h"
#include "type.h"
#include "type-parameter.h"

using namespace std;

namespace codeswitch {
namespace internal {

Interpreter::Interpreter(VM* vm, const Handle<Stack>& stack)
    : vm_(vm),
      stack_(stack),
      pcOffset_(kDonePcOffset),
      isPreparedForGC_(false) { }


// Stack methods need to go before instantiation so they can be specialized.
template <typename T>
void Interpreter::push(T value) {
  ASSERT(!isPreparedForGC());
  typename std::make_unsigned<T>::type unsignedValue = value;
  stack_->push(static_cast<u64>(unsignedValue));
}


template <>
void Interpreter::push<bool>(bool value) {
  ASSERT(!isPreparedForGC());
  stack_->push(static_cast<u64>(value));
}


template <>
void Interpreter::push<f32>(f32 value) {
  ASSERT(!isPreparedForGC());
  auto bits = static_cast<u64>(f32ToBits(value));
  stack_->push(bits);
}


template <>
void Interpreter::push<f64>(f64 value) {
  ASSERT(!isPreparedForGC());
  stack_->push(value);
}


template <>
void Interpreter::push<void*>(void* value) {
  static_assert(sizeof(void*) == sizeof(i64), "Specialization requires 64-bit");
  ASSERT(!isPreparedForGC());
  stack_->push(value);
}


template <>
void Interpreter::push<Block*>(Block* value) {
  static_assert(sizeof(Block*) == sizeof(i64), "Specialization requires 64-bit");
  ASSERT(!isPreparedForGC());
  stack_->push(value);
}


template <typename T>
T Interpreter::pop() {
  ASSERT(!isPreparedForGC());
  return static_cast<T>(stack_->pop<u64>());
}


template <>
f32 Interpreter::pop<f32>() {
  ASSERT(!isPreparedForGC());
  auto bits = static_cast<u32>(stack_->pop<u64>());
  return f32FromBits(bits);
}


template <>
f64 Interpreter::pop<f64>() {
  ASSERT(!isPreparedForGC());
  return stack_->pop<f64>();
}


template <>
Block* Interpreter::pop<Block*>() {
  static_assert(sizeof(Block*) == sizeof(i64), "Specialization requires 64-bit");
  ASSERT(!isPreparedForGC());
  return stack_->pop<Block*>();
}


class Interpreter::GCSafeScope {
 public:
  explicit GCSafeScope(Interpreter* interpreter)
      : interpreter_(interpreter),
        pcOffset_(interpreter_->pcOffset_) {
    interpreter_->prepareForGC();
  }

  ~GCSafeScope() {
    ASSERT(pcOffset_ == interpreter_->pcOffset_);
    interpreter_->unprepareForGC();
  }

 private:
  Interpreter* interpreter_;
  length_t pcOffset_;
};


#define CHECK_NON_NULL(block)                                         \
  if ((block) == nullptr) {                                           \
    throwBuiltinException(BUILTIN_NULL_POINTER_EXCEPTION_CLASS_ID);   \
    break;                                                            \
  }                                                                   \


#define CHECK_INITIALIZED(block)                                      \
  if ((block) == nullptr) {                                           \
    throwBuiltinException(BUILTIN_UNINITIALIZED_EXCEPTION_CLASS_ID);  \
    break;                                                            \
  }                                                                   \


i64 Interpreter::call(const Handle<Function>& callee) {
  // TODO: figure out a reasonable way to manage locals here.
  SealHandleScope disallowHandles(vm_);
  AllowAllocationScope disallowAllocation(vm_->heap(), false);

  // Set up initial stack frame.
  ASSERT(pcOffset_ == kDonePcOffset);
  enter(callee);

  // Interpreter loop.
  i64 result = 0xdeadc0dedeadcafell;
  while (pcOffset_ != kDonePcOffset) {
    u8 bc = function_->instructionsStart()[pcOffset_++];
    auto opc = static_cast<Opcode>(bc);

    switch (opc) {
      case NOP:
        break;

      case RET: {
        auto value = pop<i64>();
        leave();
        if (pcOffset_ == kDonePcOffset) {
          result = value;
        } else {
          push(value);
        }
        break;
      }

      case BRANCH: {
        i64 blockIndex = readVbn();
        pcOffset_ = function_->blockOffset(toLength(blockIndex));
        break;
      }

      case BRANCHIF: {
        auto trueBlockIndex = toLength(readVbn());
        auto falseBlockIndex = toLength(readVbn());
        auto condition = pop<bool>();
        pcOffset_ = function_->blockOffset(condition ? trueBlockIndex : falseBlockIndex);
        break;
      }

      case PUSHTRY: {
        auto tryBlockIndex = toLength(readVbn());
        auto catchBlockIndex = toLength(readVbn());
        pcOffset_ = function_->blockOffset(tryBlockIndex);
        Handler handler = { stack_->framePointerOffset(),
                            stack_->stackPointerOffset(),
                            function_->blockOffset(catchBlockIndex) };
        handlers_.push_back(handler);
        break;
      }

      case POPTRY: {
        auto doneBlockIndex = toLength(readVbn());
        pcOffset_ = function_->blockOffset(doneBlockIndex);
        handlers_.pop_back();
        break;
      }

      case THROW: {
        auto exception = pop<Block*>();
        doThrow(exception);
        break;
      }

      case DROP:
        pop<i64>();
        break;

      case DUP: {
        i64 value = mem<i64>(stack_->sp());
        push(value);
        break;
      }

      case DUPI: {
        auto index = readVbn();
        auto value = mem<i64>(stack_->sp(), 0, index);
        push(value);
        break;
      }

      case SWAP: {
        i64& top = mem<i64>(stack_->sp());
        i64& other = mem<i64>(stack_->sp(), 0, 1);
        swap(top, other);
        break;
      }

      case SWAP2: {
        i64& top = mem<i64>(stack_->sp());
        i64& other = mem<i64>(stack_->sp(), 0, 2);
        swap(top, other);
        break;
      }

      case UNIT:
      case FALSE:
      case NUL:
        push(0);
        break;

      case TRUE:
        push(1);
        break;

      case UNINITIALIZED:
        push(kUninitialized);
        break;

      case I8: {
        auto value = static_cast<i8>(readVbn());
        push(value);
        break;
      }

      case I16: {
        auto value = static_cast<i16>(readVbn());
        push(value);
        break;
      }

      case I32: {
        auto value = static_cast<i32>(readVbn());
        push(value);
        break;
      }

      case I64: {
        auto value = readVbn();
        push(value);
        break;
      }

      case F32: {
        auto value = mem<u32>(function_->instructionsStart(), pcOffset_);
        pcOffset_ += 4;
        push(value);
        break;
      }

      case F64: {
        auto value = mem<u64>(function_->instructionsStart(), pcOffset_);
        pcOffset_ += 8;
        push(value);
        break;
      }

      case STRING: {
        auto index = readVbn();
        String* string = block_cast<String>(function_->package()->getString(index));
        push<Block*>(string);
        break;
      }

      case LDLOCAL: {
        auto index = readVbn();
        auto addr = localAddressFromIndex(index);
        auto value = mem<i64>(addr);
        push(value);
        break;
      }

      case STLOCAL: {
        auto index = readVbn();
        auto addr = localAddressFromIndex(index);
        auto value = pop<i64>();
        mem<i64>(addr) = value;
        break;
      }

      case LDG: {
        auto index = toLength(readVbn());
        auto global = function_->package()->getGlobal(index);
        auto value = global->getRaw();
        push(value);
        break;
      }

      case LDGF: {
        auto depIndex = toLength(readVbn());
        auto externIndex = toLength(readVbn());
        auto value = function_->package()->dependencies()->get(depIndex)
            ->linkedGlobals()->get(externIndex)->getRaw();
        push(value);
        break;
      }

      case STG: {
        auto index = toLength(readVbn());
        auto value = pop<i64>();
        auto global = function_->package()->getGlobal(index);
        global->setRaw(value);
        break;
      }

      case STGF: {
        auto depIndex = toLength(readVbn());
        auto externIndex = toLength(readVbn());
        auto value = pop<i64>();
        auto global = function_->package()->dependencies()->get(depIndex)
            ->linkedGlobals()->get(externIndex);
        global->setRaw(value);
        break;
      }

      case LD8: loadObject<i8>(); break;
      case LD16: loadObject<i16>(); break;
      case LD32: loadObject<i32>(); break;
      case LD64: loadObject<i64>(); break;
      case LDP: loadObject<i64>(); break;

      case LDPC: {
        auto index = readVbn();
        auto block = pop<Block*>();
        CHECK_NON_NULL(block);
        auto offset = block->meta()->clas()->findFieldOffset(index);
        auto value = mem<Block*>(block, offset);
        CHECK_INITIALIZED(value);
        push(value);
        break;
      }

      case ST8: storeObject<i8>(); break;
      case ST16: storeObject<i16>(); break;
      case ST32: storeObject<i32>(); break;
      case ST64: storeObject<i64>(); break;

      case STP: {
        auto index = readVbn();
        auto block = pop<Block*>();
        auto value = pop<Block*>();
        CHECK_NON_NULL(block);
        auto offset = block->meta()->clas()->findFieldOffset(index);
        auto addr = &mem<Block*>(block, offset);
        *addr = value;
        vm_->heap()->recordWrite(addr, value);
        break;
      }

      case ALLOCOBJ: {
        auto classId = readVbn();
        Object* obj = nullptr;
        {
          GCSafeScope gcSafe(this);
          HandleScope handleScope(vm_);
          auto meta = getMetaForClassId(classId);
          obj = *Object::create(vm_->heap(), meta);
        }
        push<Block*>(obj);
        break;
      }

      case ALLOCOBJF: {
        auto depIndex = toLength(readVbn());
        auto externIndex = toLength(readVbn());
        Object* obj = nullptr;
        {
          GCSafeScope gcSafe(this);
          HandleScope handleScope(vm_);
          auto clas = handle(function_->package()->dependencies()->get(depIndex)
              ->linkedClasses()->get(externIndex));
          auto meta = Class::ensureAndGetInstanceMeta(clas);
          obj = *Object::create(vm_->heap(), meta);
        }
        push<Block*>(obj);
        break;
      }

      case ALLOCARRI: {
        auto classId = readVbn();
        auto length = readVbn();
        Object* obj = nullptr;
        {
          GCSafeScope gcSafe(this);
          HandleScope handleScope(vm_);
          auto meta = getMetaForClassId(classId);
          obj = *Object::create(vm_->heap(), meta, length);
        }
        push<Block*>(obj);
        break;
      }

      case ALLOCARRIF: {
        auto depIndex = toLength(readVbn());
        auto externIndex = toLength(readVbn());
        auto length = toLength(readVbn());
        Object* obj = nullptr;
        {
          GCSafeScope gcSafe(this);
          HandleScope handleScope(vm_);
          auto clas = handle(function_->package()->dependencies()->get(depIndex)
              ->linkedClasses()->get(externIndex));
          auto meta = Class::ensureAndGetInstanceMeta(clas);
          obj = *Object::create(vm_->heap(), meta, length);
        }
        push<Block*>(obj);
        break;
      }

      case PKG: {
        auto depIndex = readVbn();
        auto package = function_->package()->dependencies()->get(depIndex)->package();
        push<Block*>(package);
        break;
      }

      case CLS: {
        auto classId = readVbn();
        Class* clas = isBuiltinId(classId)
            ? vm_->roots()->getBuiltinClass(classId)
            : function_->package()->getClass(classId);
        push<Block*>(clas);
        break;
      }

      case CLSF: {
        auto depIndex = toLength(readVbn());
        auto externIndex = toLength(readVbn());
        auto clas = function_->package()->dependencies()->get(depIndex)
            ->linkedClasses()->get(externIndex);
        push<Block*>(clas);
        break;
      }

      case TYCS:
      case TYVS:
        readVbn();
        break;

      case TYCSF:
        readVbn();
        readVbn();
        break;

      case TYCD: {
        auto classId = readVbn();
        Type* type = nullptr;
        {
          GCSafeScope gcSafe(this);
          HandleScope handleScope(vm_);
          auto clas = handle(isBuiltinId(classId)
              ? vm_->roots()->getBuiltinClass(classId)
              : function_->package()->getClass(classId));
          vector<Local<Type>> typeArgs(
              clas->typeParameterCount(), handle(vm_->roots()->erasedType()));
          type = *Type::create(vm_->heap(), clas, typeArgs);
        }
        push<Block*>(type);
        break;
      }

      case TYCDF: {
        auto depIndex = readVbn();
        auto externIndex = readVbn();
        Type* type = nullptr;
        {
          GCSafeScope gcSafe(this);
          HandleScope handleScope(vm_);
          auto clas = handle(function_->package()->dependencies()->get(depIndex)
              ->linkedClasses()->get(externIndex));
          vector<Local<Type>> typeArgs(
              clas->typeParameterCount(), handle(vm_->roots()->erasedType()));
          type = *Type::create(vm_->heap(), clas, typeArgs);
        }
        push<Block*>(type);
        break;
      }

      case TYFLAGS:
        break;

      case TYFLAGD: {
        auto flags = static_cast<Type::Flags>(readVbn());
        {
          GCSafeScope gcSafe(this);
          HandleScope handleScope(vm_);
          auto type = handle(mem<Type*>(stack_->sp()));
          type = Type::createWithFlags(vm_->heap(), type, flags);
          mem<Type*>(stack_->sp()) = *type;
        }
        break;
      }

      case CAST:
        break;

      case CALLG: {
        auto functionId = readVbn();
        ASSERT(function_->hasPointerMapAtPcOffset(pcOffset_));
        if (isBuiltinId(functionId)) {
          handleBuiltin(static_cast<BuiltinId>(functionId));
        } else {
          auto functionIndex = toLength(functionId);
          Persistent<Function> callee(function_->package()->getFunction(functionIndex));
          enter(callee);
        }
        break;
      }

      case CALLGF: {
        auto packageIndex = toLength(readVbn());
        auto externIndex = toLength(readVbn());
        Persistent<Function> callee(function_->package()->dependencies()->get(packageIndex)
            ->linkedFunctions()->get(externIndex));
        enter(callee);
        break;
      }

      case CALLV: {
        auto argCount = readVbn();
        auto methodIndex = toLength(readVbn());
        ASSERT(function_->hasPointerMapAtPcOffset(pcOffset_));
        auto receiver = mem<Object*>(stack_->sp(), 0, argCount - 1);
        CHECK_NON_NULL(receiver);
        Persistent<Function> callee(block_cast<Function>(
            receiver->meta()->getData(methodIndex)));
        if (callee->hasBuiltinId()) {
          handleBuiltin(callee->builtinId());
        } else {
          enter(callee);
        }
        break;
      }

      case EQP: eq<Block*>(); break;
      case NEP: ne<Block*>(); break;

      #define INTERPRET_INT_OP(NAME, name)                            \
      case NAME##I8: name<i8>(); break;                               \
      case NAME##I16: name<i16>(); break;                             \
      case NAME##I32: name<i32>(); break;                             \
      case NAME##I64: name<i64>(); break;                             \

      INTERPRET_INT_OP(ADD, add)
      INTERPRET_INT_OP(SUB, sub)
      INTERPRET_INT_OP(MUL, mul)
      INTERPRET_INT_OP(DIV, div)
      INTERPRET_INT_OP(MOD, mod)
      INTERPRET_INT_OP(LSL, lsl)
      INTERPRET_INT_OP(LSR, lsr)
      INTERPRET_INT_OP(ASR, asr)
      INTERPRET_INT_OP(AND, and_)
      INTERPRET_INT_OP(OR, or_)
      INTERPRET_INT_OP(XOR, xor_)
      INTERPRET_INT_OP(EQ, eq)
      INTERPRET_INT_OP(NE, ne)
      INTERPRET_INT_OP(LT, lt)
      INTERPRET_INT_OP(LE, le)
      INTERPRET_INT_OP(GT, gt)
      INTERPRET_INT_OP(GE, ge)
      INTERPRET_INT_OP(NEG, neg)
      INTERPRET_INT_OP(INV, inv)

      #undef INTERPRET_INT_OP

      #define INTERPRET_FLOAT_OP(NAME, name)                          \
      case NAME##F32: name<f32>(); break;                             \
      case NAME##F64: name<f64>(); break;                             \

      INTERPRET_FLOAT_OP(ADD, add)
      INTERPRET_FLOAT_OP(SUB, sub)
      INTERPRET_FLOAT_OP(MUL, mul)
      INTERPRET_FLOAT_OP(DIV, div)
      INTERPRET_FLOAT_OP(EQ, eq)
      INTERPRET_FLOAT_OP(NE, ne)
      INTERPRET_FLOAT_OP(LT, lt)
      INTERPRET_FLOAT_OP(LE, le)
      INTERPRET_FLOAT_OP(GT, gt)
      INTERPRET_FLOAT_OP(GE, ge)
      INTERPRET_FLOAT_OP(NEG, neg)

      #undef INTERPRET_FLOAT_OP

      case NOTB: {
        auto value = pop<bool>();
        push(!value);
        break;
      }

      case TRUNCI8: trunc<i8>(); break;
      case TRUNCI16: trunc<i16>(); break;
      case TRUNCI32: trunc<i32>(); break;

      case TRUNCF32: convert<f64, f32>(); break;

      case SEXTI16_8: convert<i8, i16>(); break;
      case SEXTI32_8: convert<i8, i32>(); break;
      case SEXTI64_8: convert<i8, i64>(); break;
      case SEXTI32_16: convert<i16, i32>(); break;
      case SEXTI64_16: convert<i16, i64>(); break;
      case SEXTI64_32: convert<i32, i64>(); break;

      case ZEXTI16:
      case ZEXTI32:
      case ZEXTI64:
        break;

      case EXTF64: convert<f32, f64>(); break;
      case FCVTI32: convert<f32, i32>(); break;
      case FCVTI64: convert<f64, i64>(); break;
      case ICVTF32: convert<i32, f32>(); break;
      case ICVTF64: convert<i64, f64>(); break;

      case FTOI32:
      case FTOI64:
      case ITOF32:
      case ITOF64:
        break;

      default:
        UNIMPLEMENTED();
    }
  }

  return result;
}


void Interpreter::ensurePointerMap(const Handle<Function>& function) {
  // It is not safe to enter a function if we don't have pointer maps for it. These can only
  // be generated if all the other functions called by this function are accessible (so we
  // can't do it before all packages are loaded and validated). Since this requires some
  // analysis, we only do it for functions which are actually called.
  if (function->stackPointerMap() == nullptr) {
    GCSafeScope gcSafe(this);
    HandleScope handleScope(vm_);
    auto stackPointerMap = StackPointerMap::buildFrom(vm_->heap(), function);
    function->setStackPointerMap(*stackPointerMap);
  }
}


void Interpreter::handleBuiltin(BuiltinId id) {
  // Builtin functions are implemented here, in C++, but the rest of the code behaves as if
  // they are normal functions. Some of these functions may trigger the garbage collector, so
  // it is important to maintain the stack in a way that the garbage collector expects. All
  // arguments should stay on the stack until we are prepared to return. GCSafeScope should
  // be used when needed.
  switch (id) {
    case BUILTIN_ROOT_CLASS_TO_STRING_ID: {
      String* result = nullptr;
      {
        GCSafeScope gcSafe(this);
        HandleScope handleScope(vm_);
        result = *String::fromUtf8CString(vm_->heap(), "Object");
      }
      pop<Block*>();  // receiver
      push<Block*>(result);
      break;
    }
    case BUILTIN_ROOT_CLASS_TYPEOF_ID: {
      Type* type = nullptr;
      {
        GCSafeScope gcSafe(this);
        HandleScope handleScope(vm_);
        auto receiver = handle(mem<Block*>(stack_->sp() + kPrepareForGCSize));
        auto clas = handle(receiver->meta()->clas());
        vector<Local<Type>> typeArgs;
        typeArgs.reserve(clas->typeParameterCount());
        for (length_t i = 0; i < clas->typeParameterCount(); i++) {
          ASSERT((clas->typeParameter(i)->flags() & STATIC_FLAG) != 0);
          typeArgs.push_back(handle(vm_->roots()->erasedType()));
        }
        type = *Type::create(vm_->heap(), clas, typeArgs);
      }
      pop<Block*>();  // receiver
      push<Block*>(type);
      break;
    }

    case BUILTIN_ROOT_CLASS_CTOR_ID:
    case BUILTIN_EXCEPTION_CTOR_ID:
    case BUILTIN_NULL_POINTER_EXCEPTION_CTOR_ID:
      mem<word_t>(stack_->sp()) = 0;
      break;

    case BUILTIN_TYPE_CTOR_ID: {
      auto clas = block_cast<Class>(pop<Block*>());
      auto receiver = block_cast<Type>(pop<Block*>());
      new(receiver, 1) Type(clas);
      push<i8>(0);
      break;
    }

    case BUILTIN_TYPE_IS_SUBTYPE_OF_ID: {
      bool result = false;
      {
        GCSafeScope gcSafe(this);
        HandleScope handleScope(vm_);
        auto other = handle(mem<Type*>(stack_->sp() + kPrepareForGCSize));
        auto receiver = handle(mem<Type*>(stack_->sp() + kPrepareForGCSize + kSlotSize));
        result = Type::isSubtypeOf(other, receiver);
      }
      pop<Block*>();  // other
      pop<Block*>();  // receiver
      push<i8>(result ? 1 : 0);
      break;
    }

    case BUILTIN_STRING_TO_STRING_ID: {
      // return receiver
      break;
    }

    case BUILTIN_STRING_CONCAT_OP_ID: {
      String* result = nullptr;
      {
        GCSafeScope gcSafe(this);
        HandleScope handleScope(vm_);
        auto right = handle(mem<String*>(stack_->sp() + kPrepareForGCSize));
        auto left = handle(mem<String*>(stack_->sp() + kPrepareForGCSize + kSlotSize));
        result = *String::concat(vm_->heap(), left, right);
      }
      pop<Block*>();  // right
      pop<Block*>();  // left
      push<Block*>(result);
      break;
    }

    case BUILTIN_STRING_LT_OP_ID:
      push<i8>(strcmp() < 0 ? 1 : 0);
      break;

    case BUILTIN_STRING_LE_OP_ID:
      push<i8>(strcmp() <= 0 ? 1 : 0);
      break;

    case BUILTIN_STRING_GT_OP_ID:
      push<i8>(strcmp() > 0 ? 1 : 0);
      break;

    case BUILTIN_STRING_GE_OP_ID:
      push<i8>(strcmp() >= 0 ? 1 : 0);
      break;

    case BUILTIN_STRING_EQ_OP_ID:
      push<i8>(strcmp() == 0 ? 1 : 0);
      break;

    case BUILTIN_STRING_NE_OP_ID:
      push<i8>(strcmp() != 0 ? 1 : 0);
      break;

    case BUILTIN_UNIT_TO_STRING_ID: {
      pop<Block*>();  // receiver
      push<Block*>(vm_->roots()->getBuiltinName(BUILTIN_UNIT_TYPE_ID)->components()->get(0));
      break;
    }

    case BUILTIN_BOOLEAN_TO_STRING_ID: {
      bool value = pop<bool>();
      auto string = value ? vm_->roots()->trueString() : vm_->roots()->falseString();
      push<Block*>(string);
      break;
    }

    case BUILTIN_I8_TO_STRING_ID:
      intToString<i8>();
      break;

    case BUILTIN_I16_TO_STRING_ID:
      intToString<i16>();
      break;

    case BUILTIN_I32_TO_STRING_ID:
      intToString<i32>();
      break;

    case BUILTIN_I64_TO_STRING_ID:
      intToString<i64>();
      break;

    case BUILTIN_F32_TO_STRING_ID:
      floatToString<f32>();
      break;

    case BUILTIN_F64_TO_STRING_ID:
      floatToString<f64>();
      break;

    case BUILTIN_PRINT_FUNCTION_ID: {
      auto string = block_cast<String>(pop<Block*>());
      auto stlString = string->toUtf8StlString();
      cout << stlString;
      push<i8>(0);
      break;
    }

    case BUILTIN_READ_FUNCTION_ID: {
      string stlString;
      getline(cin, stlString);
      if (!cin.good()) {
        throwBuiltinException(BUILTIN_EXCEPTION_CLASS_ID);
        break;
      }
      String* result = nullptr;
      {
        GCSafeScope gcSafe(this);
        HandleScope handleScope(vm_);
        result = *String::fromUtf8String(vm_->heap(),
                                         reinterpret_cast<const u8*>(stlString.data()),
                                         stlString.length());
      }
      push<Block*>(result);
      break;
    }

    default:
      UNIMPLEMENTED();
  }
}


Local<Meta> Interpreter::getMetaForClassId(i64 classId) {
  ASSERT(isPreparedForGC());
  if (isBuiltinId(classId)) {
    return handle(vm_->roots()->getBuiltinMeta(classId));
  }
  Local<Class> clas(function_->package()->getClass(classId));
  return Class::ensureAndGetInstanceMeta(clas);
}


void Interpreter::enter(const Handle<Function>& callee) {
  // Make sure we have pointer maps for the callee before we build a stack frame for it.
  // This may trigger garbage collection (since pointer maps are allocated like everything
  // else), but that's fine since we're at a safepoint, and we haven't built the frame yet.
  ASSERT((callee->flags() & (ABSTRACT_FLAG | EXTERN_FLAG)) == 0);
  ensurePointerMap(callee);

  stack_->align(kWordSize);
  stack_->push(static_cast<word_t>(pcOffset_));
  stack_->push(function_ ? *function_ : nullptr);
  stack_->push(stack_->fp());
  stack_->setFp(stack_->sp());
  stack_->setSp(stack_->sp() - callee->localsSize());

  function_ = callee;
  pcOffset_ = 0;
}


void Interpreter::leave() {
  auto parametersSize = function_->parametersSize();
  auto fp = stack_->fp();
  pcOffset_ = toLength(mem<word_t>(fp, kCallerPcOffsetOffset));
  auto caller = mem<Function*>(fp, kFunctionOffset);
  function_ = caller ? Persistent<Function>(caller) : Persistent<Function>();
  stack_->setSp(fp + kFrameControlSize + parametersSize);
  fp = mem<Address>(fp);
  stack_->setFp(fp);
}


void Interpreter::doThrow(Block* exception) {
  if (handlers_.empty()) {
    // If the exception is unhandled, we need to completely unwind the stack and reset the state
    // of the interpreter in case it is used again.
    stack_->resetPointers();
    function_ = Persistent<Function>();
    pcOffset_ = kPcNotSet;
    throw Error("unhandled exception");
  } else {
    // If there is a handler, we pop the exception, unwind the stack, push the exception, and
    // resume execution there.
    Handler handler = handlers_.back();
    handlers_.pop_back();
    Address fp = handler.fpOffset + stack_->base();
    stack_->push(static_cast<word_t>(pcOffset_));
    for (auto frame : **stack_) {
      if (frame.fp() == fp)
        break;
      function_.set(frame.function());
    }
    stack_->setFramePointerOffset(handler.fpOffset);
    stack_->setStackPointerOffset(handler.spOffset);
    pcOffset_ = handler.pcOffset;
    push(exception);
  }
}


ptrdiff_t Interpreter::localOffsetFromIndex(i64 index) {
  if (index >= 0) {
    // parameter. 0 is the first parameter (highest address on stack).
    return kFrameControlSize + function_->parameterOffset(toLength(index));
  } else {
    // local. -1 is the first local, and they grow down.
    // TODO: this assumes all locals are word-sized.
    return static_cast<ptrdiff_t>(index) * static_cast<ptrdiff_t>(kWordSize);
  }
}


Address Interpreter::localAddressFromOffset(ptrdiff_t offset) {
  Address fp = stack_->fp();
  return fp + offset;
}


Address Interpreter::localAddressFromIndex(i64 index) {
  return localAddressFromOffset(localOffsetFromIndex(index));
}


i64 Interpreter::readVbn() {
  return codeswitch::internal::readVbn(function_->instructionsStart(), &pcOffset_);
}


void Interpreter::prepareForGC() {
  ASSERT(!isPreparedForGC());
  if (pcOffset_ != kDonePcOffset) {
    ASSERT(function_->hasPointerMapAtPcOffset(pcOffset_));
    stack_->push(static_cast<word_t>(pcOffset_));
  }
  ASSERT(!vm_->heap()->isAllocationAllowed());
  vm_->heap()->setIsAllocationAllowed(true);
  isPreparedForGC_ = true;
}


void Interpreter::unprepareForGC() {
  ASSERT(isPreparedForGC());
  ASSERT(vm_->heap()->isAllocationAllowed());
  if (pcOffset_ != kDonePcOffset)
    stack_->pop<word_t>();
  vm_->heap()->setIsAllocationAllowed(false);
  isPreparedForGC_ = false;
}


void Interpreter::throwBuiltinException(BuiltinId id) {
  // TODO: ensure this allocation succeeds because we don't have a pointer map here.
  auto exnMeta = vm_->roots()->getBuiltinMeta(id);
  auto exn = new(vm_->heap(), exnMeta) Object;
  doThrow(exn);
}


template <typename T>
void Interpreter::loadObject() {
  auto index = readVbn();
  auto block = pop<Block*>();
  if (block == nullptr) {
    throwBuiltinException(BUILTIN_NULL_POINTER_EXCEPTION_CLASS_ID);
    return;
  }
  auto offset = block->meta()->clas()->findFieldOffset(index);
  auto value = mem<T>(block, offset);
  push(value);
}


template <typename T>
void Interpreter::storeObject() {
  auto index = readVbn();
  auto block = pop<Block*>();
  auto value = pop<T>();
  if (block == nullptr) {
    throwBuiltinException(BUILTIN_NULL_POINTER_EXCEPTION_CLASS_ID);
    return;
  }
  auto offset = block->meta()->clas()->findFieldOffset(index);
  mem<T>(block, offset) = value;
}


#define DEFINE_BINOP(name, op)                                    \
template <typename T>                                                 \
void Interpreter::name() {                                            \
  auto right = pop<T>();                                              \
  auto left = pop<T>();                                               \
  push(left op right);                                                \
}                                                                     \

DEFINE_BINOP(add, +)
DEFINE_BINOP(sub, -)
DEFINE_BINOP(mul, *)
DEFINE_BINOP(div, /)
DEFINE_BINOP(mod, %)
DEFINE_BINOP(lsl, <<)
DEFINE_BINOP(asr, >>)
DEFINE_BINOP(and_, &)
DEFINE_BINOP(or_, |)
DEFINE_BINOP(xor_, ^)
DEFINE_BINOP(eq, ==)
DEFINE_BINOP(ne, !=)
DEFINE_BINOP(lt, <)
DEFINE_BINOP(le, <=)
DEFINE_BINOP(gt, >)
DEFINE_BINOP(ge, >=)

#undef DEFINE_BINOP

// We don't define lsr with the macro because we need to do an unsigned shift. C++ doesn't have
// separate operators for logical and arithmetic shifts.
template <typename T>
void Interpreter::lsr() {
  auto right = static_cast<u64>(pop<T>());
  auto left = static_cast<u64>(pop<T>());
  push(left >> right);
}


template <typename T>
void Interpreter::inv() {
  auto value = pop<T>();
  push(~value);
}


template <typename T>
void Interpreter::neg() {
  auto value = pop<T>();
  push(-value);
}


template <typename T>
void Interpreter::trunc() {
  auto value = static_cast<i64>(pop<T>());
  push(value);
}


template <typename From, typename To>
void Interpreter::convert() {
  auto from = pop<From>();
  auto to = static_cast<To>(from);
  push<To>(to);
}


int Interpreter::strcmp() {
  auto right = block_cast<String>(pop<Block*>());
  auto left = block_cast<String>(pop<Block*>());
  auto cmp = left->compare(right);
  return cmp;
}


template <typename T>
void Interpreter::intToString() {
  auto value = pop<T>();
  push<T>(value);  // hack to restore the stack
  String* result = nullptr;
  {
    GCSafeScope gcSafe(this);
    HandleScope handleScope(vm_);
    stringstream stream;
    stream << value;
    auto stlString = stream.str();
    auto size = stlString.length();   // same as length since these should be ascii chars
    result = *String::fromUtf8String(vm_->heap(),
                                     reinterpret_cast<const u8*>(stlString.data()),
                                     size, size);
  }
  pop<T>();  // receiver
  push<Block*>(result);
}


template <typename T>
void Interpreter::floatToString() {
  auto value = pop<T>();
  push<T>(value);  // hack to restore the stack
  String* result = nullptr;
  {
    GCSafeScope gcSafe(this);
    HandleScope handleScope(vm_);
    stringstream stream;
    stream << value;
    auto stlString = stream.str();
    auto size = stlString.length();
    result = *String::fromUtf8String(vm_->heap(),
                                     reinterpret_cast<const u8*>(stlString.data()),
                                     size, size);
  }
  pop<T>();  // receiver
  push<Block*>(result);
}

}
}

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "function-inl.h"

#include <algorithm>
#include <vector>
#include "block-inl.h"
#include "bytecode.h"
#include "class-inl.h"
#include "field.h"
#include "package-inl.h"
#include "type.h"
#include "utils.h"

using namespace std;

namespace codeswitch {
namespace internal {

Function* Function::tryAllocate(Heap* heap, word_t instructionsSize) {
  word_t size = sizeForFunction(instructionsSize);
  Function* function = reinterpret_cast<Function*>(heap->allocateRaw(size));
  if (function == nullptr)
    return nullptr;

  function->setMeta(FUNCTION_BLOCK_TYPE);
  return function;
}


Handle<Function> Function::allocate(Heap* heap, word_t instructionsSize) {
  DEFINE_ALLOCATION(heap, Function, tryAllocate(heap, instructionsSize))
}


void Function::initialize(u32 flags,
                          TaggedArray* typeParameters,
                          BlockArray* types,
                          word_t localsSize,
                          const vector<u8>& instructions,
                          WordArray* blockOffsets,
                          Package* package,
                          StackPointerMap* stackPointerMap) {
  setFlags(flags);
  setTypeParameters(typeParameters);
  setTypes(types);
  setLocalsSize(localsSize);
  setInstructionsSize(instructions.size());
  setBlockOffsets(blockOffsets);
  setPackage(package);
  setStackPointerMap(stackPointerMap);
  copy_n(instructions.data(), instructions.size(), instructionsStart());
}


void Function::printFunction(FILE* out) {
  fprintf(out, "Function @%p\n", reinterpret_cast<void*>(this));
  fprintf(out, "  instructions size: %d\n", static_cast<int>(instructionsSize()));
  fprintf(out, "  parameter count: %d\n", static_cast<int>(parameterCount()));
}


struct FrameState {
  FrameState(word_t localsSlots, Type* defaultType)
      : pcOffset(kNotSet) {
    typeMap.insert(typeMap.begin(), localsSlots, defaultType);
  }

  void push(Type* type) {
    typeMap.push_back(type);
  }

  Type* pop() {
    auto type = typeMap.back();
    typeMap.pop_back();
    return type;
  }

  Type* top() { return typeMap.back(); }

  void setLocal(i64 slot, Type* type) {
    ASSERT(slot < 0);
    word_t index = -slot - 1;
    ASSERT(index < typeMap.size());
    typeMap[index] = type;
  }

  void pushTypeArg(Type* type) {
    ASSERT(type->isObject());
    // TODO: support classes with type parameters
    ASSERT(type->length() == 1);
    typeArgs.push_back(type);
  }

  void popTypeArgs() {
    typeArgs.clear();
  }

  Type* substituteReturnType(Function* callee) {
    ASSERT(typeArgs.size() == callee->typeParameterCount());
    vector<pair<TypeParameter*, Type*>> typeBindings;
    typeBindings.reserve(typeArgs.size());
    for (word_t i = 0; i < typeArgs.size(); i++) {
      pair<TypeParameter*, Type*> binding(callee->typeParameter(i), typeArgs[i]);
      typeBindings.push_back(binding);
    }
    Type* retTy = callee->returnType()->substitute(typeBindings);
    return retTy;
  }

  size_t size() { return typeMap.size(); }

  bool operator < (const FrameState& other) const {
    return pcOffset < other.pcOffset;
  }

  vector<Type*> typeMap;
  vector<Type*> typeArgs;
  word_t pcOffset;
};


StackPointerMap* StackPointerMap::tryBuildFrom(Heap* heap, Function* function) {
  ASSERT(function->instructionsSize() > 0);

  Roots* roots = heap->vm()->roots();
  Package* package = function->package();

  // Constrct a pointer map for the parameters.
  vector<Type*> parametersMap;
  for (word_t i = 0, n = function->parameterCount(); i < n; i++) {
    parametersMap.push_back(function->parameterType(i));
  }

  // Construct a pointer map for each point in the function where the garbage collector may be
  // invoked: every allocation and every function call. We do this by constructing a frame
  // state for the beginning of the function, and simulating the effect each instruction has
  // on the pointers in the frame. We save frame states at points where they're needed. The
  // blocks of the function are traversed in depth first order.
  vector<FrameState> maps;
  BitSet visitedBlockOffsets;
  vector<FrameState> blocksToVisit;
  blocksToVisit.push_back(FrameState(function->localsSize() / kWordSize,
                          Type::unitType(roots)));
  blocksToVisit.front().pcOffset = 0;
  u8* bytecode = function->instructionsStart();

  while (!blocksToVisit.empty()) {
    FrameState currentMap = blocksToVisit.back();
    blocksToVisit.pop_back();
    if (visitedBlockOffsets.contains(currentMap.pcOffset))
      continue;
    word_t pcOffset = currentMap.pcOffset;
    visitedBlockOffsets.add(pcOffset);

    bool blockDone = false;
    while (!blockDone) {
      Opcode opc = static_cast<Opcode>(bytecode[pcOffset++]);
      switch (opc) {
        case NOP:
          break;

        case RET:
          currentMap.pop();
          blockDone = true;
          break;

        case BRANCH: {
          i64 blockIndex = readVbn(bytecode, &pcOffset);
          currentMap.pcOffset = function->blockOffset(blockIndex);
          blocksToVisit.push_back(move(currentMap));
          blockDone = true;
          break;
        }

        case BRANCHIF: {
          currentMap.pop();
          i64 trueBlockIndex = readVbn(bytecode, &pcOffset);
          currentMap.pcOffset = function->blockOffset(trueBlockIndex);
          blocksToVisit.push_back(currentMap);
          i64 falseBlockIndex = readVbn(bytecode, &pcOffset);
          currentMap.pcOffset = function->blockOffset(falseBlockIndex);
          blocksToVisit.push_back(move(currentMap));
          blockDone = true;
          break;
        }

        case PUSHTRY: {
          i64 tryBlockIndex = readVbn(bytecode, &pcOffset);
          currentMap.pcOffset = function->blockOffset(tryBlockIndex);
          blocksToVisit.push_back(currentMap);
          i64 catchBlockIndex = readVbn(bytecode, &pcOffset);
          currentMap.pcOffset = function->blockOffset(catchBlockIndex);
          currentMap.push(roots->getBuiltinType(BUILTIN_EXCEPTION_CLASS_ID));
          blocksToVisit.push_back(move(currentMap));
          blockDone = true;
          break;
        }

        case POPTRY: {
          i64 doneBlockIndex = readVbn(bytecode, &pcOffset);
          currentMap.pcOffset = function->blockOffset(doneBlockIndex);
          blocksToVisit.push_back(move(currentMap));
          blockDone = true;
          break;
        }

        case THROW:
          currentMap.pop();
          blockDone = true;
          break;

        case DROP:
          currentMap.pop();
          break;

        case DUP:
          currentMap.push(currentMap.top());
          break;

        case DUPI: {
          auto slot = readVbn(bytecode, &pcOffset);
          auto index = currentMap.size() - slot - 1;
          auto type = currentMap.typeMap[index];
          currentMap.typeMap.push_back(type);
          break;
        }

        case SWAP: {
          auto index = currentMap.size() - 2;
          auto top = currentMap.top();
          auto other = currentMap.typeMap[index];
          currentMap.typeMap.back() = other;
          currentMap.typeMap[index] = top;
          break;
        }

        case SWAP2: {
          auto index = currentMap.size() - 3;
          auto top = currentMap.top();
          auto other = currentMap.typeMap[index];
          currentMap.typeMap.back() = other;
          currentMap.typeMap[index] = top;
          break;
          break;
        }

        case UNIT:
          currentMap.push(Type::unitType(roots));
          break;

        case TRUE:
          currentMap.push(Type::booleanType(roots));
          break;

        case FALSE:
          currentMap.push(Type::booleanType(roots));
          break;

        case NUL:
          currentMap.push(Type::nullType(roots));
          break;

        case UNINITIALIZED:
          currentMap.push(Type::nullType(roots));
          break;

        case I8:
          readVbn(bytecode, &pcOffset);
          currentMap.push(Type::i8Type(roots));
          break;

        case I16:
          readVbn(bytecode, &pcOffset);
          currentMap.push(Type::i16Type(roots));
          break;

        case I32:
          readVbn(bytecode, &pcOffset);
          currentMap.push(Type::i32Type(roots));
          break;

        case I64:
          readVbn(bytecode, &pcOffset);
          currentMap.push(Type::i64Type(roots));
          break;

        case F32:
          pcOffset += 4;
          currentMap.push(Type::f32Type(roots));
          break;

        case F64:
          pcOffset += 8;
          currentMap.push(Type::f64Type(roots));
          break;

        case STRING: {
          readVbn(bytecode, &pcOffset);
          currentMap.push(roots->getBuiltinType(BUILTIN_STRING_CLASS_ID));
          break;
        }

        case LDLOCAL: {
          i64 offset = readVbn(bytecode, &pcOffset);
          i64 slot = offset / kWordSize;
          auto type = slot >= 0
              ? parametersMap[slot]
              : currentMap.typeMap[-slot - 1];
          currentMap.push(type);
          break;
        }

        case STLOCAL: {
          i64 offset = readVbn(bytecode, &pcOffset);
          i64 slot = offset / kWordSize;
          if (slot < 0) {
            auto type = currentMap.pop();
            currentMap.typeMap[-slot - 1] = type;
          }
          break;
        }

        case LD8:
        case LD16:
        case LD32:
        case LD64:
        case LDP:
        case LDPC: {
          i64 offset = readVbn(bytecode, &pcOffset);
          auto clas = currentMap.pop()->asClass();
          auto fieldIndex = clas->findFieldIndex(static_cast<word_t>(offset));
          currentMap.push(Field::cast(clas->fields()->get(fieldIndex))->type());
          break;
        }

        case ST8:
        case ST16:
        case ST32:
        case ST64:
        case STP:
          readVbn(bytecode, &pcOffset);
          currentMap.pop();
          currentMap.pop();
          break;

        case ALLOCOBJ: {
          i64 classId = readVbn(bytecode, &pcOffset);
          currentMap.pcOffset = pcOffset;
          maps.push_back(currentMap);
          Type* type;
          if (isBuiltinId(classId)) {
            type = roots->getBuiltinType(classId);
          } else {
            type = Type::tryAllocate(heap, 1);
            if (type == nullptr)
              return nullptr;
            type->initialize(Class::cast(package->classes()->get(classId)));
          }
          currentMap.push(type);
          break;
        }

        case ALLOCARRI: {
          i64 classId = readVbn(bytecode, &pcOffset);
          readVbn(bytecode, &pcOffset);  // length is unused
          currentMap.pcOffset = pcOffset;
          maps.push_back(currentMap);
          Type* type;
          if (isBuiltinId(classId)) {
            type = roots->getBuiltinType(classId);
          } else {
            type = Type::tryAllocate(heap, 1);
            if (type == nullptr)
              return nullptr;
            type->initialize(Class::cast(package->classes()->get(classId)));
          }
          currentMap.push(type);
          break;
        }

        case TYCS: {
          auto classId = readVbn(bytecode, &pcOffset);
          Class* clas = isBuiltinId(classId)
              ? roots->getBuiltinClass(static_cast<BuiltinId>(classId))
              : package->getClass(classId);
          Type* type = Type::tryAllocate(heap, 1);
          if (type == nullptr)
            return nullptr;
          type->initialize(clas, Type::NO_FLAGS);
          currentMap.pushTypeArg(type);
          break;
        }

        case TYVS: {
          auto typeParamId = readVbn(bytecode, &pcOffset);
          ASSERT(!isBuiltinId(typeParamId));
          TypeParameter* param = package->getTypeParameter(typeParamId);
          Type* type = Type::tryAllocate(heap, 1);
          if (type == nullptr)
            return nullptr;
          type->initialize(param, Type::NO_FLAGS);
          currentMap.pushTypeArg(type);
          break;
        }

        case CALLG: {
          auto paramCount = static_cast<word_t>(readVbn(bytecode, &pcOffset));
          i64 functionId = readVbn(bytecode, &pcOffset);
          currentMap.pcOffset = pcOffset;
          maps.push_back(currentMap);
          Function* callee = isBuiltinId(functionId)
              ? roots->getBuiltinFunction(static_cast<BuiltinId>(functionId))
              : package->getFunction(functionId);
          ASSERT(paramCount == callee->parameterCount());
          for (word_t i = 0; i < paramCount; i++)
            currentMap.pop();
          auto returnType = currentMap.substituteReturnType(callee);
          currentMap.popTypeArgs();
          currentMap.push(returnType);
          break;
        }

        case CLS: {
          readVbn(bytecode, &pcOffset);
          currentMap.push(Type::rootClassType(roots));
          break;
        }

        case CALLV: {
          i64 argCount = readVbn(bytecode, &pcOffset);
          i64 methodIndex = readVbn(bytecode, &pcOffset);
          currentMap.pcOffset = pcOffset;
          maps.push_back(currentMap);
          word_t slot = currentMap.size() - argCount;
          auto clas = currentMap.typeMap[slot]->asClass();
          auto callee = clas->getMethod(methodIndex);

          for (word_t i = 0, n = callee->parameterCount(); i < n; i++)
            currentMap.pop();
          auto returnType = currentMap.substituteReturnType(callee);
          currentMap.popTypeArgs();
          currentMap.push(returnType);
          break;
        }

        case ADDI8:
        case SUBI8:
        case MULI8:
        case DIVI8:
        case MODI8:
        case LSLI8:
        case LSRI8:
        case ASRI8:
        case ANDI8:
        case ORI8:
        case XORI8:
          currentMap.pop();
          currentMap.pop();
          currentMap.push(Type::i8Type(roots));
          break;

        case ADDI16:
        case SUBI16:
        case MULI16:
        case DIVI16:
        case MODI16:
        case LSLI16:
        case LSRI16:
        case ASRI16:
        case ANDI16:
        case ORI16:
        case XORI16:
          currentMap.pop();
          currentMap.pop();
          currentMap.push(Type::i16Type(roots));
          break;

        case ADDI32:
        case SUBI32:
        case MULI32:
        case DIVI32:
        case MODI32:
        case LSLI32:
        case LSRI32:
        case ASRI32:
        case ANDI32:
        case ORI32:
        case XORI32:
          currentMap.pop();
          currentMap.pop();
          currentMap.push(Type::i32Type(roots));
          break;

        case ADDI64:
        case SUBI64:
        case MULI64:
        case DIVI64:
        case MODI64:
        case LSLI64:
        case LSRI64:
        case ASRI64:
        case ANDI64:
        case ORI64:
        case XORI64:
          currentMap.pop();
          currentMap.pop();
          currentMap.push(Type::i64Type(roots));
          break;

        case ADDF32:
        case SUBF32:
        case MULF32:
        case DIVF32:
          currentMap.pop();
          currentMap.pop();
          currentMap.push(Type::f32Type(roots));
          break;

        case ADDF64:
        case SUBF64:
        case MULF64:
        case DIVF64:
          currentMap.pop();
          currentMap.pop();
          currentMap.push(Type::f64Type(roots));
          break;

        case EQI8:
        case EQI16:
        case EQI32:
        case EQI64:
        case EQF32:
        case EQF64:
        case EQP:
        case NEI8:
        case NEI16:
        case NEI32:
        case NEI64:
        case NEF32:
        case NEF64:
        case NEP:
        case LTI8:
        case LTI16:
        case LTI32:
        case LTI64:
        case LTF32:
        case LTF64:
        case LEI8:
        case LEI16:
        case LEI32:
        case LEI64:
        case LEF32:
        case LEF64:
        case GTI8:
        case GTI16:
        case GTI32:
        case GTI64:
        case GTF32:
        case GTF64:
        case GEI8:
        case GEI16:
        case GEI32:
        case GEI64:
        case GEF32:
        case GEF64:
          currentMap.pop();
          currentMap.pop();
          currentMap.push(Type::booleanType(roots));
          break;

        case NEGI8:
        case INVI8:
          currentMap.pop();
          currentMap.push(Type::i8Type(roots));
          break;

        case NEGI16:
        case INVI16:
          currentMap.pop();
          currentMap.push(Type::i16Type(roots));
          break;

        case NEGI32:
        case INVI32:
          currentMap.pop();
          currentMap.push(Type::i32Type(roots));
          break;

        case NEGI64:
        case INVI64:
          currentMap.pop();
          currentMap.push(Type::i64Type(roots));
          break;

        case NEGF32:
          currentMap.pop();
          currentMap.push(Type::f32Type(roots));
          break;

        case NEGF64:
          currentMap.pop();
          currentMap.push(Type::f64Type(roots));
          break;

        case NOTB:
          currentMap.pop();
          currentMap.push(Type::booleanType(roots));
          break;

        case TRUNCI8:
          currentMap.pop();
          currentMap.push(Type::i8Type(roots));
          break;

        case TRUNCI16:
        case SEXTI16_8:
        case ZEXTI16:
          currentMap.pop();
          currentMap.push(Type::i16Type(roots));
          break;

        case TRUNCI32:
        case SEXTI32_8:
        case SEXTI32_16:
        case ZEXTI32:
        case FCVTI32:
        case FTOI32:
          currentMap.pop();
          currentMap.push(Type::i32Type(roots));
          break;

        case SEXTI64_8:
        case SEXTI64_16:
        case SEXTI64_32:
        case ZEXTI64:
        case FCVTI64:
        case FTOI64:
          currentMap.pop();
          currentMap.push(Type::i64Type(roots));
          break;

        case TRUNCF32:
        case ICVTF32:
        case ITOF32:
          currentMap.pop();
          currentMap.push(Type::f32Type(roots));
          break;

        case EXTF64:
        case ICVTF64:
        case ITOF64:
          currentMap.pop();
          currentMap.push(Type::f64Type(roots));
          break;

        default:
          UNIMPLEMENTED();
      }
    }
  }

  // Sort the pointer maps by pc offset.
  sort(maps.begin(), maps.end());

  // Determine how big the final bitmap will be.
  word_t bitmapLength = parametersMap.size();
  for (FrameState state : maps) {
    bitmapLength += state.typeMap.size();
  }

  // Allocate and build the final data structure.
  word_t arrayLength = StackPointerMap::kHeaderLength +
      maps.size() * StackPointerMap::kEntryLength +
      align(bitmapLength, kBitsInWord) / kWordSize;
  WordArray* array = WordArray::tryAllocate(heap, arrayLength);
  if (array == nullptr)
    return nullptr;

  StackPointerMap* stackPointerMap = StackPointerMap::cast(array);
  stackPointerMap->setEntryCount(maps.size());
  word_t mapOffset = parametersMap.size();
  for (word_t i = 0, n = maps.size(); i < n; i++) {
    stackPointerMap->setPcOffset(i, maps[i].pcOffset);
    stackPointerMap->setMapOffset(i, mapOffset);
    stackPointerMap->setMapCount(i, maps[i].typeMap.size());
    mapOffset += maps[i].typeMap.size();
  }
  stackPointerMap->setBitmapLength(bitmapLength);
  Bitmap bitmap = stackPointerMap->bitmap();
  word_t bitOffset = 0;
  for (Type* type : parametersMap) {
    bitmap.set(bitOffset++, type->isObject());
  }
  for (FrameState state : maps) {
    for (Type* type : state.typeMap) {
      bitmap.set(bitOffset++, type->isObject());
    }
  }

  return stackPointerMap;
}


Handle<StackPointerMap> StackPointerMap::buildFrom(Heap* heap, Handle<Function> function) {
  DEFINE_ALLOCATION(heap, StackPointerMap, tryBuildFrom(heap, *function))
}


void StackPointerMap::getLocalsRegion(word_t pc, word_t* localsOffset, word_t* localsCount) {
  word_t begin = 0;
  word_t end = entryCount();
  word_t middle = begin + (end - begin) / 2;
  ASSERT(end != 0);
  while (pc != pcOffset(middle)) {
    ASSERT(middle < end);
    if (pc < pcOffset(middle))
      end = middle;
    else
      begin = middle + 1;
    middle = begin + (end - begin) / 2;
  }
  *localsOffset = mapOffset(middle);
  *localsCount = mapCount(middle);
}

}
}

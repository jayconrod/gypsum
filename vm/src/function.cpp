// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "function.h"

#include <algorithm>
#include <vector>
#include "block.h"
#include "bytecode.h"
#include "class.h"
#include "field.h"
#include "package.h"
#include "roots-inl.h"
#include "type.h"
#include "utils.h"

using namespace std;

namespace codeswitch {
namespace internal {

#define FUNCTION_POINTER_LIST(F) \
  F(Function, typeParameters_)   \
  F(Function, types_)            \
  F(Function, blockOffsets_)     \
  F(Function, package_)          \
  F(Function, stackPointerMap_)  \


DEFINE_POINTER_MAP(Function, FUNCTION_POINTER_LIST)

#undef FUNCTION_POINTER_LIST


void* Function::operator new(size_t, Heap* heap, length_t instructionsSize) {
  ASSERT(instructionsSize <= kMaxLength);
  word_t size = sizeForFunction(instructionsSize);
  Function* function = reinterpret_cast<Function*>(heap->allocate(size));
  function->instructionsSize_ = instructionsSize;
  return function;
}


Function::Function(u32 flags,
                   TaggedArray<TypeParameter>* typeParameters,
                   BlockArray<Type>* types,
                   word_t localsSize,
                   const vector<u8>& instructions,
                   LengthArray* blockOffsets,
                   Package* package,
                   StackPointerMap* stackPointerMap)
    : Block(FUNCTION_BLOCK_TYPE),
      flags_(flags),
      builtinId_(0),
      typeParameters_(this, typeParameters),
      types_(this, types),
      localsSize_(localsSize),
      instructionsSize_(instructions.size()),
      blockOffsets_(this, blockOffsets),
      package_(this, package),
      stackPointerMap_(this, stackPointerMap) {
  ASSERT(instructionsSize_ <= kMaxLength);
  copy_n(instructions.begin(), instructions.size(), instructionsStart());
}


Local<Function> Function::create(Heap* heap,
                                 u32 flags,
                                 const Handle<TaggedArray<TypeParameter>>& typeParameters,
                                 const Handle<BlockArray<Type>>& types,
                                 word_t localsSize,
                                 const vector<u8>& instructions,
                                 const Handle<LengthArray>& blockOffsets,
                                 const Handle<Package>& package) {
  RETRY_WITH_GC(heap, return Local<Function>(new(heap, instructions.size()) Function(
      flags, *typeParameters, *types, localsSize, instructions,
      *blockOffsets, *package, nullptr)));
}


word_t Function::sizeForFunction(length_t instructionsSize) {
  ASSERT(instructionsSize <= kMaxLength);
  return align(sizeof(Function), kWordSize) + instructionsSize;
}


void Function::printFunction(FILE* out) {
  fprintf(out, "Function @%p\n", reinterpret_cast<void*>(this));
  fprintf(out, "  instructions size: %d\n", static_cast<int>(instructionsSize()));
  fprintf(out, "  parameter count: %d\n", static_cast<int>(parameterCount()));
}


TypeParameter* Function::typeParameter(length_t index) const {
  auto paramTag = typeParameters()->get(index);
  if (paramTag.isNumber()) {
    return package()->getTypeParameter(paramTag.getNumber());
  } else {
    return paramTag.getPointer();
  }
}


word_t Function::parametersSize() const {
  word_t size = 0;
  for (length_t i = 0, n = parameterCount(); i < n; i++) {
    size += align(parameterType(i)->typeSize(), kWordSize);
  }
  return size;
}


ptrdiff_t Function::parameterOffset(length_t index) const {
  ptrdiff_t offset = 0;
  for (length_t i = parameterCount() - 1; i > index; i--) {
    offset += align(parameterType(i)->typeSize(), kWordSize);
  }
  return offset;
}


u8* Function::instructionsStart() const {
  auto start = align(address() + sizeof(Function), kWordSize);
  return reinterpret_cast<u8*>(start);
}


StackPointerMap* StackPointerMap::cast(Block* block) {
  WordArray* array = WordArray::cast(block);
  return reinterpret_cast<StackPointerMap*>(array);
}


Bitmap StackPointerMap::bitmap() {
  word_t* base = reinterpret_cast<word_t*>(
      elements() + kHeaderLength + entryCount() * kEntryLength);
  return Bitmap(base, bitmapLength());
}


void StackPointerMap::getParametersRegion(word_t* paramOffset, word_t* paramCount) {
  *paramOffset = 0;
  // The parameter region is first in the bitmap. We determine its size by checking the offset
  // of the first locals region. If there are no other regions, then it is the size of the
  // whole bitmap.
  if (entryCount() == 0) {
    *paramCount = bitmapLength();
  } else {
    *paramCount = mapOffset(0);
  }
}


struct FrameState {
  FrameState(word_t localsSlots, Type* defaultType)
      : pcOffset(-1) {
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
  length_t pcOffset;
};


StackPointerMap* StackPointerMap::tryBuildFrom(Heap* heap, Function* function) {
  ASSERT(function->instructionsSize() > 0);

  Roots* roots = heap->vm()->roots();
  Package* package = function->package();

  // Constrct a pointer map for the parameters.
  vector<Type*> parametersMap;
  for (length_t i = 0, n = function->parameterCount(); i < n; i++) {
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
    length_t pcOffset = currentMap.pcOffset;
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
          auto slot = readVbn(bytecode, &pcOffset);
          auto type = slot >= 0
              ? parametersMap[slot]
              : currentMap.typeMap[-slot - 1];
          currentMap.push(type);
          break;
        }

        case STLOCAL: {
          auto slot = readVbn(bytecode, &pcOffset);
          auto type = currentMap.pop();
          if (slot < 0)
            currentMap.typeMap[-slot - 1] = type;
          break;
        }

        case LD8:
        case LD16:
        case LD32:
        case LD64:
        case LDP:
        case LDPC: {
          auto index = readVbn(bytecode, &pcOffset);
          auto clas = currentMap.pop()->asClass();
          auto type = Field::cast(clas->fields()->get(index))->type();
          currentMap.push(type);
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
            type = new(heap, 1) Type(Class::cast(package->classes()->get(classId)));
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
            type = new(heap, 1) Type(Class::cast(package->classes()->get(classId)));
          }
          currentMap.push(type);
          break;
        }

        case TYCS: {
          auto classId = readVbn(bytecode, &pcOffset);
          Class* clas = isBuiltinId(classId)
              ? roots->getBuiltinClass(static_cast<BuiltinId>(classId))
              : package->getClass(classId);
          Type* type = new(heap, 1) Type(clas, Type::NO_FLAGS);
          currentMap.pushTypeArg(type);
          break;
        }

        case TYVS: {
          auto typeParamId = readVbn(bytecode, &pcOffset);
          ASSERT(!isBuiltinId(typeParamId));
          TypeParameter* param = package->getTypeParameter(typeParamId);
          Type* type = new(heap, 1) Type(param, Type::NO_FLAGS);
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
  length_t arrayLength = StackPointerMap::kHeaderLength +
      maps.size() * StackPointerMap::kEntryLength +
      align(bitmapLength, kBitsInWord) / kWordSize;
  WordArray* array = new(heap, arrayLength) WordArray;
  if (array == nullptr)
    return nullptr;

  StackPointerMap* stackPointerMap = StackPointerMap::cast(array);
  stackPointerMap->setEntryCount(maps.size());
  length_t mapOffset = parametersMap.size();
  for (length_t i = 0, n = maps.size(); i < n; i++) {
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


Local<StackPointerMap> StackPointerMap::buildFrom(Heap* heap, Local<Function> function) {
  DEFINE_ALLOCATION(heap, StackPointerMap, tryBuildFrom(heap, *function))
}


void StackPointerMap::getLocalsRegion(length_t pc, word_t* localsOffset, word_t* localsCount) {
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

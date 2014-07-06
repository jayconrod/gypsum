// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef block_visitor_h
#define block_visitor_h

#include "array.h"
#include "block.h"
#include "function-inl.h"
#include "stack-inl.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

// This class is used to iterate over the pointers of any block, possibly updating them. It
// implements static polymorphism by following the curious recurring template pattern (CRTP).
template <class BlockVisitor>
class BlockVisitorBase {
 public:
  BlockVisitor* self() { return static_cast<BlockVisitor*>(this); }

  // Clients should call this.
  inline void visit(Block* block);

  // Subclasses should override this.
  void visitPointer(Block** p) { }

 protected:
  // These are helper functions. They can be overriden if needed.
  inline void visitMetaWord(Block* mw);
  inline void visitBlockWithPointerMap(Block* block, Meta* meta);
  inline void visitBlockWithCustomPointers(Block* block, Meta* meta);
  inline void visitStack(Stack* stack);
  inline void visitTaggedArray(TaggedArray* array);
};


template <class BlockVisitor>
void BlockVisitorBase<BlockVisitor>::visit(Block* block) {
  self()->visitMetaWord(block);
  Meta* meta = block->meta();
  if (!meta->hasPointers()) {
    return;
  } else if (!meta->hasCustomPointers()) {
    self()->visitBlockWithPointerMap(block, meta);
  } else {
    self()->visitBlockWithCustomPointers(block, meta);
  }
}


template <class BlockVisitor>
void BlockVisitorBase<BlockVisitor>::visitMetaWord(Block* block) {
  if (block->hasEncodedMeta())
    return;

  Block* oldMeta = block->meta();
  Block* newMeta = oldMeta;
  self()->visitPointer(&newMeta);
  if (newMeta != oldMeta)
    block->setMeta(reinterpret_cast<Meta*>(newMeta));
}


template <class BlockVisitor>
void BlockVisitorBase<BlockVisitor>::visitBlockWithPointerMap(Block* block, Meta* meta) {
  auto pointers = reinterpret_cast<Tagged<Block>*>(block);
  Bitmap objectMap = meta->objectPointerMap();
  word_t i;
  ASSERT(!objectMap[0]);   // map should not be set
  for (i = 1; i < objectMap.bitCount(); i++) {
    if (objectMap[i] && pointers[i].isPointer()) {
      auto slot = reinterpret_cast<Block**>(&pointers[i]);
      self()->visitPointer(slot);
    }
  }
  if (meta->hasElements() && meta->hasElementPointers()) {
    ASSERT(!objectMap[1]);   // count should not be set
    Bitmap elementMap = meta->elementPointerMap();
    word_t count = reinterpret_cast<word_t*>(block)[1];
    for (word_t j = 0; j < count; j++) {
      for (word_t k = 0; k < elementMap.bitCount(); k++, i++) {
        if (elementMap[k] && pointers[i].isPointer()) {
          auto slot = reinterpret_cast<Block**>(&pointers[i]);
          self()->visitPointer(slot);
        }
      }
    }
  }
}


template <class BlockVisitor>
void BlockVisitorBase<BlockVisitor>::visitBlockWithCustomPointers(Block* block, Meta* meta) {
  BlockType type = meta->type();
  switch (type) {
    case STACK_BLOCK_TYPE:
      self()->visitStack(static_cast<Stack*>(block));
      break;

    case TAGGED_ARRAY_BLOCK_TYPE:
      self()->visitTaggedArray(static_cast<TaggedArray*>(block));
      break;

    default:
      UNREACHABLE();
  }
}


template <class BlockVisitor>
void BlockVisitorBase<BlockVisitor>::visitStack(Stack* stack) {
  // Before invoking the garbage collector, execution should push the current pc offset on
  // top of the stack.
  word_t pcOffset = *reinterpret_cast<word_t*>(stack->sp());

  // Loop over the stack frames.
  for (StackFrame frame : *stack) {
    // Read the function from the stack and determine the correct pointer map.
    Function* function = frame.function();
    self()->visitPointer(reinterpret_cast<Block**>(&function));
    if (function != frame.function())
      frame.setFunction(function);

    StackPointerMap* fullPointerMap = function->stackPointerMap();
    Bitmap bitmap = fullPointerMap->bitmap();

    // Visit pointer locals. This includes arguments to the callee.
    word_t localsOffset, localsCount;
    fullPointerMap->getLocalsRegion(pcOffset, &localsOffset, &localsCount);
    auto local = reinterpret_cast<Block**>(frame.locals()) - 1;
    for (word_t i = 0; i < localsCount; i++, local--) {
      if (bitmap.at(localsOffset + i)) {
        self()->visitPointer(local);
      }
    }

    // If this is the last frame on the stack, visit pointer arguments. Normally the caller will
    // visit our arguments, but in this case, there is no caller.
    if (frame.isLast()) {
      word_t paramsOffset, paramsCount;
      fullPointerMap->getParametersRegion(&paramsOffset, &paramsCount);
      auto param = reinterpret_cast<Block**>(frame.parameters()) + paramsCount - 1;
      for (word_t i = 0; i < paramsCount; i++, param--) {
        if (bitmap.at(paramsOffset + i)) {
          self()->visitPointer(param);
        }
      }
    }

    // Update the pc for the next frame.
    pcOffset = frame.callerPcOffset();
  }
}


template <class BlockVisitor>
void BlockVisitorBase<BlockVisitor>::visitTaggedArray(TaggedArray* array) {
  for (word_t i = 0, n = array->length(); i < n; i++) {
    Tagged<Block>* taggedSlot = array->elements() + i;
    if (taggedSlot->isPointer()) {
      Block** slot = reinterpret_cast<Block**>(taggedSlot);
      self()->visitPointer(slot);
    }
  }
}

}
}

#endif

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef block_inl_h
#define block_inl_h

#include "block.h"

#include "memory.h"
#include "roots-inl.h"

namespace codeswitch {
namespace internal {

Meta* Block::meta() {
  // TODO: maybe make this Tagged.
  word_t addressOrType = metaWord() & ~kGCBitMask;
  if (addressOrType < Heap::kMinAddress) {
    auto type = static_cast<BlockType>(addressOrType >> kGCBitCount);
    auto meta = getVM()->roots()->getMetaForBlockType(type);
    return meta;
  } else {
    return reinterpret_cast<Meta*>(addressOrType);
  }
}


void Block::setMeta(Meta* meta) {
  auto metaWord = reinterpret_cast<word_t>(meta) | gcBits();
  setMetaWord(metaWord);
}


void Block::setMeta(BlockType type) {
  auto metaWord = static_cast<word_t>(type) << kGCBitCount | gcBits();
  setMetaWord(metaWord);
}


bool Block::hasEncodedMeta() {
  word_t mw = metaWord() & ~kGCBitMask;
  return mw < Heap::kMinAddress;
}


BlockType Block::getMetaEncoding() {
  ASSERT(hasEncodedMeta());
  return static_cast<BlockType>(metaWord() >> kGCBitCount);
}


u8 Block::gcBits() {
  return static_cast<u8>(metaWord()) & kGCBitMask;
}


void Block::setGcBits(u8 bits) {
  ASSERT((bits & kGCBitMask) == bits);
  word_t mw = metaWord();
  mw = (mw & ~kGCBitMask) | bits;
  setMetaWord(mw);
}


VM* Block::getVM() {
  Chunk* page = Chunk::fromAddress(this);
  return page->vm();
}


Heap* Block::getHeap() {
  return getVM()->heap();
}

}
}

#endif

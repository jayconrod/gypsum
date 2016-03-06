// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef field_h
#define field_h

#include <iostream>
#include "block.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

class Name;
class Type;

class Field: public Block {
 public:
  static const BlockType kBlockType = FIELD_BLOCK_TYPE;

  void* operator new (size_t, Heap* heap);
  Field(Name* name, String* sourceName, u32 flags, Type* type, u32 index, u32 offset);
  static Local<Field> create(
      Heap* heap,
      const Handle<Name>& name,
      const Handle<String>& sourceName,
      u32 flags,
      const Handle<Type>& type,
      u32 index,
      u32 offset);

  Name* name() const { return name_.get(); }
  String* sourceName() const { return sourceName_.get(); }
  u32 flags() const { return flags_; }
  Type* type() const { return type_.get(); }
  u32 index() const { return index_; }
  u32 offset() const { return offset_; }

 private:
  DECLARE_POINTER_MAP()

  Ptr<Name> name_;
  Ptr<String> sourceName_;
  u32 flags_;
  Ptr<Type> type_;
  u32 index_;
  u32 offset_;
  // Update FIELD_POINTER_LIST if pointer members change.
};

std::ostream& operator << (std::ostream& os, const Field* field);

}
}

#endif

// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef object_h
#define object_h

#include <iostream>
#include "block.h"

namespace codeswitch {
namespace internal {

class Class;
class DefnId;
class Field;
class Function;
class Type;

class Object: public Block {
 public:
  static const BlockType kBlockType = OBJECT_BLOCK_TYPE;

  void* operator new (size_t, Heap* heap, Meta* meta);
  void* operator new (size_t, Heap* heap, Meta* meta, length_t length);
  Object();
  static Local<Object> create(Heap* heap, const Handle<Meta>& meta);
  static Local<Object> create(Heap* heap, const Handle<Meta>& meta, length_t length);

  static Local<Type> typeof(const Handle<Object>& object);
  Class* clas() const;

  Function* findMethod(DefnId methodId) const;

  u64 getRawField(const Field* field) const;
  void setRawField(const Field* field, u64 bits);

  u64 getRawElement(length_t index) const;
  void setRawElement(length_t index, u64 bits);

 protected:
  explicit Object(BlockType blockType);
};

std::ostream& operator << (std::ostream& os, const Object* obj);

}
}

#endif

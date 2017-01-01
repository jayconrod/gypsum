// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef global_h
#define global_h

#include "block.h"
#include "defnid.h"
#include "heap.h"

namespace codeswitch {
namespace internal {

class Name;
class Object;
class Type;

class Global: public Block {
 public:
  static const BlockType kBlockType = GLOBAL_BLOCK_TYPE;

  DEFINE_NEW(Global)
  Global(DefnId id, Name* name, String* sourceName, u32 flags, Type* type);
  static Local<Global> create(Heap* heap, DefnId id, const Handle<Name>& name,
                              const Handle<String>& sourceName, u32 flags,
                              const Handle<Type>& type);

  DefnId id() const { return id_; }
  Name* name() const { return name_.get(); }
  String* sourceName() const { return sourceName_.get(); }
  u32 flags() const { return flags_; }
  Type* type() const { return type_.get(); }

  bool isObject() const;
  Object* getObject() const;
  void setObject(Object* value);
  Object** objectSlot();

  bool isPrimitive() const;
  i64 getPrimitive() const;
  void setPrimitive(i64 value);

  i64 getRaw() const;
  void setRaw(i64 value);

 private:
  DECLARE_POINTER_MAP()

  const DefnId id_;
  Ptr<Name> name_;
  Ptr<String> sourceName_;
  u32 flags_;
  Ptr<Type> type_;
  union {
    i64 primitive;
    word_t object;   // can't use Ptr since this must be POD
  } value_;
  // Update GLOBAL_POINTER_LIST if pointer members change.
  // Note that the value is not included in the pointer map, since it is not always a pointer.
};


std::ostream& operator << (std::ostream& os, const Global* global);


}
}

#endif

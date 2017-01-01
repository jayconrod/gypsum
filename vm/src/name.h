// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef name_h
#define name_h

#include <iostream>
#include <string>
#include "block.h"

namespace codeswitch {
namespace internal {

class String;

class Name: public Block {
 public:
  static const BlockType kBlockType = NAME_BLOCK_TYPE;

  static const length_t kMaxComponentLength = 1000;
  static const length_t kMaxComponentCount = 100;

  DEFINE_NEW(Name)
  explicit Name(BlockArray<String>* components);
  static Local<Name> create(Heap* heap, const Handle<BlockArray<String>>& components);

  enum NameUsage {
    DEFN_NAME,
    PACKAGE_NAME
  };

  static Local<Name> fromString(Heap* heap, const Handle<String>& nameString, NameUsage usage);
  static Local<String> toString(Heap* heap, const Handle<Name>& name);
  std::string toStlString();

  BlockArray<String>* components() const { return components_.get(); }

  int compare(const Name* other) const;
  bool equals(const Name* other) const { return compare(other) == 0; }
  u32 hashCode() const;

 private:
  DECLARE_POINTER_MAP()

  Ptr<BlockArray<String>> components_;
  // Update NAME_POINTER_LIST if pointers change.
};

std::ostream& operator << (std::ostream& os, const Name* packageName);

}
}

#endif

// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef class_h
#define class_h

#include "block.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class BlockArray;
class Field;
class Function;
class Package;
template <class T>
class TaggedArray;
class Type;

class Class: public Block {
 public:
  void* operator new(size_t, Heap* heap);
  Class(u32 flags,
        Type* supertype,
        BlockArray<Field>* fields,
        Type* elementType,
        WordArray* constructors,
        WordArray* methods,
        Package* package,
        Meta* instanceMeta);
  static Local<Class> create(Heap* heap);
  static Local<Class> create(Heap* heap,
                             u32 flags,
                             const Handle<Type>& supertype,
                             const Handle<BlockArray<Field>>& fields,
                             const Handle<Type>& elementType,
                             const Handle<WordArray>& constructors,
                             const Handle<WordArray>& methods,
                             const Handle<Package>& package,
                             const Handle<Meta>& instanceMeta);

  void printClass(FILE* out);
  DEFINE_CAST(Class)

  // Most members can be set after construction, even though we would like to consider Class
  // as immutable. This is necessary since Class and Type have a cyclic relationship. We may
  // need to allocate empty Class objects early, then fill them after other objects which
  // refer to them have been allocated.

  u32 flags() const { return flags_; }
  void setFlags(u32 flags) { flags_ = flags; }

  DEFINE_INL_PTR_ACCESSORS2(Type*, supertype, setSupertype)

  DEFINE_INL_PTR_ACCESSORS2(BlockArray<Field>*, fields, setFields)
  word_t findFieldIndex(word_t offset) const;
  word_t findFieldOffset(word_t index) const;

  DEFINE_INL_PTR_ACCESSORS2(Type*, elementType, setElementType)

  DEFINE_INL_PTR_ACCESSORS2(WordArray*, constructors, setConstructors)
  Function* getConstructor(word_t index) const;

  DEFINE_INL_PTR_ACCESSORS2(WordArray*, methods, setMethods)
  Function* getMethod(word_t index) const;

  DEFINE_INL_PTR_ACCESSORS2(Package*, package, setPackage)
  DEFINE_INL_PTR_ACCESSORS2(Meta*, instanceMeta, setInstanceMeta)

  /** Constructs a new instance Meta whether one already exists or not. Does not use handles
   *  or invoke the garbage collector. This is used by Roots, since GC is not available there.
   *  `ensureAndGetInstaceMeta` should be called normally.
   */
  Meta* buildInstanceMeta();
  static Local<Meta> ensureAndGetInstanceMeta(const Handle<Class>& clas);
  static void ensureInstanceMeta(const Handle<Class>& clas);

  bool isSubclassOf(Class* other) const;

 private:
  void computeSizeAndPointerMap(u32* size, bool* hasPointers, BitSet* pointerMap) const;
  void computeSizeAndPointerMapForType(Type* type, u32* size,
                                       bool* hasPointers, BitSet* pointerMap) const;

  DECLARE_POINTER_MAP()
  u32 flags_;
  Type* supertype_;
  BlockArray<Field>* fields_;
  Type* elementType_;
  WordArray* constructors_;
  WordArray* methods_;
  Package* package_;
  Meta* instanceMeta_;
  // Update CLASS_POINTER_LIST if pointer members change.
};

}
}

#endif

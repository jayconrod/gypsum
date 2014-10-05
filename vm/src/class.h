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
class I32Array;
typedef I32Array LengthArray;
typedef I32Array IdArray;
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
        IdArray* constructors,
        IdArray* methods,
        Package* package,
        Meta* instanceMeta);
  static Local<Class> create(Heap* heap);
  static Local<Class> create(Heap* heap,
                             u32 flags,
                             const Handle<Type>& supertype,
                             const Handle<BlockArray<Field>>& fields,
                             const Handle<Type>& elementType,
                             const Handle<IdArray>& constructors,
                             const Handle<IdArray>& methods,
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
  length_t findFieldIndex(word_t offset) const;
  word_t findFieldOffset(length_t index) const;

  DEFINE_INL_PTR_ACCESSORS2(Type*, elementType, setElementType)

  DEFINE_INL_PTR_ACCESSORS2(IdArray*, constructors, setConstructors)
  Function* getConstructor(length_t index) const;

  DEFINE_INL_PTR_ACCESSORS2(IdArray*, methods, setMethods)
  Function* getMethod(length_t index) const;

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
  IdArray* constructors_;
  IdArray* methods_;
  Package* package_;
  Meta* instanceMeta_;
  // Update CLASS_POINTER_LIST if pointer members change.
};

}
}

#endif

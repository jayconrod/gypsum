// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef class_h
#define class_h

#include <iostream>
#include "block.h"
#include "defnid.h"
#include "hash-table.h"
#include "object-type-defn.h"
#include "utils.h"

namespace codeswitch {
namespace internal {

template <class T>
class BlockArray;
class Field;
class Function;
class I32Array;
typedef I32Array LengthArray;
class Package;
class PackageIdArray;
class Name;
template <class T>
class TaggedArray;
class Type;
class TypeParameter;

class Class: public ObjectTypeDefn {
 public:
  static const BlockType kBlockType = CLASS_BLOCK_TYPE;

  /**
   * Size in bytes of the metadata at the beginning of each instance of a `Class`.
   *
   * The first field of an object can be found at this offset.
   */
  static const u32 kObjectMetaSize = kWordSize;

  void* operator new(size_t, Heap* heap);
  Class(DefnId id,
        Name* name,
        String* sourceName,
        u32 flags,
        BlockArray<TypeParameter>* typeParameters,
        BlockArray<Type>* supertypes,
        BlockArray<Field>* fields,
        BlockArray<Function>* constructors,
        BlockArray<Function>* methods,
        Package* package,
        Meta* instanceMeta = nullptr,
        Type* elementType = nullptr);
  static Local<Class> create(Heap* heap, DefnId id);
  static Local<Class> create(Heap* heap,
                             DefnId id,
                             const Handle<Name>& name,
                             const Handle<String>& sourceName,
                             u32 flags,
                             const Handle<BlockArray<TypeParameter>>& typeParameters,
                             const Handle<BlockArray<Type>>& supertypes,
                             const Handle<BlockArray<Field>>& fields,
                             const Handle<BlockArray<Function>>& constructors,
                             const Handle<BlockArray<Function>>& methods,
                             const Handle<Package>& package,
                             const Handle<Meta>& instanceMeta = Local<Meta>(),
                             const Handle<Type>& elementType = Local<Type>());

  // Most members can be set after construction, even though we would like to consider Class
  // as immutable. This is necessary since Class and Type have a cyclic relationship. We may
  // need to allocate empty Class objects early, then fill them after other objects which
  // refer to them have been allocated.

  DefnId id() const { return id_; }

  Name* name() const { return name_.get(); }
  void setName(Name* name) { name_.set(this, name); }

  String* sourceName() const { return sourceName_.get(); }
  void setSourceName(String* sourceName) { sourceName_.set(this, sourceName); }

  u32 flags() const { return flags_; }
  void setFlags(u32 flags) { flags_ = flags; }

  BlockArray<TypeParameter>* typeParameters() const { return typeParameters_.get(); }
  void setTypeParameters(BlockArray<TypeParameter>* newTypeParameters) {
    typeParameters_.set(this, newTypeParameters);
  }

  BlockArray<Type>* supertypes() const { return supertypes_.get(); }
  void setSupertypes(BlockArray<Type>* newSupertypes) { supertypes_.set(this, newSupertypes); }
  Type* baseClassType() const;
  Class* baseClass() const;

  BlockArray<Field>* fields() const { return fields_.get(); }
  void setFields(BlockArray<Field>* newFields) { fields_.set(this, newFields); }
  BlockArray<Field>* flatFields() const { return flatFields_.get(); }
  void setFlatFields(BlockArray<Field>* newFlatFields) { flatFields_.set(this, newFlatFields); }
  Field* findField(Name* name) const;
  length_t findFieldIndex(word_t offset) const;
  word_t findFieldOffset(length_t index) const;
  Class* findFieldClass(length_t index);

  BlockArray<Function>* constructors() const { return constructors_.get(); }
  void setConstructors(BlockArray<Function>* newConstructors) {
    constructors_.set(this, newConstructors);
  }

  BlockArray<Function>* methods() const { return methods_.get(); }
  void setMethods(BlockArray<Function>* newMethods) { methods_.set(this, newMethods); }
  BlockArray<Function>* flatMethods() const { return flatMethods_.get(); }
  void setFlatMethods(BlockArray<Function>* newFlatMethods) {
    flatMethods_.set(this, newFlatMethods);
  }
  DefnIdHashMap<Function>* methodIdIndex() const { return methodIdIndex_.get(); }
  void setMethodIdIndex(DefnIdHashMap<Function>* newMethodIdIndex) {
    methodIdIndex_.set(this, newMethodIdIndex);
  }
  Function* findMethod(DefnId methodId) const;

  Package* package() const { return package_.get(); }
  void setPackage(Package* newPackage) { package_.set(this, newPackage); }
  Meta* instanceMeta() const { return instanceMeta_.get(); }
  void setInstanceMeta(Meta* newInstanceMeta) { instanceMeta_.set(this, newInstanceMeta); }

  Type* elementType() const { return elementType_.get(); }
  void setElementType(Type* newElementType) { elementType_.set(this, newElementType); }

  BlockHashMap<Name, Field>* fieldNameIndex() const { return fieldNameIndex_.get(); }
  void setFieldNameIndex(BlockHashMap<Name, Field>* index) {
    fieldNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<Name, Field>> ensureAndGetFieldNameIndex(
      const Handle<Class>& clas);
  BlockHashMap<String, Field>* fieldSourceNameIndex() const {
    return fieldSourceNameIndex_.get();
  }
  void setFieldSourceNameIndex(BlockHashMap<String, Field>* index) {
    fieldSourceNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<String, Field>> ensureAndGetFieldSourceNameIndex(
      const Handle<Class>& clas);
  BlockHashMap<Name, Function>* methodNameIndex() const { return methodNameIndex_.get(); }
  void setMethodNameIndex(BlockHashMap<Name, Function>* index) {
    methodNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<Name, Function>> ensureAndGetMethodNameIndex(
      const Handle<Class>& clas);
  BlockHashMap<String, Function>* methodSourceNameIndex() const {
    return methodSourceNameIndex_.get();
  }
  void setMethodSourceNameIndex(BlockHashMap<String, Function>* index) {
    methodSourceNameIndex_.set(this, index);
  }
  static Local<BlockHashMap<String, Function>> ensureAndGetMethodSourceNameIndex(
      const Handle<Class>& clas);
  BlockHashMap<String, Function>* constructorSignatureIndex() const {
    return constructorSignatureIndex_.get();
  }
  void setConstructorSignatureIndex(BlockHashMap<String, Function>* index) {
    constructorSignatureIndex_.set(this, index);
  }
  static Local<BlockHashMap<String, Function>> ensureAndGetConstructorSignatureIndex(
      const Handle<Class>& clas);

  bool isSubclassOf(const Class* other) const;

  /**
   * Builds a {@link Meta} for instances of this class if it doesn't exist, then returns it.
   *
   * Note that fields and methods must be flattened in order for the {@link Meta} to be created,
   * so {@link #ensureFlatFields} and {@link #ensureFlatMethods} will be called internally.
   */
  static Local<Meta> ensureInstanceMeta(const Handle<Class>& clas);

  /**
   * Builds a list of all {@link Field}s in this class if it doesn't exist, then returns it.
   *
   * When serialized into a package file, a `Class` only includes fields it defines. It does
   * not include inherited fields, since these may change in another package and break
   * binary compatibility. We need to build the full "flat" list of fields at run-time, which
   * is what this method does.
   */
  static Local<BlockArray<Field>> ensureFlatFields(const Handle<Class>& clas);

 private:
  DECLARE_POINTER_MAP()
  const DefnId id_;
  Ptr<Name> name_;
  Ptr<String> sourceName_;
  u32 flags_;
  Ptr<BlockArray<TypeParameter>> typeParameters_;
  Ptr<BlockArray<Type>> supertypes_;
  Ptr<BlockArray<Field>> fields_;
  Ptr<BlockArray<Field>> flatFields_;
  Ptr<BlockArray<Function>> constructors_;
  Ptr<BlockArray<Function>> methods_;
  Ptr<BlockArray<Function>> flatMethods_;
  Ptr<DefnIdHashMap<Function>> methodIdIndex_;
  Ptr<Package> package_;
  Ptr<Meta> instanceMeta_;
  Ptr<Type> elementType_;
  Ptr<BlockHashMap<Name, Field>> fieldNameIndex_;
  Ptr<BlockHashMap<String, Field>> fieldSourceNameIndex_;
  Ptr<BlockHashMap<Name, Function>> methodNameIndex_;
  Ptr<BlockHashMap<String, Function>> methodSourceNameIndex_;
  Ptr<BlockHashMap<String, Function>> constructorSignatureIndex_;
  // Update CLASS_POINTER_LIST if pointer members change.
};

std::ostream& operator << (std::ostream& os, const Class* clas);

}
}

#endif

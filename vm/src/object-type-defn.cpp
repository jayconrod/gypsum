// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "object-type-defn.h"

#include <unordered_map>
#include <unordered_set>
#include "array.h"
#include "class.h"
#include "defnid.h"
#include "function.h"
#include "heap.h"
#include "trait.h"
#include "type.h"
#include "type-parameter.h"

#define DELEGATE_TO_SUBCLASS(stmt) \
  if (isa<Class>(this)) { \
    auto self = block_cast<Class>(this); \
    stmt \
  } else { \
    auto self = block_cast<Trait>(this); \
    stmt \
  }

#define DELEGATE_TO_SUBCLASS_CONST(stmt) \
  if (isa<Class>(this)) { \
    auto self = const_block_cast<Class>(this); \
    stmt \
  } else { \
    auto self = const_block_cast<Trait>(this); \
    stmt \
  }


using std::pair;
using std::unordered_map;
using std::unordered_set;
using std::vector;

namespace codeswitch {
namespace internal {

DefnId ObjectTypeDefn::id() const {
  DELEGATE_TO_SUBCLASS_CONST(return self->id();)
}


BlockArray<Type>* ObjectTypeDefn::supertypes() const {
  DELEGATE_TO_SUBCLASS_CONST(return self->supertypes();)
}


void ObjectTypeDefn::setSupertypes(BlockArray<Type>* newSupertypes) {
  DELEGATE_TO_SUBCLASS(self->setSupertypes(newSupertypes);)
}


BlockArray<TypeParameter>* ObjectTypeDefn::typeParameters() const {
  DELEGATE_TO_SUBCLASS_CONST(return self->typeParameters();)
}


void ObjectTypeDefn::setTypeParameters(BlockArray<TypeParameter>* newTypeParameters) {
  DELEGATE_TO_SUBCLASS(self->setTypeParameters(newTypeParameters);)
}


TypeParameter* ObjectTypeDefn::typeParameter(length_t index) const {
  return typeParameters()->get(index);
}


length_t ObjectTypeDefn::typeParameterCount() const {
  return typeParameters()->length();
}


BlockArray<Function>* ObjectTypeDefn::methods() const {
  DELEGATE_TO_SUBCLASS_CONST(return self->methods();)
}


void ObjectTypeDefn::setMethods(BlockArray<Function>* newMethods) {
  DELEGATE_TO_SUBCLASS(self->setMethods(newMethods);)
}


BlockArray<Function>* ObjectTypeDefn::flatMethods() const {
  DELEGATE_TO_SUBCLASS_CONST(return self->flatMethods();)
}


void ObjectTypeDefn::setFlatMethods(BlockArray<Function>* newFlatMethods) {
  DELEGATE_TO_SUBCLASS(self->setFlatMethods(newFlatMethods);)
}


class FlatMethodsBuilder {
 public:
  explicit FlatMethodsBuilder(Heap* heap)
      : heap_(heap) { }

  /**
   * Replaces overriden methods with the given method or appends the method onto the end of
   * the flat method list if it overrides nothing.
   */
  void addMethod(const Handle<Function>& method) {
    if (method->overrides() == nullptr) {
      // This method overrides nothing. Just append it.
      flatMethods_.push_back(method);
      methodIndices_[method->id()].insert(flatMethods_.size() - 1);
    } else {
      // This method overrides at least one other method. We may not have inherited the
      // overriden methods yet though because we may not have found a path to the base yet.
      bool didOverrideMethod = false;
      for (auto overrideId : getOverridenMethodIds(method)) {
        // Replace the method in our method list if we've inherited it.
        if (methodIndices_.find(overrideId) != methodIndices_.end()) {
          for (auto index : methodIndices_[overrideId]) {
            flatMethods_[index] = method;
            didOverrideMethod = false;
          }
        }
      }

      // If we did not replace anything, add the method to the end of the list.
      if (!didOverrideMethod) {
        flatMethods_.push_back(method);
        auto index = flatMethods_.size() - 1;
        for (auto overrideId : getOverridenMethodIds(method)) {
          methodIndices_[overrideId].insert(index);
        }
      }
    }
  }

  Local<BlockArray<Function>> buildFlatMethods() {
    auto flatMethodsArray = BlockArray<Function>::create(heap_, flatMethods_.size());
    for (length_t i = 0; i < flatMethods_.size(); i++) {
      flatMethodsArray->set(i, *flatMethods_[i]);
    }
    return flatMethodsArray;
  }

  Local<DefnIdHashMap<Function>> buildMethodIdIndex() {
    auto methodIdIndex = DefnIdHashMap<Function>::create(heap_);
    for (auto& entry : methodsById_) {
      DefnIdHashMap<Function>::add(methodIdIndex, entry.first, entry.second);
    }
    return methodIdIndex;
  }

 private:
  /**
   * Returns ids of the non-overriding methods that this method overrides, directly or
   * indirectly.
   *
   * If this is called on a static or non-overriding method, it just returns the method's id.
   */
  unordered_set<DefnId> getOverridenMethodIds(const Handle<Function>& method) {
    if (method->overrides() == nullptr) {
      return unordered_set<DefnId>{method->id()};
    } else {
      unordered_set<DefnId> allOverrideIds;
      auto overrides = handle(method->overrides());
      for (length_t i = 0; i < overrides->length(); i++) {
        auto overrideIds = getOverridenMethodIds(handle(overrides->get(i)));
        allOverrideIds.insert(overrideIds.begin(), overrideIds.end());
      }
      return allOverrideIds;
    }
  }

  Heap* heap_;
  vector<Local<Function>> flatMethods_;
  unordered_set<DefnId> inheritedMethodIds_;
  unordered_map<DefnId, unordered_set<size_t>> methodIndices_;
  unordered_map<DefnId, Local<Function>> methodsById_;
};


Local<BlockArray<Function>>
ObjectTypeDefn::ensureFlatMethods(const Handle<ObjectTypeDefn>& defn) {
  FlatMethodsBuilder builder(defn->getHeap());

  unordered_set<DefnId> inheritedBaseIds;
  auto supertypes = handle(defn->supertypes());
  for (length_t i = 0; i < supertypes->length(); i++) {
    auto base = handle(supertypes->get(i)->asClassOrTrait());
    if (inheritedBaseIds.find(base->id()) != inheritedBaseIds.end()) {
      continue;
    }
    inheritedBaseIds.insert(base->id());
    for (auto baseSupertype : *base->supertypes()) {
      inheritedBaseIds.insert(baseSupertype->asClassOrTrait()->id());
    }

    auto baseMethods = handle(base->flatMethods());
    for (length_t i = 0; i < baseMethods->length(); i++) {
      builder.addMethod(handle(baseMethods->get(i)));
    }
  }

  auto defnMethods = handle(defn->methods());
  for (length_t i = 0; i < defnMethods->length(); i++) {
    builder.addMethod(handle(defnMethods->get(i)));
  }

  auto flatMethods = builder.buildFlatMethods();
  defn->setFlatMethods(*flatMethods);

  if (isa<Class>(*defn)) {
    auto methodIdIndex = builder.buildMethodIdIndex();
    block_cast<Class>(*defn)->setMethodIdIndex(*methodIdIndex);
  }

  return flatMethods;
}


ObjectTypeDefn* ObjectTypeDefn::findCommonBase(ObjectTypeDefn* other) {
  // TODO: This implementation doesn't fully consider traits. It only finds bases within
  // the class tree. This matches what the compiler does. A proper implementation would find
  // common base definitions in the supertype lists and return the first one.
  auto roots = getVM()->roots();

  if (this == other) {
    return this;
  }
  auto nothingClass = roots->getBuiltinClass(BUILTIN_NOTHING_CLASS_ID);
  if (this == nothingClass) {
    return other;
  }
  if (other == nothingClass) {
    return this;
  }

  vector<ObjectTypeDefn*> selfBases = bases();
  vector<ObjectTypeDefn*> otherBases = other->bases();
  auto si = selfBases.size() - 1;
  auto oi = otherBases.size() - 1;
  if (selfBases[si] != otherBases[oi]) {
    return nullptr;
  }

  while (si > 0 && oi > 0 && selfBases[si] == otherBases[oi]) {
    si--;
    oi--;
  }
  return selfBases[si + 1];
}


vector<ObjectTypeDefn*> ObjectTypeDefn::bases() {
  vector<ObjectTypeDefn*> bases;
  bases.push_back(this);
  auto clas = this;
  while (!clas->supertypes()->isEmpty()) {
    clas = clas->supertypes()->get(0)->asClass();
    bases.push_back(clas);
  }
  return bases;
}

}
}

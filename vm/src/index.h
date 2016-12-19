// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef index_h
#define index_h

#include <string>
#include <vector>
#include "array.h"
#include "handle.h"
#include "hash-table.h"
#include "name.h"
#include "string.h"

namespace codeswitch {
namespace internal {

class Function;
class Package;

template <class K, class T, class GetKey, class Filter>
static Local<BlockHashMap<K, T>> buildIndex(
    const Handle<BlockArray<T>>& defns,
    GetKey getKey,
    Filter filter) {
  auto index = BlockHashMap<K, T>::create(defns->getHeap());
  HandleScope handleScope(defns->getVM());
  for (length_t i = 0, n = defns->length(); i < n; i++) {
    auto defn = handle(defns->get(i));
    if (filter(defn)) {
      auto key = getKey(defn);
      if (key) {
        BlockHashMap<K, T>::add(index, key, defn);
      }
    }
  }
  return index;
}


template <class T>
Local<Name> getDefnName(const Handle<T>& defn) {
  return handle(defn->name());
}


template <class T, class Filter>
static Local<BlockHashMap<Name, T>> buildNameIndex(
    const Handle<BlockArray<T>>& defns,
    Filter filter) {
  return buildIndex<Name, T>(defns, getDefnName<T>, filter);
}


template <class T>
Local<String> getDefnSourceName(const Handle<T>& defn) {
  return defn->sourceName() ? handle(defn->sourceName()) : Local<String>();
}


template <class T, class Filter>
static Local<BlockHashMap<String, T>> buildSourceNameIndex(
    const Handle<BlockArray<T>>& defns,
    Filter filter) {
  return buildIndex<String, T>(defns, getDefnSourceName<T>, filter);
}


template <class T>
bool allDefnFilter(const Handle<T>& defn) {
  return true;
}

Local<Name> mangleFunctionName(const Handle<Function>& function);

Local<Name> mangleFunctionName(const Handle<Function>& function,
                               const Handle<Package>& package);

Local<String> mangleSignature(const Handle<Function>& function);

Local<String> mangleFunctionSourceName(const Handle<Function>& function);

Local<Name> mangleName(const Handle<Name>& name, const std::string& signature);

Local<String> mangleSourceName(const Handle<String>& sourceName, const std::string& signature);

std::string buildSignature(const std::vector<Local<Type>>& types,
                           const Handle<Package>& package);

std::string demangleFunctionName(Name* name);

}
}

#endif

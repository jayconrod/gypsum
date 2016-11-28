// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef defnid_h
#define defnid_h

#include <iostream>
#include "utils.h"

namespace codeswitch {
namespace internal {

/**
 * Uniquely identifies a definition in the VM at run-time.
 *
 * A `DefnId` comprises three values: a kind (function, class, etc.), a package id, and an
 * index within the package.
 *
 * `DefnId` can be used to refer to definitions without having a pointer to them. It can also
 * be used as a hash table key.
 */
struct DefnId {
  enum Kind {
    NOT_VALID,
    GLOBAL,
    FUNCTION,
    CLASS,
    TRAIT,
    TYPE_PARAMETER,
  };

  DefnId()
      : kind(NOT_VALID), packageId(kIdNotSet), index(kIndexNotSet), isLocal(false) { }
  DefnId(Kind kind, id_t packageId, length_t index, bool isLocal = false)
      : kind(kind), packageId(packageId), index(index), isLocal(isLocal) { }

  bool isValid() const { return kind != NOT_VALID; }
  bool operator == (const DefnId& id) const {
    return kind == id.kind &&
        packageId == id.packageId &&
        index == id.index &&
        isLocal == id.isLocal;
  }
  bool operator != (const DefnId& id) const {
    return !(*this == id);
  }

  Kind kind;
  id_t packageId;
  length_t index;
  bool isLocal;
};


std::ostream& operator << (std::ostream& os, DefnId id);

}
}


namespace std {

template <>
struct hash<codeswitch::internal::DefnId> {
  std::size_t operator () (const codeswitch::internal::DefnId& id) const {
    using codeswitch::internal::hashMix;
    auto h = std::hash<int>()(static_cast<int>(id.kind));
    h = hashMix(h ^ std::hash<codeswitch::internal::id_t>()(id.packageId));
    h = hashMix(h ^ std::hash<codeswitch::internal::length_t>()(id.index));
    h = hashMix(h ^ std::hash<bool>()(id.isLocal));
    return h;
  }
};

}

#endif

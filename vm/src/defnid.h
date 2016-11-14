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
    GLOBAL,
    FUNCTION,
    CLASS,
    TRAIT,
    TYPE_PARAMETER,
  };

  DefnId(Kind kind, id_t packageId, length_t index)
      : kind(kind), packageId(packageId), index(index) { }

  static DefnId invalidGlobal() { return DefnId(GLOBAL, kIdNotSet, kLengthNotSet); }
  static DefnId invalidFunction() { return DefnId(FUNCTION, kIdNotSet, kLengthNotSet); }
  static DefnId invalidClass() { return DefnId(CLASS, kIdNotSet, kLengthNotSet); }
  static DefnId invalidTrait() { return DefnId(TRAIT, kIdNotSet, kLengthNotSet); }
  static DefnId invalidTypeParameter() {
    return DefnId(TYPE_PARAMETER, kIdNotSet, kLengthNotSet);
  }

  const Kind kind;
  const id_t packageId;
  const length_t index;
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
    return h;
  }
};

}

#endif

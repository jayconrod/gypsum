// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "defnid.h"

namespace codeswitch {
namespace internal {

using std::ostream;

ostream& operator << (ostream& os, DefnId id) {
  const char* kind;
  switch (id.kind) {
    case DefnId::GLOBAL: kind = "global"; break;
    case DefnId::FUNCTION: kind = "function"; break;
    case DefnId::CLASS: kind = "class"; break;
    case DefnId::TRAIT: kind = "trait"; break;
    case DefnId::TYPE_PARAMETER: kind = "type-parameter"; break;
    default: kind = "invalid";
  }
  os << kind << '(' << id.packageId << ", " << id.index << ", " << id.isLocal << ')';
  return os;
}

}
}

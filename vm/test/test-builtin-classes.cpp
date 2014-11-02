// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include "block.h"
#include "handle.h"
#include "heap.h"
#include "string.h"
#include "type.h"
#include "vm.h"

using namespace codeswitch::internal;

TEST(StringSize) {
  VM vm;
  AllowAllocationScope allowAllocation(vm.heap(), true);
  HandleScope handleScope(&vm);
  auto str = String::fromUtf8CString(vm.heap(), "");
  ASSERT_EQ(0, str->elementsLength());
  ASSERT_EQ(str->sizeOfBlock(), sizeof(String));
}


TEST(TypeSize) {
  VM vm;
  Persistent<Type> ty(Type::i8Type(vm.roots()));
  ASSERT_EQ(0, ty->elementsLength());
  ASSERT_EQ(ty->sizeOfBlock(), sizeof(Type));
}

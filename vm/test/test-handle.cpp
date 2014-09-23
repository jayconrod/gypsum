// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <algorithm>
#include <memory>
#include "block.h"
#include "handle.h"
#include "heap.h"
#include "roots.h"

using namespace std;
using namespace codeswitch::internal;


static int countHandles(HandleStorage& storage) {
  return count_if(storage.begin(), storage.end(), [](Block** b) { return true; });
}


TEST(HandleScopeAndLocals) {
  VM vm(0);
  int vmHandles = countHandles(vm.handleStorage());
  {
    HandleScope scopeA(&vm);
    Local<Block> a(vm.roots()->metaMeta());
    ASSERT_EQ(vmHandles + 1, countHandles(vm.handleStorage()));
    {
      HandleScope scopeB(&vm);
      Local<Block> b(vm.roots()->metaMeta());
      ASSERT_EQ(vmHandles + 2, countHandles(vm.handleStorage()));
    }
    ASSERT_EQ(vmHandles + 1, countHandles(vm.handleStorage()));
  }
  ASSERT_EQ(vmHandles, countHandles(vm.handleStorage()));
}


TEST(PersistentHandles) {
  VM vm(0);
  int vmHandles = countHandles(vm.handleStorage());
  auto a = new Persistent<Block>(vm.roots()->metaMeta());
  ASSERT_EQ(vmHandles + 1, countHandles(vm.handleStorage()));
  auto b = new Persistent<Block>(vm.roots()->metaMeta());
  ASSERT_EQ(vmHandles + 2, countHandles(vm.handleStorage()));
  delete a;
  ASSERT_EQ(vmHandles + 1, countHandles(vm.handleStorage()));
  delete b;
  ASSERT_EQ(vmHandles, countHandles(vm.handleStorage()));
}

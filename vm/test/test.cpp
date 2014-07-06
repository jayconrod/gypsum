// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

TestBase* TestBase::testHead_ = NULL;

TestBase::TestBase(const char* name)
    : name_(name) {
  next_ = testHead_;
  testHead_ = this;
}

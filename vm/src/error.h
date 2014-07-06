// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef error_h
#define error_h

namespace codeswitch {
namespace internal {

class Error {
 public:
  Error(const char* message)
      : message_(message) { }

  const char* message() { return message_; }
  
 private:
   const char* message_;
};

}
}

#endif

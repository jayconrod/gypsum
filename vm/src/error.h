// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef error_h
#define error_h

#include <string>

namespace codeswitch {
namespace internal {

class Error {
 public:
  Error(const std::string& message)
      : message_(message) { }

  std::string message() const { return message_; }

 private:
  std::string message_;
};

}
}

#endif

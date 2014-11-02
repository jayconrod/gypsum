// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "utils.h"

#include <iostream>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>

using namespace std;

namespace codeswitch {
namespace internal {

void abort(const char* fileName, int lineNumber, const char* reason, ...) {
  va_list ap;
  va_start(ap, reason);
  char buffer[512];
  snprintf(buffer, sizeof(buffer), "%s: %d: ", fileName, lineNumber);
  cerr << buffer;
  vsnprintf(buffer, sizeof(buffer), reason, ap);
  cerr << buffer << endl;
  va_end(ap);
  ::abort();
}


u32 utf8Decode(const u8* bytes, const u8* end) {
  return utf8Decode(&bytes, end);
}


u32 utf8Decode(const u8** bytes, const u8* end) {
  if (*bytes == end)
    return UTF8_DECODE_ERROR;
  u8 byte = *(*bytes)++;
  if ((byte & 0x80) == 0) {
    return static_cast<u32>(byte);
  }
  int byteCount = 0;
  u32 codePoint = 0;
  if ((byte & 0xe0) == 0xc0) {
    byteCount = 2;
    codePoint = byte & 0x1f;
  } else if ((byte & 0xf0) == 0xe0) {
    byteCount = 3;
    codePoint = byte & 0xf;
  } else if ((byte & 0xf8) == 0xf0) {
    byteCount = 4;
    codePoint = byte & 0x7;
  } else {
    return UTF8_DECODE_ERROR;
  }
  while (byteCount > 1) {
    if (*bytes == end)
      return UTF8_DECODE_ERROR;
    byteCount--;
    byte = *(*bytes)++;
    if ((byte & 0xc0) != 0x80)
      return UTF8_DECODE_ERROR;
    codePoint = (codePoint << 6) | (byte & 0x3f);
  }
  return codePoint;
}


word_t utf8EncodeSize(u32 codePoint) {
  if (codePoint < 0x80) {
    return 1;
  } else if (codePoint < 0x800) {
    return 2;
  } else if (codePoint < 0x10000) {
    return 3;
  } else {
    ASSERT(codePoint <= 0x10fff);
    return 4;
  }
}


void utf8Encode(u32 codePoint, u8** bytes) {
  word_t size = utf8EncodeSize(codePoint);
  if (size == 1) {
    *(*bytes)++ = static_cast<u8>(codePoint);
  } else {
    word_t shift = (size - 1) * 6;
    u8 topBits = static_cast<u8>(codePoint >> shift);
    if (size == 2) {
      *(*bytes)++ = 0xc0 | topBits;
    } else if (size == 3) {
      *(*bytes)++ = 0xe0 | topBits;
    } else {
      ASSERT(size == 4);
      *(*bytes)++ = 0xf0 | topBits;
    }
    while (shift > 0) {
      shift -= 6;
      u8 bits = static_cast<u8>((codePoint >> shift) & 0x3f);
      *(*bytes)++ = 0x80 | bits;
    }
  }
}

}
}

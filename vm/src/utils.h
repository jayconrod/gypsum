// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef utils_h
#define utils_h

#include <cstddef>
#include <cstdint>

namespace codeswitch {
namespace internal {

typedef unsigned char u8;
typedef signed char i8;
typedef unsigned short u16;
typedef signed short i16;
typedef unsigned int u32;
typedef signed int i32;
typedef unsigned long long u64;
typedef signed long long i64;
typedef float f32;
typedef double f64;

class I64Array;
class I32Array;
#if WORDSIZE == 64
typedef unsigned long long word_t;
typedef I64Array WordArray;
#elif WORDSIZE == 32
typedef unsigned long word_t;
typedef I32Array WordArray;
#else
#error WORDSIZE must be 32 or 64
#endif
typedef word_t Address;
const int kWordSize = WORDSIZE / 8;
const int kBitsInWord = WORDSIZE;
const word_t kNotSet = static_cast<word_t>(-1);
const word_t kNotFound = static_cast<word_t>(-1);
void* const kFailed = reinterpret_cast<void*>(1);
void* const kUninitialized = nullptr;

const int KB = 1024;
const int MB = 1024 * KB;
const int GB = 1024 * MB;

void abort(const char* fileName, int lineNumber, const char* reason, ...);

#define CHECK(cond) \
do { \
  if (!(cond)) { \
    ABORT("assertion failed: " #cond); \
  } \
} while (0)

#define ASSERT(cond) CHECK(cond)

#define UNREACHABLE() ABORT("unreachable code")

#define UNIMPLEMENTED() ABORT("unimplemented code")

#define ABORT(reason) \
abort(__FILE__, __LINE__, (reason))


#define NON_COPYABLE(className) \
 private: \
  className(const className& copy); \
  className& operator = (const className& copy);


#define USE(e) (void) (e)


#define ARRAY_LENGTH(a) (sizeof(a) / sizeof((a)[0]))


template <class T>
T min(T a, T b) {
  return a <= b ? a : b;
}


template <class T>
T max(T a, T b) {
  return a > b ? a : b;
}


template <class T>
T& mem(Address base, word_t offset = 0, word_t index = 0) {
  return *reinterpret_cast<T*>(base + offset + index * sizeof(T));
}


template <class T>
T& mem(void* base, word_t offset = 0, word_t index = 0) {
  return mem<T>(reinterpret_cast<Address>(base), offset, index);
}


template <class T>
T mem(const void* base, word_t offset = 0, word_t index = 0) {
  return mem<T>(reinterpret_cast<Address>(base), offset, index);
}


inline word_t isPowerOf2(word_t n) {
  return (n & (n - 1UL)) == 0;
}


inline word_t align(word_t n, word_t alignment) {
  ASSERT(isPowerOf2(alignment));
  return (n + alignment - 1UL) & ~(alignment - 1UL);
}


inline bool isAligned(word_t n, word_t alignment) {
  return (n & (alignment - 1UL)) == 0;
}


inline bool bit(word_t n, word_t bit) {
  return (n & (1UL << bit)) != 0;
}


inline word_t bitExtract(word_t n, word_t width, word_t shift) {
  return (n >> shift) & ((1UL << width) - 1UL);
}


inline word_t bitInsert(word_t n, word_t value, word_t width, word_t shift) {
  word_t mask = ((1UL << width) - 1UL) << shift;
  return (n & ~mask) | ((value << shift) & mask);
}


inline u32 f32ToBits(f32 value) {
  return *reinterpret_cast<u32*>(&value);
}


inline f32 f32FromBits(u32 bits) {
  return *reinterpret_cast<f32*>(&bits);
}


inline u64 f64ToBits(f64 value) {
  return *reinterpret_cast<u64*>(&value);
}


inline f64 f64FromBits(u64 bits) {
  return *reinterpret_cast<f64*>(&bits);
}


const u32 UTF8_DECODE_ERROR = 0xffffffffU;
u32 utf8Decode(const u8** bytes, const u8* end);
u32 utf8Decode(const u8* bytes, const u8* end);
word_t utf8EncodeSize(u32 codePoint);
void utf8Encode(u32 codePoint, u8** bytes);


#define DECLARE_ACCESSORS(type, getter, setter) \
  inline type getter(); \
  inline void setter(type);


#define DEFINE_INL_ACCESSORS(type, getter, setter, offset) \
  type getter() { return mem<type>(this, offset); } \
  void setter(type _newValue) { mem<type>(this, offset) = _newValue; }


#define DEFINE_INL_PTR_ACCESSORS(type, getter, setter, offset) \
  type getter() { return mem<type>(this, offset); } \
  void setter(type _newValue) { \
    type* p = &mem<type>(this, offset); \
    *p = _newValue; \
    Heap::recordWrite(p, _newValue); \
  }


#define DEFINE_INL_BIT_ACCESSORS(type, getter, setter, offset, width, shift) \
  type getter() { \
    word_t w = mem<word_t>(this, offset); \
    return static_cast<type>(bitExtract(w, width, shift)); \
  } \
  void setter(type _newValue) { \
    word_t w = mem<word_t>(this, offset); \
    mem<word_t>(this, offset) = bitInsert(w, static_cast<word_t>(_newValue), width, shift); \
  }


#define DEFINE_INL_BOOL_ACCESSORS(getter, setter, offset, shift) \
  bool getter() { \
    word_t w = mem<word_t>(this, offset); \
    return static_cast<bool>(bitExtract(w, 1, shift)); \
  } \
  void setter(bool _newValue) { \
    word_t w = mem<word_t>(this, offset); \
    mem<word_t>(this, offset) = bitInsert(w, static_cast<word_t>(_newValue), 1, shift); \
  }


#define DEFINE_INL_INDEX_ACCESSORS(type, getter, setter, index) \
  type getter() { return get(index); } \
  void setter(type _newValue) { set(index, _newValue); }


#define DEFINE_INL_ENTRY_ACCESSORS(type, getter, setter, headerLength, entryLength, entryIndex)\
  type getter(word_t index) { \
    return get(headerLength + index * entryLength + entryIndex); \
  } \
  void setter(word_t index, type _newValue) { \
    set(headerLength + index * entryLength + entryIndex, _newValue); \
  }


// Printing formats for words
#if WORDSIZE == 32
#define WFD "%d"
#define WFX "%x"
#else
#define WFD "%lld"
#define WFX "%llx"
#endif


// Dummy values for filling memory that shouldn't be read.
const u8 kGarbageByte = 0xA5;
#if WORDSIZE == 32
const word_t kGarbageWord = 0xDEADBEEF;
#else
const word_t kGarbageWord = 0xDEADBEEF0BADC0DEULL;
#endif

}
}

#endif

// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef utils_h
#define utils_h

#include <string>
#include <vector>

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
const size_t kWordSize = WORDSIZE / 8;
const int kBitsInWord = WORDSIZE;
const word_t kNotSet = static_cast<word_t>(-1);
const word_t kNotFound = static_cast<word_t>(-1);
void* const kFailed = reinterpret_cast<void*>(1);
void* const kUninitialized = nullptr;
typedef u32 length_t;
const length_t kMaxLength = 0x7fffffffu;
const length_t kLengthNotSet = ~0u;
const length_t kIndexNotSet = ~0u;
const length_t kPcNotSet = ~0u;
typedef i32 id_t;
const id_t kIdNotSet = -1;

const id_t kLocalPackageId = -1;
const id_t kBuiltinPackageId = -2;
struct DefnId {
  id_t packageId;
  length_t defnIndex;
};

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

#define USE(e) (void) (e)

#define ASSERT(cond) CHECK(cond)

#define UNREACHABLE() ABORT("unreachable code")

#define UNIMPLEMENTED() ABORT("unimplemented code")

#define ABORT(reason) \
abort(__FILE__, __LINE__, (reason))


#define STATIC_ASSERT(cond) static_assert(cond, #cond)

#define CHECK_SUBTYPE_VALUE(type, value) \
do { if (false) { type _t = (value); USE(_t); } } while (0)


#define NON_COPYABLE(className) \
  className(const className&) = delete; \
  className(const className&&) = delete; \
  className& operator = (const className&) = delete;


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
bool inRange(T x, T low, T high) {
  return low <= x && x <= high;
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
const T& mem(const void* base, word_t offset = 0, word_t index = 0) {
  return mem<T>(reinterpret_cast<Address>(base), offset, index);
}


inline word_t isPowerOf2(word_t n) {
  return (n & (n - 1UL)) == 0;
}


template <class T>
inline T roundUpToPowerOf2(T n) {
  if (n == 0)
    return 0;
  T p = 1;
  while (p < n)
    p <<= 1;
  return p;
}


constexpr inline word_t align(word_t n, word_t alignment) {
  return (n + alignment - 1UL) & ~(alignment - 1UL);
}


constexpr inline word_t alignDown(word_t n, word_t alignment) {
  return n & ~(alignment - 1UL);
}


constexpr inline bool isAligned(word_t n, word_t alignment) {
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


template <typename T>
length_t toLength(T n) {
  auto len = static_cast<length_t>(n);
  ASSERT((0 <= len && len <= kMaxLength) || len == kLengthNotSet);
  return len;
}


inline u32 f32ToBits(f32 value) {
  union {
    f32 from;
    u32 to;
  } cast;
  cast.from = value;
  return cast.to;
}


inline f32 f32FromBits(u32 bits) {
  union {
    u32 from;
    f32 to;
  } cast;
  cast.from = bits;
  return cast.to;
}


inline u64 f64ToBits(f64 value) {
  union {
    f64 from;
    u64 to;
  } cast;
  cast.from = value;
  return cast.to;
}


inline f64 f64FromBits(u64 bits) {
  union {
    u64 from;
    f64 to;
  } cast;
  cast.from = bits;
  return cast.to;
}


inline size_t hashMix(size_t n) {
  n = (n ^ 61) ^ (n >> 16);
  n = n + (n << 3);
  n = n ^ (n >> 4);
  n = n * 0x27d4eb2d;
  n = n ^ (n >> 15);
  return n;
}


std::vector<std::string> split(const std::string& str, char delim);


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


// TODO: replace the one above with this
#define DEFINE_INL_ACCESSORS2(type, name, setter) \
  type name() const { return name##_; }           \
  void setter(type _value) { name##_ = _value; }  \


#define DEFINE_INL_PTR_ACCESSORS(type, getter, setter, offset) \
  type getter() { return mem<type>(this, offset); } \
  void setter(type _newValue) { \
    type* p = &mem<type>(this, offset); \
    *p = _newValue; \
    Heap::recordWrite(p, _newValue); \
  }


// TODO: replace the one above with this
#define DEFINE_INL_PTR_ACCESSORS2(type, name, setter) \
  type name() const { return name##_; }               \
  void setter(type _newValue) {                       \
    name##_ = _newValue;                              \
    Heap::recordWrite(&name##_, _newValue);           \
  }                                                   \


#define DEFINE_INL_CAST_ACCESSORS(type, rawType, getter, setter, offset) \
  type getter() { return static_cast<type>(mem<rawType>(this, offset); } \
  void setter(type _newValue) { mem<rawType>(this, offset) = static_cast<rawType>(_newValue); }


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


// Pointer maps are bit maps used by the garbage collector to identify pointers within a block.
// They are stored in metas. C++ classes which abstract objects on the garbage-collected heap
// usually define a static word_t which can be used to build metas for these classes. This is
// possible since the offsets of pointers in these classes are known at compile-time.
// Constructing the bit maps by hand is error-prone, so these macros help.

// Call this in the private section of a class definition which needs a pointer map.
#define DECLARE_POINTER_MAP()      \
  friend class Roots;              \
  static const word_t kPointerMap; \
  static word_t buildPointerMap(); \


// Used by DEFINE_POINTER_MAP below. Don't call directly.
#define POINTER_MAP_BIT(className, fieldName) \
  | (1 << (offsetof(className, fieldName) / kWordSize))


// Call this in a .cpp file. The second argument should be a macro of the form:
//   #define FOO_POINTER_LIST(F)
//     F(Foo, ptrA_)
//     F(Foo, ptrB_)
// (backslashes elided to keep compiler happy).
#define DEFINE_POINTER_MAP(className, POINTER_LIST)                 \
word_t className::buildPointerMap() {                               \
  return 0 POINTER_LIST(POINTER_MAP_BIT);                           \
}                                                                   \
const word_t className::kPointerMap = className::buildPointerMap(); \


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

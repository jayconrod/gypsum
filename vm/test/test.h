// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef test_h
#define test_h

#include <string>
#include <exception>

class TestException: public std::exception {
 public:
  TestException(const std::string& message)
      : message_(message) { }
  virtual ~TestException() throw() {}

  const std::string& message() const { return message_; }
  virtual const char* what() const throw() { return message_.c_str(); }

 private:
  std::string message_;
};


class TestBase {
 public:
  TestBase(const char* name);

  const char* name() const { return name_; }

  virtual void setUp() { }
  virtual void tearDown() { }
  virtual void test() = 0;

  static TestBase* head() { return testHead_; }
  TestBase* next() { return next_; }

 private:
  TestBase* next_;
  static TestBase* testHead_;

  const char* name_;
};


inline void assertTrue(bool result, const std::string& message) {
  if (!result)
    throw TestException(message);
}

inline void assertFalse(bool result, const std::string& message) {
  if (result)
    throw TestException(message);
}

inline void assertEquals(bool equals, const std::string& message) {
  if (!equals)
    throw TestException(message);
}

#define S(x) #x
#define S_(x) S(x)
#define MESSAGE(msg) __FILE__ ":" S_(__LINE__) ": " msg


#define ASSERT_TRUE(cond) assertTrue((cond), MESSAGE(#cond))
#define ASSERT_FALSE(cond) assertFalse((cond), MESSAGE(#cond))
#define ASSERT_EQ(expected, actual) assertEquals((expected) == (actual), \
    MESSAGE(#expected " == " #actual))

#define TEST(name) \
class Test ## name : public TestBase { \
 public: \
  Test ## name () \
      : TestBase(#name) { } \
  virtual void test(); \
}; \
Test ## name name ## Instance; \
void Test ## name ::test()

#endif

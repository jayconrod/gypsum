// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <iostream>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#include <codeswitch.h>

#include "test.h"

using std::cout;
using std::endl;
using std::make_tuple;
using std::string;
using std::stringstream;
using std::vector;

using codeswitch::Function;
using codeswitch::Package;
using codeswitch::Name;
using codeswitch::NativeFunctionSearch;
using codeswitch::Object;
using codeswitch::SEARCH_REGISTERED_FUNCTIONS;
using codeswitch::SEARCH_LIBRARY_FUNCTIONS;
using codeswitch::SEARCH_LINKED_FUNCTIONS;
using codeswitch::String;
using codeswitch::VMOptions;
using codeswitch::VM;


static string dirName(const string& path) {
  auto pos = path.find_last_of('/');
  return pos != string::npos ? path.substr(0, pos) : path;
}


static int64_t registered_f(VM* vm) {
  return 56;
}


static int64_t integerParamsFunction(VM* vm, int64_t a, int64_t b) {
  return a + b;
}


static double floatParamsFunction(VM* vm, double a, double b) {
  return a + b;
}


static String stringParamsFunction(VM* vm, String a, String b) {
  return a + b;
}


static Object nullObjectFunction(VM* vm) {
  return Object();
}


static String manyParametersFunction(VM* vm,
    int64_t a, double b, String c,
    int64_t d, double e, String f,
    int64_t g, double h, String i,
    int64_t j, double k, String l,
    int64_t m, double n, String o,
    int64_t p, double q, String r,
    int64_t s, double t, String u,
    int64_t v, double w, String x) {
  stringstream ss;
  ss << a << ' ';
  ss << b << ' ';
  ss << c.toStdString() << ' ';
  ss << d << ' ';
  ss << e << ' ';
  ss << f.toStdString() << ' ';
  ss << g << ' ';
  ss << h << ' ';
  ss << i.toStdString() << ' ';
  ss << j << ' ';
  ss << k << ' ';
  ss << l.toStdString() << ' ';
  ss << m << ' ';
  ss << n << ' ';
  ss << o.toStdString() << ' ';
  ss << p << ' ';
  ss << q << ' ';
  ss << r.toStdString() << ' ';
  ss << s << ' ';
  ss << t << ' ';
  ss << u.toStdString() << ' ';
  ss << v << ' ';
  ss << w << ' ';
  ss << x.toStdString();
  String result(*vm, ss.str());
  return result;
}


static Function* recursiveGypsumPtr = nullptr;
static String recursiveNativeFunction(VM* vm, int64_t n, String a, String b, String result) {
  if (n == 0) {
    return result;
  } else {
    return recursiveGypsumPtr->callForString(
        n - 1, String(*vm, "foo"), String(*vm, "bar"),
        result + "," + a + b);
  }
}


int main(int argc, char* argv[]) {
  string programPath(argv[0]);
  auto programDir = dirName(programPath);

  VMOptions vmOptions;
  vmOptions.packageSearchPaths.push_back(programDir);
  vmOptions.nativeFunctions.push_back(
      make_tuple("NativeCalls.registered", "f", reinterpret_cast<void(*)()>(registered_f)));
  vmOptions.nativeFunctions.push_back(
      make_tuple("NativeCalls.registered", "integerParams",
          reinterpret_cast<void(*)()>(integerParamsFunction)));
  vmOptions.nativeFunctions.push_back(
      make_tuple("NativeCalls.registered", "floatParams",
          reinterpret_cast<void(*)()>(floatParamsFunction)));
  vmOptions.nativeFunctions.push_back(
      make_tuple("NativeCalls.registered", "stringParams",
          reinterpret_cast<void(*)()>(stringParamsFunction)));
  vmOptions.nativeFunctions.push_back(
      make_tuple("NativeCalls.registered", "nullObject",
          reinterpret_cast<void(*)()>(nullObjectFunction)));
  vmOptions.nativeFunctions.push_back(
      make_tuple("NativeCalls.registered", "manyParameters",
          reinterpret_cast<void(*)()>(manyParametersFunction)));
  vmOptions.nativeFunctions.push_back(
      make_tuple("NativeCalls.registered", "recursiveNative",
          reinterpret_cast<void(*)()>(recursiveNativeFunction)));
  VM vm(vmOptions);
  auto fName = Name::fromStringForDefn(String(vm, "f"));
  auto gName = Name::fromStringForDefn(String(vm, "g"));

  // Check that we can call a native function in a shared library directly.
  auto sharedlibPath = programDir + "/NativeCalls.sharedlib-1.csp";
  auto sharedlib = vm.loadPackageFromFile(sharedlibPath,
      vector<NativeFunctionSearch>{SEARCH_LIBRARY_FUNCTIONS});
  auto sharedlib_f = sharedlib.getFunction(fName);
  auto result = sharedlib_f.callForI64();
  ASSERT_EQ(12, result);

  // Check that we can call a native function in a shared library through another function.
  auto sharedlib_g = sharedlib.getFunction(gName);
  result = sharedlib_g.callForI64();
  ASSERT_EQ(12, result);

  // Check that we can call a native function in a static library.
  auto staticlibPath = programDir + "/NativeCalls.staticlib-1.csp";
  auto staticlib = vm.loadPackageFromFile(staticlibPath,
      vector<NativeFunctionSearch>{SEARCH_LINKED_FUNCTIONS});
  auto staticlib_f = staticlib.getFunction(fName);
  result = staticlib_f.callForI64();
  ASSERT_EQ(34, result);

  // Check that we can call a native function in a static library through another function.
  auto staticlib_g = staticlib.getFunction(gName);
  result = staticlib_g.callForI64();
  ASSERT_EQ(34, result);

  // Check that we can call a native function registered with the VM.
  auto registeredPath = programDir + "/NativeCalls.registered-1.csp";
  auto registered = vm.loadPackageFromFile(registeredPath,
      vector<NativeFunctionSearch>{SEARCH_REGISTERED_FUNCTIONS});
  auto registered_f = registered.getFunction(fName);
  result = registered_f.callForI64();
  ASSERT_EQ(56, result);

  // Check that we can call a native function registered with the VM through another function.
  auto registered_g = registered.getFunction(gName);
  result = registered_g.callForI64();
  ASSERT_EQ(56, result);

  // Check that we can call a function with integer parameters.
  auto integerParamsName = Name::fromStringForDefn(String(vm, "integerParams"));
  auto integerParams = registered.getFunction(integerParamsName);
  result = integerParams.callForI64(static_cast<int64_t>(12), static_cast<int64_t>(34));
  ASSERT_EQ(46, result);

  // Check that we can call a function with floating point parameters.
  auto floatParamsName = Name::fromStringForDefn(String(vm, "floatParams"));
  auto floatParams = registered.getFunction(floatParamsName);
  auto fresult = floatParams.callForF64(3.0, -1.0);
  ASSERT_EQ(2.0, fresult);

  // Check that we can call a function with string parameters.
  auto stringParamsName = Name::fromStringForDefn(String(vm, "stringParams"));
  auto stringParams = registered.getFunction(stringParamsName);
  auto sresult = stringParams.callForString(String(vm, "foo"), String(vm, "bar"));
  ASSERT_EQ(0, sresult.compare(String(vm, "foobar")));

  // Check that we can call a function that returns null.
  auto nullObjectName = Name::fromStringForDefn(String(vm, "nullObject"));
  auto nullObject = registered.getFunction(nullObjectName);
  auto oresult = nullObject.callForObject();
  ASSERT_FALSE(oresult.isValid());

  // Check that we can call a function with many parameters. This will force some of the
  // parameters onto the stack.
  auto manyParametersName = Name::fromStringForDefn(String(vm, "manyParameters"));
  auto manyParameters = registered.getFunction(manyParametersName);
  sresult = manyParameters.callForString(
      static_cast<int64_t>(1), 2., String(vm, "3"),
      static_cast<int64_t>(4), 5., String(vm, "6"),
      static_cast<int64_t>(7), 8., String(vm, "9"),
      static_cast<int64_t>(10), 11., String(vm, "12"),
      static_cast<int64_t>(13), 14., String(vm, "15"),
      static_cast<int64_t>(16), 17., String(vm, "18"),
      static_cast<int64_t>(19), 20., String(vm, "21"),
      static_cast<int64_t>(22), 23., String(vm, "24"));
  ASSERT_EQ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24",
      sresult.toStdString());

  // Check that we can call a pair of functions which recurse between interpreted and native.
  auto recursiveNativeName = Name::fromStringForDefn(String(vm, "recursiveNative"));
  auto recursiveNative = registered.getFunction(recursiveNativeName);
  auto recursiveGypsumName = Name::fromStringForDefn(String(vm, "recursiveGypsum"));
  auto recursiveGypsum = registered.getFunction(recursiveGypsumName);
  recursiveGypsumPtr = &recursiveGypsum;
  sresult = recursiveNative.callForString(
      static_cast<int64_t>(4), String(vm, "baz"), String(vm, "quux"), String(vm, ""));
  ASSERT_EQ(",bazquux,foobar,bazquux,foobar", sresult.toStdString());

  return 0;
}


// Hack to force static lib to get linked.
extern "C" int64_t NativeCalls__staticlib___f(VM* vm);


void unused() {
  NativeCalls__staticlib___f(nullptr);
}

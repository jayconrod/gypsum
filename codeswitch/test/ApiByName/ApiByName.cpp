// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <iostream>
#include <string>

#include <codeswitch.h>

#include "test.h"

using std::cerr;
using std::endl;
using std::string;

using codeswitch::CallBuilder;
using codeswitch::Error;
using codeswitch::Name;
using codeswitch::String;
using codeswitch::VM;
using codeswitch::VMOptions;


int main(int argc, char* argv[]) {
  VMOptions vmOptions;
  for (int i = 1; i < argc; i++) {
    vmOptions.packageSearchPaths.push_back(argv[i]);
  }
  VM vm(vmOptions);
  auto packageName = Name::fromStringForPackage(String(&vm, "test.ApiByName"));
  auto package = vm.loadPackage(packageName);
  ASSERT_TRUE(package);

  // Check that when we load a global that doesn't exist, we get a bad reference.
  {
    auto global = package.findGlobal(Name::fromStringForDefn(&vm, "blarg"));
    ASSERT_FALSE(global);
  }

  // Check that we can load a global constant by name.
  {
    auto global = package.findGlobal(Name::fromStringForDefn(&vm, "pub-const"));
    ASSERT_TRUE(global.isConstant());
    ASSERT_EQ(12L, global.value().asI64());
  }

  // Check that we can load a global variable by source name.
  {
    auto global = package.findGlobal("pub-var");
    ASSERT_FALSE(global.isConstant());
    ASSERT_EQ(34L, global.value().asI64());
    global.setValue(static_cast<int64_t>(35L));
    ASSERT_EQ(35L, global.value().asI64());
  }

  // Check that we can load a non-public global by source name.
  {
    auto global = package.findGlobal("hidden-var");
    ASSERT_TRUE(global);
    global = package.findGlobal(Name::fromStringForDefn(&vm, "hidden-var"));
    ASSERT_TRUE(global);
  }

  // Check that when we load a function that doesn't exist, we get a bad reference.
  {
    auto function = package.findFunction(Name::fromStringForDefn(&vm, "bogus"), "(I)");
    ASSERT_FALSE(function);
  }

  // Check that we can load a function by name.
  {
    auto function = package.findFunction(Name::fromStringForDefn(&vm, "pub-fn"), "");
    ASSERT_TRUE(function);
    ASSERT_EQ(12L, function.call().asI64());
  }

  // Check that we can load a static method by name.
  {
    auto function = package.findFunction(
        Name::fromStringForDefn(&vm, "PubClass.pub-static-method"),
        "");
    ASSERT_TRUE(function);
    ASSERT_EQ(34L, function.call().asI64());
  }

  // Check that we can load a public function by its source name.
  {
    auto function = package.findFunction("pub-fn", "");
    ASSERT_TRUE(function);
    ASSERT_EQ(12L, function.call().asI64());
  }

  // Check that we can load a non-public function by its source name.
  {
    auto function = package.findFunction("hidden-fn", "");
    ASSERT_TRUE(function);
    function = package.findFunction(Name::fromStringForDefn(&vm, "hidden-fn"), "");
    ASSERT_TRUE(function);
  }

  // Check that when we load a class that doesn't exist, we get a bad reference.
  {
    auto clas = package.findClass(Name::fromStringForDefn(&vm, "quux"));
    ASSERT_FALSE(clas);
  }

  // Check that we can load a class by its name.
  {
    auto clas = package.findClass(Name::fromStringForDefn(&vm, "PubClass"));
    ASSERT_TRUE(clas);
  }

  // Check that we can load a public class by its source name.
  {
    auto clas = package.findClass("PubClass");
    ASSERT_TRUE(clas);
  }

  // Check that we can load a non-public class by its source name.
  {
    auto clas = package.findClass("PrivClass");
    ASSERT_TRUE(clas);
    clas = package.findClass(Name::fromStringForDefn(&vm, "PrivClass"));
    ASSERT_TRUE(clas);
  }

  auto fooClass = package.findClass("Foo");
  auto fooObj = package.findGlobal("foo").value().asObject();
  string fooSig("(C:Foo)");

  auto barTrait = package.findTrait("Bar");
  string barSig("(T:Bar)");

  // Check that when we load a field that doesn't exist, we get a bad reference.
  {
    auto field = fooClass.findField(Name::fromStringForDefn(&vm, "ieieieie"));
    ASSERT_FALSE(field);
  }

  // Check that we can load a constant field by name.
  {
    auto field = fooClass.findField(Name::fromStringForDefn(&vm, "Foo.pub-const"));
    ASSERT_TRUE(field);
    ASSERT_TRUE(field.isConstant());
    ASSERT_EQ(12L, fooObj.getField(field).asI64());
  }

  // Check that we can load a public variable field by its source name.
  {
    auto field = fooClass.findField("pub-var");
    ASSERT_TRUE(field);
    ASSERT_FALSE(field.isConstant());
    ASSERT_EQ(34L, fooObj.getField(field).asI64());
    fooObj.setField(field, static_cast<int64_t>(35L));
    ASSERT_EQ(35L, fooObj.getField(field).asI64());
  }

  // Check that we can load non-public fields by name.
  {
    auto field = fooClass.findField("normal-var");
    ASSERT_TRUE(field);
    field = fooClass.findField(Name::fromStringForDefn(&vm, "Foo.normal-var"));
    ASSERT_TRUE(field);
  }

  // Check that protected variables can be loaded by name.
  {
    auto field = fooClass.findField(Name::fromStringForDefn(&vm, "Foo.prot-var"));
    ASSERT_TRUE(field);
    ASSERT_FALSE(field.isConstant());
  }

  // Check that private variables can be loaded.
  {
    auto field = fooClass.findField(Name::fromStringForDefn(&vm, "Foo.priv-var"));
    ASSERT_TRUE(field);
  }

  // Check that when we load a method that doesn't exist, we get a bad reference.
  {
    auto method = fooClass.findMethod(Name::fromStringForDefn(&vm, "brak"), "()");
    ASSERT_FALSE(method);
  }

  // Check that when we load a method with a bad signature, we get a bad reference.
  {
    auto method = fooClass.findMethod(Name::fromStringForDefn(&vm, "Foo.normal-method"), "");
    ASSERT_FALSE(method);
  }

  // Check that we can load a method by name.
  {
    auto method =
        fooClass.findMethod(Name::fromStringForDefn(&vm, "Foo.normal-method"), fooSig);
    ASSERT_TRUE(method);
    ASSERT_EQ(34L, method.call(fooObj).asI64());
  }

  // Check that we can load a public method by its source name.
  {
    auto method = fooClass.findMethod("pub-method", fooSig);
    ASSERT_TRUE(method);
    ASSERT_EQ(12L, method.call(fooObj).asI64());
  }

  // Check that we can load a non-public method by its source name.
  {
    auto method = fooClass.findMethod("normal-method", fooSig);
    ASSERT_TRUE(method);
  }

  // Check that we can load a public static method by its source name.
  {
    auto method = fooClass.findMethod("static-method", "");
    ASSERT_TRUE(method);
    ASSERT_EQ(123L, method.call().asI64());
  }

  // Check that we can load a protected method by name.
  {
    auto method = fooClass.findMethod(Name::fromStringForDefn(&vm, "Foo.prot-method"), fooSig);
    ASSERT_TRUE(method);
    ASSERT_EQ(56L, method.call(fooObj).asI64());
  }

  // Check that we can load a private method by name.
  {
    auto method = fooClass.findMethod(Name::fromStringForDefn(&vm, "Foo.priv-method"), fooSig);
    ASSERT_TRUE(method);
  }

  // Check that we can load a trait method by name.
  {
    auto method = barTrait.findMethod(
        Name::fromStringForDefn(&vm, "Bar.trait-pub-method"), barSig);
    ASSERT_TRUE(method);
    ASSERT_EQ(12L, method.call(fooObj).asI64());
  }

  // Check that we can load a trait method by its source name.
  {
    auto method = barTrait.findMethod("trait-pub-method", barSig);
    ASSERT_TRUE(method);
    ASSERT_EQ(12L, method.call(fooObj).asI64());
  }

  // Check that we can create a new instance of a class.
  {
    auto ctor = fooClass.findConstructor("(C:Foo,L)");
    ASSERT_TRUE(ctor);
    auto object = ctor.newInstance(static_cast<int64_t>(12));
    ASSERT_EQ(fooClass, object.clas());
  }

  // Check that we can look up and call a function by name.
  {
    auto result = package.callFunction(Name::fromStringForDefn(&vm, "pub-fn")).asI64();
    ASSERT_EQ(12L, result);
  }

  // Check that we can look up and call a function by source name.
  {
    ASSERT_EQ(12L, package.callFunction(String(&vm, "pub-fn")).asI64());
    ASSERT_EQ(12L, package.callFunction("pub-fn").asI64());
  }

  // Check that we can look up and call a method by name from a class.
  {
    auto foo = package.findGlobal("foo").value().asObject();
    auto clas = foo.clas();
    ASSERT_EQ(12L, clas.callMethod(Name::fromStringForDefn(&vm, "Foo.pub-method"), foo).asI64());
  }

  // Check that we can look up and call a method by source name from a class.
  {
    auto foo = package.findGlobal("foo").value().asObject();
    auto clas = foo.clas();
    ASSERT_EQ(12L, clas.callMethod(String(&vm, "pub-method"), foo).asI64());
    ASSERT_EQ(12L, clas.callMethod("pub-method", foo).asI64());
  }

  // Check that we can look up and call a constructor.
  {
    auto clas = package.findClass("Foo");
    auto foo = clas.newInstance(static_cast<int64_t>(12));
    ASSERT_TRUE(foo);
  }

  // Check that an exception is thrown when calling a function that's not a constructor.
  {
    auto clas = package.findClass("Foo");
    auto methodName = Name::fromStringForDefn(&vm, "Foo.pub-method");
    auto method = package.findFunction(methodName, "(C:Foo)");
    try {
      CallBuilder builder(clas, method);
      cerr << "expected exception" << endl;
      return 1;
    } catch (Error& e) {
      // pass.
    }
  }

  // Check that we can look up and call a method by name from an object.
  {
    auto foo = package.findGlobal("foo").value().asObject();
    ASSERT_EQ(12L, foo.callMethod(Name::fromStringForDefn(&vm, "Foo.pub-method")).asI64());
  }

  // Check that we can look up and call a method by source name from an object.
  {
    auto foo = package.findGlobal("foo").value().asObject();
    ASSERT_EQ(12L, foo.callMethod(String(&vm, "pub-method")).asI64());
    ASSERT_EQ(12L, foo.callMethod("pub-method").asI64());
  }

  return 0;
}

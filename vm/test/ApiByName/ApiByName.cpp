// Copyright 2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include <string>

#include <codeswitch.h>

#include "test.h"

using std::string;

using codeswitch::Name;
using codeswitch::String;
using codeswitch::VM;
using codeswitch::VMOptions;


static string dirName(const string& path) {
  auto pos = path.find_last_of('/');
  return pos != string::npos ? path.substr(0, pos) : path;
}


int main(int argc, char* argv[]) {
  VMOptions vmOptions;
  vmOptions.packageSearchPaths.push_back(dirName(argv[0]));
  VM vm(vmOptions);
  auto packageName = Name::fromStringForPackage(String(&vm, "ApiByName"));
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
  string fooSig("(C:3Foo)");

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

  // Check that we can create a new instance of a class.
  {
    auto ctor = fooClass.findConstructor("(C:3Foo,L)");
    ASSERT_TRUE(ctor);
    auto object = fooClass.newInstance(ctor, static_cast<int64_t>(12));
    ASSERT_EQ(fooClass, object.clas());
  }

  return 0;
}

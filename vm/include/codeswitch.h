// Copyright 2014-2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#ifndef codeswitch_h
#define codeswitch_h

#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include <cstdint>


namespace codeswitch {

class Class;
class Error;
class Field;
class Function;
class Global;
class Name;
class Object;
class Package;
class String;
class Value;

/** Used to specify where native functions should be loaded from. */
enum NativeFunctionSearch {
  /**
   * Native functions will be loaded from a dynamically loaded library that corresponds with
   * the package they were loaded from. For example, if a native function is declared in
   * foo-1.csp, it will be loaded from libfoo-1.so in the same directory.
   */
  SEARCH_LIBRARY_FUNCTIONS,

  /**
   * Native functions will be loaded from the VM's binary or any library it's dynamically
   * linked with, according to the system search order.
   */
  SEARCH_LINKED_FUNCTIONS,

  /** Native functions will be loaded from a table of functions registered with the VM. */
  SEARCH_REGISTERED_FUNCTIONS,
};


/** Used to specify behavior when a new VM is created. */
struct VMOptions {
  VMOptions() {}

  /**
   * Determines how the VM searches for implementations of native functions. This may contain
   * any subset of search commands in any order, but no duplicates are allowed. If this is
   * empty, no native functions may be used. The default order is
   * {@link SEARCH_REGISTERED_FUNCTIONS}, then {@link SEARCH_LIBRARY_FUNCTIONS}.
   */
  std::vector<NativeFunctionSearch> nativeFunctionSearchOrder =
      std::vector<NativeFunctionSearch>{SEARCH_REGISTERED_FUNCTIONS, SEARCH_LIBRARY_FUNCTIONS};

  /**
   * A table of implementations for native functions. Functions can be registered here
   * explicitly instead of relying on the VM to look them up in loaded libraries or in the
   * binary that contains the VM. Each element of this list contains a package name, a
   * function name, and a pointer to a function.
   */
  std::vector<std::tuple<std::string, std::string, void(*)()>> nativeFunctions;

  /**
   * When true, the VM will perform additional checks to verify the heap is in a good state
   * after garbage collection and when the VM is destroyed. Useful for debugging the VM.
   */
  bool verifyHeap = false;

  /**
   * When a package is loaded by its symbolic name, these directories are searched in order
   * for a suitable package file to load.
   */
  std::vector<std::string> packageSearchPaths;
};


/**
 * A CodeSwitch virtual machine.
 *
 * Each instance of this class represents a separate machine
 * with its own set of packages and its own garbage collected heap. Virtual machines are
 * completely independent and share no global state. After a virtual machine is destroyed,
 * it is no longer safe to reference any related objects.
 */
class VM final {
 public:
  class Impl;

  /** Constructs a new virtual machine */
  VM();
  VM(const VMOptions& vmOptions);
  VM(const VM&) = delete;
  VM(VM&& vm);
  VM& operator = (const VM&) = delete;
  VM& operator = (VM&&);
  ~VM();

  /**
   * Loads a package by its symbolic name.
   *
   * The VM will search the package directories for files with names matching the form
   * "foo.bar-1.2.csp" or "foo.bar.csp" (replace "foo.bar" with the package name and "1.2"
   * with any version number. When a package with a matching name is found, it is returned.
   *
   * If the found package has dependencies that haven't been loaded yet, they will be
   * loaded automatically in the same fashion. If any loaded package has an initialization
   * function, it will be executed.
   *
   * @param name the symbolic name of the package. Must be a valid reference.
   * @param nativeFunctionSearchOrder overrides the native function search order for this
   *     package only. If this is empty, the default VM order is used. This does not affect
   *     packages that this package depends on which get loaded at the same time.
   * @return the loaded package. This will always be a valid reference.
   * @throws Error if a problem was encountered when loading the package or one of
   *   its dependencies.
   */
  Package loadPackage(const Name& name,
      const std::vector<NativeFunctionSearch>& nativeFunctionSearchOrder
          = std::vector<NativeFunctionSearch>());

  /**
   * Loads a package by its file name.
   *
   * The VM will load this package file directly and will not search the package directories.
   *
   * If the found package has dependencies that haven't been loaded yet, they will be
   * loaded automatically in the same fashion. If any loaded package has an initialization
   * function, it will be executed.
   *
   * @param fileName a relative or absolute path to the package file
   * @param nativeFunctionSearchOrder overrides the native function search order for this
   *     package only. If this is empty, the default VM order is used. This does not affect
   *     packages that this package depends on which get loaded at the same time.
   * @return the loaded package. This will always be a valid reference.
   * @throws Error if a problem was encountered when loading the package or one of
   *   its dependencies.
   */
  Package loadPackageFromFile(const std::string& fileName,
      const std::vector<NativeFunctionSearch>& nativeFunctionSearchOrder
          = std::vector<NativeFunctionSearch>());

 private:
  std::unique_ptr<Impl> impl_;

  friend class String;
};


class Impl;


/**
 * Internal base class for API objects that point to objects on the managed heap.
 *
 * Reference objects maintain pointers to these objects, which are registered with the
 * garbage collector. The garbage collector will not delete objects while they are referenced.
 * Referenced pointers will be updated when objects are moved during garbage collection.
 *
 * A reference is *valid* if it points to an object. If it is not valid, it points to nothing.
 * Most API methods require valid pointers. Use {@code isValid} and the Boolean operators to
 * test validity.
 */
class Reference {
 protected:
  Reference();
  explicit Reference(Impl* impl);
  Reference(const Reference& ref);
  Reference(Reference&& ref);
  Reference& operator = (const Reference& ref);
  Reference& operator = (Reference&& ref);
 public:
  ~Reference();

  /** Returns true if the reference points to an object */
  bool isValid() const;

  /**
   * Releases the referenced object. If there are no other references to the object, it may
   * be eligible for garbage collection. This reference will no longer be valid.
   */
  void clear();

  /** Returns true if the reference points to an object */
  explicit operator bool () const {
    return isValid();
  }

  /** Returns false if the reference points to an object */
  bool operator ! () const {
    return !isValid();
  }

 protected:
  Impl* impl_;
};


/**
 * A loadable collection of related globals, functions, and classes.
 *
 * Packages are loaded from files using {@link VM#loadPackage} or
 * {@link VM#loadPackageFromFile}. Package may reference definitions in other packages. When
 * a package is loaded, its dependencies are automatically loaded first.
 */
class Package final : public Reference {
 public:
  Package() = default;
  explicit Package(Impl* impl);

  /**
   * Returns the package's entry function, if it has one.
   *
   * @return the package's entry function. If the package has no entry function, an invalid
   *     reference is returned.
   */
  Function entryFunction() const;

  /**
   * Finds and returns a global from the package by name.
   *
   * @return the named global from the package. If the package has no global by this name,
   *     and invalid reference is returned.
   */
  Global findGlobal(const Name& name) const;

  /**
   * Finds and returns a public global from the package by its short name from source code.
   * Static fields are not searched.
   *
   * @return the named global from the package. If the package has no global by this name,
   *     and invalid reference is returned.
   */
  Global findGlobal(const String& sourceName) const;

  /**
   * Finds and returns a public global from the package by its short name from source code.
   * Static fields are not searched.
   *
   * @return the named global from the package. If the package has no global by this name,
   *     and invalid reference is returned.
   */
  Global findGlobal(const std::string& sourceName) const;

  /**
   * Finds and returns a function from the package by name.
   *
   * @return the named function from the package. If the package has no function by this name,
   *     an invalid reference is returned.
   */
  Function findFunction(const Name& name) const;

  /**
   * Finds and returns a public function from the package by its short name from source code.
   * Methods (static or otherwise) are not searched.
   *
   * @return the named function from the package. If the package has no function by this name,
   *     an invalid reference is returned.
   */
  Function findFunction(const String& sourceName) const;

  /**
   * Finds and returns a public function from the package by its short name from source code.
   * Methods (static or otherwise) are not searched.
   *
   * @return the named function from the package. If the package has no function by this name,
   *     an invalid reference is returned.
   */
  Function findFunction(const std::string& sourceName) const;

  /**
   * Finds and returns a class from the package by name.
   *
   * @return the named class from the package. If the package has no class by this name,
   *     an invalid reference is returned.
   */
  Class findClass(const Name& name) const;

  /**
   * Finds and returns a public class from the package by its short name from source code.
   *
   * @return the named class from the package. If the package has no class by this name,
   *     an invalid reference is returned.
   */
  Class findClass(const String& sourceName) const;

  /**
   * Finds and returns a public class from the package by its short name from source code.
   *
   * @return the named class from the package. If the package has no class by this name,
   *     an invalid reference is returned.
   */
  Class findClass(const std::string& sourceName) const;
};


/**
 * A global variable or static field of a class.
 *
 * Objects of this class can be used to get or set the values of global variables. Some
 * globals are constant and their values cannot be changed through this API.
 */
class Global final : public Reference {
 public:
  Global() = default;
  explicit Global(Impl* impl);

  /**
   * Returns whether the value of this global can be altered through the API. If true,
   * {@link #setValue} must not be called.
   */
  bool isConstant() const;

  /** Returns the value of this global. */
  Value value() const;

  /** Sets the value of this global. {@link #isConstant} must be false. */
  void setValue(const Value& value);
};


/**
 * An executable function definition.
 *
 * Functions can be called using either the provided `call` methods or
 * `CallBuilder`.
 */
class Function final : public Reference {
 public:
  Function() = default;
  explicit Function(Impl* impl);

  /**
   * Calls a function that returns `unit` (equivalent to `void`)
   *
   * @param args arguments to call the function with. These are type-checked.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  void call(Ts... args);

  /**
   * Calls a function that returns `boolean`
   *
   * @param args arguments to call the function with. These are type-checked.
   * @return the result of the function call.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  bool callForBoolean(Ts... args);

  /**
   * Calls a function that returns `i8`
   *
   * @param args arguments to call the function with. These are type-checked.
   * @return the result of the function call.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  int8_t callForI8(Ts... args);

  /**
   * Calls a function that returns `i16`
   *
   * @param args arguments to call the function with. These are type-checked.
   * @return the result of the function call.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  int16_t callForI16(Ts... args);

  /**
   * Calls a function that returns `i32`
   *
   * @param args arguments to call the function with. These are type-checked.
   * @return the result of the function call.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  int32_t callForI32(Ts... args);

  /**
   * Calls a function that returns `i64`
   *
   * @param args arguments to call the function with. These are type-checked.
   * @return the result of the function call.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  int64_t callForI64(Ts... args);

  /**
   * Calls a function that returns `f32`
   *
   * @param args arguments to call the function with. These are type-checked.
   * @return the result of the function call.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  float callForF32(Ts... args);

  /**
   * Calls a function that returns `f64`
   *
   * @param args arguments to call the function with. These are type-checked.
   * @return the result of the function call.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  double callForF64(Ts... args);

  /**
   * Calls a function that returns `String`
   *
   * @param args arguments to call the function with. These are type-checked.
   * @return the result of the function call.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  String callForString(Ts... args);

  /**
   * Calls a function that returns any object type
   *
   * @param args arguments to call the function with. These are type-checked.
   * @return the result of the function call.
   * @throws Error if there is a type error with the arguments or return type or if the function
   *   throws an exception.
   */
  template <class... Ts>
  Object callForObject(Ts... args);

  friend class CallBuilder;
};


/** A class definition. */
class Class final : public Reference {
 public:
  Class() = default;
  explicit Class(Impl* impl);

  /**
   * Finds and returns a method of the class by name.
   *
   * @return the named method from the class. If the class has no method by this name,
   *     an invalid reference is returned.
   */
  Function findMethod(const Name& name) const;

  /**
   * Finds and returns a public method of the class by its short name from source code.
   *
   * @return the named method from the class. If the class has no method by this name,
   *     an invalid reference is returned.
   */
  Function findMethod(const String& sourceName) const;

  /**
   * Finds and returns a public method of the class by its short name from source code.
   *
   * @return the named method from the class. If the class has no method by this name,
   *     an invalid reference is returned.
   */
  Function findMethod(const std::string& sourceName) const;

  /**
   * Finds and returns a field of the class by name.
   *
   * @return the named field from the class. If the class has no field by this name,
   *     an invalid reference is returned.
   */
  Field findField(const Name& name) const;

  /**
   * Finds and returns a public field of the class by its short name from source code.
   * Static fields are not searched.
   *
   * @return the named field from the class. If the class has no field by this name,
   *     an invalid reference is returned.
   */
  Field findField(const String& sourceName) const;

  /**
   * Finds and returns a public field of the class by its short name from source code.
   * Static fields are not searched.
   *
   * @return the named field from the class. If the class has no field by this name,
   *     an invalid reference is returned.
   */
  Field findField(const std::string& sourceName) const;
};


/**
 * Represents a field within an object. Can be passed to {@link Object} methods to load
 * and store values in those field.
 */
class Field final : public Reference {
 public:
  Field() = default;
  explicit Field(Impl* impl);
};


/**
 * Builds a call to a function, pushing individual arguments onto a stack.
 *
 * `CallBuilder` can be used if you need more control over a function call than the
 * {@link Function#call} family of methods offers. Note that none of the `arg` methods
 * have any side effects outside the builder object until a `call` method is called.
 *
 * Objects of this class actually manage pointers to objects on the garbage collected heap.
 * Objects are in an "invalid" state if they are created with the default constructor or
 * after being on the right side of a move assignment or construction.
 */
class CallBuilder final {
 public:
  class Impl;

  /**
   * Constructs a new call builder
   *
   * @param function the function to be called. Must be a valid reference.
   */
  explicit CallBuilder(const Function& function);
  ~CallBuilder();

  CallBuilder(CallBuilder&& builder) = delete;
  CallBuilder& operator = (const CallBuilder&) = delete;
  CallBuilder& operator = (CallBuilder&& builder) = delete;

  /** Adds a `unit` value to the argument list */
  CallBuilder& argUnit();

  /** Adds a `boolean` value to the argument list */
  CallBuilder& arg(bool value);

  /** Adds an `i8` value to the argument list */
  CallBuilder& arg(int8_t value);

  /** Adds an `i16` value to the argument list */
  CallBuilder& arg(int16_t value);

  /** Adds an `i32` value to the argument list */
  CallBuilder& arg(int32_t value);

  /** Adds an `i64` value to the argument list */
  CallBuilder& arg(int64_t value);

  /** Adds an `f32` value to the argument list */
  CallBuilder& arg(float value);

  /** Adds an `f64` value to the argument list */
  CallBuilder& arg(double value);

  /** Adds a {@link Object} value to the argument list */
  CallBuilder& arg(const Object& value);

  /** Signals no arguments are needed */
  CallBuilder& args() { return *this; }

  /** Adds several values to the argument list, starting with a `boolean` value */
  template <class... Ts>
  CallBuilder& args(bool value, Ts&&... rest);

  /** Adds several values to the argument list, starting with an `i8` value */
  template <class... Ts>
  CallBuilder& args(int8_t value, Ts&&... rest);

  /** Adds several values to the argument list, starting with an `i16` value */
  template <class... Ts>
  CallBuilder& args(int16_t value, Ts&&... rest);

  /** Adds several values to the argument list, starting with an `i32` value */
  template <class... Ts>
  CallBuilder& args(int32_t value, Ts&&... rest);

  /** Adds several values to the argument list, starting with an `i64` value */
  template <class... Ts>
  CallBuilder& args(int64_t value, Ts&&... rest);

  /** Adds several values to the argument list, starting with an `f32` value */
  template <class... Ts>
  CallBuilder& args(float value, Ts&&... rest);

  /** Adds several values to the argument list, starting with an `f64` value */
  template <class... Ts>
  CallBuilder& args(double value, Ts&&... rest);

  /** Adds several values to the argument list, starting with an `Object` value */
  template <class... Ts>
  CallBuilder& args(const Object& value, Ts&&... rest);

  /** Calls the function and clears the argument list. The result is ignored. */
  void call();

  /**
   * Calls the function and clears the argument list
   *
   * @return the function's return value. The return type must be `boolean`.
   */
  bool callForBoolean();

  /**
   * Calls the function and clears the argument list
   *
   * @return the function's return value. The return type must be `i8`.
   */
  int8_t callForI8();

  /**
   * Calls the function and clears the argument list
   *
   * @return the function's return value. The return type must be `i16`.
   */
  int16_t callForI16();

  /**
   * Calls the function and clears the argument list
   *
   * @return the function's return value. The return type must be `i32`.
   */
  int32_t callForI32();

  /**
   * Calls the function and clears the argument list
   *
   * @return the function's return value. The return type must be `i64`.
   */
  int64_t callForI64();

  /**
   * Calls the function and clears the argument list
   *
   * @return the function's return value. The return type must be `f32`.
   */
  float callForF32();

  /**
   * Calls the function and clears the argument list
   *
   * @return the function's return value. The return type must be `f64`.
   */
  double callForF64();

  /**
   * Calls the function and clears the argument list
   *
   * @return the function's return value. The return type must be `String`. If `null` is
   *   returned, this will be an invalid reference.
   */
  String callForString();

  /**
   * Calls the function and clears the argument list
   *
   * @return the function's return value. The return type must be some object type. If `null`
   *  is returned, this will be an invalid reference.
   */
  Object callForObject();

 private:
  std::unique_ptr<Impl> impl_;
};


/**
 * A symbolic name for a package or definition.
 *
 * Names have one or more non-empty string components. Components are separated by dots when
 * written together (for example, foo.bar.baz). Package names have some restriction on what
 * characters they can contain (see {@link #fromStringForPackage}).
 *
 * Objects of this class actually manage pointers to objects on the garbage collected heap.
 * Objects are in an "invalid" state if they are created with the default constructor or
 * after being on the right side of a move assignment or construction.
 */
class Name final : public Reference {
 public:
  Name() = default;
  explicit Name(Impl* impl);

  /**
   * Parses a string to create a new name for a definition
   *
   * @param str the string to parse. It is split on '.' characters, and the pieces form the
   *   components of the new name. The pieces may contain any character except '.', but must
   *   not be empty.
   * @return the newly constructed name. This will always be a valid reference.
   * @throws Error if str could not be parsed.
   */
  static Name fromStringForDefn(const String& str);

  /**
   * Parses a string to create a new name for a package
   *
   * @param str the string to parse. It is split on '.' characters, and the pieces form the
   *   components of the new name. Each piece must start with a character in the range
   *   A-Z or a-z. After that characters may be in the ranges A-Z, a-z, 0-9, or _.
   * @return the newly constructed name. This will always be a valid reference.
   * @throws Error if str could not be parsed.
   */
  static Name fromStringForPackage(const String& str);

  friend class Package;
  friend class VM;
};


/**
 * An object of any type on the CodeSwitch garbage collected heap.
 *
 * Objects of this class actually manage pointers to objects on the garbage collected heap.
 * Objects are in an "invalid" state if they are created with the default constructor or
 * after being on the right side of a move assignment or construction.
 */
class Object: public Reference {
 public:
  Object() = default;
  explicit Object(Impl* impl);

  /** Returns whether this object is an instance of the given class. */
  bool isInstanceOf(const Class& clas) const;

  /** Returns the class this object is an instance of. */
  Class clas() const;

  /**
   * Loads the value stored in a field of the object.
   *
   * @param field a field which must have been retrieved from this object's {@link Class}
   *     (which can be obtained by calling {@link #clas}) or any superclass.
   */
  Value getField(const Field& field) const;

  /**
   * Loads the value stored in the named field of the object.
   *
   * This method is provided for convenience, not speed. It performs a lookup on every call.
   * If you are loading from the same field repeatedly, consider
   * {@link #getField(const Field&)} instead.
   *
   * @param fieldSourceName the name from source code of a public, non-static field.
   */
  Value getField(const std::string& fieldSourceName) const;

  /**
   * Sets the value of a field in the object.
   *
   * @param field a field which must have been retrieved from this object's {@link Class}
   *     (which can be obtained by calling {@link #clas}) or any superclass.
   * @param value the value to store in the field.
   */
  void setField(const Field& field, const Value& value);

  /**
   * Sets the value of the named field in the object.
   *
   * This method is provided for convenience, not speed. It performs a lookup on every call.
   * If you are loading from the same field repeatedly, consider
   * {@link #setField(const Field&, const Value&)} instead.
   *
   * @param fieldSourceName the name from source code of a public, non-static field.
   * @param the value to store in the field.
   */
  void setField(const std::string& fieldSourceName, const Value& value);

  friend class CallBuilder;
};


/**
 * A unicode string. Just a sequence of unicode code points.
 *
 * Objects of this class actually manage pointers to objects on the garbage collected heap.
 * Objects are in an "invalid" state if they are created with the default constructor or
 * after being on the right side of a move assignment or construction.
 */
class String final : public Object {
 public:
  String() = default;
  explicit String(Impl* impl);

  /**
   * Constructs a new string from an STL string
   *
   * @param vm the CodeSwitch virtual machine
   * @param str a UTF-8 used to build the CodeSwitch string
   */
  String(VM* vm, const std::string& str);

  /** Concatenates two strings */
  String operator + (const String& other) const;

  /** Concatenates a string with an STL string */
  String operator + (const std::string& other) const;

  /**
   * Compares two strings lexicographically, by code point.
   *
   * @return 0 if the strings are equal, negative if the receiver is less than {@code other},
   *     positive if the receiver is greater than {@code other}.
   * @todo take locale into account.
   */
  int compare(const String& other) const;

  /** Creates an STL UTF-8 encoded string from this string */
  std::string toStdString() const;

  friend class CallBuilder;
  friend class Name;
};


/**
 * Top-level exception class for API errors.
 */
class Error final {
 public:
  class Impl;

  Error();
  explicit Error(Impl* impl);
  Error(const Error&) = delete;
  Error(Error&& error);
  Error& operator = (const Error&) = delete;
  Error& operator = (Error&& error);
  ~Error();

  explicit operator bool () const;
  bool operator ! () const;

  /** Returns a message contained by the error for logging or displaying to users */
  const char* message() const;

 private:
  std::unique_ptr<Impl> impl_;
};


/**
 * Represents a value that can be returned or passed as an argument to a {@link Function}.
 * Values can also be loaded from and stored to {@link Global}s.
 *
 * Values can be created implicitly from primitive types and object references. This lets you
 * pass primitives directly to methods like {@link Global#setValue}. Note that creating a
 * value by moving an object reference is significantly faster than copying the reference,
 * so this should be preferred when possible.
 */
class Value final {
 public:
  class Impl;

  Value();
  Value(bool b);
  Value(int8_t n);
  Value(int16_t n);
  Value(int32_t n);
  Value(int64_t n);
  Value(float n);
  Value(double n);
  Value(const Object& o);
  Value(Object&& o);

  bool asBoolean() const;
  int8_t asI8() const;
  int16_t asI16() const;
  int32_t asI32() const;
  int64_t asI64() const;
  float asF32() const;
  double asF64() const;
  const String& asString() const;
  const Object& asObject() const;
  String&& moveString();
  Object&& moveObject();

 private:
  uint64_t bits_;
  Object ref_;
  uint8_t tag_;

  friend Impl;
};


template <class... Ts>
void Function::call(Ts... args) {
  CallBuilder(*this).args(args...).call();
}


template <class... Ts>
bool Function::callForBoolean(Ts... args) {
  return CallBuilder(*this).args(args...).callForBoolean();
}


template <class... Ts>
int8_t Function::callForI8(Ts... args) {
  return CallBuilder(*this).args(args...).callForI8();
}


template <class... Ts>
int16_t Function::callForI16(Ts... args) {
  return CallBuilder(*this).args(args...).callForI16();
}


template <class... Ts>
int32_t Function::callForI32(Ts... args) {
  return CallBuilder(*this).args(args...).callForI32();
}


template <class... Ts>
int64_t Function::callForI64(Ts... args) {
  return CallBuilder(*this).args(args...).callForI64();
}


template <class... Ts>
float Function::callForF32(Ts... args) {
  return CallBuilder(*this).args(args...).callForF32();
}


template <class... Ts>
double Function::callForF64(Ts... args) {
  return CallBuilder(*this).args(args...).callForF64();
}


template <class... Ts>
String Function::callForString(Ts... args) {
  return CallBuilder(*this).args(args...).callForString();
}


template <class... Ts>
Object Function::callForObject(Ts... args) {
  return CallBuilder(*this).args(args...).callForObject();
}


template <class... Ts>
CallBuilder& CallBuilder::args(bool value, Ts&&... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(int8_t value, Ts&&... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(int16_t value, Ts&&... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(int32_t value, Ts&&... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(int64_t value, Ts&&... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(float value, Ts&&... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(double value, Ts&&... rest) {
  arg(value);
  return args(rest...);
}


template <class... Ts>
CallBuilder& CallBuilder::args(const Object& value, Ts&&... rest) {
  arg(value);
  return args(rest...);
}

}

#endif

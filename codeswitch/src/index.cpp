// Copyright Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "index.h"

#include <sstream>
#include <vector>

#include "function.h"
#include "package.h"
#include "type.h"
#include "type-parameter.h"

using std::string;
using std::stringstream;
using std::vector;

namespace codeswitch {
namespace internal {

static void mangleFunctionSignature(stringstream& str,
                                    const Handle<Function>& function,
                                    const Handle<Package>& package);

static void mangleType(stringstream& str,
                       const Handle<Type>& type,
                       vector<Local<TypeParameter>>& variables,
                       const Handle<Package>& package);

static void mangleTypeParameter(stringstream& str,
                                const Handle<TypeParameter>& typeParam,
                                vector<Local<TypeParameter>>& variables,
                                const Handle<Package>& package);


Local<Name> mangleFunctionName(const Handle<Function>& function) {
  Local<Package> package(function->getVM(), function->package());
  return mangleFunctionName(function, package);
}


Local<Name> mangleFunctionName(const Handle<Function>& function,
                               const Handle<Package>& package) {
  HandleScope handleScope(function->getVM());
  auto heap = function->getHeap();

  // Compute the last component of the function's mangled name.
  stringstream str;
  auto components = handle(function->name()->components());
  auto lastComponent = handle(components->get(components->length() - 1));
  str << lastComponent->toUtf8StlString();
  mangleFunctionSignature(str, function, package);

  // Build a new name using the mangled component.
  auto mangledComponents = BlockArray<String>::create(heap, components->length());
  for (length_t i = 0; i < components->length() - 1; i++) {
    mangledComponents->set(i, components->get(i));
  }
  auto mangledComponent = String::fromUtf8String(heap, str.str());
  mangledComponents->set(components->length() - 1, *mangledComponent);
  auto mangledName = Name::create(heap, mangledComponents);

  return handleScope.escape(*mangledName);
}


Local<String> mangleSignature(const Handle<Function>& function) {
  stringstream str;
  Local<Package> package(function->getVM(), function->package());
  mangleFunctionSignature(str, function, package);
  return String::fromUtf8String(function->getHeap(), str.str());
}


Local<String> mangleFunctionSourceName(const Handle<Function>& function) {
  if (!function->sourceName())
    return Local<String>();

  HandleScope handleScope(function->getVM());
  auto sourceName = handle(function->sourceName());
  stringstream str;
  str << sourceName->toUtf8StlString();
  Local<Package> package(function->getVM(), function->package());
  mangleFunctionSignature(str, function, package);
  auto mangledSourceName = String::fromUtf8String(function->getHeap(), str.str());
  return handleScope.escape(*mangledSourceName);
}


static void mangleFunctionSignature(
    stringstream& str,
    const Handle<Function>& function,
    const Handle<Package>& package) {
  auto typeParams = handle(function->typeParameters());
  vector<Local<TypeParameter>> variables;
  if (typeParams->length() > 0) {
    variables.reserve(typeParams->length());
    str << '[';
    for (length_t i = 0; i < typeParams->length(); i++) {
      if (i > 0)
        str << ',';
      variables.push_back(handle(typeParams->get(i)));
      mangleTypeParameter(str, variables.back(), variables, package);
    }
    str << ']';
  }
  if (function->parameterTypes()->length() > 0) {
    str << '(';
    auto paramTypes = handle(function->parameterTypes());
    for (length_t i = 0; i < paramTypes->length(); i++) {
      if (i > 0)
        str << ',';
      mangleType(str, handle(paramTypes->get(i)), variables, package);
    }
    str << ')';
  }
}


static void mangleType(stringstream& str,
                       const Handle<Type>& type,
                       vector<Local<TypeParameter>>& variables,
                       const Handle<Package>& package) {
  auto heap = type->getHeap();
  switch (type->form()) {
    case Type::UNIT_TYPE:
      str << 'U';
      break;

    case Type::BOOLEAN_TYPE:
      str << 'Z';
      break;

    case Type::I8_TYPE:
      str << 'B';
      break;

    case Type::I16_TYPE:
      str << 'S';
      break;

    case Type::I32_TYPE:
      str << 'I';
      break;

    case Type::I64_TYPE:
      str << 'L';
      break;

    case Type::F32_TYPE:
      str << 'F';
      break;

    case Type::F64_TYPE:
      str << 'D';
      break;

    case Type::CLASS_TYPE:
    case Type::EXTERN_CLASS_TYPE: {
      str << 'C';
      auto clas = handle(type->asClass());
      if (clas->package() == nullptr) {
        str << "::";
      } else if (clas->package() != package.getOrNull()) {
        auto classPackageNameStr = Name::toString(heap, handle(clas->package()->name()));
        str << classPackageNameStr->toUtf8StlString() << ':';
      } else {
        str << ':';
      }
      auto classNameStr = Name::toString(heap, handle(clas->name()));
      str << classNameStr->toUtf8StlString();
      if (type->typeArgumentCount() > 0) {
        str << '[';
        for (length_t i = 0; i < type->typeArgumentCount(); i++) {
          if (i > 0)
            str << ',';
          mangleType(str, handle(type->typeArgument(i)), variables, package);
        }
        str << ']';
      }
      if (type->isNullable())
        str << '?';
      break;
    }

    case Type::TRAIT_TYPE:
    case Type::EXTERN_TRAIT_TYPE: {
      str << 'T';
      auto trait = handle(type->asTrait());
      if (trait->package() == nullptr) {
        str << "::";
      } else if (trait->package() != package.getOrNull()) {
        auto traitPackageNameStr = Name::toString(heap, handle(trait->package()->name()));
        str << traitPackageNameStr->toUtf8StlString() << ':';
      } else {
        str << ':';
      }
      auto traitNameStr = Name::toString(heap, handle(trait->name()));
      str << traitNameStr->toUtf8StlString();
      if (type->typeArgumentCount() > 0) {
        str << '[';
        for (length_t i = 0; i < type->typeArgumentCount(); i++) {
          if (i > 0)
            str << ',';
          mangleType(str, handle(type->typeArgument(i)), variables, package);
        }
        str << ']';
      }
      if (type->isNullable())
        str << '?';
      break;
    }

    case Type::VARIABLE_TYPE:
    case Type::EXTERN_VARIABLE_TYPE: {
      str << 'V';
      auto typeParameter = handle(type->asVariable());
      length_t index = kLengthNotSet;
      for (length_t i = 0; i < variables.size(); i++) {
        if (*variables[i] == *typeParameter) {
          index = i;
          break;
        }
      }
      ASSERT(index != kLengthNotSet);
      str << index;
      if (type->isNullable())
        str << '?';
      break;
    }

    case Type::EXISTENTIAL_TYPE: {
      auto oldSize = variables.size();
      str << "E[";
      for (length_t i = 0; i < type->existentialVariableCount(); i++) {
        if (i > 0)
          str << ',';
        variables.push_back(handle(type->existentialVariable(i)));
        mangleTypeParameter(str, variables.back(), variables, package);
      }
      str << ']';
      mangleType(str, handle(type->existentialInnerType()), variables, package);
      variables.erase(variables.begin() + oldSize, variables.end());
      break;
    }

    case Type::LABEL_TYPE:
    case Type::ANY_TYPE:
    case Type::NO_TYPE:
      UNREACHABLE();
  }

}


void mangleTypeParameter(stringstream& str,
                         const Handle<TypeParameter>& typeParam,
                         vector<Local<TypeParameter>>& variables,
                         const Handle<Package>& package) {
  if ((STATIC_FLAG & typeParam->flags()) != 0) {
    str << 's';
  }
  str << '<';
  mangleType(str, handle(typeParam->upperBound()), variables, package);
  str << '>';
  mangleType(str, handle(typeParam->lowerBound()), variables, package);
}


Local<Name> mangleName(const Handle<Name>& name, const string& signature) {
  HandleScope handleScope(name->getVM());
  auto heap = name->getHeap();
  auto components = handle(name->components());
  auto n = components->length();
  auto mangledComponents = BlockArray<String>::create(heap, n);
  for (length_t i = 0; i < n - 1; i++) {
    mangledComponents->set(i, components->get(i));
  }
  auto lastComponent = handle(components->get(n - 1));
  auto mangledComponent = mangleSourceName(lastComponent, signature);
  mangledComponents->set(n - 1, *mangledComponent);
  auto mangledName = Name::create(heap, mangledComponents);
  return handleScope.escape(*mangledName);
}


Local<String> mangleSourceName(const Handle<String>& sourceName, const string& signature) {
  stringstream str;
  str << sourceName->toUtf8StlString() << signature;
  return String::fromUtf8String(sourceName->getHeap(), str.str());
}


string buildSignature(const vector<Local<Type>>& types, const Handle<Package>& package) {
  if (types.empty()) {
    return "";
  }
  stringstream str;
  str << "(";
  vector<Local<TypeParameter>> variables;
  const char* delim = "";
  for (auto& type : types) {
    str << delim;
    delim = ",";
    mangleType(str, type, variables, package);
  }
  str << ")";
  return str.str();
}


string demangleFunctionName(Name* name) {
  string nameStr;
  auto sep = "";
  for (auto component : *name->components()) {
    nameStr += sep;
    sep = "__";

    for (auto ch : *component) {
      if (ch == '[' || ch == '(')
        break;
      auto valid = ('0' <= ch && ch <= '9') ||
          ('A' <= ch && ch <= 'Z') ||
          ('a' <= ch && ch <= 'z') ||
          ch == '_';
      nameStr += valid ? static_cast<char>(ch) : '_';
    }
  }
  return nameStr;
}

}
}

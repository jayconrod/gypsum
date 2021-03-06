# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.

import argparse
import os
import os.path
import re
import sys

import ast
from compile_info import CompileInfo, STD_NAME
from compiler import compile
from errors import CompileException
from externalization import externalize
from ids import AstId, TARGET_PACKAGE_ID
from inheritance_analysis import analyzeInheritance
from ir import Package, PackageVersion, PackageDependency, Name
from lexer import *
from location import NoLoc
from package_loader import PackageLoader
from parser import *
from scope_analysis import *
from serialize import serialize
from type_analysis import analyzeTypeDeclarations, analyzeTypes


def main():
    sys.setrecursionlimit(10000)

    PackageName = lambda s: Name.fromString(s, isPackageName=True)
    cmdline = argparse.ArgumentParser(description="Compile source files into CodeSwitch packages")
    cmdline.add_argument("sources", metavar="source", type=str, nargs="+",
                         help="Source file names")
    cmdline.add_argument("-p", "--package-name", action="store", type=PackageName,
                         default=PackageName("default"),
                         help="Name of the package being compiled")
    cmdline.add_argument("-v", "--package-version", action="store", type=PackageVersion.fromString,
                         default=PackageVersion([0]),
                         help="Version of the package being compiled")
    cmdline.add_argument("-d", "--depends", action="append", type=str, default=[],
                         help="Additional package dependencies")
    cmdline.add_argument("--no-std", action="store_true",
                         help="Do not add a dependency on the standard library")
    cmdline.add_argument("-P", "--package-path", action="append", type=str, default=[],
                         help="Directories containing packages that could be imported")
    cmdline.add_argument("-o", "--output", action="store",
                         default="out.csp",
                         help="Name of the output file")
    cmdline.add_argument("--print-tokens", action="store_true",
                         help="Print tokens after lexical analysis")
    cmdline.add_argument("--print-ast", action="store_true",
                         help="Print abstract syntax tree after syntax analysis")
    cmdline.add_argument("--print-scope", action="store_true",
                         help="Print scope info after scope analysis")
    cmdline.add_argument("--print-types", action="store_true",
                         help="Print types after type analysis")
    cmdline.add_argument("--print-ir", action="store_true",
                         help="Print intermediate representation after compilation")
    cmdline.add_argument("--print-stack", action="store_true",
                         help="Print compiler stack on error")
    args = cmdline.parse_args()

    try:
        astModules = []
        for sourceFileName in args.sources:
            with open(sourceFileName) as inFile:
                source = inFile.read()
            tokens = lex(sourceFileName, source)
            if args.print_tokens:
                for tok in tokens:
                    sys.stdout.write(str(tok) + "\n")
            astModule = parse(sourceFileName, tokens)
            if args.print_ast:
                printer = ast.Printer(sys.stdout)
                printer.visit(astModule)
            astModules.append(astModule)
        astPackage = ast.Package(astModules, NoLoc)
        astPackage.id = AstId(-1)

        package = Package(TARGET_PACKAGE_ID, args.package_name, args.package_version)
        loader = PackageLoader(args.package_path if len(args.package_path) > 0 else None)
        loader.ensurePackageInfo()
        if len(args.depends) > 0:
            depPackages = loader.loadPackageFiles(args.depends)
            each(package.ensureDependency, depPackages)
        isUsingStd = not args.no_std
        if isUsingStd:
            stdPackage = loader.loadPackage(STD_NAME, NoLoc)
            package.ensureDependency(stdPackage)
        info = CompileInfo(astPackage, package, loader, isUsingStd=isUsingStd)

        analyzeDeclarations(info)
        if args.print_scope:
            sys.stderr.write("--print-scope not supported right now\n")
        analyzeTypeDeclarations(info)
        analyzeInheritance(info)
        analyzeTypes(info)
        if args.print_types:
            sys.stderr.write("--print-types not supported right now\n")
        convertClosures(info)
        externalize(info)
        compile(info)

        package = info.package
        if args.print_ir:
            sys.stdout.write("%s\n" % str(package))
        serialize(package, args.output)

    except (CompileException, IOError) as err:
        if args.print_stack:
            raise
        if isinstance(err, CompileException):
            sys.stderr.write("%s\n" % str(err))
        else:
            sys.stderr.write("%s: error: %s\n" % (sourceFileName, str(err)))
        sys.exit(1)

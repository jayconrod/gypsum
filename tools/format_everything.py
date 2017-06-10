#!/usr/bin/env python

import argparse
import os
import os.path
import sys

from gypsum.name import Name

from rewriter import (
    readStdSources,
    rewriteFile,
    rewriteTests,
)


def main():
    cmdline = argparse.ArgumentParser(description="Format and rewrite Gypsum source code")
    cmdline.add_argument("files", metavar="file", type=str, nargs="*",
                         help="Source and test file names")
    cmdline.add_argument("--std-sources", action="store", type=str,
                         default="gypsum/utils_test.py",
                         help="Test file containing std sources")
    cmdline.add_argument("--all", action="store_true",
                         help="Process all known files containing Gypsum code")
    args = cmdline.parse_args()

    sys.setrecursionlimit(10000)
    if not os.path.exists("WORKSPACE"):
        sys.stderr.write("error: must be run from workspace root directory")

    fileNames = args.files
    if args.all:
        if len(args.files) > 0:
            sys.stderr.write("error: cannot give both files and --all\n")
            sys.exit(1)
        for dirName in ("gypsum", "codeswitch", "std", "examples"):
            for dirPath, _, baseNames in os.walk(dirName):
                for baseName in baseNames:
                    if (baseName.endswith(".gy") or
                        (baseName.startswith("test_") and
                         baseName.endswith(".py") and
                         baseName not in ("test_lexer.py", "test_parser.py"))):
                        fileNames.append(os.path.join(dirPath, baseName))

    stdName = Name.fromString("std", isPackageName=True)
    stdSources = {}
    if args.std_sources is not None:
        stdSources = readStdSources(args.std_sources)

    for fileName in fileNames:
        sys.stderr.write("processing %s...\n" % fileName)
        baseName = os.path.basename(fileName)
        if baseName.endswith(".gy"):
            rewriteFile(fileName, stdName, [])
        elif baseName.startswith("test_") and baseName.endswith(".py"):
            rewriteTests(fileName, stdSources)


if __name__ == "__main__":
    main()

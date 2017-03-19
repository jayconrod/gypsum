#!/usr/bin/env python

import argparse
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
    cmdline.add_argument("files", metavar="file", type=str, nargs="+",
                         help="Source and test file names")
    cmdline.add_argument("--std-sources", action="store", type=str,
                         help="Test file containing std sources")
    args = cmdline.parse_args()

    sys.setrecursionlimit(10000)
    if not os.path.exists("WORKSPACE"):
        sys.stderr.write("error: must be run from workspace root directory")
    stdName = Name.fromString("std", isPackageName=True)
    stdSources = {}
    if args.std_sources is not None:
        stdSources = readStdSources(args.std_sources)

    for fileName in sys.argv[1:]:
        baseName = os.path.basename(fileName)
        if baseName.endswith(".gy"):
            rewriteFile(fileName, stdName, [])
        elif baseName.startswith("test_") and baseName.endswith(".py"):
            rewriteTests(fileName, stdSources)


if __name__ == "__main__":
    main()

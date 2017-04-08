import os
import re
import StringIO

from gypsum import ast
from gypsum.compile_info import CompileInfo, STD_NAME
from gypsum.errors import CompileException
from gypsum.ids import (AstId, TARGET_PACKAGE_ID)
from gypsum.ir import (Package, PackageVersion)
from gypsum.layout import layout
from gypsum.lexer import lex
from gypsum.location import NoLoc
from gypsum.package_loader import PackageLoader
from gypsum.parser import parse
from gypsum.utils import (decodeString, encodeString)

from formatter import (
    Format,
    Formatter,
)


def rewriteFile(fileName, packageName, packagePaths):
    with open(fileName) as inFile:
        source = inFile.read()
    outFileName = fileName + ".fmt"
    fmt = Format()
    with open(outFileName, "w") as outFile:
        _rewriteSource(fileName, 0, source, packageName, packagePaths, fmt, outFile)
    os.rename(outFileName, fileName)


def rewriteTests(fileName, stdSources):
    outFileName = fileName + ".fmt"
    fmt = Format(
        linesBetweenTopDefns = 0,
        linesBetweenInnerDefns = 0,
        newlineAtEnd = False)
    with open(outFileName, "w") as outFile:
        stdSourcesUsed = []
        sourceLines = []
        inSource = False
        with open(fileName) as inFile:
            lineNumber = 1
            for line in inFile:
                sourceDone = False
                if not inSource:
                    m1 = re.match("^\s*source\s*=\s*([A-Z_]+)\s*\+\s*\\\\$", line)
                    if m1:
                        inSource = True
                        stdSourcesUsed.append(m1.group(1))
                        m2 = None
                    else:
                        m2 = re.match('^\s*source\s*=\s*(".*")(\s*\+\s*\\\\)?$', line)
                    if m2:
                        inSource = True
                        sourceLines.append(decodeString(m2.group(1)))
                        sourceDone = m2.group(2) is None
                    if m1 is None and m2 is None:
                        outFile.write(line)
                else:
                    m1 = re.match("^\s*([A-Z_]+)\s*\+\s*\\\\$", line)
                    if m1:
                        stdSourcesUsed.append(m1.group(1))
                        m2 = None
                    else:
                        m2 = re.match('^\s*(".*")(\s*\+\s*\\\\)?$', line)
                    if m2:
                        sourceLines.append(decodeString(m2.group(1)))
                        sourceDone = m2.group(2) is None
                    if m1 is None and m2 is None:
                        raise IOError("%s:%d: unexpected line in source" %
                                          (fileName, lineNumber))
                if sourceDone:
                    inSource = False
                    combinedStdSource = "".join(stdSources[s] for s in stdSourcesUsed)
                    fullSource = combinedStdSource + "".join(sourceLines)
                    out = StringIO.StringIO()
                    lineOffset = lineNumber - 1 - combinedStdSource.count('\n')
                    _rewriteSource(fileName, lineOffset, fullSource, STD_NAME, [], fmt, out)
                    formattedSource = out.getvalue()[len(combinedStdSource):]
                    formattedLines = formattedSource.splitlines(True)
                    quotedLines = stdSourcesUsed + map(encodeString, formattedLines)
                    if len(quotedLines) == 0:
                        outFile.write('        source = ""\n')
                    elif len(quotedLines) == 1:
                        outFile.write('        source = %s\n' % quotedLines[0])
                    else:
                        outFile.write('        source = %s + \\\n' % quotedLines[0])
                        for i in xrange(1, len(quotedLines) - 1):
                            outFile.write('                 %s + \\\n' % quotedLines[i])
                        outFile.write('                 %s\n' % quotedLines[-1])
                    del stdSourcesUsed[:]
                    del sourceLines[:]

                lineNumber += 1

    os.rename(outFileName, fileName)


def readStdSources(fileName):
    stdSources = {}
    inSource = False
    with open(fileName) as inFile:
        lineNumber = 1
        sourceName = None
        sourceLines = []
        for line in inFile:
            sourceDone = False
            if not inSource:
                m = re.match('^([A-Z_]+)\s*=\s*(".*")(\s*\+\s*\\\\)?$', line)
                if m:
                    inSource = True
                    sourceName = m.group(1)
                    sourceLines.append(decodeString(m.group(2)))
                    sourceDone = m.group(3) is None
            else:
                m = re.match('^\s*(".*")(\s*\+\s*\\\\)?$', line)
                if m:
                    sourceLines.append(decodeString(m.group(1)))
                    sourceDone = m.group(2) is None
                else:
                    raise IOError("%s:%d: unexpected line in source" % (fileName, lineNumber))
            if sourceDone:
                inSource = False
                if sourceName in stdSources:
                    raise IOError("%s:%d: source %s defined multiple times" %
                                  (fileName, lineNumber, sourceName))
                combinedSource = "".join(sourceLines)
                stdSources[sourceName] = combinedSource
                sourceName = None
                del sourceLines[:]

            lineNumber += 1

    return stdSources


def _rewriteSource(fileName, lineOffset, source, packageName, packagePaths, fmt, out):
    comments, source = _readComments(source)
    lineOffset += comments.count('\n')
    try:
        rawTokens = lex(fileName, source)
        layoutTokens = layout(rawTokens)
        astModule = parse(fileName, layoutTokens)
        astPackage = ast.Package([astModule], NoLoc)
        astPackage.id = AstId(-1)

        package = Package(TARGET_PACKAGE_ID,
                          packageName,
                          PackageVersion.fromString("0"))
        loader = PackageLoader(packagePaths)
        loader.ensurePackageInfo()
        info = CompileInfo(astPackage, package, loader, isUsingStd=True)

        out.write(comments)
        formatter = Formatter(fmt, info, out)
        formatter.format()
    except CompileException as e:
        e.location.fileName = fileName
        e.location.beginRow += lineOffset
        e.location.endRow += lineOffset
        raise e


def _readComments(source):
    i = 0
    while i < len(source):
        if (i < len(source) - 3 and
            source[i] == '/' and
            source[i + 1] == '/'):
            while i < len(source) and source[i] != '\n':
                i += 1
            i += 1
            continue
        if source[i] == '\n':
            i += 1
            continue
        break
    return source[:i], source[i:]

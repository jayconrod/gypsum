import os
import re
import sys

def main():
    for fileName in sys.argv[1:]:
        with open(fileName) as inFile:
            outFileName = fileName + ".out"
            with open(outFileName, "w") as outFile:
                lineNumber = 1
                for line in inFile:
                    m = re.match('\s*(.*(?:analyzeFromSource|checkFunction|compileFromSource))\((".*")(.*)$', line)
                    if m:
                        if m.group(3).startswith(" +"):
                            sys.stderr.write("%s:%d: multi-line source\n" % (fileName, lineNumber))
                        else:
                            outFile.write("        source = %s\n" % m.group(2))
                            outFile.write("        %s(source%s\n" % (m.group(1), m.group(3)))
                    else:
                        outFile.write(line)
                    lineNumber += 1
        os.rename(outFileName, fileName)

if __name__ == "__main__":
    main()

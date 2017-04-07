import os
import re
import sys

def main():
    for fileName in sys.argv[1:]:
        with open(fileName) as inFile:
            outFileName = fileName + ".out"
            with open(outFileName, "w") as outFile:
                for line in inFile:
                    m = re.search('info = self\.analyzeFromSource\((".*")\)$', line)
                    if m:
                        outFile.write("        source = %s\n" % m.group(1))
                        outFile.write("        info = self.analyzeFromSource(source)\n")
                    else:
                        outFile.write(line)
        os.rename(outFileName, fileName)

if __name__ == "__main__":
    main()

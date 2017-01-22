# Copyright Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


# Don't run this script directly. Instead, run:
#     bazel build //:gypsum

from setuptools import setup, find_packages

setup(
    name="gypsum",
    version="0",
    description="Gypsum compiler",
    long_description="Compiler for the Gypsum programming language",
    url="https://github.com/jayconrod/gypsum",
    author="Jay Conrod",
    author_email="jayconrod@gmail.com",
    license="GPLv3",
    classifiers=[
        "Development Status :: 2 - Pre-Alpha",
        "Environment :: Console",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: POSIX",
        "Programming Language :: Python :: 2.7",
        "Topic :: Software Development :: Compilers",
    ],
    keywords="gypsum compiler",
    packages=find_packages(),
    package_data={
        "gypsum": [
            "builtins.yaml",
            "flags.yaml",
            "opcodes.yaml",
        ],
    },
    entry_points={
        "console_scripts": [
            "gypsumc=gypsum:main",
        ],
    },
)

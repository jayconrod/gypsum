# Configurations
mode ?= debug
use_pch ?= yes
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  OS := osx
  POSIX := yes
  use_pch := no
  SHARED_LIB_SUFFIX := .dylib
else
ifeq ($(UNAME_S),Linux)
  OS := linux
  POSIX := yes
  SHARED_LIB_SUFFIX := .so
else
  $(warning Unknown OS: $(UNAME_S))
  OS := linux
  POSIX := yes
  SHARED_LIB_SUFFIX := .so
endif
endif
UNAME_M := $(shell uname -m)
ifeq ($(UNAME_M),x86_64)
  ARCH := x64
else
  $(error Unknown architecture: $(UNAME_M))
endif
STATIC_LIB_SUFFIX := .a

# Places
TOP := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
OUT_BASE_DIR := $(TOP)/out
OUT_DIR := $(OUT_BASE_DIR)/$(mode)
GEN_DIR := $(OUT_DIR)/gen
DOC_DIR := $(OUT_DIR)/doc

COMMON_DIR := $(TOP)/common
COMPILER_DIR := $(TOP)/compiler
VM_DIR := $(TOP)/vm
EXAMPLE_DIR := $(TOP)/examples
STD_DIR := $(TOP)/std

# Tools and commands
CXX ?= g++
CXX_COMPILE := $(CXX) -c -std=c++11
CXX_DEPENDS := $(CXX) -std=c++11 -MM
CXX_LINK_EXEC := $(CXX)
AS ?= as
AS_COMPILE := $(AS) -c
AR ?= ar
CXX_LINK_STATIC := $(AR) rcs
CXX_LINK_SHARED := $(CXX) -shared
RM ?= rm
REMOVE := $(RM) -r
PYTHON ?= python
PYINSTALLER := pyinstaller --noconfirm --log-level=WARN
DOXYGEN ?= doxygen

# Internal tools and dependencies.
GY_COMPILER := $(OUT_DIR)/compiler
GY_COMPILE := $(GY_COMPILER) --package-path $(OUT_DIR)
STD_NAME := std
STD_VERSION := 1
STD_PACKAGE = $(OUT_DIR)/$(STD_NAME)-$(STD_VERSION).csp
GY_DEPS := $(GY_COMPILER) $(STD_PACKAGE)

# Flags
CXXFLAGS := -Werror -Wall -Wno-invalid-offsetof -fPIC
LDFLAGS :=
INCLUDES :=
ifeq ($(mode),debug)
  CXXFLAGS += -O0 -g3
  DEFINES += -DDEBUG
endif
ifeq ($(mode),release)
  CXXFLAGS += -O3 -flto
  LDFLAGS += -O3 -flto
endif
ifeq ($(OS),linux)
  DEFINES += -DCS_PLATFORM_LINUX
endif
ifeq ($(OS),osx)
  DEFINES += -DCS_PLATFORM_OSX
endif

# Top-level targets. Included Makefiles should add dependencies.
.PHONY: all check clean

all:

check:

clean:
	$(REMOVE) $(OUT_BASE_DIR)


# Include Makefiles from other projects.
include $(VM_DIR)/Makefile
include $(COMPILER_DIR)/Makefile
include $(EXAMPLE_DIR)/Makefile
include $(STD_DIR)/Makefile

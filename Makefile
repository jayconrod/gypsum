# Configurations
mode ?= debug
use_pch ?= yes
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  OS := osx
  POSIX := yes
  use_pch := no
else
ifeq ($(UNAME_S),Linux)
  OS := linux
  POSIX := yes
else
  $(warning Unknown OS: $(UNAME_S))
  OS := linux
  POSIX := yes
endif
endif


# Places
TOP := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
OUT_BASE_DIR := $(TOP)/out
OUT_DIR := $(OUT_BASE_DIR)/$(mode)
GEN_DIR := $(OUT_DIR)/gen

COMMON_DIR := $(TOP)/common
COMPILER_DIR := $(TOP)/compiler
VM_DIR := $(TOP)/vm
EXAMPLE_DIR := $(TOP)/examples


# Tools and commands
CXX ?= g++
CXX_COMPILE := $(CXX) -c -std=c++11
CXX_DEPENDS := $(CXX) -std=c++11 -MM
CXX_LINK_EXEC := $(CXX)
AR ?= ar
CXX_LINK_STATIC := $(AR) rcs
RM ?= rm
REMOVE := $(RM) -r
PYTHON ?= python
GY_COMPILER ?= $(OUT_DIR)/compiler
PYINSTALLER := pyinstaller --noconfirm --log-level=WARN


# Flags
CXXFLAGS := -Werror -Wall -Wno-invalid-offsetof
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

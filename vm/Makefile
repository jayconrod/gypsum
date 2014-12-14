# Configurations
mode ?= debug
use_pch ?= yes
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  OS := osx
  use_pch := no
else
ifeq ($(UNAME_S),Linux)
  OS := linux
else
  $(warning Unknown OS: $(UNAME_S))
  OS := linux
endif
endif


# Places
HERE := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
LIB_SRC_DIR := $(HERE)/src
TEST_SRC_DIR := $(HERE)/test
INCLUDE_DIR := $(HERE)/include
PROGRAM_SRC_DIR := $(HERE)/programs
COMMON_DIR := $(HERE)/../common
OUT_BASE_DIR := $(HERE)/out
OUT_DIR := $(OUT_BASE_DIR)/$(mode)
GEN_DIR := $(OUT_DIR)/gen


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
GY_COMPILER ?= $(HERE)/../compiler/compiler
MKTEST := $(TEST_SRC_DIR)/mktest.py


# Flags
CXXFLAGS := -Werror -Wall -Wno-invalid-offsetof
LDFLAGS :=
DEFINES := -DWORDSIZE=64 -DPAGESIZE=4096
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

INCLUDES := -I$(INCLUDE_DIR) $(addprefix -iquote,$(LIB_SRC_DIR) $(TEST_SRC_DIR) $(GEN_DIR))

# Sources and intermediates
csp_packages = $(addprefix $(GEN_DIR)/,$(addsuffix .csp,$(notdir $(basename $1))))
csp_tests = $(addprefix $(GEN_DIR)/,$(addsuffix .cpp,$(notdir $(basename $1))))
objects = $(addprefix $(OUT_DIR)/obj/,$(addsuffix .o,$(notdir $(basename $1))))
deps = $(addprefix $(OUT_DIR)/obj/,$(addsuffix .d,$(notdir $(basename $1))))
programs = $(addprefix $(OUT_DIR)/,$(notdir $(basename $1)))

LIB_NAME := codeswitch
LIB_SOURCES := $(wildcard $(LIB_SRC_DIR)/*.cpp) $(wildcard $(LIB_SRC_DIR)/$(OS)/*.cpp)
LIB_GEN := $(GEN_DIR)/opcodes.h $(GEN_DIR)/builtins.h $(GEN_DIR)/flags.h $(GEN_DIR)/roots-builtins.cpp
LIB_GEN_OBJS := $(call objects,$(filter %.cpp,$(LIB_GEN)))
LIB_OBJECTS := $(call objects,$(LIB_SOURCES)) $(LIB_GEN_OBJS)
LIB_DEPS := $(call deps,$(LIB_SOURCES))
LIB := $(OUT_DIR)/$(LIB_NAME).a

LIB_PREFIX := $(LIB_SRC_DIR)/prefix.h
ifeq ($(use_pch),yes)
  LIB_PREFIX_PCH := prefix_.h
  LIB_PREFIX_COMPILED := $(GEN_DIR)/$(LIB_PREFIX_PCH).gch
  INCLUDES += -inlcude $(LIB_PREFIX_PCH)
else
  INCLUDES += -include $(LIB_PREFIX)
endif

TEST_GY_SOURCES := $(wildcard $(TEST_SRC_DIR)/*.gy)
TEST_CSP_PACKAGES := $(call csp_packages,$(TEST_GY_SOURCES))
TEST_CSP_TESTS := $(call csp_tests,$(TEST_CSP_PACKAGES))
TEST_CXX_SOURCES := $(wildcard $(TEST_SRC_DIR)/*.cpp)
TEST_SOURCES := $(TEST_CXX_SOURCES) $(TEST_CSP_TESTS)
TEST_OBJECTS := $(call objects,$(TEST_SOURCES))
TEST_DEPS := $(call deps,$(TEST_SOURCES))
TEST := $(OUT_DIR)/test-driver

PROGRAM_SOURCES := $(wildcard $(PROGRAM_SRC_DIR)/*.cpp)
PROGRAM_OBJECTS := $(call objects,$(PROGRAM_SOURCES))
PROGRAM_DEPS := $(call deps,$(PROGRAM_SOURCES))
PROGRAMS := $(call programs,$(PROGRAM_SOURCES))

SOURCES := $(LIB_SOURCES) $(TEST_SOURCES) $(PROGRAM_SOURCES)
DEPS := $(LIB_DEPS) $(TEST_DEPS) $(PROGRAM_DEPS)
OBJECTS := $(LIB_OBJECTS) $(TEST_OBJECTS) $(PROGRAM_OBJECTS)
GEN := $(LIB_GEN)

# Rules
.PHONY: all clean test

all: $(PROGRAMS) test

clean:
	$(REMOVE) $(OUT_BASE_DIR)

test: $(TEST)
	@$(TEST)

$(PROGRAMS) $(TEST):
	$(CXX_LINK_EXEC) $(LDFLAGS) $^ -o $@

$(LIB):
	$(CXX_LINK_STATIC) $@ $^

ifeq ($(use_pch),yes)
$(LIB_PREFIX_COMPILED): $(LIB_PREFIX)
	@mkdir -p $(dir $@)
	$(CXX_COMPILE) $(CXXFLAGS) -x c++-header $(DEFINES) $(INCLUDES) $< -o $@
endif

$(OBJECTS):
	@mkdir -p $(dir $@)
	@$(CXX_DEPENDS) $(CXXFLAGS) $(DEFINES) $(INCLUDES) -MT $@ $(filter %.cpp,$^) >$(call deps,$(filter %.cpp,$^))
	$(CXX_COMPILE) $(CXXFLAGS) $(DEFINES) $(INCLUDES) $(filter %.cpp,$^) -o $@

$(TEST_CSP_PACKAGES):
	@mkdir -p $(dir $@)
	$(GY_COMPILER) $< -o $@

$(TEST_CSP_TESTS):
	@mkdir -p $(dir $@)
	$(PYTHON) $(MKTEST) $< $@

$(GEN):
	@mkdir -p $(dir $@)
	$(PYTHON) $^ $@


# Dependencies
$(GEN_DIR)/opcodes.h: $(LIB_SRC_DIR)/gen_opcodes.py $(COMMON_DIR)/opcodes.yaml
$(GEN_DIR)/builtins.h: $(LIB_SRC_DIR)/gen_builtins_h.py $(COMMON_DIR)/builtins.yaml
$(GEN_DIR)/flags.h: $(LIB_SRC_DIR)/gen_flags_h.py $(COMMON_DIR)/flags.yaml
$(GEN_DIR)/roots-builtins.cpp: $(LIB_SRC_DIR)/gen_roots_builtins_cpp.py $(COMMON_DIR)/builtins.yaml

$(LIB): $(LIB_OBJECTS)

$(TEST): $(TEST_OBJECTS) $(LIB)

$(OBJECTS): $(filter %.h,$(GEN))
ifeq ($(use_pch),yes)
$(LIB_PREFIX_COMPILED): $(filter %.h,$(GEN))
$(OBJECTS): $(LIB_PREFIX_COMPILED)
endif

$(OBJECTS) $(LIB_PREFIX_COMPILED): $(filter %.h,$(GEN))
$(OBJECTS): $(LIB_PREFIX_COMPILED)

$(foreach src,$(PROGRAM_SOURCES),\
	$(eval $(call programs,$(src)): $(call objects,$(src)) $(LIB)))

$(foreach src,$(SOURCES),\
  $(eval $(call objects,$(src)): $(src)))

$(foreach src,$(filter %.cpp,$(LIB_GEN)),\
  $(eval $(call objects,$(src)): $(src)))

$(foreach src,$(TEST_GY_SOURCES),\
  $(eval $(call csp_packages,$(src)): $(src)))

$(foreach pkg,$(TEST_CSP_PACKAGES),\
  $(eval $(call csp_tests,$(pkg)): $(pkg) $(MKTEST)))

$(foreach dep,$(DEPS),\
  $(eval -include $(dep)))

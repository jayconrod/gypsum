STD_IO_DIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
STD_IO_SRC_DIR := $(STD_IO_DIR)/src

objects = $(addprefix $(OUT_DIR)/obj/,$(addsuffix .o,$(notdir $(basename $1))))
deps = $(addprefix $(OUT_DIR)/obj/,$(addsuffix .d,$(notdir $(basename $1))))

STD_IO_NAME := std.io
STD_IO_VERSION := 1
STD_IO_PACKAGE := $(OUT_DIR)/$(STD_IO_NAME)-$(STD_IO_VERSION).csp
STD_IO_SOURCES := $(wildcard $(STD_IO_SRC_DIR)/*.gy)

STD_IO_NATIVE_LIB := $(OUT_DIR)/lib$(STD_IO_NAME)-$(STD_IO_VERSION)$(SHARED_LIB_SUFFIX)
STD_IO_NATIVE_SOURCES := $(wildcard $(STD_IO_SRC_DIR)/*.cpp)
STD_IO_NATIVE_OBJECTS := $(call objects,$(filter %.cpp,$(STD_IO_NATIVE_SOURCES)))
STD_IO_NATIVE_DEPS := $(call deps,$(STD_IO_NATIVE_SOURCES))
CS_HEADER := $(VM_DIR)/include/codeswitch.h

.PHONY: check-std-io

build-std: $(STD_IO_PACKAGE) $(STD_IO_NATIVE_LIB)

$(STD_IO_PACKAGE): $(STD_IO_SOURCES) $(STD_PACKAGE) $(GY_COMPILER)
	$(GY_COMPILER) --package-path $(OUT_DIR) --package-name $(STD_IO_NAME) --package-version $(STD_IO_VERSION) -o $@ $(STD_IO_SOURCES)

$(STD_IO_NATIVE_LIB): $(STD_IO_NATIVE_OBJECTS)
	$(CXX_LINK_SHARED) $^ -o $@

$(STD_IO_NATIVE_OBJECTS):
	@mkdir -p $(dir $@)
	@$(CXX_DEPENDS) $(CXXFLAGS) $(DEFINES) $(INCLUDES) -MT $@ $(filter %.cpp,$^) >$(call deps,$(filter %.cpp,$^))
	$(CXX_COMPILE) $(CXXFLAGS) $(DEFINES) $(INCLUDES) $(LIB_PREFIX_INCLUDE) $(filter %.cpp,$^) -o $@

$(foreach src,$(filter %.cpp,$(STD_IO_NATIVE_SOURCES)),\
  $(eval $(call objects,$(src)): $(src) $(CS_HEADER)))

$(foreach dep,$(STD_IO_NATIVE_DEPS),\
  $(eval -include $(dep)))

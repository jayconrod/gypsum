EXAMPLE_SOURCES = $(wildcard $(EXAMPLE_DIR)/*.gy)
EXAMPLE_OUT_DIR = $(OUT_DIR)/examples

csp_packages = $(addprefix $(EXAMPLE_OUT_DIR)/,$(addsuffix .csp,$(notdir $(basename $1))))

EXAMPLE_PACKAGES := $(call csp_packages,$(EXAMPLE_SOURCES))

.PHONY: build-examples

all: build-examples

build-examples: $(EXAMPLE_PACKAGES)

$(EXAMPLE_PACKAGES):
	@mkdir -p $(EXAMPLE_OUT_DIR)
	$(GY_COMPILER) $< -o $@

$(foreach src,$(EXAMPLE_SOURCES),\
  $(eval $(call csp_packages,$(src)): $(src) $(GY_COMPILER)))

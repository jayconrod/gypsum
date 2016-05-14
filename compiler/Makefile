COMPILER_SOURCES := $(filter-out test_%.py,$(wildcard $(COMPILER_DIR)/*.py))
COMMON_FILES := $(wildcard $(COMMON_DIR)/*.yaml)

.PHONY: build-compiler check-compiler

all: build-compiler

build-compiler: $(OUT_DIR)/compiler

$(OUT_DIR)/compiler: $(COMPILER_DIR)/compiler.spec $(COMPILER_SOURCES) $(COMMON_FILES)
	GYPSUM_COMMON_FILES="$(COMMON_FILES)" $(PYINSTALLER) $< --distpath=$(OUT_DIR) --workpath=$(GEN_DIR)/pyinstaller

check: check-compiler

check-compiler:
	$(COMPILER_DIR)/run-tests

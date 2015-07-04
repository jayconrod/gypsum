STD_SRC_DIR := $(STD_DIR)/src
STD_SOURCES := $(wildcard $(STD_SRC_DIR)/*.gy)

.PHONY: build-std check-std

all: build-std

build-std: $(STD_PACKAGE)

$(STD_PACKAGE): $(STD_SOURCES) $(GY_COMPILER)
	$(GY_COMPILER) --no-std --package-name $(STD_NAME) --package-version $(STD_VERSION) -o $@ $(STD_SOURCES)

check: check-std

# TODO: write a testing framework and add some tests.
check-std: build-std

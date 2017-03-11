#!/bin/bash

set -euxo pipefail

echo ">>> Running all Bazel tests"
bazel \
    --output_base=$HOME/.cache/bazel \
    --batch \
    --host_jvm_args=-Xmx500m \
    --host_jvm_args=-Xms500m \
    test \
    --verbose_failures \
    --sandbox_debug \
    --test_output=errors \
    --test_strategy=standalone \
    --spawn_strategy=standalone \
    --genrule_strategy=standalone \
    --local_resources=400,1,1.0 \
    //...


echo ">>> Running all examples"
for label in $(bazel query //examples:all); do
    bazel \
        --output_base=$HOME/.cache/bazel \
        --batch \
        --host_jvm_args=-Xmx500m \
        --host_jvm_args=-Xms500m \
        run \
        --verbose_failures \
        --sandbox_debug \
        --test_output=errors \
        --test_strategy=standalone \
        --spawn_strategy=standalone \
        --genrule_strategy=standalone \
        --local_resources=400,1,1.0 \
        "$label"
done


echo ">>> All presubmit tests passed."

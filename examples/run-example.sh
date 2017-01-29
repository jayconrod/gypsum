#!/bin/bash

if [ $# -ne 1 -o ! -f "$1" ]; then
    echo "usage: $0 example.gy" >&2
    exit 1
fi

if ! type bazel 2>&1 >/dev/null ; then
    cat >&2 <<EOF
error: bazel not found
Bazel is needed to build Gypsum, CodeSwitch, and these examples.
It can be found at https://bazel.build/
EOF
    exit 1
fi

bin_dir=$(readlink -f $(dirname "${BASH_SOURCE[0]}")/../bazel-bin)
example_name=$(basename "$1" .gy)

bazel build //codeswitch
bazel build "//examples:$example_name"
exec "$bin_dir/codeswitch/codeswitch" -P "$bin_dir/std" "$bin_dir/examples/$example_name-1.csp"

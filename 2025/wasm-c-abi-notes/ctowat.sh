#!/usr/bin/env bash
set -euo pipefail
set -x

if [ $# -ne 1 ]; then
    echo "usage: $0 file.c" >&2
    exit 1
fi

src="$1"
base="${src%.*}"

# Compile C to wasm (linked, with names, no runtime)
# Linking is needed to construct the names section in WASM.
clang \
  --target=wasm32-unknown-unknown \
  -O2 -g \
  -nostdlib \
  -Wl,--no-entry \
  -Wl,--export-all \
  "$src" \
  -o "${base}.wasm"

# Print WAT to stdout
wasm-tools print "${base}.wasm"

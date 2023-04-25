#!/bin/bash

set -eux
set -o pipefail

mkdir -p target
wat2wasm examples/hello.wat -o target/hello.wasm
wat2wasm examples/watenv.wat -o target/watenv.wasm
tinygo build -o target/hellogo.wasm -target=wasi examples/hellogo/hellogo.go &
tinygo build -o target/goenv.wasm -target=wasi examples/goenv/goenv.go &

(cd examples/rustenv; cargo build --target wasm32-wasi --release)
cp examples/rustenv/target/wasm32-wasi/release/rustenv.wasm target/

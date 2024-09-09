.PHONY: build serve

# Here we have two variants of the build target: one for the Go compiler and one
# for TinyGo. You can run either of them to build the WebAssembly binary.
# Each has their own wasm_exec.js file, so the right one has to be downoaded
# before buildling.

wasm_exec_go.js:
	wget https://raw.githubusercontent.com/golang/go/release-branch.go1.22/misc/wasm/wasm_exec.js -O wasm_exec_go.js
	touch $@

wasm_exec_tinygo.js:
	wget https://raw.githubusercontent.com/tinygo-org/tinygo/release/targets/wasm_exec.js -O wasm_exec_tinygo.js
	touch $@

build: wasm_exec_go.js
	cp wasm_exec_go.js wasm_exec.js
	GOOS=js GOARCH=wasm go build -o harmonic.wasm harmonic.go

build-tinygo: wasm_exec_tinygo.js
	cp wasm_exec_tinygo.js wasm_exec.js
	GOOS=js GOARCH=wasm tinygo build -o harmonic.wasm harmonic.go

serve:
	go run github.com/eliben/static-server@latest -port 8873 .

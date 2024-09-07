.PHONY: build serve

build: wasm_exec.js
	GOOS=js GOARCH=wasm go build -o harmonic.wasm harmonic.go
	
wasm_exec.js:
	wget https://raw.githubusercontent.com/golang/go/release-branch.go1.22/misc/wasm/wasm_exec.js

serve:
	go run github.com/eliben/static-server@latest -port 8873 .

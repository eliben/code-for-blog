.PHONY: build serve

build: wasm_exec.js
	GOOS=js GOARCH=wasm go build -o life.wasm life.go
	
wasm_exec.js:
	wget https://raw.githubusercontent.com/golang/go/release-branch.go1.24/lib/wasm/wasm_exec.js

serve:
	go run github.com/eliben/static-server@latest -port 8873 .

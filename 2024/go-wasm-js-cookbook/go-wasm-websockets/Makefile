.PHONY: build serve

build: wasm_exec.js
	cd wasmsrc && \
	GOOS=js GOARCH=wasm go build -o ../wsclient.wasm wsclient.go
	
wasm_exec.js:
	wget https://raw.githubusercontent.com/golang/go/release-branch.go1.24/lib/wasm/wasm_exec.js

# In this sample, server.go already serves everything over HTTP, so we don't
# need to invoke static-server separately.
serve:
	go run server.go

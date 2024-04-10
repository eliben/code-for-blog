.PHONY: build serve

build:
	mkdir -p embed_data
	cp ../../data/cl100k_base.tiktoken embed_data/
	GOOS=js GOARCH=wasm go build -o assets/gobpe.wasm main.go

serve:
	go run github.com/eliben/static-server@latest -port 8873 assets

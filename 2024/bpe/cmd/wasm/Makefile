.PHONY: build serve

build:
	GOOS=js GOARCH=wasm go build -o assets/gobpe.wasm main.go

serve:
	go run github.com/eliben/static-server@latest -port 8873 assets

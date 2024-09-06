.PHONY: build serve

build:
	GOOS=js GOARCH=wasm go build -o harmonic.wasm harmonic.go

serve:
	go run github.com/eliben/static-server@latest -port 8873 .

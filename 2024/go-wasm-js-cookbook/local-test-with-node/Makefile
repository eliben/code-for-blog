.PHONY: test

test: main_test.go supportfiles/wasm_exec.js supportfiles/wasm_exec_node.js supportfiles/go_js_wasm_exec
	GOOS=js GOARCH=wasm go test -exec=supportfiles/go_js_wasm_exec -v .

run: main.go supportfiles/wasm_exec.js supportfiles/wasm_exec_node.js supportfiles/go_js_wasm_exec
	GOOS=js GOARCH=wasm go run -exec=supportfiles/go_js_wasm_exec main.go

supportfiles:
	mkdir -p supportfiles

supportfiles/wasm_exec.js: supportfiles
	wget https://raw.githubusercontent.com/golang/go/release-branch.go1.22/misc/wasm/wasm_exec.js -O supportfiles/wasm_exec.js

supportfiles/wasm_exec_node.js: supportfiles
	wget https://raw.githubusercontent.com/golang/go/release-branch.go1.22/misc/wasm/wasm_exec_node.js -O supportfiles/wasm_exec_node.js

supportfiles/go_js_wasm_exec: supportfiles
	wget https://raw.githubusercontent.com/golang/go/release-branch.go1.22/misc/wasm/go_js_wasm_exec -O supportfiles/go_js_wasm_exec
	chmod +x $@

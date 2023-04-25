package main

import (
	"bytes"
	"context"
	_ "embed"
	"fmt"
	"log"
	"net/http"
	"os"
	"strings"

	"github.com/tetratelabs/wazero"
	"github.com/tetratelabs/wazero/api"
	"github.com/tetratelabs/wazero/imports/wasi_snapshot_preview1"
)

// invokeWasmModule invokes the given WASM module (given as a file path),
// setting its env vars according to env. Returns the module's stdout.
func invokeWasmModule(modname string, wasmPath string, env map[string]string) (string, error) {
	ctx := context.Background()

	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	// Instantiate the wasm runtime, setting up exported functions from the host
	// that the wasm module can use for logging purposes.
	_, err := r.NewHostModuleBuilder("env").
		NewFunctionBuilder().
		WithFunc(func(v uint32) {
			log.Printf("[%v]: %v", modname, v)
		}).
		Export("log_i32").
		NewFunctionBuilder().
		WithFunc(func(ctx context.Context, mod api.Module, ptr uint32, len uint32) {
			// Read the string from the module's exported memory.
			if bytes, ok := mod.Memory().Read(ptr, len); ok {
				log.Printf("[%v]: %v", modname, string(bytes))
			} else {
				log.Printf("[%v]: log_string: unable to read wasm memory", modname)
			}
		}).
		Export("log_string").
		Instantiate(ctx)
	if err != nil {
		return "", err
	}

	wasmObj, err := os.ReadFile(wasmPath)
	if err != nil {
		return "", err
	}

	// Set up stdout redirection and env vars for the module.
	var stdoutBuf bytes.Buffer
	config := wazero.NewModuleConfig().WithStdout(&stdoutBuf)

	for k, v := range env {
		config = config.WithEnv(k, v)
	}

	// Instantiate the module. This invokes the _start function by default.
	_, err = r.InstantiateWithConfig(ctx, wasmObj, config)
	if err != nil {
		return "", err
	}

	return stdoutBuf.String(), nil
}

func httpHandler(w http.ResponseWriter, req *http.Request) {
	parts := strings.Split(strings.Trim(req.URL.Path, "/"), "/")
	if len(parts) < 1 {
		http.Error(w, "want /{modulename} prefix", http.StatusBadRequest)
		return
	}
	mod := parts[0]
	log.Printf("module %v requested with query %v", mod, req.URL.Query())

	env := map[string]string{
		"http_path":   req.URL.Path,
		"http_method": req.Method,
		"http_host":   req.Host,
		"http_query":  req.URL.Query().Encode(),
		"remote_addr": req.RemoteAddr,
	}

	modpath := fmt.Sprintf("target/%v.wasm", mod)
	log.Printf("loading module %v", modpath)
	out, err := invokeWasmModule(mod, modpath, env)
	if err != nil {
		log.Printf("error loading module %v", modpath)
		http.Error(w, "unable to find module "+modpath, http.StatusNotFound)
		return
	}

	// The module's stdout is written into the response.
	fmt.Fprint(w, out)
}

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("/", httpHandler)
	log.Fatal(http.ListenAndServe(":8080", mux))
}

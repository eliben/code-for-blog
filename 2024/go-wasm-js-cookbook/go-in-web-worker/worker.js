// Web worker for invoking generateBoard from Go in a separate thread.
importScripts("wasm_exec.js");
console.log("Worker is running");

// Load the WASM module with Go code.
const go = new Go();
WebAssembly.instantiateStreaming(fetch("harmonic.wasm"), go.importObject).then(
    (result) => {
        go.run(result.instance);
        console.log("Worker loaded WASM module");
    }).catch((err) => {
        console.error("Worker failed to load WASM module: ", err)
    });

// The worker's logic is very simple: it waits for a "calculate" message with
// the parameter, runs calculate and returns a "result" message.
onmessage = ({ data }) => {
    let { action, payload } = data;
    console.log("Worker received message: ", action, payload);
    switch (action) {
        case "calculate":
            let result = calcHarmonic(payload);
            postMessage({ action: "result", payload: result });
            break;
        default:
            throw (`unknown action '${action}'`);
    }
};

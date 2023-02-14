// Sample of indirect calls in WASM.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.

const fs = require('fs');
const wasmfile = fs.readFileSync(__dirname + '/table.wasm');

// This object is imported into wasm.
const importObject = {
    env: {
        jstimes3: (n) => 3 * n,
    }
}

WebAssembly.instantiate(new Uint8Array(wasmfile), importObject).then(obj => {
    // Get two exported functions from wasm.
    let times2 = obj.instance.exports.times2;
    let times3 = obj.instance.exports.times3;

    console.log('times2(12) =>', times2(12));
    console.log('times3(12) =>', times3(12));
});

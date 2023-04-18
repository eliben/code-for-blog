// Load and test the `itoa` WASM function.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
const assert = require('node:assert');
const fs = require('fs');

(async () => {
    const n = parseInt(process.argv[2] || "42");
    const bytes = fs.readFileSync(__dirname + '/itoa.wasm');

    // This object is imported by the wasm module.
    let importObject = {
        env: {
            log: function (n) {
                console.log(`log: ${n}`);
            }
        }
    };

    let obj = await WebAssembly.instantiate(new Uint8Array(bytes), importObject);
    let mem = obj.instance.exports.memory;

    console.log(`invoking itoa(${n})`);
    let [ptr, len] = obj.instance.exports.itoa(n);
    console.log(`ptr=${ptr} len=${len}`);
    let str = extract_string(mem, ptr, len);
    console.log(`out=${str}`);

    // Memory scan to detect all non-zero slots. Useful for debugging.
    // let v = new Uint8Array(mem.buffer, 0, 10000);
    // for (let i = 0; i < 10000; i++) {
    //     if (v[i] !== 0) {
    //         console.log(`mem[${i}]=${v[i]}`);
    //     }
    // }

    do_test(mem, obj.instance.exports.itoa);
})();

function do_test(mem, itoa_func) {
    let to_string = (n) => {
        let [ptr, len] = itoa_func(n);
        return extract_string(mem, ptr, len);
    }

    assert.equal(to_string(0), '0');
    assert.equal(to_string(1), '1');
    assert.equal(to_string(4), '4');
    assert.equal(to_string(9), '9');
    assert.equal(to_string(12), '12');
    assert.equal(to_string(28), '28');
    assert.equal(to_string(55), '55');
    assert.equal(to_string(10), '10');
    assert.equal(to_string(99), '99');
    assert.equal(to_string(234), '234');
    assert.equal(to_string(881), '881');
    assert.equal(to_string(1360), '1360');
    assert.equal(to_string(20088), '20088');

    // Now brute-force round robin conversion for the first few numbers.
    for (let i = 0; i < 21000; i++) {
        let s = to_string(i);
        assert.equal(i, parseInt(s));
    }
}

// extract_string extracts a string from a wasm memory buffer, given the
// memory object, and offset and the length of the string. Returns a newly
// allocated JS string.
function extract_string(mem, offset, len) {
    const buf = new Uint8Array(mem.buffer, offset, len);
    return str = new TextDecoder('utf8').decode(buf);   
}

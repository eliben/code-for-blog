import assert from 'node:assert/strict';
import { solve } from '../eqsolve.js';

function assertArraysAlmostEqual(a, b) {
    assert.equal(a.length, b.length);
    for (let i = 0; i < a.length; i++) {
        assert(Math.abs(a[i] - b[i]) < 1e-6, `a[${i}]=${a[i]}   !=   b[${i}]=${b[i]}`);
    }
}

let m1 = [
    [1, 2],
    [4, 5],
];
assertArraysAlmostEqual(solve(m1, [3, 6]), [-1, 2]);

// Example from Wikipedia
let m2 = [
    [2, 1, -1],
    [-3, -1, 2],
    [-2, 1, 2],
];
assertArraysAlmostEqual(solve(m2, [8, -11, -3]), [2, 3, -1]);

// Example from Wikipedia
let m3 = [
    [1, 3, 1],
    [1, 1, -1],
    [3, 11, 5],
];
assert.throws(() => solve(m3, [9, 1, 35], "no unique solution"));
import assert from 'node:assert/strict';
import { solve } from '../eqsolve.js';

function assertArraysAlmostEqual(a, b) {
    assert.equal(a.length, b.length);
    for (let i = 0; i < a.length; i++) {
        assert(Math.abs(a[i] - b[i]) < 1e-6, `a[${i}]=${a[i]}   !=   b[${i}]=${b[i]}`);
    }
}

{
    let m = [
        [1, 2],
        [4, 5],
    ];
    assertArraysAlmostEqual(solve(m, [3, 6]), [-1, 2]);
}

{
    let m = [
        [2, 1, -1],
        [-3, -1, 2],
        [-2, 1, 2],
    ];
    assertArraysAlmostEqual(solve(m, [8, -11, -3]), [2, 3, -1]);
}

{
    let m = [
        [9, 3, 4],
        [4, 3, 4],
        [1, 1, 1],
    ];
    assertArraysAlmostEqual(solve(m, [7, 8, 3]), [-0.2, 4, -0.8]);
}

{
    let m = [
        [1, 2, 0.1, 0.2],
        [4, 1, -5, 0.1],
        [-7, 7, -7, 7],
        [2, -1, 0.5, 0.5],
    ];
    assertArraysAlmostEqual(solve(m, [9, 8, 9, 0.5]), [1.834305, 3.5379, 0.578231, 0.160349]);
}

{
    let m = [
        [1, 3, 1],
        [1, 1, -1],
        [3, 11, 5],
    ];
    assert.throws(() => solve(m, [9, 1, 35], "no unique solution"));
}

{
    let m = [
        [2, 4, -3],
        [5, 10, -7],
        [3, 6, 5],
    ];
    assert.throws(() => solve(m, [-1, -2, 9], "no unique solution"));
}
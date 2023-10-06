import assert from 'node:assert/strict';
import { solve } from '../eqsolve.js';
import { buildSplineEquations } from '../spline.js';

function assertArraysAlmostEqual(a, b) {
    assert.equal(a.length, b.length);
    for (let i = 0; i < a.length; i++) {
        assert(Math.abs(a[i] - b[i]) < 1e-4, `a[${i}]=${a[i]}   !=   b[${i}]=${b[i]}`);
    }
}

// Print the 2D array, with each row on a separate line.
function print2DArray(arr) {
    for (let i = 0; i < arr.length; i++) {
        let rowstr = arr[i].map(e => e.toString().padStart(5, ' ')).join(" ");
        console.log(rowstr);
    }
}

// Tests for eqsolve.js
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

// Tests for spline.js
{
    // Example from https://www.youtube.com/watch?v=wBqFnJNJH1w
    let xs = [1, 3, 5, 8];
    let ys = [2, 3, 9, 10];
    let [A, b] = buildSplineEquations(xs, ys);
    let coeffs = solve(A, b);
    console.log(coeffs);

    assertArraysAlmostEqual(coeffs, [0.19956, -0.59868, 0.30043, 2.09868, -0.37280, 4.55263, -15.1535, 17.55263, 0.11549, -2.771929, 21.46929, -43.4853]);
}

{
    // Example from https://pythonnumericalmethods.berkeley.edu/notebooks/chapter17.03-Cubic-Spline-Interpolation.html
    let xs = [0, 1, 2];
    let ys = [1, 3, 2];
    let [A, b] = buildSplineEquations(xs, ys);
    let coeffs = solve(A, b);

    assertArraysAlmostEqual(coeffs, [-0.75, 0, 2.75, 1, 0.75, -4.5, 7.25, -0.5]);
}

console.log('success');

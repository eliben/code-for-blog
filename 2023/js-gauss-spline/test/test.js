import assert from 'node:assert/strict';
import {solve} from '../eqsolve.js';

let arr = [
    [1, 2],
    [4, 5],
]; 
assert.deepEqual(solve(arr, [3, 6]), [-1, 2]);



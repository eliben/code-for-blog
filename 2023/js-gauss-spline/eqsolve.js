// Code to solve a system of linear equations using Gauss-Jordan elimination.
// The main entry point is the solve() function.
'use strict';

// This code uses an array-of-arrays representation of 2D matrices, e.g.:
// 
// let mat = [
//     [-1, 4, -2, -15],
//     [-4, 6, 1, -5],
//     [-6, -6, -2, -10],
// ];

// solve solves the system of linear equations Ax = b, where A is a matrix
// and b is an array representing a column vector. The solution x is returned
// as an array. solve throws an exception if the system doesn't have a unique
// solution.
// A is modified in place - it should be cloned outside this function if you
// want to preserve the original.
export function solve(A, b) {
    // Step 1: create the augmented matrix [A|b], while making sure all
    // dimensions match. The resulting matrix has R rows and R+1 columns.
    let R = A.length;
    if (R != b.length) {
        throw new Error("A and b must have the same number of rows");
    }
    for (let i = 0; i < R; i++) {
        if (A[i].length != R) {
            throw new Error("A must be square");
        }
        A[i].push(b[i]);
    }

    // Step 2: perform Gaussian elimination on the augmented matrix. This
    // modifies A to be in row echelon form.
    gaussEliminate(A);

    // Step 3: back-substitution. This modifies A to be in reduced row
    // echelon form.
    for (let i = R - 1; i >= 0; i--) {
        // For each row, take its pivot and divide the last column by it,
        // then eliminate the pivot from all rows above.
        let pivot = A[i][i];
        if (pivot == 0) {
            throw new Error("System has no unique solution");
        }
        for (let j = i - 1; j >= 0; j--) {
            let f = A[j][i] / pivot;
            A[j][i] = 0;
            A[j][R] -= A[i][R] * f;
        }
        A[i][i] = 1;
        A[i][R] /= pivot;
    }

    // Step 4: extract the solution vector from the last column of A.
    let x = [];
    for (let i = 0; i < R; i++) {
        x.push(A[i][R]);
    }
    return x;
}

// Print the 2D array, with each row on a separate line.
function print2DArray(arr) {
    for (let i = 0; i < arr.length; i++) {
        let rowstr = arr[i].map(e => e.toString().padStart(3, ' ')).join(" ");
        console.log(rowstr);
    }
}

// findPivotRow finds the "pivot" row in arr, for column col and beginning
// with startRow. The pivot row is the row with the largest (in absolute value)
// element in column col among rows [startRow:arr.length). The index of the
// pivot row is returned.
function findPivotRow(arr, startRow, col) {
    let maxidx = startRow;
    for (let i = startRow + 1; i < arr.length; i++) {
        if (Math.abs(arr[i][col]) > Math.abs(arr[maxidx][col])) {
            maxidx = i;
        }
    }
    return maxidx;
}

// swapRows swaps rows i and j in arr, in place.
function swapRows(arr, i, j) {
    if (i != j) {
        let tmp = arr[i];
        arr[i] = arr[j];
        arr[j] = tmp;
    }
}

// gaussEliminate performs Gaussian elimination on arr, in place. After running,
// arr will be in row echelon form.
// This code follows the pseudocode from Wikipedia, with partial pivoting
// (https://en.wikipedia.org/wiki/Gaussian_elimination). It selects the largest
// possible absolute value for each column to improve numerical stability.
// TODO: more comments
function gaussEliminate(arr) {
    let nrows = arr.length;
    let ncols = arr[0].length;

    let h = 0;
    let k = 0;

    while (h < nrows && k < ncols) {
        let pivotRow = findPivotRow(arr, h, k);
        if (arr[pivotRow][k] == 0) {
            // No pivot in this column; move on to the next one.
            k++;
        } else {
            // Swap current row with the pivot row, so we can use the pivot's
            // leading element to eliminate below.
            swapRows(arr, h, pivotRow);

            for (let i = h + 1; i < nrows; i++) {
                let f = arr[i][k] / arr[h][k];
                arr[i][k] = 0;
                for (let j = k + 1; j < ncols; j++) {
                    arr[i][j] -= arr[h][j] * f;
                }
            }
            h++;
            k++;
        }
    }
}

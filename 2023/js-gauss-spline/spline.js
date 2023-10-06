'use strict';

// xs is an array of x coordinates (sorted from left to right), ys is an array
// of corresponding y coordinates. For N points, returns a pair [A, b] where
// Ax = b represents the system of linear equations to solve to determine the
// coefficients of the cubic polynomials that interpolate the points.
// There are N-1 polynomials, so A is NxN and b is an array of N values.
function buildSplineEquations(xs, ys) {
    // Npolys is the number of (cubic) polynomials we interpolate between
    // the given points. Ncoeffs is the number of coefficients they all have
    // together (4 per poly: ax^3 + bx^2 + cx + d).
    // Npoints is the number of points.
    const Npoints = xs.length;
    const Npolys = Npoints - 1;
    const Ncoeffs = 4 * Npolys;

    // The matrix A is the coefficient matrix for the system of linear
    // equations we need to solve to find the coefficients of the polynomials.
    // It has Ncoeffs rows and columns.
    // for each poly i, A[4*i][0..3] are the 4 coefficients of this poly.
    let A = [];
    for (let i = 0; i < Ncoeffs; i++) {
        A.push(Array(Ncoeffs).fill(0));
    }

    // The vector b is the right-hand side of the system of linear equations.
    // It has Ncoeffs values.
    let b = Array(Ncoeffs).fill(0);

    // Now we start filling in the matrix A and vector b.
    // First, we fill in the constraints that the polynomials must pass
    // through the given points. This populates the first 2*Npolys rows.
    let nrow = 0;
    for (let i = 0; i < Npolys; i++, nrow += 2) {
        // Poly i passes through points i and i+1.
        A[nrow][4 * i] = xs[i] ** 3;
        A[nrow][4 * i + 1] = xs[i] ** 2;
        A[nrow][4 * i + 2] = xs[i];
        A[nrow][4 * i + 3] = 1;
        b[nrow] = ys[i];

        A[nrow + 1][4 * i] = xs[i + 1] ** 3;
        A[nrow + 1][4 * i + 1] = xs[i + 1] ** 2;
        A[nrow + 1][4 * i + 2] = xs[i + 1];
        A[nrow + 1][4 * i + 3] = 1;
        b[nrow + 1] = ys[i + 1];
    }

    // Constraints for the first derivatives. This works on non-boundary points,
    // so it gives us (Npolys - 1) equations.
    for (let i = 0; i < Npolys - 1; i++, nrow++) {
        // Poly i and poly i+1 must have the same first derivative at
        // point i+1.
        A[nrow][4 * i] = 3 * xs[i + 1] ** 2;
        A[nrow][4 * i + 1] = 2 * xs[i + 1];
        A[nrow][4 * i + 2] = 1;
        A[nrow][4 * (i + 1)] = -3 * xs[i + 1] ** 2;
        A[nrow][4 * (i + 1) + 1] = -2 * xs[i + 1];
        A[nrow][4 * (i + 1) + 2] = -1;
    }

    // Constraints for the second derivatives. This also gives us (Npolys - 1)
    // equations.
    for (let i = 0; i < Npolys - 1; i++, nrow++) {
        // Poly i and poly i+1 must have the same second derivative at
        // point i+1.
        A[nrow][4 * i] = 6 * xs[i + 1];
        A[nrow][4 * i + 1] = 2;
        A[nrow][4 * (i + 1)] = -6 * xs[i + 1];
        A[nrow][4 * (i + 1) + 1] = -2;
    }

    // The final two equations come from the "natural" boundary conditions;
    // the first and last polys must have zero second derivative at the
    // endpoints.
    A[nrow][0] = 6 * xs[0];
    A[nrow][1] = 2;
    A[nrow + 1][4 * (Npolys - 1)] = 6 * xs[Npolys];
    A[nrow + 1][4 * (Npolys - 1) + 1] = 2;

    return [A, b];
}

// Print the 2D array, with each row on a separate line.
function print2DArray(arr) {
    for (let i = 0; i < arr.length; i++) {
        let rowstr = arr[i].map(e => e.toString().padStart(5, ' ')).join(" ");
        console.log(rowstr);
    }
}

let xs = [0, 1, 2];
let ys = [1, 3, 2];

let [A, b] = buildSplineEquations(xs, ys);
print2DArray(A);
console.log(`----\nb = ${b}`);



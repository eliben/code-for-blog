#include <cstdio>
#include "benchmark/benchmark.h"


// A simple Matrix of unsigned integers laid out row-major in a 1D array. M is
// number of rows, N is number of columns.
struct Matrix {
  unsigned* data = nullptr;
  size_t M = 0, N = 0;
};


void ShowMatrix(const Matrix& m) {
  for (size_t row = 0; row < m.M; ++row) {
    for (size_t col = 0; col < m.N; ++col) {
      printf("%6d ", m.data[row * m.N + col]);
    }
    printf("\n");
  }
}

// Create and return a new matrix MxN, initialized with running integers
// starting with incr and advancing by incr to the next element. For example,
// CreateNewMatrix(2, 3, 5) will produce the matrix:
//
//   5  10  15
//  20  15  30
//
Matrix CreateNewMatrix(size_t M, size_t N, unsigned incr) {
  Matrix m;
  m.M = M;
  m.N = N;
  m.data = new unsigned[M * N];

  unsigned value = incr;
  for (size_t idx = 0; idx < M * N; ++idx) {
    m.data[idx] = value;
    value += incr;
  }

  return m;
}

// Add matrix x into matrix y (y += x), iterating over the columns of each row
// before going to the next row.
__attribute__((noinline))
void AddMatrixByRow(Matrix& y, const Matrix& x) {
  assert(y.M == x.M);
  assert(y.N == x.N);

  for (size_t row = 0; row < y.M; ++row) {
    for (size_t col = 0; col < y.N; ++col) {
      y.data[row * y.N + col] += x.data[row * x.N + col];
    }
  }
}

// Same as AddMatrixByRow, but with vectorization disabled.
__attribute__((noinline))
__attribute__((optimize("no-tree-vectorize")))
void AddMatrixByRowNoVec(Matrix& y, const Matrix& x) {
  assert(y.M == x.M);
  assert(y.N == x.N);

  for (size_t row = 0; row < y.M; ++row) {
    for (size_t col = 0; col < y.N; ++col) {
      y.data[row * y.N + col] += x.data[row * x.N + col];
    }
  }
}

// Add matrix x into matrix y (y += x), iterating over the rows of each column
// before going to the next column.
__attribute__((noinline))
void AddMatrixByCol(Matrix& y, const Matrix& x) {
  assert(y.M == x.M);
  assert(y.N == x.N);

  for (size_t col = 0; col < y.N; ++col) {
    for (size_t row = 0; row < y.M; ++row) {
      y.data[row * y.N + col] += x.data[row * x.N + col];
    }
  }
}

// Sanity checking that our implementation is correct.
void Test() {
  Matrix y1 = CreateNewMatrix(4, 5, 2);
  Matrix y2 = CreateNewMatrix(4, 5, 2);
  Matrix y3 = CreateNewMatrix(4, 5, 2);
  Matrix x = CreateNewMatrix(4, 5, 3);

  AddMatrixByRow(y1, x);
  AddMatrixByRowNoVec(y2, x);
  AddMatrixByCol(y3, x);

  for (size_t row = 0; row < x.M; ++row) {
    for (size_t col = 0; col < x.N; ++col) {
      if (y1.data[row * y1.N + col] != y2.data[row * y2.N + col] ||
          y1.data[row * y1.N + col] != y3.data[row * y3.N + col]) {
        printf("TEST ERROR at %zu,%zu\n", row, col);
      }
    }
  }
  printf("TEST OK\n");
}

// Generate matrix sizes for benchmark runs.
void BenchArgs(benchmark::internal::Benchmark* b) {
  b->ArgPair(64, 64);
  b->ArgPair(128, 128);
  b->ArgPair(256, 256);
  b->ArgPair(512, 512);
}

// Run benchmarks for each AddMatrix* variation.
void BM_AddByRow(benchmark::State& state) {
  Matrix y = CreateNewMatrix(state.range_x(), state.range_y(), 3);
  Matrix x = CreateNewMatrix(state.range_x(), state.range_y(), 2);
  while (state.KeepRunning()) {
    AddMatrixByRow(y, x);
  }
  state.SetItemsProcessed(int64_t(state.iterations()) * state.range_x() *
                          state.range_y());
}
BENCHMARK(BM_AddByRow)->Apply(BenchArgs);

void BM_AddByRowNoVec(benchmark::State& state) {
  Matrix y = CreateNewMatrix(state.range_x(), state.range_y(), 3);
  Matrix x = CreateNewMatrix(state.range_x(), state.range_y(), 2);
  while (state.KeepRunning()) {
    AddMatrixByRowNoVec(y, x);
  }
  state.SetItemsProcessed(int64_t(state.iterations()) * state.range_x() *
                          state.range_y());
}
BENCHMARK(BM_AddByRowNoVec)->Apply(BenchArgs);

void BM_AddByCol(benchmark::State& state) {
  Matrix y = CreateNewMatrix(state.range_x(), state.range_y(), 3);
  Matrix x = CreateNewMatrix(state.range_x(), state.range_y(), 2);
  while (state.KeepRunning()) {
    AddMatrixByCol(y, x);
  }
  state.SetItemsProcessed(int64_t(state.iterations()) * state.range_x() *
                          state.range_y());
}
BENCHMARK(BM_AddByCol)->Apply(BenchArgs);

int main(int argc, const char** argv) {
  benchmark::Initialize(&argc, argv);

  Test();

  benchmark::RunSpecifiedBenchmarks();
  return 0;
}

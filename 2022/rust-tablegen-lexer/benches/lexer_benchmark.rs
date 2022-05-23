// Benchmarking for the project, using the `criterion` crate.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lexer::owning;
use lexer::slice;

pub fn slice_lexer_benchmark(c: &mut Criterion) {
    let data = slice::read_testfile();

    let mut group = c.benchmark_group("lexer");

    group.bench_function("slice_all_push", |b| {
        b.iter(|| {
            let toks = slice::tokenize_all_push(black_box(&data));
            black_box(toks);
        })
    });

    group.bench_function("slice_push_prealloc", |b| {
        b.iter(|| {
            let toks = slice::tokenize_all_push_prealloc(black_box(&data));
            black_box(toks);
        })
    });

    group.bench_function("slice_push_collect", |b| {
        b.iter(|| {
            let toks = slice::tokenize_all_collect(black_box(&data));
            black_box(toks);
        })
    });

    group.bench_function("owning_all_push", |b| {
        b.iter(|| {
            let toks = owning::tokenize_all_push(black_box(&data));
            black_box(toks);
        })
    });

    group.bench_function("owning_push_prealloc", |b| {
        b.iter(|| {
            let toks = owning::tokenize_all_push_prealloc(black_box(&data));
            black_box(toks);
        })
    });

    group.bench_function("owning_push_collect", |b| {
        b.iter(|| {
            let toks = owning::tokenize_all_collect(black_box(&data));
            black_box(toks);
        })
    });
}

criterion_group!(benches, slice_lexer_benchmark);
criterion_main!(benches);

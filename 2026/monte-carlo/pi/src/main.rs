// Monte-Carlo simulation for calculating the value of Pi via relative area
// of a quarter circle to square and random throws.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
fn main() {
    let runs: u64 = std::env::args()
        .nth(1)
        .map(|arg| arg.parse().expect("expected a positive integer"))
        .unwrap_or(10_000_000);
    assert!(runs > 0, "number of runs must be positive");

    let workers = std::thread::available_parallelism()
        .map_or(1, |n| n.get() as u64)
        .min(runs);
    let runs = runs
        .checked_next_multiple_of(workers)
        .expect("number of runs is too large");
    let count = runs / workers;
    let handles: Vec<_> = (0..workers)
        .map(|_| {
            std::thread::spawn(move || {
                let mut rng = fastrand::Rng::new();
                let mut inside = 0_u64;
                for _ in 0..count {
                    let x = rng.f64();
                    let y = rng.f64();
                    if x * x + y * y <= 1.0 {
                        inside += 1;
                    }
                }
                inside
            })
        })
        .collect();
    let inside: u64 = handles
        .into_iter()
        .map(|handle| handle.join().expect("worker thread panicked"))
        .sum();

    println!("{}", 4.0 * inside as f64 / runs as f64);
}

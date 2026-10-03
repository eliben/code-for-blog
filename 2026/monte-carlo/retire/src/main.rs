use rand::rng;
use rand_distr::{Distribution, Normal};

fn main() {
    // Create a random number generator
    let mut rng = rng();

    // Initialize a Normal distribution: mean = 2.0, standard deviation = 3.0
    let normal = Normal::new(2.0, 3.0).unwrap();

    // Draw a sample from the distribution
    let sample: f64 = normal.sample(&mut rng);

    println!("Random value from normal distribution: {}", sample);
}

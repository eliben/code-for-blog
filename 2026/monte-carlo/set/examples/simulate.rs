// Simulations for blog post.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.

use set_cards::Card;
use std::io;

fn main() -> io::Result<()> {
    let mut rng = fastrand::Rng::new();
    let ngames = 10000000;
    let mut ncount = 0;
    let full_deck = set_cards::make_full_deck();

    for _ in 0..ngames {
        let mut deck = full_deck.clone();

        // The deck is deterministic. We have to draw 12 random cards to form
        // a hand.
        let mut hand: Vec<Card> = Vec::with_capacity(12);
        for _ in 0..12 {
            // Select a random card from the deck and add it to the hand,
            // removing it from the deck.
            let index = rng.usize(..deck.len());
            hand.push(deck.swap_remove(index));
        }

        if set_cards::count_sets(&hand) == 0 {
            ncount += 1;
        }
    }

    println!("Number of games: {}", ngames);
    println!("Number of games without a set: {}", ncount);
    println!("Probability of no set: {}", ncount as f64 / ngames as f64);
    Ok(())
}

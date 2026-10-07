// Generate interesting decks for blog post.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.

use set_cards::{Card, render_grid};
use std::{fs, io, path::PathBuf};

fn main() -> io::Result<()> {
    let output = PathBuf::from(std::env::args_os().nth(1).unwrap_or_else(|| "/tmp".into()));
    fs::create_dir_all(&output)?;

    let mut rng = fastrand::Rng::new();
    let mut attempt = 0;

    loop {
        let mut deck = set_cards::make_full_deck();

        // Select 12 random cards from the deck.
        let mut hand: Vec<Card> = Vec::with_capacity(12);
        for _ in 0..12 {
            // Select a random card from the deck and add it to the hand,
            // removing it from the deck.
            let index = rng.usize(..deck.len());
            hand.push(deck.swap_remove(index));
        }
        println!("Attempt {:3}", attempt);
        attempt += 1;

        if set_cards::count_sets(&hand) == 0 {
            let path = output.join("no-set-hand.svg");
            let svg = render_grid(&hand, 4);
            fs::write(&path, svg)?;
            println!("Wrote {}", path.display());
            break;
        }
    }

    Ok(())
}

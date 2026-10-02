
use std::{fs, io, path::PathBuf};
use set_cards::{Card, Color, Number, Shading, Shape, render_card, render_grid};


fn main() -> io::Result<()> {
    let output = PathBuf::from(
        std::env::args_os()
            .nth(1)
            .unwrap_or_else(|| "/tmp".into()),
    );
    fs::create_dir_all(&output)?;

    let deck = set_cards::make_full_deck();

    // Generate a set of 12 cards with a single SET.
    // Generate random hand, count sets; repeat until found.
    let mut rng = fastrand::Rng::new();

    loop {
        // Select 12 random cards from the deck.
        let hand: Vec<_> = (0..12).map(|_| deck[rng.usize(..deck.len())]).collect();

        if set_cards::count_sets(&hand) == 1 {
            let path = output.join("one-set-hand.svg");
            let svg = render_grid(&hand, 4);
            fs::write(&path, svg)?;
            println!("Wrote {}", path.display());
            break;
        }
    }

    Ok(())
}
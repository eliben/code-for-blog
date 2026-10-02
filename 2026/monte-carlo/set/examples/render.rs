//! Run with `cargo run --example render -- [output-directory]`.

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

    let card = Card::new(
        Number::Three,
        Color::Purple,
        Shading::Striped,
        Shape::Squiggle,
    );
    let hand: Vec<_> = deck.iter().step_by(7).copied().collect();
    for (filename, svg) in [
        ("card.svg", render_card(&card)),
        ("grid.svg", render_grid(&hand, 3)),
        ("deck.svg", render_grid(&deck, 9)),
    ] {
        let path = output.join(filename);
        fs::write(&path, svg)?;
        println!("Wrote {}", path.display());
    }
    Ok(())
}

# SET cards

A Rust library for rendering SET cards as self-contained SVGs, with no external
dependencies. Cards use red, green, and purple diamonds, ovals, and squiggles,
with empty, horizontally striped, or solid shading.

```rust
use set_cards::{Card, Color, Number, Shading, Shape, render_card, render_grid};

fn main() -> std::io::Result<()> {
    let cards = [
        Card::new(Number::One, Color::Red, Shading::Empty, Shape::Diamond),
        Card::new(Number::Two, Color::Green, Shading::Striped, Shape::Oval),
        Card::new(Number::Three, Color::Purple, Shading::Solid, Shape::Squiggle),
    ];

    std::fs::write("card.svg", render_card(&cards[0]))?;
    std::fs::write("cards.svg", render_grid(&cards, 2))?;
    // The grid has two cards in the first row and one in the second.
    Ok(())
}
```

`render_grid` preserves input order, fills rows left to right, and starts a new row
after `columns` cards. A partial final row is left-aligned. Columns must be greater
than zero; zero panics. Empty input produces a blank 24 × 24 SVG. If the input has
fewer cards than columns, the output fits the occupied columns.

Cards are 180 × 120 pixels, with a white background and a rounded border. Grids
have 12-pixel gaps and outer margins; space outside the cards is transparent.
Every SVG includes a `viewBox`, so it can be resized without losing detail. SVGs
contain no fonts, scripts, or external assets. Open them directly in a browser or
embed them in a webpage using an `<img>` element. PNG conversion is outside the
library's scope.

Generate a single card, a 12-card grid, and the complete 81-card deck:

```sh
cargo run --example render
# Or choose the output directory:
cargo run --example render -- /tmp/set-cards
```

The default output directory is `rendered/`, containing `card.svg`, `grid.svg`, and
`deck.svg`. These files can be regenerated and are ignored by Git.

```sh
cargo test
cargo clippy --all-targets -- -D warnings
```

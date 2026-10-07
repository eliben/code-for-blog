// SET cards SVG rendering.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
use std::fmt::Write;

use crate::{Card, Color, Shading, Shape};

const CARD_WIDTH: usize = 180;
const CARD_HEIGHT: usize = 120;
const GAP: usize = 12;
const MARGIN: usize = 12;
const SYMBOL_WIDTH: usize = 32;
const SYMBOL_HEIGHT: usize = 76;
const SYMBOL_SPACING: usize = 44;

/// Render one card as a complete, self-contained SVG document.
///
/// The default size is 180 × 120 pixels. The SVG's `viewBox` allows it to scale
/// to any size without losing detail. The card has a white background, a thin
/// rounded border, and one to three upright symbols arranged horizontally.
pub fn render_card(card: &Card) -> String {
    let mut svg = start_svg(CARD_WIDTH, CARD_HEIGHT);
    append_card(&mut svg, card, 0, 0);
    svg.push_str("</svg>\n");
    svg
}

/// Render cards in input order, left to right, starting a new row after every
/// `columns` cards. The last row may be incomplete and is left-aligned.
///
/// Cards are 180 × 120 pixels, with 12 pixels between cards and around the grid.
/// If there are fewer cards than columns, the width fits the occupied columns.
/// An empty slice produces a blank 24 × 24 SVG. The space outside cards is
/// transparent, and the whole image can be scaled using its `viewBox`.
///
/// # Panics
///
/// Panics if `columns` is zero, including for an empty slice.
pub fn render_grid(cards: &[Card], columns: usize) -> String {
    assert!(columns > 0, "render_grid requires at least one column");

    let used_columns = columns.min(cards.len());
    let rows = cards.len().div_ceil(columns);
    let width = 2 * MARGIN + used_columns * CARD_WIDTH + used_columns.saturating_sub(1) * GAP;
    let height = 2 * MARGIN + rows * CARD_HEIGHT + rows.saturating_sub(1) * GAP;
    let mut svg = start_svg(width, height);

    for (index, card) in cards.iter().enumerate() {
        let x = MARGIN + (index % columns) * (CARD_WIDTH + GAP);
        let y = MARGIN + (index / columns) * (CARD_HEIGHT + GAP);
        append_card(&mut svg, card, x, y);
    }

    svg.push_str("</svg>\n");
    svg
}

fn start_svg(width: usize, height: usize) -> String {
    let mut svg = format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{width}\" height=\"{height}\" viewBox=\"0 0 {width} {height}\" role=\"img\">\n"
    );
    svg.push_str("<title>SET cards</title>\n<defs>\n");
    for color in [Color::Red, Color::Green, Color::Purple] {
        let (name, ink) = palette(color);
        writeln!(
            svg,
            "<pattern id=\"set-stripes-{name}\" patternUnits=\"userSpaceOnUse\" width=\"6\" height=\"6\"><path d=\"M0 3H6\" stroke=\"{ink}\" stroke-width=\"1.5\"/></pattern>"
        )
        .unwrap();
    }
    svg.push_str("</defs>\n");
    svg
}

fn palette(color: Color) -> (&'static str, &'static str) {
    match color {
        Color::Red => ("red", "#d62828"),
        Color::Green => ("green", "#16824b"),
        Color::Purple => ("purple", "#873ba3"),
    }
}

fn append_card(svg: &mut String, card: &Card, x: usize, y: usize) {
    let count = card.number.count();
    let (color, ink) = palette(card.color);
    let shading = match card.shading {
        Shading::Empty => "empty",
        Shading::Striped => "striped",
        Shading::Solid => "solid",
    };
    let shape = match card.shape {
        Shape::Diamond => "diamond",
        Shape::Oval => "oval",
        Shape::Squiggle => "squiggle",
    };

    // Writing into a String cannot fail, so the formatting results can be unwrapped.
    writeln!(svg, "<g class=\"card\" transform=\"translate({x} {y})\">").unwrap();
    writeln!(
        svg,
        "<title>{count} {color} {shading} {shape}{}</title>",
        if count == 1 { "" } else { "s" }
    )
    .unwrap();
    writeln!(
        svg,
        "<rect x=\"1\" y=\"1\" width=\"{}\" height=\"{}\" rx=\"9\" fill=\"white\" stroke=\"#555\" stroke-width=\"1.5\"/>",
        CARD_WIDTH - 2,
        CARD_HEIGHT - 2
    )
    .unwrap();

    let fill = match card.shading {
        Shading::Empty => "none".to_owned(),
        Shading::Solid => ink.to_owned(),
        Shading::Striped => format!("url(#set-stripes-{color})"),
    };
    let start_x = (CARD_WIDTH - SYMBOL_WIDTH - (count - 1) * SYMBOL_SPACING) / 2;
    let symbol_y = (CARD_HEIGHT - SYMBOL_HEIGHT) / 2;

    for index in 0..count {
        let symbol_x = start_x + index * SYMBOL_SPACING;
        writeln!(
            svg,
            "<g class=\"symbol\" transform=\"translate({symbol_x} {symbol_y})\" fill=\"{fill}\" stroke=\"{ink}\" stroke-width=\"2\" stroke-linejoin=\"round\">"
        )
        .unwrap();
        svg.push_str(match card.shape {
            Shape::Diamond => "<path d=\"M16 0L32 38L16 76L0 38Z\"/>\n",
            Shape::Oval => "<rect width=\"32\" height=\"76\" rx=\"16\"/>\n",
            // Parallel sides keep the body broad through a gentle bend.
            Shape::Squiggle => {
                "<path d=\"M4 8C16 28 -8 48 4 68C10 78 34 78 28 68C16 48 40 28 28 8C22 -2 -2 -2 4 8Z\"/>\n"
            }
        });
        svg.push_str("</g>\n");
    }
    svg.push_str("</g>\n");
}

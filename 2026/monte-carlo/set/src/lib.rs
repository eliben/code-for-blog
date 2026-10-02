//! SET cards and dependency-free SVG rendering.
//!
//! Render a single card or arrange cards in a grid, then save the returned SVG
//! string or embed it in a document.
//!
//! ```
//! use set_cards::{Card, Color, Number, Shading, Shape, render_card, render_grid};
//!
//! let card = Card::new(Number::Two, Color::Green, Shading::Striped, Shape::Oval);
//! let svg = render_card(&card);
//! let grid = render_grid(&[card; 7], 3); // Rows of 3, 3, and 1 cards.
//! ```

mod card;
mod render;

pub use card::*;
pub use render::{render_card, render_grid};

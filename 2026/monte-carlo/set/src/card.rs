/// The number of symbols on a card.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Number {
    One,
    Two,
    Three,
}

impl Number {
    /// The number of symbols as an integer.
    pub const fn count(self) -> usize {
        match self {
            Self::One => 1,
            Self::Two => 2,
            Self::Three => 3,
        }
    }
}

/// The color of the symbols on a card.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Color {
    Red,
    Green,
    Purple,
}

/// The fill of the symbols on a card.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Shading {
    Empty,
    Striped,
    Solid,
}

/// The shape of the symbols on a card.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Shape {
    Diamond,
    Oval,
    Squiggle,
}

/// A SET card, described by its four independent attributes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Card {
    pub number: Number,
    pub color: Color,
    pub shading: Shading,
    pub shape: Shape,
}

impl Card {
    pub const fn new(number: Number, color: Color, shading: Shading, shape: Shape) -> Self {
        Self {
            number,
            color,
            shading,
            shape,
        }
    }
}

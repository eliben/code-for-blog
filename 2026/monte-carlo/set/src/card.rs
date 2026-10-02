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

    pub fn to_coord(self) -> usize {
        match self {
            Self::One => 1,
            Self::Two => 2,
            Self::Three => 0,
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

impl Color {
    pub fn to_coord(self) -> usize {
        match self {
            Self::Green => 0,
            Self::Purple => 1,
            Self::Red => 2,
        }
    }
}

/// The fill of the symbols on a card.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Shading {
    Empty,
    Striped,
    Solid,
}

impl Shading {
    pub fn to_coord(self) -> usize {
        match self {
            Self::Empty => 0,
            Self::Striped => 1,
            Self::Solid => 2,
        }
    }
}

/// The shape of the symbols on a card.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Shape {
    Diamond,
    Oval,
    Squiggle,
}

impl Shape {
    pub fn to_coord(self) -> usize {
        match self {
            Self::Diamond => 0,
            Self::Oval => 1,
            Self::Squiggle => 2,
        }
    }
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

    pub fn to_coord(self) -> [usize; 4] {
        [
            self.number.to_coord(),
            self.color.to_coord(),
            self.shading.to_coord(),
            self.shape.to_coord(),
        ]
    }
}


// Determines if three cards form a valid SET.
pub fn is_set(card1: Card, card2: Card, card3: Card) -> bool {
    let c1 = card1.to_coord();
    let c2 = card2.to_coord();
    let c3 = card3.to_coord();

    (0..4).all(|i| (c1[i] + c2[i] + c3[i]) % 3 == 0)
}


// Determines if a collection of cards contains at least one valid SET.
pub fn has_set(cards: &[Card]) -> bool {
    for i in 0..cards.len() {
        for j in (i + 1)..cards.len() {
            for k in (j + 1)..cards.len() {
                if is_set(cards[i], cards[j], cards[k]) {
                    return true;
                }
            }
        }
    }
    false
}

// Creates a full deck of 81 unique SET cards.
pub fn make_full_deck() -> Vec<Card> {
    let mut deck = Vec::with_capacity(81);
    for &number in &[Number::One, Number::Two, Number::Three] {
        for &color in &[Color::Red, Color::Green, Color::Purple] {
            for &shading in &[Shading::Empty, Shading::Striped, Shading::Solid] {
                for &shape in &[Shape::Diamond, Shape::Oval, Shape::Squiggle] {
                    deck.push(Card::new(number, color, shading, shape));
                }
            }
        }
    }
    deck
}
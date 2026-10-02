use set_cards::{Card, Color, Number, Shading, Shape, render_grid};

const CARD: Card = Card::new(Number::One, Color::Red, Shading::Empty, Shape::Diamond);

#[test]
fn grid_wraps_in_input_order_and_keeps_the_partial_row() {
    let last = Card::new(Number::Two, Color::Green, Shading::Solid, Shape::Oval);
    let svg = render_grid(&[CARD, CARD, CARD, CARD, last], 3);
    let cards: Vec<_> = svg.split("<g class=\"card\"").skip(1).collect();

    assert_eq!(cards.len(), 5);
    for (card, position) in cards.iter().zip([
        "translate(12 12)",
        "translate(204 12)",
        "translate(396 12)",
        "translate(12 144)",
        "translate(204 144)",
    ]) {
        assert!(card.starts_with(&format!(" transform=\"{position}\"")));
    }
    assert!(cards[4].contains("<title>2 green solid ovals</title>"));
    assert!(svg.contains("viewBox=\"0 0 588 276\""));
}

#[test]
fn column_counts_larger_than_the_hand_do_not_add_blank_columns() {
    let svg = render_grid(&[CARD, CARD], usize::MAX);
    assert!(svg.contains("viewBox=\"0 0 396 144\""));
}

#[test]
fn empty_grid_has_positive_dimensions_and_no_cards() {
    let svg = render_grid(&[], 3);
    assert!(svg.contains("viewBox=\"0 0 24 24\""));
    assert!(!svg.contains("class=\"card\""));
    assert!(svg.ends_with("</svg>\n"));
}

#[test]
#[should_panic(expected = "render_grid requires at least one column")]
fn zero_columns_is_rejected() {
    render_grid(&[CARD], 0);
}

#[test]
#[should_panic(expected = "render_grid requires at least one column")]
fn zero_columns_is_rejected_even_for_an_empty_grid() {
    render_grid(&[], 0);
}

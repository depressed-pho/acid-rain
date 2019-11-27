/* There are no sensible defaults so it doesn't implement Default. */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub enum HorizontalAlignment {
    Left,
    Center,
    Right,
    Leading,
    Trailing
}

/* There are no sensible defaults so it doesn't implement Default. */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub enum VerticalAlignment {
    Top,
    Center,
    Bottom
}

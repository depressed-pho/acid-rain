use std::ops::BitOr;

#[derive(Debug)]
pub enum Attribute {
    Normal,
    Standout,
    Underline,
    Reverse,
    Blink,
    Dim,
    Bold,
    Protect,
    Invisible,
    AltCharset,
    Italic
}

#[derive(Debug)]
pub struct AttrSet {
    raw: ncurses::attr_t
}

impl From<Attribute> for AttrSet {
    fn from(attr: Attribute) -> Self {
        let raw = match attr {
            Attribute::Normal     => ncurses::A_NORMAL(),
            Attribute::Standout   => ncurses::A_STANDOUT(),
            Attribute::Underline  => ncurses::A_UNDERLINE(),
            Attribute::Reverse    => ncurses::A_REVERSE(),
            Attribute::Blink      => ncurses::A_BLINK(),
            Attribute::Dim        => ncurses::A_DIM(),
            Attribute::Bold       => ncurses::A_BOLD(),
            Attribute::Protect    => ncurses::A_PROTECT(),
            Attribute::Invisible  => ncurses::A_INVIS(),
            Attribute::AltCharset => ncurses::A_ALTCHARSET(),
            Attribute::Italic     => ncurses::A_ITALIC()
        };
        Self { raw }
    }
}

impl From<AttrSet> for ncurses::attr_t {
    fn from(set: AttrSet) -> Self {
        set.raw
    }
}

impl BitOr for AttrSet {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self {
            raw: self.raw | rhs.raw
        }
    }
}

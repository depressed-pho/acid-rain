use std::ops::BitOr;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct AttrSet {
    raw: ncurses::attr_t
}

impl Default for AttrSet {
    fn default() -> Self {
        Attribute::Normal.into()
    }
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
        /* OR-ing Normal with anything else does not make sense, but
         * the OR operator returning Result or Option is a terrible
         * idea. What should we do then? For now we just panic when
         * that happens. */
        if self.raw & ncurses::A_NORMAL() != 0 || rhs.raw & ncurses::A_NORMAL() != 0 {
            panic!("OR-ing Attribute::Normal with anything else does not make sense.");
        }
        else {
            Self {
                raw: self.raw | rhs.raw
            }
        }
    }
}

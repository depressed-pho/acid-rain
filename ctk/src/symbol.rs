#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub enum Symbol {
    /* VT100 symbols */
    UpperLeftCorner,
    LowerLeftCorner,
    UpperRightCorner,
    LowerRightCorner,
    LeftTee,
    RightTee,
    BottomTee,
    TopTee,
    HorizontalLine,
    VerticalLine,
    Plus,
    ScanLine1,
    ScanLine9,
    Diamond,
    CheckerBoard,
    Degree,
    PlusMinus,
    Bullet,

    /* Teletype 5410v1 symbols */
    LeftArrow,
    RightArrow,
    DownArrow,
    UpArrow,
    Board,
    Lantern,
    Block,

    /* AT&T symbols */
    ScanLine3,
    ScanLine7,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Pi,
    NotEqual,
    Sterling

    // FIXME: https://invisible-island.net/ncurses/man/curs_add_wch.3x.html
    // Add thick and double-lines variants.
}

impl From<Symbol> for ncurses::chtype {
    fn from(s: Symbol) -> Self {
        match s {
            Symbol::UpperLeftCorner    => ncurses::ACS_ULCORNER(),
            Symbol::LowerLeftCorner    => ncurses::ACS_LLCORNER(),
            Symbol::UpperRightCorner   => ncurses::ACS_URCORNER(),
            Symbol::LowerRightCorner   => ncurses::ACS_LRCORNER(),
            Symbol::LeftTee            => ncurses::ACS_LTEE(),
            Symbol::RightTee           => ncurses::ACS_RTEE(),
            Symbol::BottomTee          => ncurses::ACS_BTEE(),
            Symbol::TopTee             => ncurses::ACS_TTEE(),
            Symbol::HorizontalLine     => ncurses::ACS_HLINE(),
            Symbol::VerticalLine       => ncurses::ACS_VLINE(),
            Symbol::Plus               => ncurses::ACS_PLUS(),
            Symbol::ScanLine1          => ncurses::ACS_S1(),
            Symbol::ScanLine9          => ncurses::ACS_S9(),
            Symbol::Diamond            => ncurses::ACS_DIAMOND(),
            Symbol::CheckerBoard       => ncurses::ACS_CKBOARD(),
            Symbol::Degree             => ncurses::ACS_DEGREE(),
            Symbol::PlusMinus          => ncurses::ACS_PLMINUS(),
            Symbol::Bullet             => ncurses::ACS_BULLET(),

            Symbol::LeftArrow          => ncurses::ACS_LARROW(),
            Symbol::RightArrow         => ncurses::ACS_RARROW(),
            Symbol::DownArrow          => ncurses::ACS_DARROW(),
            Symbol::UpArrow            => ncurses::ACS_UARROW(),
            Symbol::Board              => ncurses::ACS_BOARD(),
            Symbol::Lantern            => ncurses::ACS_LANTERN(),
            Symbol::Block              => ncurses::ACS_BLOCK(),

            Symbol::ScanLine3          => ncurses::ACS_S3(),
            Symbol::ScanLine7          => ncurses::ACS_S7(),
            Symbol::LessThanOrEqual    => ncurses::ACS_LEQUAL(),
            Symbol::GreaterThanOrEqual => ncurses::ACS_GEQUAL(),
            Symbol::Pi                 => ncurses::ACS_PI(),
            Symbol::NotEqual           => ncurses::ACS_NEQUAL(),
            Symbol::Sterling           => ncurses::ACS_STERLING()
        }
    }
}

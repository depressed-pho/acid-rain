use ctk::attribute::AttrSet;
use ctk::color::{BoxedColor, Color};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// An appearance is an attributed character (styled and colored) that
/// represents a tile, an item, or an entity on a terminal. On Unicode
/// capable terminals it may consist of several code points, but must
/// still form a single grapheme cluster. Its displayed width must be
/// exactly 1.
#[derive(Debug)]
pub struct Appearance {
    unicode: String,
    ascii: char,
    attrs: AttrSet,
    fg_color: BoxedColor,
    bg_color: BoxedColor
}

impl Appearance {
    // We really want to use "typed-builder" in crates.io but it's not
    // flexible enough for us. Hand-rolling the compile-time check is
    // seriously tedious so we only do a runtime check.
    pub fn builder() -> AppearanceBuilder {
        AppearanceBuilder::default()
    }
}

#[derive(Debug, Default)]
pub struct AppearanceBuilder {
    unicode: Option<String>,
    ascii: Option<char>,
    attrs: (bool, AttrSet),
    fg_color: (bool, BoxedColor),
    bg_color: (bool, BoxedColor)
}

impl AppearanceBuilder {
    pub fn unicode(mut self, v: impl Into<String>) -> Self {
        // Option::expect_none() is still nightly-only.
        if self.unicode.is_some() {
            panic!("unicode() has been called twice.");
        }
        else {
            let s = v.into();
            if s.width() != 1 {
                panic!("The displayed width must be exactly 1: {}", s);
            }
            self.unicode = Some(s);
        }
        self
    }

    pub fn ascii(mut self, v: impl Into<char>) -> Self {
        // Option::expect_none() is still nightly-only.
        if self.ascii.is_some() {
            panic!("ascii() has been called twice.");
        }
        else {
            let c = v.into();
            if c.width() != Some(1) {
                panic!("The displayed width must be exactly 1: {}", c);
            }
            if (c as i32) < 0x20 || (c as i32) > 0x7e {
                panic!("The character is not a printable ASCII character: {}", c);
            }
            self.ascii = Some(c);
        }
        self
    }

    pub fn attrs(mut self, v: impl Into<AttrSet>) -> Self {
        if self.attrs.0 {
            panic!("attrs() has been called twice.");
        }
        else {
            self.attrs = (true, v.into());
        }
        self
    }

    pub fn fg_color(mut self, v: impl Color + Send + Sync) -> Self {
        if self.fg_color.0 {
            panic!("fg_color() has been called twice.");
        }
        else {
            self.fg_color = (true, BoxedColor::new(v));
        }
        self
    }

    pub fn bg_color(mut self, v: impl Color + Send + Sync) -> Self {
        if self.bg_color.0 {
            panic!("bg_color() has been called twice.");
        }
        else {
            self.bg_color = (true, BoxedColor::new(v));
        }
        self
    }

    pub fn build(self) -> Appearance {
        Appearance {
            unicode: self.unicode.expect("unicode() has not been called but it has no default value"),
            ascii: self.ascii.expect("ascii() has not been called but it has no default value"),
            attrs: self.attrs.1,
            fg_color: self.fg_color.1,
            bg_color: self.bg_color.1
        }
    }
}

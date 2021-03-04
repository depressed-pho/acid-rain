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
    unicode_: String,
    ascii_: char,
    attrs_: AttrSet,
    fg_color_: BoxedColor,
    bg_color_: BoxedColor
}

impl Appearance {
    /// Create a builder for building `Appearance`. On the builder,
    /// call `.unicode(...)`, `.ascii(...)`, `.attrs(...)`(optional),
    /// `.fg_color(...)`(optional), `.bg_color(...)`(optional) to set
    /// the values of the fields. Finally, call `.build()` to create
    /// the instance of Appearance.
    pub fn builder() -> AppearanceBuilder<(), (), (), (), ()> {
        // We really want to use "typed-builder" in crates.io but it's
        // not flexible enough for us. Hand-rolling the compile-time
        // check is seriously tedious but we do it anyway...
        AppearanceBuilder {
            fields: ((), (), (), (), ())
        }
    }

    pub fn unicode(&self) -> &str {
        &self.unicode_
    }

    pub fn ascii(&self) -> char {
        self.ascii_
    }

    pub fn attrs(&self) -> AttrSet {
        self.attrs_
    }

    pub fn fg_color(&self) -> impl Color {
        self.fg_color_.clone()
    }

    pub fn bg_color(&self) -> impl Color {
        self.bg_color_.clone()
    }
}

/// Builder for [Appearance] instances.
///
/// See [Appearance::builder()] for more info.
#[derive(Debug)]
pub struct AppearanceBuilder<T1, T2, T3, T4, T5> {
    fields: (T1, T2, T3, T4, T5)
}

impl<Ascii_, Attrs_, FgColor_, BgColor_> AppearanceBuilder<(), Ascii_, Attrs_, FgColor_, BgColor_> {
    pub fn unicode(self, v: impl Into<String>) -> AppearanceBuilder<String, Ascii_, Attrs_, FgColor_, BgColor_> {
        let (_, ascii, attrs, fg_color, bg_color) = self.fields;
        let unicode = v.into();
        if unicode.width() != 1 {
            panic!("The displayed width must be exactly 1: {}", unicode);
        }
        AppearanceBuilder {
            fields: (unicode, ascii, attrs, fg_color, bg_color)
        }
    }
}

impl<Unicode_, Attrs_, FgColor_, BgColor_> AppearanceBuilder<Unicode_, (), Attrs_, FgColor_, BgColor_> {
    pub fn ascii(self, v: impl Into<char>) -> AppearanceBuilder<Unicode_, char, Attrs_, FgColor_, BgColor_> {
        let (unicode, _, attrs, fg_color, bg_color) = self.fields;
        // Custom validation: typed-builder doesn't support this.
        let ascii = v.into();
        if ascii.width() != Some(1) {
            panic!("The displayed width must be exactly 1: {}", ascii);
        }
        if (ascii as i32) < 0x20 || (ascii as i32) > 0x7e {
            panic!("The character is not a printable ASCII character: {}", ascii);
        }
        AppearanceBuilder {
            fields: (unicode, ascii, attrs, fg_color, bg_color)
        }
    }
}

impl<Unicode_, Ascii_, FgColor_, BgColor_> AppearanceBuilder<Unicode_, Ascii_, (), FgColor_, BgColor_> {
    pub fn attrs(self, v: impl Into<AttrSet>) -> AppearanceBuilder<Unicode_, Ascii_, AttrSet, FgColor_, BgColor_> {
        let (unicode, ascii, _, fg_color, bg_color) = self.fields;
        let attrs = v.into();
        AppearanceBuilder {
            fields: (unicode, ascii, attrs, fg_color, bg_color)
        }
    }
}

impl<Unicode_, Ascii_, Attrs_, BgColor_> AppearanceBuilder<Unicode_, Ascii_, Attrs_, (), BgColor_> {
    pub fn fg_color(self, v: impl Color) -> AppearanceBuilder<Unicode_, Ascii_, Attrs_, BoxedColor, BgColor_> {
        let (unicode, ascii, attrs, _, bg_color) = self.fields;
        // Custom conversion: typed-builder doesn't support this.
        let fg_color = BoxedColor::new(v);
        AppearanceBuilder {
            fields: (unicode, ascii, attrs, fg_color, bg_color)
        }
    }
}

impl<Unicode_, Ascii_, Attrs_, FgColor_> AppearanceBuilder<Unicode_, Ascii_, Attrs_, FgColor_, ()> {
    pub fn bg_color(self, v: impl Color) -> AppearanceBuilder<Unicode_, Ascii_, Attrs_, FgColor_, BoxedColor> {
        let (unicode, ascii, attrs, fg_color, _) = self.fields;
        // Custom conversion: typed-builder doesn't support this.
        let bg_color = BoxedColor::new(v);
        AppearanceBuilder {
            fields: (unicode, ascii, attrs, fg_color, bg_color)
        }
    }
}

impl<Attrs_, FgColor_, BgColor_> AppearanceBuilder<String, char, Attrs_, FgColor_, BgColor_>
where Attrs_: AppearanceBuilderOptional<AttrSet>,
      FgColor_: AppearanceBuilderOptional<BoxedColor>,
      BgColor_: AppearanceBuilderOptional<BoxedColor> {
    /// Finalise the builder and create an instance of [Appearance].
    pub fn build(self) -> Appearance {
        let (unicode, ascii, attrs, fg_color, bg_color) = self.fields;
        Appearance {
            unicode_: unicode,
            ascii_: ascii,
            attrs_: attrs.into(),
            fg_color_: fg_color.into(),
            bg_color_: bg_color.into()
        }
    }
}

#[doc(hide)]
pub trait AppearanceBuilderOptional<T> {
    fn into(self) -> T;
}

impl AppearanceBuilderOptional<AttrSet> for () {
    fn into(self) -> AttrSet {
        AttrSet::default()
    }
}

impl AppearanceBuilderOptional<AttrSet> for AttrSet {
    fn into(self) -> AttrSet {
        self
    }
}

impl AppearanceBuilderOptional<BoxedColor> for () {
    fn into(self) -> BoxedColor {
        BoxedColor::default()
    }
}

impl AppearanceBuilderOptional<BoxedColor> for BoxedColor {
    fn into(self) -> BoxedColor {
        self
    }
}

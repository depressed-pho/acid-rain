pub(crate) mod manager;

use color_space::{FromColor, FromRgb, Rgb, ToRgb};
use downcast_rs::{Downcast, impl_downcast};
use std::fmt::Debug;
use std::sync::Arc;

/// A color to be used for the foreground or the background of a
/// character on a terminal. If the terminal doesn't have a capability
/// of using colors, setting colors will do nothing.
pub trait Color: Debug + Downcast + Sync + Send {
    /// If you don't know what this is, leave it unimplemented.
    fn magic_index(&self) -> Option<i32> {
        None
    }

    /// Colors need to be convertible to RGB as it's the only color
    /// model ncurses supports, But we can't use Into<RGBColor> as a
    /// bound of this trait because then Color cannot be turned into a
    /// trait object.
    fn as_rgb(&self) -> RGBColor;
}

impl_downcast!(Color);

/// Any type implementing ToRgb can also be a Color.
impl<T: ToRgb + Debug + Sync + Send + 'static> Color for T {
    fn as_rgb(&self) -> RGBColor {
        FromColor::<T>::from_color(self)
    }
}

/// A sized type holding a trait object of some Color. This type
/// itself implements Color.
#[derive(Clone, Debug)]
pub struct BoxedColor {
    inner: Arc<dyn Color>
}

impl BoxedColor {
    pub fn new(color: impl Color) -> Self {
        // If it's already a BoxedColor, we should not wrap it
        // again. That would cause infinitely many indirections.
        if let Some(boxed) = (&color as &dyn Color).downcast_ref::<BoxedColor>() {
            (*boxed).clone()
        }
        else {
            Self {
                inner: Arc::new(color)
            }
        }
    }
}

impl Default for BoxedColor {
    fn default() -> Self {
        Self::new(DefaultColor())
    }
}

impl Color for BoxedColor {
    fn magic_index(&self) -> Option<i32> {
        self.inner.magic_index()
    }

    fn as_rgb(&self) -> RGBColor {
        self.inner.as_rgb()
    }
}

/// Predefined common colors for ANSI terminals.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ANSIColor {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White
}

impl ToRgb for ANSIColor {
    fn to_rgb(&self) -> Rgb {
        let on  = (680.0 / 1000.0) * 255.0; // See ncurses/base/lib_color.c
        let off = 0.0;
        match self {
            ANSIColor::Black   => Rgb::new(off, off, off),
            ANSIColor::Red     => Rgb::new(on , off, off),
            ANSIColor::Green   => Rgb::new(off, on , off),
            ANSIColor::Yellow  => Rgb::new(on , on , off),
            ANSIColor::Blue    => Rgb::new(off, off, on ),
            ANSIColor::Magenta => Rgb::new(on , off, on ),
            ANSIColor::Cyan    => Rgb::new(off, on , on ),
            ANSIColor::White   => Rgb::new(on , on , on )
        }
    }
}

/// A color represented with red, green, and blue with each component
/// being 0..255. On a terminal which doesn't have a capability of
/// using RGB color, the closest available color will be chosen
/// instead.
///
/// Application authors need not use this. They can use
/// [color_space::Rgb] or whatever color space that fits their
/// need. The only reason this type exists is to save memory by
/// representing RGB colors with just 3 octets each.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct RGBColor {
    pub r: u8,
    pub g: u8,
    pub b: u8
}

/// Any type that is ToRgb can also be converted to RGBColor.
impl<T: ToRgb> FromColor<T> for RGBColor {
    fn from_color(color: &T) -> Self {
        RGBColor::from_rgb(&color.to_rgb())
    }
}

impl FromRgb for RGBColor {
    fn from_rgb(rgb: &Rgb) -> Self {
        RGBColor {
            r: rgb.r as u8,
            g: rgb.g as u8,
            b: rgb.b as u8
        }
    }
}

impl ToRgb for RGBColor {
    fn to_rgb(&self) -> Rgb {
        Rgb::new(self.r as f64, self.g as f64, self.b as f64)
    }
}

/// The default foreground or background color defined by the
/// terminal. Most terminals define a default color, but if your
/// terminal doesn't define one, it will be fell back to black
/// background and white foreground.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct DefaultColor();

impl Color for DefaultColor {
    fn magic_index(&self) -> Option<i32> {
        Some(-1)
    }

    fn as_rgb(&self) -> RGBColor {
        panic!("DefaultColor doesn't represent a specific color so it's impossible to convert it to RGB.");
    }
}

impl Default for DefaultColor {
    fn default() -> Self {
        Self()
    }
}
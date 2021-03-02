pub(crate) mod manager;

use downcast_rs::{Downcast, impl_downcast};
use std::fmt::Debug;

/// A color to be used for the foreground or the background of a
/// character on a terminal. On a terminal which doesn't have a
/// capability of using colors, setting colors will do nothing.
pub trait Color: Debug + Downcast {
    // We can't use TryInto<RGBColor> as a bound of this trait because
    // then Color cannot be turned into a trait object because of its
    // associated type 'Error'.
    fn as_rgb(&self) -> Option<RGBColor>;
}

impl_downcast!(Color);

/// A color represented with red, green, and blue components. (0, 0,
/// 0) represents black while (255, 255, 255) represents white. On a
/// terminal which doesn't have a capability of using RGB color, the
/// closest available color will be chosen instead.
#[derive(Clone, Copy, Debug)]
pub struct RGBColor {
    pub r: u8,
    pub g: u8,
    pub b: u8
}

impl RGBColor {
    pub(crate) fn distance(&self, rhs: Self) -> f64 {
        ((f64::from(self.r) - f64::from(rhs.r)).powi(2) +
         (f64::from(self.g) - f64::from(rhs.g)).powi(2) +
         (f64::from(self.b) - f64::from(rhs.b)).powi(2)).sqrt()
    }
}

impl Color for RGBColor {
    fn as_rgb(&self) -> Option<RGBColor> {
        Some(*self)
    }
}

/// The default foreground or background color defined by the
/// terminal. Most terminals define a default color, but if your
/// terminal doesn't define one, it will be fell back to black
/// background and white foreground.
#[derive(Clone, Copy, Debug)]
pub struct DefaultColor();

impl Color for DefaultColor {
    /// DefaultColor doesn't represent a specific color so it's
    /// impossible to convert it to RGB.
    fn as_rgb(&self) -> Option<RGBColor> {
        None
    }
}

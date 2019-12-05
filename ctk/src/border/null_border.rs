use crate::Graphics;
use crate::border::Border;
use crate::dimension::Insets;
use num::Zero;

pub struct NullBorder {}

impl Default for NullBorder {
    fn default() -> Self {
        NullBorder {}
    }
}

impl Border for NullBorder {
    fn get_insets(&self) -> Insets {
        Insets::zero()
    }

    fn paint(&self, _: &mut Graphics) {
        // Paint nothing.
    }
}

use async_trait::async_trait;
use crate::Graphics;
use crate::border::Border;
use crate::dimension::Insets;
use num::Zero;

#[derive(Debug)]
pub struct NullBorder {}

impl Default for NullBorder {
    fn default() -> Self {
        NullBorder {}
    }
}

#[async_trait]
impl Border for NullBorder {
    fn get_insets(&self) -> Insets {
        Insets::zero()
    }

    async fn paint(&self, _: &mut Graphics) {
        // Paint nothing.
    }
}

mod button_border;
pub use button_border::*;

mod null_border;
pub use null_border::*;

use async_trait::async_trait;
use crate::Graphics;
use crate::dimension::Insets;
use std::fmt::Debug;

/// A trait describing an object capable of rendering a border around
/// the edges of a Ctk component.
#[async_trait]
pub trait Border: Debug + Send + Sync {
    /// Return the insets of the border.
    fn get_insets(&self) -> Insets;

    /// Paint the border for the specified component.
    async fn paint(&self, g: &mut Graphics);
}

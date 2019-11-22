mod button_border;
pub use button_border::*;

mod null_border;
pub use null_border::*;

use crate::ctk::Graphics;
use crate::ctk::dimension::Insets;

/** A trait describing an object capable of rendering a border around
 * the edges of a Ctk component.
 */
pub trait Border {
    /** Return the insets of the border.
     */
    fn get_insets(&self) -> Insets;

    /** Paint the border for the specified component.
     */
    fn paint(&self, g: &mut Graphics);
}

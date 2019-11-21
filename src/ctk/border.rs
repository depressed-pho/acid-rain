use crate::ctk::Component;
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
    fn paint(c: &mut Component);
}

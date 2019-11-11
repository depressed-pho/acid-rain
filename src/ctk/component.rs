mod label;
pub use label::*;

use crate::ctk::Graphics;

pub trait Component {
    /** Borrow the graphics context associated to this
     * component. Every component must own one.
     */
    fn graphics(&self) -> &Graphics;

    /** Refresh the content of the graphics context. */
    fn paint(&mut self) -> Result<(), ()>;
}

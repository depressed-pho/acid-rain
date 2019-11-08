mod label;
pub use label::*;

use crate::ctk::Buffer;

pub trait Component {
    /** Borrow the graphics buffer associated to this component. Every
     * component must own one.
     */
    fn buffer(&self) -> &Buffer;

    /** Refresh the content of the off-screen buffer. */
    fn paint(&mut self) -> Result<(), ()>;
}

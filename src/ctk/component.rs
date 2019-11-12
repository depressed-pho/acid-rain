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

    /** Validate sub-components if the component has any.
     *
     * A component is a container if it has a layout manager and
     * sub-components. In which case this method must invoke the
     * validate() method on the layout manager. Otherwise it should do
     * nothing. The default implementation does nothing as if the
     * component is not a container.
     */
    fn validate(&mut self) {}
}

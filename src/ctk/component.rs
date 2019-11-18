mod label;
pub use label::*;

use crate::ctk::Graphics;
use crate::ctk::dimension::{
    Dimension,
    Insets,
    Rectangle
};

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

    /** Get the bounds of this component in the form of a Rectangle
     * struct. The bounds specify this component's width, height, and
     * location relative to its parent.
     */
    fn get_bounds(&self) -> Rectangle;

    /** Get the size of this component, i.e. the size of the bounds.
     */
    fn get_size(&self) -> Dimension {
        Dimension::from(self.get_bounds())
    }

    /** If a border has been set on this component, returns the
     * border's insets; otherwise returns an empty insets. */
    fn get_insets(&self) -> Insets {
        // FIXME: not implemented properly yet
        Insets::default()
    }
}

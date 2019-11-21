mod label;
pub use label::*;

use crate::ctk::Graphics;
use crate::ctk::RootWindow;
use crate::ctk::dimension::{
    Dimension,
    Insets,
    Point,
    Rectangle
};

pub trait Component {
    /** Borrow the graphics context associated to this
     * component. Every component must own one.
     */
    fn graphics(&self) -> &Graphics;

    /** Paint the content of the graphics context if it might not
     * have the desired content. This method must recursively repaint
     * sub-components if the component is a container.
     */
    fn paint(&mut self);

    /** Copy the content of the graphics context to the curses
     * screen. This method must recursively do the copying if the
     * component is a container.
     */
    fn refresh(&self, root: &RootWindow);

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

    /** Move and resize this component. This method changes
     * layout-related information, and therefore, must invalidate the
     * component hierarchy if the component is a container. It must
     * also resize the graphics context.
     */
    fn set_bounds(&mut self, b: Rectangle);

    /** Get the location of this component, i.e. the position of the
     * bounds.
     */
    fn get_location(&self) -> Point {
        Point::from(self.get_bounds())
    }

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

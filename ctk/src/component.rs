mod button;
pub use button::*;

mod label;
pub use label::*;

mod panel;
pub use panel::*;

use crate::{
    Border,
    RootWindow
};
use crate::dimension::{
    Dimension,
    Insets,
    Point,
    Rectangle,
    SizeRequirements
};
use std::fmt::Debug;

pub trait Component: Debug {
    /** Paint the content of the graphics context if it might not have
     * the desired content. This method must recursively repaint
     * sub-components if the component is a container.
     */
    fn paint(&mut self);

    /** Copy the content of the graphics context to the curses
     * screen. This method must recursively do the copying if the
     * component is a container.
     */
    fn refresh(&self, root: &RootWindow, offset: Point);

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

    /** Set the size of this component.
     */
    fn set_size(&mut self, size: Dimension) {
        self.set_bounds(
            Rectangle {
                size,
                ..self.get_bounds()
            });
    }

    /** Update the size of this component by applying a function to
     * the current value.
     */
    fn update_size(&mut self, f: &dyn Fn(Dimension) -> Dimension) {
        self.set_size(f(self.get_size()));
    }

    /** Get the size requirements of this component. They are hints
     * for layout managers and they usually, but not always, honor
     * them.
     */
    fn get_size_requirements(&self) -> SizeRequirements;

    /** Get the border of this component.
     */
    fn get_border(&self) -> &Box<dyn Border>;

    /** Set the possible border of this component.
     */
    fn set_border(&mut self, b: Box<dyn Border>);

    /** Returns the border's insets.
     */
    fn get_insets(&self) -> Insets {
        self.get_border().get_insets()
    }
}

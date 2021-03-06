mod button;
pub use button::*;

mod label;
pub use label::*;

mod panel;
pub use panel::*;

use async_trait::async_trait;
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
use num::Zero;
use std::fmt::Debug;

#[async_trait]
pub trait Component: Debug + Send + Sync {
    /// Paint the content of the graphics context if it might not have
    /// the desired content. This method must recursively repaint
    /// sub-components if the component is a container.
    async fn paint(&mut self);

    /// Copy the content of the graphics context to the curses
    /// screen. This method must recursively do the copying if the
    /// component is a container. This method is unsafe because it
    /// must not be executed outside of the Ctk context.
    async unsafe fn refresh(&self, root: &RootWindow, offset: Point);

    /// Validate sub-components if the component has any.
    ///
    /// A component is a container if it has a layout manager and
    /// sub-components. In which case this method must invoke the
    /// [validate()](crate::layout::Layout::validate()) method on the
    /// layout manager. Otherwise it should do nothing. The default
    /// implementation does nothing as if the component was not a
    /// container.
    async fn validate(&mut self) {}

    /// Get the bounds of this component. The bounds specify this
    /// component's width, height, and location relative to its
    /// parent.
    fn get_bounds(&self) -> Rectangle;

    /// Move and resize this component. This method changes
    /// layout-related information, and therefore, must
    /// [invalidate()](crate::layout::Layout::invalidate()) the
    /// component hierarchy if the component is a container. It must
    /// also resize the graphics context.
    async fn set_bounds(&mut self, b: Rectangle);

    /// Get the location of this component, i.e. the position of the
    /// bounds.
    fn get_location(&self) -> Point {
        Point::from(self.get_bounds())
    }

    /// Get the size of this component, i.e. the size of the bounds.
    fn get_size(&self) -> Dimension {
        Dimension::from(self.get_bounds())
    }

    /// Set the size of this component.
    async fn set_size(&mut self, size: Dimension) {
        self.set_bounds(
            Rectangle {
                size,
                ..self.get_bounds()
            }).await;
    }

    /// Get the size requirements of this component. They are hints
    /// for layout managers and they usually, but not always, honor
    /// them.
    async fn get_size_requirements(&self) -> SizeRequirements;

    /// Get the border of this component.
    fn get_border(&self) -> &Box<dyn Border>;

    /// Set the possible border of this component.
    fn set_border(&mut self, b: Box<dyn Border>);

    /// Returns the border's insets.
    fn get_insets(&self) -> Insets {
        self.get_border().get_insets()
    }

    /// Return the inner area inside the border.
    fn get_inner(&self) -> Rectangle {
        Rectangle {
            pos: Point::zero(),
            size: self.get_size()
        }.shrink(self.get_insets())
    }
}

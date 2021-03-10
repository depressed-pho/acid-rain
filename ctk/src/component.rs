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
use num_traits::Zero;
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};

pub trait Component: Debug {
    /// Wrap the component in a smart component pointer.
    fn into_ref(self) -> ComponentRef<Self> where Self: Sized {
        ComponentRef::new(self)
    }

    fn get_parent(&self) -> Option<ComponentRef<dyn Component>>;

    fn set_parent(&mut self, p: Option<ComponentRef<dyn Component>>);

    /// Paint the content of the graphics context if it might not have
    /// the desired content. This method must recursively repaint
    /// sub-components if the component is a container.
    fn paint(&mut self);

    /// Copy the content of the graphics context to the curses
    /// screen. This method must recursively do the copying if the
    /// component is a container.
    fn refresh(&self, root: &RootWindow, offset: Point);

    /// Validate sub-components if the component has any.
    ///
    /// A component is a container if it has a layout manager and
    /// sub-components. In which case this method must invoke the
    /// [validate()](crate::layout::Layout::validate()) method on the
    /// layout manager. Otherwise it should do nothing. The default
    /// implementation does nothing as if the component was not a
    /// container.
    fn validate(&mut self) {}

    /// Get the bounds of this component. The bounds specify this
    /// component's width, height, and location relative to its
    /// parent.
    fn get_bounds(&self) -> Rectangle;

    /// Move and resize this component. This method changes
    /// layout-related information, and therefore, must
    /// [invalidate()](crate::layout::Layout::invalidate()) the
    /// component hierarchy if the component is a container. It must
    /// also resize the graphics context.
    fn set_bounds(&mut self, b: Rectangle);

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
    fn set_size(&mut self, size: Dimension) {
        self.set_bounds(
            Rectangle {
                size,
                ..self.get_bounds()
            });
    }

    /// Update the size of this component by applying a function to
    /// the current value.
    fn update_size(&mut self, f: &dyn Fn(Dimension) -> Dimension) {
        self.set_size(f(self.get_size()));
    }

    /// Get the size requirements of this component. They are hints
    /// for layout managers and they usually, but not always, honor
    /// them.
    fn get_size_requirements(&self) -> SizeRequirements;

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

/// This is a smart pointer containing a component. It is currently
/// reference-counted, but this may change in future (i.e. GC).
#[derive(Debug)]
pub struct ComponentRef<T: Component + ?Sized> {
    ptr: Rc<RefCell<T>>
}

impl<T: Component> ComponentRef<T> {
    pub fn new(component: T) -> Self {
        Self {
            ptr: Rc::new(RefCell::new(component))
        }
    }
}

impl<T: Component + ?Sized> ComponentRef<T> {
    pub fn borrow(&self) -> impl Deref<Target = T> + '_ {
        self.ptr.borrow()
    }

    pub fn borrow_mut(&self) -> impl DerefMut<Target = T> + '_ {
        self.ptr.borrow_mut()
    }

    /// This may be removed in the future.
    pub fn downgrade(&self) -> WeakComponentRef<T> {
        WeakComponentRef {
            ptr: Rc::downgrade(&self.ptr)
        }
    }
}

/// Weak pointer to a component. This may be removed in the future.
#[derive(Debug)]
pub struct WeakComponentRef<T: Component + ?Sized> {
    ptr: Weak<RefCell<T>>
}

impl<T: Component + ?Sized> WeakComponentRef<T> {
    pub fn upgrade(&self) -> Option<ComponentRef<T>> {
        self.ptr.upgrade().map(|ptr| ComponentRef { ptr })
    }
}

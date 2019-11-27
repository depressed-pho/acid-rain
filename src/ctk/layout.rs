mod grid_layout;
pub use grid_layout::GridLayout;

pub mod spring_layout;
pub use spring_layout::SpringLayout;

use std::cell::RefCell;
use std::rc::Rc;

use crate::ctk::{
    Component
};
use crate::ctk::dimension::SizeRequirements;

/** Layout manager is responsible for laying out sub-components in a
 * container component.
 *
 * Note that layout managers cannot be safely shared by multiple
 * components.
 */
pub trait Layout {
    /** Do laying out sub-components if the layout manager is invalid.
     *
     * The argument c is the parent component which owns this layout
     * manager.
     *
     * A layout manager is said to be invalid if it has any
     * sub-component which needs to be moved or resized. Adding or
     * removing a sub-component must always invalidate the layout
     * manager, and also it must become invalid if the method
     * invalidate() is invoked. The latter case typically happens when
     * the parent component is somehow resized.
     *
     * Note that sub-components may also be containers themselves so
     * layout managers must call validate() on each of them,
     * regardless of whether the layout manager is itself invalid.
     */
    fn validate(&mut self, parent: &dyn Component);
    fn invalidate(&mut self);

    // We can't simply do "-> impl Iterator<...>" due to E0562:
    // https://github.com/rust-lang/rfcs/blob/master/text/1522-conservative-impl-trait.md
    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Rc<RefCell<dyn Component>>> + 'a>;

    fn get_size_requirements(&self) -> SizeRequirements;
}

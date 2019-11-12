mod grid_layout;
pub use grid_layout::*;

use crate::ctk::{
    Component
};

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
    fn validate(&mut self, c: &Component);
    fn invalidate(&mut self);
}

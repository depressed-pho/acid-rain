pub mod grid_layout;
pub use grid_layout::GridLayout;

pub mod group_layout;
pub use group_layout::GroupLayout;

pub mod spring_layout;
pub use spring_layout::SpringLayout;

use async_trait::async_trait;
use std::fmt::Debug;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::{
    Component
};
use crate::dimension::SizeRequirements;

/// Layout manager is responsible for laying out sub-components in a
/// container component.
///
/// Note that layout managers generally cannot be safely shared by
/// multiple components. The only reason components take an Rc pointer
/// to a layout is so that you can mutate the layout after
/// constructing the component tree.
#[async_trait]
pub trait Layout: Debug + Send + Sync {
    /// Do laying out sub-components if the layout manager is invalid.
    ///
    /// The argument `parent` is the parent component which owns this
    /// layout manager.
    ///
    /// A layout manager is said to be invalid if it has any
    /// sub-component which needs to be moved or resized. Adding or
    /// removing a sub-component must always invalidate the layout
    /// manager, and also it must become invalid if the method
    /// [Layout::invalidate()] is invoked. The latter case typically
    /// happens when the parent component is somehow resized.
    ///
    /// Note that sub-components may also be containers themselves so
    /// layout managers must call [Component::validate()] on each of
    /// them, regardless of whether the layout manager is itself
    /// invalid.
    async fn validate(&mut self, parent: &dyn Component);
    fn invalidate(&mut self);

    // We can't simply do "-> impl Iterator<...>" due to E0562:
    // https://github.com/rust-lang/rfcs/blob/master/text/1522-conservative-impl-trait.md
    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Arc<RwLock<dyn Component>>> + Send + 'a>;

    async fn get_size_requirements(&self, parent: &dyn Component) -> SizeRequirements;
}

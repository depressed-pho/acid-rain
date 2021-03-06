use async_trait::async_trait;
use crate::{
    Border,
    Component,
    Graphics,
    Layout
};
use crate::border::NullBorder;
use crate::dimension::{
    Dimension,
    Point,
    Rectangle,
    SizeRequirements
};
use num::Zero;
use std::sync::Arc;
use tokio::sync::RwLock;

/// The root window is a special window which covers the entire
/// terminal screen. Its position is fixed to (0, 0), and its size can
/// only be changed by changing the terminal size itself.
#[derive(Debug)]
pub struct RootWindow {
    screen: ncurses::WINDOW,
    graphics: Graphics,
    bounds: Rectangle,
    layout: Arc<RwLock<dyn Layout>>,
    /* We really want to do Box<dyn Border + Copy> but Rust currently
     * doesn't allow that: E0225 */
    border: Box<dyn Border>,
    dirty: bool
}

/// Mark Graphics as Send. NCurses doesn't rely on thread-local
/// storages or anything so WINDOW pointers can be safely sent between
/// threads, although that doesn't make sense in general.
unsafe impl Send for RootWindow {}

/// Mark Graphics as Sync as well. While ncurses::WINDOW itself does
/// not prevent data races at all, we make sure that we only touch it
/// in the Ctk context.
unsafe impl Sync for RootWindow {}

impl RootWindow {
    pub(crate) fn new(screen: ncurses::WINDOW, layout: Arc<RwLock<dyn Layout>>) -> RootWindow {
        let bounds = Self::bounds_from_screen(screen);

        let mut graphics = Graphics::new();
        graphics.set_size(bounds.size);

        RootWindow {
            screen,
            graphics,
            bounds,
            layout,
            border: Box::new(NullBorder {}),
            dirty: true
        }
    }

    pub(crate) async fn resize(&mut self) {
        self.set_bounds(
            Self::bounds_from_screen(self.screen)).await;
    }

    fn bounds_from_screen(screen: ncurses::WINDOW) -> Rectangle {
        let (mut width, mut height) = (0, 0);
        ncurses::getmaxyx(screen, &mut height, &mut width);

        Rectangle {
            pos: Point::zero(),
            size: Dimension { width, height }
        }
    }
}

#[async_trait]
impl Component for RootWindow {
    async fn paint(&mut self) {
        if self.dirty {
            self.border.paint(&mut self.graphics).await;
        }
        self.dirty = false;
        for child in self.layout.read().await.children() {
            child.write().await.paint().await;
        }
    }

    async unsafe fn refresh(&self, root: &Self, offset: Point) {
        assert!(self as *const Self == root as *const Self);
        assert!(offset.is_zero());

        self.graphics.refresh(root, self.get_location());
        for child in self.layout.read().await.children() {
            child.read().await.refresh(root, offset).await;
        }
    }

    async fn validate(&mut self) {
        self.layout.write().await.validate(self).await;
    }

    fn get_bounds(&self) -> Rectangle {
        self.bounds
    }

    /// For internal use only. User code must not invoke this.
    async fn set_bounds(&mut self, b: Rectangle) {
        self.bounds = b;
        self.layout.write().await.invalidate();
        if self.graphics.set_size(b.size) {
            self.dirty = true;
        }
    }

    async fn get_size_requirements(&self) -> SizeRequirements {
        SizeRequirements::exactly(self.get_size())
    }

    fn get_border(&self) -> &Box<dyn Border> {
        &self.border
    }

    fn set_border(&mut self, b: Box<dyn Border>) {
        self.border = b;
    }
}

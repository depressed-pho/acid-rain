use async_trait::async_trait;
use crate::{
    Border,
    Component,
    Graphics,
    Layout,
    RootWindow,
};
use crate::border::NullBorder;
use crate::dimension::{
    Point,
    Rectangle,
    SizeRequirements
};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug)]
pub struct Panel {
    graphics: Graphics,
    bounds: Rectangle,
    layout: Arc<RwLock<dyn Layout>>,
    border: Box<dyn Border>,
    dirty: bool
}

impl Panel {
    pub fn new(layout: Arc<RwLock<dyn Layout>>) -> Panel {
        Panel {
            graphics: Graphics::new(),
            bounds: Rectangle::default(),
            layout,
            border: Box::new(NullBorder {}),
            dirty: true
        }
    }
}

#[async_trait]
impl Component for Panel {
    async fn paint(&mut self) {
        if self.dirty {
            self.border.paint(&mut self.graphics).await;
        }
        self.dirty = false;
        for child in self.layout.read().await.children() {
            child.write().await.paint().await;
        }
    }

    async unsafe fn refresh(&self, root: &RootWindow, offset: Point) {
        let pos = self.get_location() + offset;
        self.graphics.refresh(root, pos);
        for child in self.layout.read().await.children() {
            child.read().await.refresh(root, pos).await;
        }
    }

    async fn validate(&mut self) {
        self.layout.write().await.validate(self).await;
    }

    fn get_bounds(&self) -> Rectangle {
        self.bounds
    }

    async fn set_bounds(&mut self, b: Rectangle) {
        self.bounds = b;
        self.layout.write().await.invalidate();
        if self.graphics.set_size(b.size) {
            self.dirty = true;
        }
    }

    async fn get_size_requirements(&self) -> SizeRequirements {
        self.layout.read().await.get_size_requirements(self).await
    }

    fn get_border(&self) -> &Box<dyn Border> {
        &self.border
    }

    fn set_border(&mut self, b: Box<dyn Border>) {
        self.border = b;
    }
}

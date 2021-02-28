use ctk::{
    Border,
    Component,
    Graphics,
    RootWindow
};
use ctk::border::NullBorder;
use ctk::dimension::{
    Point,
    Rectangle,
    SizeRequirements
};
use rain_core::world::World;
use std::sync::{Arc, RwLock};

#[derive(Debug)]
pub struct WorldView<W: World> {
    graphics: Graphics,
    bounds: Rectangle,
    border: Box<dyn Border>,
    dirty: bool,
    world: Arc<RwLock<W>>
}

impl<W: World> WorldView<W> {
    pub fn new(world: Arc<RwLock<W>>) -> Self {
        Self {
            graphics: Graphics::new(),
            bounds: Rectangle::default(),
            border: Box::new(NullBorder::default()),
            dirty: true,
            world
        }
    }
}

impl<W: World> Component for WorldView<W> {
    fn paint(&mut self) {
        if self.dirty {
            // FIXME
        }
        self.dirty = true;
    }

    fn refresh(&self, root: &RootWindow, offset: Point) {
        self.graphics.refresh(root, self.get_location() + offset);
    }

    fn get_bounds(&self) -> Rectangle {
        self.bounds
    }

    fn set_bounds(&mut self, b: Rectangle) {
        self.bounds = b;
        if self.graphics.set_size(b.size) {
            self.dirty = true;
        }
    }

    fn get_size_requirements(&self) -> SizeRequirements {
        SizeRequirements::any()
    }

    fn get_border(&self) -> &Box<dyn Border> {
        &self.border
    }

    fn set_border(&mut self, b: Box<dyn Border>) {
        self.border = b;
    }
}

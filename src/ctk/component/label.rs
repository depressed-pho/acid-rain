use crate::ctk::{
    Component,
    Graphics
};
use crate::ctk::dimension::{
    Dimension,
    Rectangle
};

pub struct Label {
    graphics: Graphics,
    bounds: Rectangle,
    dirty: bool,
    text: String
}

impl Label {
    pub fn new<T: Into<String>>(text: T) -> Label {
        Label {
            graphics: Graphics::new(),
            bounds: Rectangle::default(),
            dirty: true,
            text: text.into()
        }
    }
}

impl Component for Label {
    fn graphics(&self) -> &Graphics {
        &self.graphics
    }

    fn paint(&mut self) {
        if self.dirty {
            unimplemented!();
        }
        self.dirty = false;
    }

    fn refresh(&self, scr: Dimension) {
        self.graphics.refresh(self.get_location(), scr);
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
}

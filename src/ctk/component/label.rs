use crate::ctk::{
    Component,
    Graphics
};
use crate::ctk::dimension::{
    Dimension,
    Point,
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
            // FIXME: word wrapping, and labels longer than the inner width.
            // FIXME: also alignment
            let insets = self.get_insets();
            // FIXME: Clear the context before drawing.
            self.graphics.draw_string(&self.text, Point { x: insets.left, y: insets.top });
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

use crate::ctk::{
    Component,
    Graphics
};
use crate::ctk::dimension::Rectangle;

pub struct Label {
    graphics: Graphics,
    bounds: Rectangle,
    text: String
}

impl Label {
    pub fn new<T: Into<String>>(text: T) -> Label {
        Label {
            graphics: Graphics::new(),
            bounds: Rectangle::default(),
            text: text.into()
        }
    }
}

impl Component for Label {
    fn graphics(&self) -> &Graphics {
        &self.graphics
    }

    fn paint(&mut self) -> Result<(), ()> {
        unimplemented!();
    }

    fn get_bounds(&self) -> Rectangle {
        self.bounds
    }
}

use crate::ctk::Component;
use crate::ctk::Graphics;

pub struct Label {
    graphics: Graphics,
    text: String
}

impl Label {
    pub fn new<T: Into<String>>(text: T) -> Label {
        Label {
            graphics: Graphics::new(),
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
}

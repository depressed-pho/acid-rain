use super::Component;

pub struct Label {
    text: String
}

impl Label {
    pub fn new<T: Into<String>>(text: T) -> Label {
        Label {
            text: text.into()
        }
    }
}

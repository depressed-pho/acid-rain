use crate::{
    Border,
    Component,
    ComponentRef,
    Graphics,
    Layout,
    RootWindow,
    WeakComponentRef
};
use crate::border::NullBorder;
use crate::dimension::{
    Point,
    Rectangle,
    SizeRequirements
};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Panel {
    parent: Option<WeakComponentRef<dyn Component>>,
    graphics: Graphics,
    bounds: Rectangle,
    layout: Rc<RefCell<dyn Layout>>,
    border: Box<dyn Border>,
    dirty: bool
}

impl Panel {
    pub fn new(layout: Rc<RefCell<dyn Layout>>) -> Panel {
        Panel {
            parent: None,
            graphics: Graphics::new(),
            bounds: Rectangle::default(),
            layout,
            border: Box::new(NullBorder {}),
            dirty: true
        }
    }
}

impl Component for Panel {
    fn get_parent(&self) -> Option<ComponentRef<dyn Component>> {
        self.parent.as_ref().map(|p| p.upgrade().unwrap())
    }

    fn set_parent(&mut self, p: Option<ComponentRef<dyn Component>>) {
        self.parent = p.map(|p| p.downgrade());
    }

    fn paint(&mut self) {
        if self.dirty {
            self.border.paint(&mut self.graphics);
        }
        self.dirty = false;
        for child in self.layout.borrow().children() {
            child.borrow_mut().paint();
        }
    }

    fn refresh(&self, root: &RootWindow, offset: Point) {
        let pos = self.get_location() + offset;
        self.graphics.refresh(root, pos);
        for child in self.layout.borrow().children() {
            child.borrow().refresh(root, pos);
        }
    }

    fn validate(&mut self) {
        self.layout.borrow_mut().validate(self);
    }

    fn get_bounds(&self) -> Rectangle {
        self.bounds
    }

    fn set_bounds(&mut self, b: Rectangle) {
        self.bounds = b;
        self.layout.borrow_mut().invalidate();
        if self.graphics.set_size(b.size) {
            self.dirty = true;
        }
    }

    fn get_size_requirements(&self) -> SizeRequirements {
        self.layout.borrow().get_size_requirements(self)
    }

    fn get_border(&self) -> &Box<dyn Border> {
        &self.border
    }

    fn set_border(&mut self, b: Box<dyn Border>) {
        self.border = b;
    }
}

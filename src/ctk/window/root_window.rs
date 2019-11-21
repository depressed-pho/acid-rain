use crate::ctk::{
    Component,
    Graphics,
    Layout
};
use crate::ctk::dimension::{
    Dimension,
    Point,
    Rectangle
};
use std::cell::RefCell;
use std::rc::Rc;

/** The root window is a special window which covers the entire
 * terminal screen. Its position is fixed to (0, 0), and its size can
 * only be changed by changing the terminal size itself.
 */
pub struct RootWindow {
    screen: ncurses::WINDOW,
    graphics: Graphics,
    bounds: Rectangle,
    layout: Rc<RefCell<dyn Layout>>,
    dirty: bool
}

impl RootWindow {
    pub(crate) fn new(screen: ncurses::WINDOW, layout: Rc<RefCell<dyn Layout>>) -> RootWindow {
        let (mut width, mut height) = (0, 0);
        ncurses::getmaxyx(screen, &mut height, &mut width);

        let bounds = Rectangle {
            pos: Point::zero(),
            size: Dimension { width, height }
        };

        let mut graphics = Graphics::new();
        graphics.set_size(bounds.size);

        RootWindow {
            screen,
            graphics,
            bounds,
            layout,
            dirty: true
        }
    }
}

impl Component for RootWindow {
    fn graphics_mut(&mut self) -> &mut Graphics {
        &mut self.graphics
    }

    fn paint(&mut self) {
        if self.dirty {
            // Do nothing.
        }
        self.dirty = false;
        for child in self.layout.borrow().children() {
            child.borrow_mut().paint();
        }
    }

    fn refresh(&self, root: &Self) {
        // The root must be self, but how do we assert that?
        self.graphics.refresh(self.get_location(), root);
        for child in self.layout.borrow().children() {
            child.borrow().refresh(root);
        }
    }

    fn validate(&mut self) {
        self.layout.borrow_mut().validate(self);
    }

    fn get_bounds(&self) -> Rectangle {
        self.bounds
    }

    /** For internal use only. User code must not invoke this.
     */
    fn set_bounds(&mut self, b: Rectangle) {
        self.bounds = b;
        self.layout.borrow_mut().invalidate();
        if self.graphics.set_size(b.size) {
            self.dirty = true;
        }
    }
}

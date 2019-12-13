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
use std::cell::RefCell;
use std::rc::Rc;

/** The root window is a special window which covers the entire
 * terminal screen. Its position is fixed to (0, 0), and its size can
 * only be changed by changing the terminal size itself.
 */
#[derive(Debug)]
pub struct RootWindow {
    screen: ncurses::WINDOW,
    graphics: Graphics,
    bounds: Rectangle,
    layout: Rc<RefCell<dyn Layout>>,
    /* We really want to do Box<dyn Border + Copy> but Rust currently
     * doesn't allow that: E0225 */
    border: Box<dyn Border>,
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
            border: Box::new(NullBorder {}),
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
            self.border.paint(&mut self.graphics);
        }
        self.dirty = false;
        for child in self.layout.borrow().children() {
            child.borrow_mut().paint();
        }
    }

    fn refresh(&self, root: &Self, offset: Point) {
        assert!(self as *const Self == root as *const Self);
        assert!(offset.is_zero());

        self.graphics.refresh(root, self.get_location());
        for child in self.layout.borrow().children() {
            child.borrow().refresh(root, offset);
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

    fn get_size_requirements(&self) -> SizeRequirements {
        SizeRequirements::exactly(self.get_size())
    }

    fn get_border(&self) -> &Box<dyn Border> {
        &self.border
    }

    fn set_border(&mut self, b: Box<dyn Border>) {
        self.border = b;
    }
}

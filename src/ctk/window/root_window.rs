use crate::ctk::{
    Component,
    Graphics,
    Layout,
    util::check
};
use crate::ctk::dimension::{
    Dimension,
    Point,
    Rectangle
};
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::Rc;

/** The root window is a special window which covers the entire
 * terminal screen. Its position is fixed to (0, 0), and its size can
 * only be changed by changing the terminal size itself.
 */
pub struct RootWindow {
    screen: ncurses::WINDOW,
    graphics: Graphics,
    bounds: Rectangle,
    layout: Rc<RefCell<dyn Layout>>
}

impl RootWindow {
    pub(crate) fn new(screen: ncurses::WINDOW, layout: Rc<RefCell<dyn Layout>>) -> RootWindow {
        let (mut width, mut height) = (0, 0);
        ncurses::getmaxyx(screen, &mut width, &mut height);

        let bounds = Rectangle {
            pos: Point { x: 0, y: 0 },
            size: Dimension {
                width: width.try_into().unwrap(),
                height: height.try_into().unwrap()
            }
        };
        // FIXME: Set the size of "graphics" here.

        RootWindow {
            screen,
            graphics: Graphics::new(),
            bounds,
            layout
        }
    }

    pub(crate) fn refresh(&mut self) -> Result<(), ()> {
        self.paint()?;
        /*
        check(
            ncurses::pnoutrefresh(
                self.graphics,
                0, // pmin_row
                0, // pmin_col
        */
        Ok(())
    }
}

impl Component for RootWindow {
    fn graphics(&self) -> &Graphics {
        &self.graphics
    }

    fn paint(&mut self) -> Result<(), ()> {
        //check(ncurses::wnoutrefresh(*self.graphics()))
        Ok(())
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
    }
}

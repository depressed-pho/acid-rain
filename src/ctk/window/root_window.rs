use crate::ctk::{
    Component,
    Graphics,
    Layout,
    util::check
};
use std::cell::RefCell;
use std::rc::Rc;

/** The root window is a special window which covers the entire
 * terminal screen. Its position is fixed to (0, 0), and its size can
 * only be changed by changing the terminal size itself.
 */
pub struct RootWindow {
    graphics: Graphics,
    layout: Rc<RefCell<dyn Layout>>
}

impl RootWindow {
    pub(crate) fn new(layout: Rc<RefCell<dyn Layout>>) -> RootWindow {
        RootWindow {
            graphics: Graphics::new(),
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
}

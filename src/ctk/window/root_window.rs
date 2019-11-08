use crate::ctk::Buffer;
use crate::ctk::component::Component;
use crate::ctk::layout::Layout;
use crate::ctk::util::check;
use std::cell::RefCell;
use std::rc::Rc;

/** The root window is a special window which covers the entire
 * terminal screen. Its position is fixed to (0, 0), and its size can
 * only be changed by changing the terminal size itself.
 */
pub struct RootWindow {
    buffer: Buffer,
    layout: Rc<RefCell<dyn Layout>>
}

impl RootWindow {
    pub(crate) fn new(layout: Rc<RefCell<dyn Layout>>) -> RootWindow {
        RootWindow {
            buffer: Buffer::new(),
            layout
        }
    }

    pub(crate) fn refresh(&mut self) -> Result<(), ()> {
        self.paint()?;
        /*
        check(
            ncurses::pnoutrefresh(
                self.buffer,
                0, // pmin_row
                0, // pmin_col
        */
        Ok(())
    }
}

impl Component for RootWindow {
    fn buffer(&self) -> &Buffer {
        &self.buffer
    }

    fn paint(&mut self) -> Result<(), ()> {
        //check(ncurses::wnoutrefresh(*self.buffer()))
        Ok(())
    }
}

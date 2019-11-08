use crate::ctk::component::Component;
use crate::ctk::layout::Layout;
use std::cell::RefCell;
use std::rc::Rc;

/** The root window is a special window which covers the entire
 * terminal screen. Its position is fixed to (0, 0), and its size can
 * only be changed by changing the terminal size itself.
 */
pub struct RootWindow {
    c_win: pancurses::Window,
    layout: Rc<RefCell<dyn Layout>>
}

impl RootWindow {
    pub(crate) fn new(
        c_win: pancurses::Window,
        layout: Rc<RefCell<dyn Layout>>) -> RootWindow {

        RootWindow {
            c_win,
            layout
        }
    }
}

impl Component for RootWindow {
    fn buffer(&self) -> &pancurses::Window {
        &self.c_win
    }
}

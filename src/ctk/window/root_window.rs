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
    c_win: ncurses::WINDOW,
    layout: Rc<RefCell<dyn Layout>>
}

impl RootWindow {
    pub(crate) fn new(
        c_win: ncurses::WINDOW,
        layout: Rc<RefCell<dyn Layout>>) -> RootWindow {

        RootWindow {
            c_win,
            layout
        }
    }
}

impl Component for RootWindow {
    fn buffer(&self) -> &ncurses::WINDOW {
        &self.c_win
    }

    fn refresh(&self) -> Result<(), ()> {
        check(ncurses::wnoutrefresh(*self.buffer()))
    }
}

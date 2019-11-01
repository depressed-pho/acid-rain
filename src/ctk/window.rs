use crate::ctk::util::check;

pub trait Component {
    /** Borrow the underlying curses window associated to this
     * component. */
    fn buffer(&self) -> &pancurses::Window;

    /** Refresh the content of the off-screen buffer curscr. */
    fn refresh(&self) -> Result<(), ()> {
        check(self.buffer().noutrefresh())
    }
}

/** The root window is a special window which covers the entire
 * terminal screen. Its position is fixed to (0, 0), and its size can
 * only be changed by changing the terminal size itself.
 */
pub struct RootWindow {
    c_win: pancurses::Window
}

impl RootWindow {
    pub(crate) fn new(c_win: pancurses::Window) -> RootWindow {
        RootWindow {
            c_win: c_win
        }
    }
}

impl Component for RootWindow {
    fn buffer(&self) -> &pancurses::Window {
        &self.c_win
    }
}

use crate::ctk::component::Component;

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

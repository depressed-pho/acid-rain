/* The root window is a special window which covers the entire
 * terminal screen. Its position is fixed to (0, 0), and its size can
 * only be changed by changing the terminal size itself.
 */
pub struct RootWindow<'a> {
    c_win: &'a pancurses_result::Window
}

impl<'a> RootWindow<'a> {
    pub(crate) fn new(c_win: &mut pancurses_result::Window) -> RootWindow {
        RootWindow {
            c_win: c_win
        }
    }
}

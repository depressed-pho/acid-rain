mod label;
pub use label::*;

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

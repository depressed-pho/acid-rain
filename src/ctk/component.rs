mod label;
pub use label::*;

use crate::ctk::util::check;

pub trait Component {
    /** Borrow the underlying curses pad associated to this
     * component. It has to be a pad rather than a regular window, if
     * you don't provide your own implementation of refresh().
     */
    fn buffer(&self) -> &ncurses::WINDOW;

    /** Refresh the content of the off-screen buffer curscr. */
    fn refresh(&self) -> Result<(), ()> {
        //check(self.buffer().noutrefresh())
        panic!("FIXME");
    }
}

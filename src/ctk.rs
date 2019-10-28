extern crate pancurses_result;

use pancurses_result::Curses;

pub struct Ctk {
    c: Curses
}

impl Ctk {
    /* Create an instance of Ctk. At most one instance can
     * exist. Violating this would corrupt your terminal.
     */
    pub fn initiate() -> Ctk {
        Ctk {
            c: pancurses_result::initscr().unwrap()
        }
    }
}

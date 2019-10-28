extern crate pancurses_result;

use pancurses_result::{
    Curses,
    CursorVisibility,
    InputBufferingMode
};

pub struct Ctk {
    curses: Curses
}

impl Ctk {
    /* Create an instance of Ctk. At most one instance can
     * exist. Violating this would cause a panic.
     */
    pub(crate) fn initiate() -> Ctk {
        let mut curses = pancurses_result::initscr().unwrap();

        /* We are going to use colors. */
        curses.start_color().unwrap();

        /* Cursor should be hidden by default. It should only be
         * visible when a text input field is active and focused. */
        curses.set_cursor_visibility(CursorVisibility::Invisible).unwrap();

        /* We don't want the TTY driver to echo inputs. */
        curses.set_echo_input(false).unwrap();

        /* We are going to rebind keys like C-s and C-c. */
        curses.set_input_buffering_mode(
            InputBufferingMode::UnbufferedNoSignals).unwrap();

        /* We don't want curses to treat RET specially. */
        curses.set_translate_new_lines(false).unwrap();

        Ctk {
            curses: curses
        }
    }
}

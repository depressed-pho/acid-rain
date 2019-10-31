mod window;

extern crate pancurses_result;

use pancurses_result::{
    Curses,
    CursorVisibility,
    InputBufferingMode
};

use window::RootWindow;

/* I admit I'm a beginner but this really boggles my mind. C'mon
 * Rust. How dare you say this is the simplest way to define a struct
 * which has both an owner and its borrower? Make the damn syntax a
 * part of the language itself!
 */
pub use rentals::Ctk;
rental! {
    mod rentals {
        use super::*;

        #[rental_mut]
        pub struct Ctk {
            curses: Box<Curses>,
            root: RootWindow<'curses>
        }
    }
}

impl Ctk {
    /* Create an instance of Ctk. At most one instance can
     * exist. Violating this would cause a panic.
     */
    pub(crate) fn initiate() -> Ctk {
        let mut curses = Box::new(pancurses_result::initscr().unwrap());

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

        Ctk::new(curses, |c: &mut Curses| {
            RootWindow::new(c.window_mut())
        })
    }
}

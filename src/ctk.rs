mod component;
mod layout;
mod util;
mod window;

extern crate pancurses;

use crate::ctk::component::Component;
use crate::ctk::util::check;
use crate::ctk::window::RootWindow;
use std::sync::Mutex;

lazy_static! {
    static ref INITIALIZED: Mutex<bool> = Mutex::new(false);
}

pub struct Ctk {
    root: RootWindow
}

impl Ctk {
    /** Create an instance of Ctk. At most one instance can
     * exist. Violating this would return an error.
     */
    pub(crate) fn initiate() -> Result<Ctk, ()> {
        {
            let mut initialized = INITIALIZED.lock().unwrap();

            if *initialized {
                return Err(());
            }
            else {
                *initialized = true;
            }
        }

        let mut tk = Ctk {
            root: RootWindow::new(pancurses::initscr())
        };

        /* We are going to use colors. */
        check(pancurses::start_color())?;

        /* Cursor should be hidden by default. It should only be
         * visible when a text input field is active and focused. */
        check(pancurses::curs_set(0))?;

        /* We don't want the TTY driver to echo inputs. */
        check(pancurses::noecho())?;

        /* We are going to rebind keys like C-s and C-c, and also
         * dislike cooked mode. */
        check(pancurses::raw())?;
        check(pancurses::cbreak())?;

        /* We don't want curses to treat RET specially. */
        check(pancurses::nonl())?;

        /* Done the initial configuration. */
        Ok(tk)
    }

    /** endwin(3) does not shutdown curses, which is why this method
     * borrows self instead of taking ownership. */
    pub(crate) fn end(&mut self) -> Result<(), ()> {
        check(pancurses::endwin())
    }

    /** The main loop. */
    pub fn main(&mut self) {
        self.update().unwrap();
    }

    /** Refresh the content of the off-screen buffer curscr, and then
     * update the screen by actually writing data to the terminal. */
    fn update(&mut self) -> Result<(), ()> {
        self.root.refresh()?;
        check(pancurses::doupdate())
    }
}

impl Drop for Ctk {
    fn drop(&mut self) {
        let mut initialized = INITIALIZED.lock().unwrap();

        self.end().unwrap();
        *initialized = false;
    }
}

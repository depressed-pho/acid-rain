extern crate ncurses;

pub mod graphics;
pub use graphics::Graphics;

pub mod component;
pub use component::Component;

pub mod layout;
pub use layout::Layout;

mod util;
use crate::ctk::util::check;

pub mod window;
use crate::ctk::window::RootWindow;

use std::cell::RefCell;
use std::sync::Mutex;
use std::rc::Rc;

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
    pub(crate) fn initiate(layout: Rc<RefCell<dyn Layout>>) -> Result<Ctk, ()> {
        {
            let mut initialized = INITIALIZED.lock().unwrap();

            if *initialized {
                return Err(());
            }
            else {
                *initialized = true;
            }
        }

        ncurses::initscr();

        /* We are going to use colors. */
        check(ncurses::start_color())?;

        /* Cursor should be hidden by default. It should only be
         * visible when a text input field is active and focused. */
        match ncurses::curs_set(ncurses::CURSOR_VISIBILITY::CURSOR_INVISIBLE) {
            Some(_) => Ok(()),
            None    => Err(())
        }?;

        /* We don't want the TTY driver to echo inputs. */
        check(ncurses::noecho())?;

        /* We are going to rebind keys like C-s and C-c, and also
         * dislike cooked mode. */
        check(ncurses::raw())?;
        check(ncurses::cbreak())?;

        /* We don't want ncurses to treat RET specially. */
        check(ncurses::nonl())?;

        /* Done the initial configuration. */
        let tk = Ctk {
            root: RootWindow::new(layout)
        };
        Ok(tk)
    }

    /** endwin(3) does not shutdown ncurses, which is why this method
     * borrows self instead of taking ownership. */
    pub(crate) fn end(&mut self) -> Result<(), ()> {
        check(ncurses::endwin())
    }

    /** The main loop. */
    pub fn main(&mut self) {
        self.update().unwrap();
    }

    /** Refresh the content of the off-screen buffer curscr, and then
     * update the screen by actually writing data to the terminal. */
    fn update(&mut self) -> Result<(), ()> {
        self.root.refresh()?;
        check(ncurses::doupdate())
    }
}

impl Drop for Ctk {
    fn drop(&mut self) {
        let mut initialized = INITIALIZED.lock().unwrap();

        self.end().unwrap();
        *initialized = false;
    }
}

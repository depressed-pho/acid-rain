extern crate libc;
extern crate ncurses;
extern crate unicode_width;
extern crate weak_table;

pub mod alignment;
pub use alignment::{
    HorizontalAlignment,
    VerticalAlignment
};

pub mod border;
pub use border::*;

pub mod component;
pub use component::*;

pub mod graphics;
pub use graphics::*;

pub mod dimension;
use crate::ctk::dimension::Point;

pub mod layout;
pub use layout::*;

pub mod symbol;
pub use symbol::*;

mod util;
use crate::ctk::util::{check, check_null};

pub mod window;
use crate::ctk::window::RootWindow;

use num::Zero;
use std::cell::RefCell;
use std::ffi::{CString, CStr};
use std::panic;
use std::sync::Mutex;
use std::rc::Rc;

lazy_static! {
    static ref INITIALIZED: Mutex<bool> = Mutex::new(false);

    static ref IS_UTF8_LOCALE: bool = {
        if cfg!(feature = "unicode") {
            let codeset = unsafe {
                CStr::from_ptr(
                    libc::nl_langinfo(libc::CODESET)).to_string_lossy()
            };
            codeset == "UTF-8"
        }
        else {
            false
        }
    };
}

pub fn is_utf8_locale() -> bool {
    *IS_UTF8_LOCALE
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

            /* And we must call setlocale() before initializing
             * libcursesw.
             */
            if cfg!(feature = "unicode") {
                /* THINKME: ncurses::setlocale() seems to be
                 * malfunctioning but I have no idea why. */
                let empty = CString::new("").unwrap();
                unsafe {
                    libc::setlocale(libc::LC_ALL, empty.as_ptr());
                }
            }
        }

        let stdscr = check_null(ncurses::initscr()).unwrap();

        /* We are going to use colors. */
        check(ncurses::start_color())?;

        /* Cursor should be hidden by default. It should only be
         * visible when a text input field is active and focused. */
        match ncurses::curs_set(ncurses::CURSOR_VISIBILITY::CURSOR_INVISIBLE) {
            Some(_) => Ok(()),
            None    => Err(())
        }?;

        /* And since the cursor is hidden by default, we can stop
         * worrying about moving the cursor around. */
        check(ncurses::leaveok(stdscr, true))?;

        /* We do scrolling in our own way. */
        check(ncurses::scrollok(stdscr, false))?;

        /* We don't want the TTY driver to echo inputs. */
        check(ncurses::noecho())?;

        /* We are going to rebind keys like C-s and C-c, and also
         * dislike cooked mode. */
        check(ncurses::raw())?;
        check(ncurses::cbreak())?;

        /* We don't want ncurses to treat RET specially. */
        check(ncurses::nonl())?;

        /* Refresh the stdscr before drawing anything. Since we don't
         * actually draw anything directly on it, it will forever be
         * blank and the first call of getch() will clear the terminal
         * entirely.
         */
        check(ncurses::wrefresh(stdscr))?;

        /* Done the initial configuration. */
        let tk = Ctk {
            root: RootWindow::new(stdscr, layout)
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
        self.root.validate();
        self.update_graphics().unwrap();

        // FIXME
        ncurses::getch();
    }

    /** Refresh the content of the off-screen buffer curscr, and then
     * update the screen by actually writing data to the terminal. */
    fn update_graphics(&mut self) -> Result<(), ()> {
        self.root.paint();
        self.root.refresh(&self.root, Point::zero());
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

pub fn default_panic_hook(_: &panic::PanicInfo) {
    let initialized = INITIALIZED.lock().unwrap();

    if *initialized {
        check(ncurses::endwin()).unwrap();
    }
}

pub fn install_default_panic_hook() {
    let old = panic::take_hook();
    panic::set_hook(Box::new(move |pi| {
        default_panic_hook(pi);
        old(pi);
    }));
}

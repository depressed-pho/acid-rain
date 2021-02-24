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
use crate::dimension::Point;

pub mod layout;
pub use layout::*;

pub mod symbol;
pub use symbol::*;

mod util;
use crate::util::{check, check_null};

pub mod window;
pub use crate::window::RootWindow;

use lazy_static::lazy_static;
use num::Zero;
use std::cell::RefCell;
use std::ffi::{CString, CStr};
use std::panic;
use std::sync::Mutex;
use tokio::select;
use tokio::task;
use tokio::io::unix::AsyncFd;

lazy_static! {
    static ref INITIALIZED: Mutex<bool> =
        Mutex::new(false);

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
    pub fn initiate(layout: RefCell<Box<dyn Layout>>) -> Result<Ctk, ()> {
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
                 * malfunctioning but I have no idea why. Its code
                 * looks just good but doesn't actually have any
                 * effect.
                 */
                // ncurses::setlocale(ncurses::LcCategory::all, "");
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

        /* We don't want ncurses::getch() to block. */
        check(ncurses::nodelay(stdscr, true))?;

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

    /** Ctk provides no main loop. Users are expected to call step()
     * in their own loop which asynchronously updates the terminal
     * and handles input events.
     */
    pub async fn step(&mut self) {
        self.root.validate();
        self.root.paint();
        self.root.refresh(&self.root, Point::zero());

        // ncurses::doupdate() may block if stdout is blocked.
        task::spawn_blocking(|| {
            check(ncurses::doupdate()).unwrap()
        }).await.unwrap();

        /* ncurses::getch() by default performs a blocking read on
         * stdin. In order to integrate it with our asynchronous
         * world, we poll stdin until they get any inputs, then call
         * getch() in a non-blocking mode.
         *
         * Note that currently we can't poll SIGWINCH because
         * tokio::signal can't do it without replacing the signal
         * handler installed by ncurses, and ncurses doesn't have a
         * public API to manually invoke the handler.
         *
         * THINKME: Maybe we should copy the logic of
         * _nc_get_screensize() here? Or perhaps save the old handler
         * and invoke it afterwards?
         */
        let stdin = AsyncFd::new(std::io::stdin()).unwrap();

        select! {
            stdin_ready = stdin.readable() => {
                self.read_keys().await;
                stdin_ready.unwrap().clear_ready();
            }
        }
    }

    async fn read_keys(&mut self) {
        while let Some(_) = ncurses::get_wch() {
            // FIXME
        }
    }
}

impl Drop for Ctk {
    fn drop(&mut self) {
        self.end().unwrap();

        let mut initialized = INITIALIZED.lock().unwrap();
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

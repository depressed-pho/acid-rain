mod alignment;
pub use alignment::*;

pub mod attribute;

pub mod border;
pub use border::*;

pub mod color;
use color::manager::ColorManager;

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
use std::ffi::CStr;
use std::panic;
use std::rc::Rc;
use std::sync::Mutex;
use std::thread_local;
use tokio::select;
use tokio::task;
use tokio::io::unix::AsyncFd;
use tokio::signal::unix::{signal, SignalKind};

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

thread_local! {
    /// We don't want to pass this to every Graphics we'll create, so
    /// we put this in TLS.
    pub(crate) static COLOR_MANAGER: RefCell<Option<ColorManager>> =
        RefCell::new(None);
}

/// Return true iff the current locale uses UTF-8.
pub fn is_utf8_locale() -> bool {
    *IS_UTF8_LOCALE
}

pub struct Ctk {
    root: RootWindow
}

impl Ctk {
    /// Create an instance of Ctk. At most one instance can
    /// exist. Violating this would return an error.
    pub fn initiate(layout: Rc<RefCell<dyn Layout>>) -> Result<Ctk, ()> {
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
                ncurses::setlocale(ncurses::LcCategory::all, "");
            }
        }

        /* This is the beginning of curses. */
        let stdscr = check_null(ncurses::initscr()).unwrap();

        /* Construct the color manager now, even if the terminal
         * doesn't support colors. */
        {
            let color_manager = ColorManager::new()?;
            COLOR_MANAGER.with(|cm| {
                *cm.borrow_mut() = Some(color_manager);
            });
        }

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

    /// endwin(3) does not shutdown ncurses, which is why this method
    /// borrows self instead of taking ownership. */
    pub(crate) fn end(&mut self) -> Result<(), ()> {
        check(ncurses::endwin())
    }

    /// Ctk provides no main loop. Users are expected to call step()
    /// in their own loop which asynchronously updates the terminal
    /// and handles input events.
    pub async fn step(&mut self) {
        self.update_graphics().await;

        /* ncurses::getch() by default performs a blocking read on
         * stdin. In order to integrate it with our asynchronous
         * world, we poll stdin and SIGWINCH until they get any
         * inputs, then call getch() in a non-blocking mode.
         *
         * Note that tokio::signal replaces the signal handler
         * installed by ncurses. This is okay because its backend
         * signal-hook-registry retains the old handler ahd invokes it
         * for us when a signal arrives.
         */
        let stdin        = AsyncFd::new(std::io::stdin()).unwrap();
        let mut sigwinch = signal(SignalKind::window_change()).unwrap();

        select! {
            stdin_ready = stdin.readable() => {
                self.read_keys().await;
                stdin_ready.unwrap().clear_ready();
            }
            sigwinch_ready = sigwinch.recv() => {
                if sigwinch_ready.is_some() {
                    self.read_keys().await;
                }
            }
        }
    }

    async fn update_graphics(&mut self) {
        self.root.validate();
        self.root.paint();
        self.root.refresh(&self.root, Point::zero());

        // ncurses::doupdate() may block if stdout is blocked.
        task::spawn_blocking(|| {
            check(ncurses::doupdate()).unwrap()
        }).await.unwrap();
    }

    async fn read_keys(&mut self) {
        while let Some(wch) = ncurses::get_wch() {
            match wch {
                ncurses::WchResult::KeyCode(ncurses::KEY_RESIZE) => {
                    // Recursively resize all the windows by resizing
                    // the root window.
                    self.root.resize();
                    self.update_graphics().await;
                }
                _ => {
                    // FIXME
                }
            }
        }
    }
}

impl Drop for Ctk {
    fn drop(&mut self) {
        self.end().unwrap();

        COLOR_MANAGER.with(|cm| {
            *cm.borrow_mut() = None;
        });

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

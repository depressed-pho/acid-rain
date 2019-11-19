use crate::ctk::util::{check, check_null};
use crate::ctk::dimension::{
    Dimension,
    Point
};
use std::convert::TryInto;
use std::cmp::{min, max};

pub struct Graphics {
    /** The pad is initially non-existent. It is created when the area
     * of the graphics context becomes non-zero.
     */
    pad: Option<ncurses::WINDOW>,
    size: Dimension
}

impl Graphics {
    pub fn new() -> Graphics {
        Graphics {
            pad: None,
            size: Dimension {
                width: 0,
                height: 0
            }
        }
    }

    /** Resize the graphics context. Returns true iff the size has
     * changed from the previous state.
     */
    pub fn set_size(&mut self, new: Dimension) -> bool {
        let old = self.size;
        self.size = new;

        if new.is_zero() {
            self.drop_pad();
        }
        else {
            match self.pad {
                Some(w) => {
                    check(ncurses::wresize(w, new.height, new.width)).unwrap()
                },
                None => {
                    self.pad = Some(
                        check_null(
                            ncurses::newpad(new.height, new.width)).unwrap());
                }
            }
        }

        old == new
    }

    fn drop_pad(&mut self) {
        match self.pad {
            Some(w) => {
                check(ncurses::delwin(w)).unwrap();
                self.pad = None;
            },
            None => ()
        }
    }

    /** Copy the content of the graphics context to the curses
     * screen.
     */
    pub fn refresh(&self, pos: Point, scr: Dimension) {
        match self.pad {
            Some(w) => {
                let pminrow = max(-pos.y, 0);
                let pmincol = max(-pos.x, 0);
                let sminrow = min(0, pos.y);
                let smincol = min(0, pos.x);
                let smaxrow = min(pos.y + self.size.height, scr.height);
                let smaxcol = min(pos.x + self.size.width, scr.width);
                check(
                    ncurses::pnoutrefresh(
                        w, pminrow, pmincol, sminrow, smincol, smaxrow, smaxcol)).unwrap();
            },
            None => {}
        }
    }
}

impl Drop for Graphics {
    fn drop(&mut self) {
        self.drop_pad();
    }
}

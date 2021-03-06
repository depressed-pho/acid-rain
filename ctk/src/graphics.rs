use crate::{
    COLOR_MANAGER,
    Component,
    RootWindow,
    Symbol
};
use crate::attribute::AttrSet;
use crate::color::{BoxedColor, Color, DefaultColor};
use crate::dimension::{
    Dimension,
    Point,
    Rectangle
};
use crate::util::{check, check_null};
use num::Zero;
use std::convert::TryInto;
use std::cmp::{min, max};
use unicode_width::UnicodeWidthChar;

#[derive(Debug)]
pub struct Graphics {
    /// The pad is initially non-existent. It is created when the area
    /// of the graphics context becomes non-zero.
    pad: Option<ncurses::WINDOW>,
    size: Dimension,
    fg_color: BoxedColor,
    bg_color: BoxedColor
}

/// Mark Graphics as Send. NCurses doesn't rely on thread-local
/// storages or anything so WINDOW pointers can be safely sent between
/// threads, although that doesn't make sense in general.
unsafe impl Send for Graphics {}

/// Mark Graphics as Sync as well. While ncurses::WINDOW itself does
/// not prevent data races at all, we make sure that we only touch it
/// in the Ctk context.
unsafe impl Sync for Graphics {}

impl Graphics {
    pub fn new() -> Graphics {
        Graphics {
            pad: None,
            size: Dimension {
                width: 0,
                height: 0
            },
            fg_color: BoxedColor::new(DefaultColor()),
            bg_color: BoxedColor::new(DefaultColor())
        }
    }

    pub fn get_size(&self) -> Dimension {
        self.size
    }

    /// Resize the graphics context. Returns true iff the size has
    /// changed from the previous state.
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
                    let w = check_null(
                        ncurses::newpad(new.height, new.width)).unwrap();

                    /* NOTE: I don't know why, but newpad() in
                     * ncursesw-6.1 seems to set 0x00 for the initial
                     * background.
                     */
                    ncurses::wbkgdset(w, 0x20);

                    self.pad = Some(w);
                }
            }
        }

        old != new
    }

    fn drop_pad(&mut self) {
        if let Some(w) = self.pad {
            check(ncurses::delwin(w)).unwrap();
            self.pad = None;
        }
    }

    /// Copy the content of the graphics context to the curses
    /// screen.
    pub unsafe fn refresh(&self, root: &RootWindow, pos: Point) {
        if let Some(w) = self.pad {
            let scr     = root.get_size();
            let pminrow = max(-pos.y, 0);
            let pmincol = max(-pos.x, 0);
            let sminrow = max(0, pos.y);
            let smincol = max(0, pos.x);
            let smaxrow = min(pos.y + self.size.height, scr.height);
            let smaxcol = min(pos.x + self.size.width, scr.width);
            check(
                ncurses::pnoutrefresh(
                    w, pminrow, pmincol, sminrow, smincol, smaxrow, smaxcol)).unwrap();
        }
    }

    pub fn clear_rect(&mut self, r: Rectangle) {
        if let Some(w) = self.pad {
            let bg = ncurses::getbkgd(w);
            for y in r.pos.y .. r.pos.y + r.size.height {
                check(
                    ncurses::mvwhline(
                        w, y, r.pos.x, bg, r.size.width)).unwrap();
            }
        }
    }

    pub fn draw_char(&mut self, c: char, p: Point) {
        let mut buf = [0; 4];
        self.draw_string(c.encode_utf8(&mut buf), p)
    }

    pub fn draw_string(&mut self, s: &str, p: Point) {
        if let Some(w) = self.pad {
            // FIXME: s might contain newlines or any other control
            // characters. What to do about them?
            assert!(p.x >= 0 && p.x < self.size.width);
            assert!(p.y >= 0 && p.y < self.size.height);
            assert!(s.find('\n') == None);

            if p.y == self.size.height-1 {
                /* How many code points can we draw without reaching
                 * the lower-right corner? The last part of the string
                 * possibly needs to be drawn with mvwinsnstr().
                 */
                let mut cursor  = p.x;
                let mut n_bytes = 0;
                for ch in s.chars() {
                    let width: i32 = ch.width().unwrap_or(1)
                        .try_into().unwrap();
                    if cursor + width >= self.size.width {
                        break;
                    }
                    else {
                        cursor  += width;
                        n_bytes += ch.len_utf8();
                    }
                }

                let (add, ins) = s.split_at(n_bytes);
                check(
                    ncurses::mvwaddnstr(
                        w, p.y, p.x, add, add.len().try_into().unwrap())).unwrap();
                check(
                    ncurses::mvwinsnstr(
                        w, p.y, cursor, ins, ins.len().try_into().unwrap())).unwrap();
            }
            else {
                check(
                    ncurses::mvwaddnstr(
                        w, p.y, p.x, s, s.len().try_into().unwrap())).unwrap();
            }
        }
    }

    pub fn draw_symbol(&mut self, s: Symbol, p: Point) {
        if let Some(w) = self.pad {
            /* We don't use mvwaddch() because it always tries to
             * advance the cursor, which is impossible if it's already
             * at the lower-right corner of the window. The function
             * returns ERR in that case.
             */
            check(ncurses::mvwaddchnstr(w, p.y, p.x, &[s.into()], 1)).unwrap();
            /* FIXME: Unicode-capable terminals tend to hate ACS
             * symbols (for a quite understandable reason). Use
             * Unicode symbols when crate::is_utf8_locale() returns
             * true. Or convince ncurses to translate them for us.
             */
        }
    }

    pub fn attr_on(&mut self, attrs: AttrSet) {
        if let Some(w) = self.pad {
            check(ncurses::wattr_on(w, attrs.into())).unwrap();
        }
    }

    pub fn attr_off(&mut self, attrs: AttrSet) {
        if let Some(w) = self.pad {
            check(ncurses::wattr_off(w, attrs.into())).unwrap();
        }
    }

    pub fn set_fg(&mut self, fg_color: impl Color) {
        self.set_colors(fg_color, self.bg_color.clone());
    }

    pub fn set_bg(&mut self, bg_color: impl Color) {
        self.set_colors(self.fg_color.clone(), bg_color);
    }

    pub fn set_colors(&mut self, fg_color: impl Color, bg_color: impl Color) {
        if let Some(w) = self.pad {
            COLOR_MANAGER.with(|cm| {
                (*cm.borrow_mut())
                    .as_mut()
                    .expect("Cannot call this method outside of the Ctk context.")
                    .set_colors(w, &fg_color, &bg_color);
            });
        }
        self.fg_color = BoxedColor::new(fg_color);
        self.bg_color = BoxedColor::new(bg_color);
    }
}

impl Drop for Graphics {
    fn drop(&mut self) {
        self.drop_pad();
    }
}

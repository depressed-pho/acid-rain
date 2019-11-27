use crate::ctk::{
    Graphics,
    HorizontalAlignment::{self, self as HA},
    VerticalAlignment::{self, self as VA}
};
use crate::ctk::dimension::{
    Dimension,
    Point,
    Rectangle
};
use std::convert::TryInto;
use std::io;
use unicode_width::UnicodeWidthStr;

/** Check if a result code from pancurses functions indicates a
 * success or a failure.
 */
pub(crate) fn check(r: i32) -> Result<(), ()> {
    if r == ncurses::ERR {
        Err(())
    }
    else {
        Ok(())
    }
}

pub(crate) fn check_null<T>(p: *mut T) -> Result<*mut T, io::Error> {
    if p.is_null() {
        Err(io::Error::last_os_error())
    }
    else {
        Ok(p)
    }
}

pub fn smallest_area_to_draw_text(text: &str) -> Dimension {
    // FIXME: Multi-line labels
    Dimension {
        width: text.width().try_into().unwrap(),
        height: 1
    }
}

pub fn draw_aligned_text(
    g: &mut Graphics, area: Rectangle, ltr: bool,
    horiz: HorizontalAlignment, vert: VerticalAlignment, text: &str) {

    // FIXME: Consider cases where word wrapping is needed, or text
    // flows over the designated area.

    let pos = Point {
        x: match horiz {
            HA::Left            => h_align_left(text, area),
            HA::Center          => h_align_center(text, area),
            HA::Right           => h_align_right(text, area),
            HA::Leading  if ltr => h_align_left(text, area),
            HA::Leading         => h_align_right(text, area),
            HA::Trailing if ltr => h_align_right(text, area),
            HA::Trailing        => h_align_left(text, area)
        },
        y: match vert {
            VA::Top    => v_align_top(text, area),
            VA::Center => v_align_center(text, area),
            VA::Bottom => v_align_bottom(text, area),
        }
    };

    g.draw_string(text, pos);
}

fn h_align_left(_t: &str, r: Rectangle) -> i32 {
    r.pos.x
}

fn h_align_center(t: &str, r: Rectangle) -> i32 {
    let w: i32 = t.width().try_into().unwrap();
    r.pos.x + (r.size.width - w) / 2
}

fn h_align_right(t: &str, r: Rectangle) -> i32 {
    let w: i32 = t.width().try_into().unwrap();
    r.pos.x + r.size.width - w
}

fn v_align_top(_t: &str, r: Rectangle) -> i32 {
    r.pos.y
}

fn v_align_center(_t: &str, r: Rectangle) -> i32 {
    r.pos.y + (r.size.height - 1) / 2
}

fn v_align_bottom(_t: &str, r: Rectangle) -> i32 {
    r.pos.y + r.size.height - 1
}

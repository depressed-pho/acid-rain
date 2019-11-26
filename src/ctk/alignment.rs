extern crate unicode_width;

use crate::ctk::{
    Graphics
};
use crate::ctk::dimension::{
    Point,
    Rectangle
};
use std::convert::TryInto;
use unicode_width::UnicodeWidthStr;

/* There are no sensible defaults so it doesn't implement Default. */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub enum HorizontalAlignment {
    Left,
    Center,
    Right,
    Leading,
    Trailing
}

/* There are no sensible defaults so it doesn't implement Default. */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub enum VerticalAlignment {
    Top,
    Center,
    Bottom
}

use HorizontalAlignment as HA;
use VerticalAlignment as VA;

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

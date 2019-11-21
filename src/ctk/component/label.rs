extern crate unicode_width;
use unicode_width::UnicodeWidthStr;

use crate::ctk::{
    Component,
    Graphics,
    HorizontalAlignment::{self, self as HA},
    RootWindow,
    VerticalAlignment::{self, self as VA}
};
use crate::ctk::dimension::{
    Point,
    Rectangle
};
use std::convert::TryInto;

pub struct Label {
    graphics: Graphics,
    bounds: Rectangle,
    dirty: bool,
    text: String,
    h_align: HorizontalAlignment,
    v_align: VerticalAlignment
}

impl Label {
    pub fn new<T: Into<String>>(text: T) -> Label {
        Label {
            graphics: Graphics::new(),
            bounds: Rectangle::default(),
            dirty: true,
            text: text.into(),
            h_align: HA::Leading,
            v_align: VA::Center
        }
    }

    pub fn set_horizontal_alignment(&mut self, h_align: HorizontalAlignment) {
        self.h_align = h_align;
        self.dirty   = true;
    }

    pub fn set_vertical_alignment(&mut self, v_align: VerticalAlignment) {
        self.v_align = v_align;
        self.dirty   = true;
    }
}

impl Component for Label {
    fn graphics_mut(&mut self) -> &mut Graphics {
        &mut self.graphics
    }

    fn paint(&mut self) {
        if self.dirty {
            // FIXME: Consider cases where word wrapping is needed, or
            // labels are longer than the inner width and overwrite
            // the border.

            let insets = self.get_insets();
            let inner  = Rectangle {
                pos: Point::zero(),
                size: self.get_size()
            }.shrink(insets);

            let ltr = true; // THINKME: hard-coded for now
            let pos = Point {
                x: match self.h_align {
                    HA::Left            => h_align_left(&self.text, inner),
                    HA::Center          => h_align_center(&self.text, inner),
                    HA::Right           => h_align_right(&self.text, inner),
                    HA::Leading  if ltr => h_align_left(&self.text, inner),
                    HA::Leading         => h_align_right(&self.text, inner),
                    HA::Trailing if ltr => h_align_right(&self.text, inner),
                    HA::Trailing        => h_align_left(&self.text, inner)
                },
                y: match self.v_align {
                    VA::Top    => v_align_top(&self.text, inner),
                    VA::Center => v_align_center(&self.text, inner),
                    VA::Bottom => v_align_bottom(&self.text, inner),
                }
            };

            self.graphics.clear_rect(inner);
            self.graphics.draw_string(&self.text, pos);
        }
        self.dirty = false;
    }

    fn refresh(&self, root: &RootWindow) {
        self.graphics.refresh(self.get_location(), root);
    }

    fn get_bounds(&self) -> Rectangle {
        self.bounds
    }

    fn set_bounds(&mut self, b: Rectangle) {
        self.bounds = b;
        if self.graphics.set_size(b.size) {
            self.dirty = true;
        }
    }
}

fn h_align_left(t: &str, r: Rectangle) -> i32 {
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

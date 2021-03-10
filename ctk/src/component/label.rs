use crate::{
    Border,
    Component,
    ComponentRef,
    Graphics,
    HorizontalAlignment::{self, self as HA},
    RootWindow,
    VerticalAlignment::{self, self as VA},
    WeakComponentRef
};
use crate::border::NullBorder;
use crate::dimension::{
    Point,
    Rectangle,
    SizeRequirements
};
use crate::util::{
    smallest_area_to_draw_text,
    draw_aligned_text
};

#[derive(Debug)]
pub struct Label {
    parent: Option<WeakComponentRef<dyn Component>>,
    graphics: Graphics,
    bounds: Rectangle,
    size_req: SizeRequirements,
    border: Box<dyn Border>,
    dirty: bool,
    text: String,
    h_align: HorizontalAlignment,
    v_align: VerticalAlignment
}

impl Label {
    pub fn new<T: Into<String>>(text: T) -> Label {
        let text_    = text.into();
        let size_req = SizeRequirements::exactly(
            smallest_area_to_draw_text(&text_));
        Label {
            parent: None,
            graphics: Graphics::new(),
            bounds: Rectangle::default(),
            size_req,
            border: Box::new(NullBorder {}),
            dirty: true,
            text: text_,
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
    fn get_parent(&self) -> Option<ComponentRef<dyn Component>> {
        self.parent.as_ref().map(|p| p.upgrade().unwrap())
    }

    fn set_parent(&mut self, p: Option<ComponentRef<dyn Component>>) {
        self.parent = p.map(|p| p.downgrade());
    }

    fn paint(&mut self) {
        if self.dirty {
            self.border.paint(&mut self.graphics);

            let inner = self.get_inner();
            self.graphics.clear_rect(inner);

            let ltr = true; // THINKME: hard-coded for now
            draw_aligned_text(
                &mut self.graphics, inner, ltr,
                self.h_align, self.v_align, &self.text);
        }
        self.dirty = false;
    }

    fn refresh(&self, root: &RootWindow, offset: Point) {
        self.graphics.refresh(root, self.get_location() + offset);
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

    fn get_size_requirements(&self) -> SizeRequirements {
        self.size_req + self.get_insets()
    }

    fn get_border(&self) -> &Box<dyn Border> {
        &self.border
    }

    fn set_border(&mut self, b: Box<dyn Border>) {
        self.border = b;
    }
}

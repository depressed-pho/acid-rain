use crate::ctk::{
    Component,
    Layout
};
use std::cell::RefCell;
use std::rc::Rc;

pub struct GridLayout {
    rows: usize,
    cols: usize,
    hgap: usize,
    vgap: usize,
    components: Vec<Rc<RefCell<dyn Component>>>,
    is_valid: bool
}

impl GridLayout {
    /** Construct a GridLayout object with all parameters set to
     * zero. At least one of rows and cols has to be set to non-zero
     * before using. hgap and vgap are both optional.
     */
    pub fn new() -> GridLayout {
        GridLayout {
            rows: 0,
            cols: 0,
            hgap: 0,
            vgap: 0,
            components: Vec::new(),
            is_valid: true
        }
    }

    pub fn add(&mut self, c: Rc<RefCell<dyn Component>>) -> &mut Self {
        self.components.push(c);
        self.invalidate();
        self
    }

    pub fn set_rows(&mut self, rows: usize) -> &mut Self {
        self.rows = rows;
        self.invalidate();
        self
    }

    pub fn set_cols(&mut self, cols: usize) -> &mut Self {
        self.cols = cols;
        self.invalidate();
        self
    }

    pub fn set_hgap(&mut self, hgap: usize) -> &mut Self {
        self.hgap = hgap;
        self.invalidate();
        self
    }

    pub fn set_vgap(&mut self, vgap: usize) -> &mut Self {
        self.vgap = vgap;
        self.invalidate();
        self
    }
}

impl Layout for GridLayout {
    fn validate(&mut self, c: &dyn Component) {
        unimplemented!();
    }

    fn invalidate(&mut self) {
        self.is_valid = false;
    }
}

use crate::ctk::{
    Component,
    Layout
};
use crate::ctk::dimension::{
    Dimension,
    Point,
    Rectangle,
    SizeRequirements
};
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::Rc;

pub struct GridLayout {
    rows: i32,
    cols: i32,
    hgap: i32,
    vgap: i32,
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

    pub fn set_rows(&mut self, rows: i32) -> &mut Self {
        self.rows = rows;
        self.invalidate();
        self
    }

    pub fn set_cols(&mut self, cols: i32) -> &mut Self {
        self.cols = cols;
        self.invalidate();
        self
    }

    pub fn set_hgap(&mut self, hgap: i32) -> &mut Self {
        self.hgap = hgap;
        self.invalidate();
        self
    }

    pub fn set_vgap(&mut self, vgap: i32) -> &mut Self {
        self.vgap = vgap;
        self.invalidate();
        self
    }

    fn do_layout(&mut self, parent: &dyn Component) {
        assert!(
            self.rows > 0 || self.cols > 0,
            "Either rows or cols must be set to non-zero");

        if self.rows > 0 && self.cols > 0 {
            assert!(
                self.components.len() <= (self.rows * self.cols).try_into().unwrap(),
                "Too many sub-components; at most {} can be added",
                self.rows * self.cols);
        }

        let n_comps: i32 = self.components.len().try_into().unwrap();
        if n_comps == 0 {
            return;
        }

        let n_cells = if self.rows > 0 {
            Dimension {
                width: (n_comps + self.rows - 1) / self.rows,
                height: self.rows
            }
        }
        else {
            Dimension {
                width: self.cols,
                height: (n_comps + self.cols - 1) / self.cols
            }
        };

        let gap         = Dimension { width: self.hgap, height: self.vgap };
        let total_gaps  = (n_cells - 1) * gap;
        let parent_size = parent.get_size();
        let insets      = parent.get_insets();
        let inner_space = Dimension {
            width: parent_size.width  - (insets.left + insets.right),
            height: parent_size.height - (insets.top  + insets.bottom)
        };
        let comp_size   = (inner_space - total_gaps) / n_cells;
        let extra_space = (inner_space - (comp_size * n_cells + total_gaps)) / 2;

        let ltr = true; // THINKME: Currently hard-coded.
        if ltr {
            for c in 0 .. n_cells.width {
                let x = insets.left + extra_space.width
                      + (comp_size.width + self.hgap) * c;

                for r in 0 .. n_cells.height {
                    let y = insets.top + extra_space.height
                          + (comp_size.height + self.vgap) * r;

                    let i: usize = (r * n_cells.width + c).try_into().unwrap();
                    if i < n_comps.try_into().unwrap() {
                        self.components[i].borrow_mut().set_bounds(
                            Rectangle {
                                pos: Point { x, y },
                                size: comp_size
                            });
                    }
                }
            }
        }
        else {
            for c in 0 .. n_cells.width {
                let x = parent_size.width - insets.right - comp_size.width - extra_space.width
                      - (comp_size.width + self.hgap) * c;

                for r in 0 .. n_cells.height {
                    let y = insets.top + extra_space.height
                          + (comp_size.height + self.vgap) * r;

                    let i: usize = (r * n_cells.width + c).try_into().unwrap();
                    if i < n_comps.try_into().unwrap() {
                        self.components[i].borrow_mut().set_bounds(
                            Rectangle {
                                pos: Point { x, y },
                                size: comp_size
                            });
                    }
                }
            }
        }
    }
}

impl Layout for GridLayout {
    fn validate(&mut self, parent: &dyn Component) {
        if !self.is_valid {
            self.do_layout(parent);
            self.is_valid = true;
        }
        for child in self.components.iter() {
            child.borrow_mut().validate();
        }
    }

    fn invalidate(&mut self) {
        self.is_valid = false;
    }

    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Rc<RefCell<dyn Component>>> + 'a> {
        Box::new(self.components.iter())
    }

    fn get_size_requirements(&self) -> SizeRequirements {
        self.components.iter().fold(
            SizeRequirements::any(),
            |acc, child| {
                acc & child.borrow().get_size_requirements()
            })
    }
}

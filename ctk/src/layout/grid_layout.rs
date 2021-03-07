use crate::{
    Component,
    Layout
};
use crate::dimension::{
    Dimension,
    Point,
    Rectangle,
    SizeRequirements
};
use num_traits::Zero;
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::Rc;

#[derive(Debug)]
pub struct GridLayout {
    cells: Dimension,
    gap: Dimension,
    components: Vec<Rc<RefCell<dyn Component>>>,
    is_valid: bool
}

impl GridLayout {
    /// Construct a GridLayout object with all parameters set to
    /// zero. At least one of rows and cols has to be set to non-zero
    /// before using. hgap and vgap are both optional.
    pub fn new() -> GridLayout {
        GridLayout {
            cells: Dimension::zero(),
            gap: Dimension::zero(),
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
        self.cells.height = rows;
        self.invalidate();
        self
    }

    pub fn set_cols(&mut self, cols: i32) -> &mut Self {
        self.cells.width = cols;
        self.invalidate();
        self
    }

    pub fn set_hgap(&mut self, hgap: i32) -> &mut Self {
        self.gap.width = hgap;
        self.invalidate();
        self
    }

    pub fn set_vgap(&mut self, vgap: i32) -> &mut Self {
        self.gap.height = vgap;
        self.invalidate();
        self
    }

    fn num_cells(&self) -> Dimension {
        assert!(
            self.cells.width > 0 || self.cells.height > 0,
            "Either rows or cols must be set to non-zero");

        if self.cells.width > 0 && self.cells.height > 0 {
            assert!(
                self.components.len() <= self.cells.area().try_into().unwrap(),
                "Too many sub-components; at most {} can be added",
                self.cells.area());
        }

        let n_comps: i32 = self.components.len().try_into().unwrap();
        if self.cells.height > 0 {
            let rows = self.cells.height;
            Dimension {
                width: (n_comps + rows - 1) / rows,
                height: rows
            }
        }
        else {
            let cols = self.cells.width;
            Dimension {
                width: cols,
                height: (n_comps + cols - 1) / cols
            }
        }
    }

    fn do_layout(&mut self, parent: &dyn Component) {
        let n_comps: i32 = self.components.len().try_into().unwrap();
        if n_comps == 0 {
            return;
        }

        let n_cells     = self.num_cells();
        let total_gaps  = (n_cells - 1) * self.gap;
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
                      + (comp_size.width + self.gap.width) * c;

                for r in 0 .. n_cells.height {
                    let y = insets.top + extra_space.height
                          + (comp_size.height + self.gap.height) * r;

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
                      - (comp_size.width + self.gap.width) * c;

                for r in 0 .. n_cells.height {
                    let y = insets.top + extra_space.height
                          + (comp_size.height + self.gap.height) * r;

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

    /// Determine the minimum, maximum, and the preferred size of the
    /// container.
    ///
    /// The minimum width of a grid layout is the largest minimum width
    /// of all of the components in the container times the number of
    /// columns, plus the left and right insets of the parent
    /// component. The remaining parts of the requirements are computed
    /// all similarly.
    fn get_size_requirements(&self, parent: &dyn Component) -> SizeRequirements {
        let n_cells   = self.num_cells();
        let insets    = parent.get_insets();
        let cell_reqs = self.components.iter().fold(
            SizeRequirements::any(),
            |acc, child| {
                acc & child.borrow().get_size_requirements()
            });

        cell_reqs * n_cells + self.gap * (n_cells - 1) + insets
    }
}

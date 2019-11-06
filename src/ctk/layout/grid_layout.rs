pub struct GridLayout {
    rows: usize,
    cols: usize,
    hgap: usize,
    vgap: usize
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
            vgap: 0
        }
    }

    pub fn set_rows(&mut self, rows: usize) -> &mut Self {
        self.rows = rows;
        self
    }

    pub fn set_cols(&mut self, cols: usize) -> &mut Self {
        self.cols = cols;
        self
    }

    pub fn set_hgap(&mut self, hgap: usize) -> &mut Self {
        self.hgap = hgap;
        self
    }

    pub fn set_vgap(&mut self, vgap: usize) -> &mut Self {
        self.vgap = vgap;
        self
    }
}

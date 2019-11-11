pub struct Graphics {
    /** The pad is initially non-existent. It is created on the first
     * call of set_size().
     */
    pad: Option<ncurses::WINDOW>
}

impl Graphics {
    pub fn new() -> Graphics {
        Graphics {
            pad: None
        }
    }
}

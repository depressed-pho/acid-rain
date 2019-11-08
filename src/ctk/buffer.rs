pub struct Buffer {
    /** The pad is initially non-existent. It is created on the first
     * call of set_size().
     */
    pad: Option<ncurses::WINDOW>
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            pad: None
        }
    }
}

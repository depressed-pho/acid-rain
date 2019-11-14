/** An Insets object is a representation of the borders of a
 * container. It specifies the space that a container must leave at
 * each of its edges. The space can be a border, a blank space, or a
 * title.
 *
 * Insets are like "padding" in CSS. That is, the space reserved for
 * insets is included in the size of the component. There is no CSS
 * "margin" equivalent in Ctk.
 */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Insets {
    pub bottom: usize,
    pub left: usize,
    pub right: usize,
    pub top: usize
}

impl Default for Insets {
    fn default() -> Self {
        Insets {
            bottom: 0,
            left: 0,
            right: 0,
            top: 0
        }
    }
}

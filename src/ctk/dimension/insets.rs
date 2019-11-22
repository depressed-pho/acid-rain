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
    pub bottom: i32,
    pub left: i32,
    pub right: i32,
    pub top: i32
}

impl Insets {
    pub fn zero() -> Self {
        Insets {
            bottom: 0,
            left: 0,
            right: 0,
            top: 0
        }
    }
}

impl Default for Insets {
    fn default() -> Self {
        Self::zero()
    }
}

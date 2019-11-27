use num::Zero;
use std::ops::Add;

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
pub struct Insets<T = i32> {
    pub bottom: T,
    pub left: T,
    pub right: T,
    pub top: T
}

impl<T> Zero for Insets<T> where T: Zero {
    fn zero() -> Self {
        Insets {
            bottom: T::zero(),
            left: T::zero(),
            right: T::zero(),
            top: T::zero()
        }
    }

    fn is_zero(&self) -> bool {
        self.bottom.is_zero() &&
            self.left.is_zero() &&
            self.right.is_zero() &&
            self.top.is_zero()
    }
}

impl<T> Default for Insets<T> where T: Zero {
    fn default() -> Self {
        Self::zero()
    }
}

/** Addition of two insets is defined as component-wise.
 */
impl<T> Add for Insets<T> where T: Add<Output = T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Insets {
            bottom: self.bottom + rhs.bottom,
            left: self.left + rhs.left,
            right: self.right + rhs.right,
            top: self.top + rhs.top
        }
    }
}

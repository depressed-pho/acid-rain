use std::cell::RefCell;
use std::ops::{Add, Mul, Sub};
use std::rc::Rc;

#[derive(Clone)]
pub struct Spring {
    rc: Rc<RefCell<dyn SpringT>>
}

pub trait SpringT {
}

/** Addition of two springs.
 */
impl Add for Spring {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        unimplemented!();
    }
}

/** Subtraction of two springs.
 */
impl Sub for Spring {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        unimplemented!();
    }
}

/** Scalar multiplication. Not general at all. See also
 * https://github.com/rust-lang/rfcs/issues/2608
 */
impl<T> Mul<T> for Spring where T: Into<f64> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self {
        unimplemented!();
    }
}

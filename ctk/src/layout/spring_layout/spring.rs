mod implementation;
pub use implementation::*;

use crate::dimension::LengthRequirements;
use std::cell::RefCell;
use std::ops::{Add, Mul, Sub};
use std::rc::Rc;

#[derive(Clone)]
pub struct Spring {
    rc: Rc<RefCell<dyn SpringImpl>>
}

impl Spring {
    pub fn wrap<T: SpringImpl + 'static>(s_impl: T) -> Self {
        Self {
            rc: Rc::new(RefCell::new(s_impl))
        }
    }
}

pub trait SpringImpl {
    /** Return the requirements for the value of this spring.
     */
    fn get_requirements(&self) -> LengthRequirements;

    /** Return the current value of this spring.
     */
    fn get_value(&self) -> i32;

    /** Set the current value of this spring.
     */
    fn set_value(&mut self, value: i32);
}

/** Addition of two springs.
 */
impl Add for Spring {
    type Output = Self;

    fn add(self, _rhs: Self) -> Self {
        unimplemented!();
    }
}

/** Subtraction of two springs.
 */
impl Sub for Spring {
    type Output = Self;

    fn sub(self, _rhs: Self) -> Self {
        unimplemented!();
    }
}

/** Scalar multiplication. Not general at all. See also
 * https://github.com/rust-lang/rfcs/issues/2608
 */
impl<T> Mul<T> for Spring where T: Into<f64> {
    type Output = Self;

    fn mul(self, _rhs: T) -> Self {
        unimplemented!();
    }
}

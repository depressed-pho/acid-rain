mod implementation;
pub use implementation::*;

use crate::dimension::LengthRequirements;
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::{Add, Mul, Sub};
use std::rc::{Rc, Weak};
use weak_table::PtrWeakHashSet;

/** A spring holds a LengthRequirements and the current value. It can
 * be visualized as a mechanical spring that provides a corrective
 * force as the spring is compressed or stretched away from its
 * preferred value. This force is modelled as linear function of the
 * distance from the preferred value, but with two different constants
 * -- one for the compressional force and one for the tensional
 * one. Those constants are specified by the minimum and maximum
 * values of the spring such that a spring at its minimum value
 * produces an equal and opposite force to that to that which is
 * created when it is at its maximum value. The difference between the
 * preferred and minimum values, therefore, represents the ease with
 * which the spring can be compressed and the difference between its
 * maximum and preferred values, indicates the ease with which the
 * Spring can be extended.
 *
 * The struct Spring is actually a reference-counting wrapper for the
 * trait SpringImpl. The reason why there has to be a wrapper is that
 * arithmetic traits such as Add can not be directly implemented for
 * Rc<RefCell<dyn SpringImpl>>.
 */
#[derive(Clone, Debug)]
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

pub trait SpringImpl: Debug {
    /** Return the requirements for the value of this spring.
     */
    fn get_requirements(&self) -> LengthRequirements;

    /** Return the current value of this spring.
     */
    fn get_value(&self) -> i32;

    /** Set the current value of this spring.
     */
    fn set_value(&mut self, value: i32);

    /** Return true iff the spring depends on any of springs in a
     * given set.
     */
    fn is_cyclic(&self, seen: SpringSet) -> bool;
}

impl SpringImpl for Spring {
    fn get_requirements(&self) -> LengthRequirements {
        self.rc.borrow().get_requirements()
    }

    fn get_value(&self) -> i32 {
        self.rc.borrow().get_value()
    }

    fn set_value(&mut self, v: i32) {
        self.rc.borrow_mut().set_value(v);
    }

    fn is_cyclic(&self, seen: SpringSet) -> bool {
        self.rc.borrow().is_cyclic(seen)
    }
}

pub struct SpringSet {
    s: PtrWeakHashSet<Weak<RefCell<dyn SpringImpl>>>
}

impl SpringSet {
    pub fn new() -> Self {
        Self {
            s: PtrWeakHashSet::new()
        }
    }
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

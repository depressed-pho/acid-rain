mod implementation;
pub use implementation::*;

use crate::dimension::LengthRequirements;
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::{Add, Mul, Sub};
use std::rc::{Rc, Weak};
use weak_table::PtrWeakHashSet;

/** A spring holds a LengthRequirements and the current length. It can
 * be visualized as a mechanical spring that provides a corrective
 * force as the spring is compressed or stretched away from its
 * preferred length. This force is modelled as linear function of the
 * distance from the preferred length, but with two different
 * constants -- one for the compressional force and one for the
 * tensional one. Those constants are specified by the minimum and
 * maximum lengths of the spring such that a spring at its minimum
 * length produces an equal and opposite force to that to that which
 * is created when it is at its maximum length. The difference between
 * the preferred and minimum lengths, therefore, represents the ease
 * with which the spring can be compressed and the difference between
 * its maximum and preferred lengths, indicates the ease with which
 * the Spring can be extended.
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
    /** Return the requirements for the length of this spring.
     */
    fn get_requirements(&self) -> LengthRequirements;

    /** Return the current length of this spring.
     */
    fn get_length(&self) -> i32;

    /** Set the current length of this spring.
     */
    fn set_length(&mut self, length: i32);

    /** Return true iff the spring depends on any of springs in a
     * given set.
     */
    fn is_cyclic(&self, seen: SpringSet) -> bool;

    fn range(&self, contract: bool) -> i32 {
        let reqs = self.get_requirements();
        if contract {
            reqs.preferred - reqs.minimum
        }
        else {
            reqs.maximum - reqs.preferred
        }
    }

    fn get_strain(&self) -> f64 {
        let reqs   = self.get_requirements();
        let length = self.get_length();
        let delta  = length - reqs.preferred;
        delta as f64 / self.range(length < reqs.preferred) as f64
    }

    fn set_strain(&mut self, strain: f64) {
        self.set_length(
            self.get_requirements().preferred +
                (strain * self.range(strain < 0.0) as f64).trunc() as i32);
    }
}

impl SpringImpl for Spring {
    fn get_requirements(&self) -> LengthRequirements {
        self.rc.borrow().get_requirements()
    }

    fn get_length(&self) -> i32 {
        self.rc.borrow().get_length()
    }

    fn set_length(&mut self, v: i32) {
        self.rc.borrow_mut().set_length(v);
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

    fn add(self, rhs: Self) -> Self {
        Spring::wrap(SumSpring::new(self, rhs))
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

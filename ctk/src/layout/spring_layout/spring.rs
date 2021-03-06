mod implementation;
pub use implementation::*;

use async_trait::async_trait;
use crate::dimension::LengthRequirements;
use std::any::Any;
use std::fmt::{self, Debug};
use std::ops::{Add, Mul, Sub, Neg};
use std::sync::{Arc, Weak};
use tokio::sync::RwLock;
use weak_table::PtrWeakHashSet;

/// A spring holds a [LengthRequirements] and the current length. It can
/// be visualized as a mechanical spring that provides a corrective
/// force as the spring is compressed or stretched away from its
/// preferred length. This force is modelled as linear function of the
/// distance from the preferred length, but with two different
/// constants -- one for the compressional force and one for the
/// tensional one. Those constants are specified by the minimum and
/// maximum lengths of the spring such that a spring at its minimum
/// length produces an equal and opposite force to that to that which
/// is created when it is at its maximum length. The difference between
/// the preferred and minimum lengths, therefore, represents the ease
/// with which the spring can be compressed and the difference between
/// its maximum and preferred lengths, indicates the ease with which
/// the [Spring] can be extended.
///
/// The struct [Spring] is actually a reference-counting wrapper for
/// the trait [SpringImpl]. The reason why there has to be a wrapper
/// is that arithmetic traits such as Add can not be directly
/// implemented for `Arc<RwLock<dyn SpringImpl>>`.
#[derive(Clone)]
pub struct Spring {
    rc: Arc<RwLock<dyn SpringImpl>>
}

impl Spring {
    pub fn wrap<T: SpringImpl + Any + 'static>(s_impl: T) -> Self {
        assert!(
            !(&s_impl as &dyn Any).is::<Spring>(),
            "Wrapping a Spring into another Spring does not make sense");
        Self {
            rc: Arc::new(RwLock::new(s_impl))
        }
    }

    pub(crate) fn unwrap(&self) -> &Arc<RwLock<dyn SpringImpl>> {
        &self.rc
    }
}

impl Debug for Spring {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        /* Format the wrapper spring as if nothing were wrapped. */
        futures::executor::block_on(async {
            self.rc.read().await.fmt(fmt)
        })
    }
}

#[async_trait]
pub trait SpringImpl: Debug + Send + Sync {
    /// Return the requirements for the length of this spring.
    async fn get_requirements(&self) -> LengthRequirements;

    /// Return the current length of this spring.
    async fn get_length(&self) -> i32;

    /// Set the current length of this spring.
    async fn set_length(&mut self, length: i32);

    /// Return true iff the spring depends on any of springs in a
    /// given set.
    async fn is_cyclic(&self, seen: &mut SpringSet) -> bool;

    async fn range(&self, contract: bool) -> i32 {
        let reqs = self.get_requirements().await;
        if contract {
            reqs.preferred - reqs.minimum
        }
        else {
            reqs.maximum - reqs.preferred
        }
    }

    async fn get_strain(&self, length: i32) -> f64 {
        let reqs  = self.get_requirements().await;
        let delta = length - reqs.preferred;
        let range = self.range(length < reqs.preferred).await;
        if range == 0 {
            /* The range being 0 means that the spring is rigid, and
             * its length should always be its preferred length. This
             * is a special case to avoid causing division by zero.
             */
            0.0
        }
        else {
            delta as f64 / range as f64
        }
    }

    async fn set_strain(&mut self, strain: f64) {
        self.set_length(
            self.get_requirements().await.preferred +
                (strain * self.range(strain < 0.0).await as f64).round() as i32).await;
    }
}

#[async_trait]
impl SpringImpl for Spring {
    async fn get_requirements(&self) -> LengthRequirements {
        self.rc.read().await.get_requirements().await
    }

    async fn get_length(&self) -> i32 {
        self.rc.read().await.get_length().await
    }

    async fn set_length(&mut self, v: i32) {
        self.rc.write().await.set_length(v).await;
    }

    async fn is_cyclic(&self, seen: &mut SpringSet) -> bool {
        self.rc.read().await.is_cyclic(seen).await
    }
}

#[derive(Debug)]
pub struct SpringSet {
    set: PtrWeakHashSet<Weak<RwLock<dyn SpringImpl>>>,
    has_cycle: bool
}

impl SpringSet {
    pub fn new() -> Self {
        Self {
            set: PtrWeakHashSet::new(),
            has_cycle: false
        }
    }

    pub async fn add(&mut self, s: &Spring) -> &mut Self {
        /* Once we find a single cycle, we don't need to check for any
         * others. */
        if !self.has_cycle {
            self.has_cycle = self.set.insert(s.unwrap().clone());

            /* Do we still have no cycles? Then recurse into the
             * supplied spring. */
            if !self.has_cycle {
                s.is_cyclic(self).await;
            }
        }
        self
    }

    pub fn is_cyclic(&self) -> bool {
        self.has_cycle
    }
}

/// Addition of two springs.
impl Add for Spring {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        SumSpring::new(self, rhs)
    }
}

/// Subtraction of two springs.
impl Sub for Spring {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        self + -rhs
    }
}

/// Negation of a spring.
impl Neg for Spring {
    type Output = Self;

    fn neg(self) -> Self {
        NegativeSpring::new(self)
    }
}

/// Scalar multiplication. Not general at all. See also
/// https://github.com/rust-lang/rfcs/issues/2608
impl<T> Mul<T> for Spring where T: Into<f64> {
    type Output = Self;

    fn mul(self, _rhs: T) -> Self {
        unimplemented!();
    }
}

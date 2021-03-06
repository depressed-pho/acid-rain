use async_trait::async_trait;
use crate::Component;
use crate::dimension::{
    LengthRequirements
};
use lazy_static::lazy_static;
use std::fmt::{self, Debug};
use std::sync::Arc;
use super::{Spring, SpringImpl, SpringSet};
use tokio::sync::RwLock;

lazy_static! {
    /* In order to evenly distribute extra spaces, it is crucial to
     * clamp the maximum length of non-compositing springs to
     * somewhere under the i32::max_value(). Otherwise the important
     * invariant s1.range() + s2.range() = (s1 + s2).range() will not
     * hold.
     */
    static ref MAX: LengthRequirements =
        LengthRequirements::at_most(i16::max_value() as i32);
}

/// A spring whose requirements are fixed at the instantiation time.
#[derive(Clone, Debug)]
pub struct StaticSpring {
    reqs: LengthRequirements,
    length: i32
}

impl StaticSpring {
    pub fn new(reqs: LengthRequirements) -> Spring {
        Spring::wrap(
            Self {
                reqs: reqs & *MAX,
                length: reqs.preferred
            })
    }
}

#[async_trait]
impl SpringImpl for StaticSpring {
    async fn get_requirements(&self) -> LengthRequirements {
        self.reqs
    }

    async fn get_length(&self) -> i32 {
        self.length
    }

    async fn set_length(&mut self, length: i32) {
        self.length = length;
    }

    async fn is_cyclic(&self, _seen: &mut SpringSet) -> bool {
        false
    }
}

/// A spring whose requirements and length are defined by that of a
/// supplied component. The spring keeps track of changes in the
/// component.
#[derive(Clone)]
pub struct WidthSpring {
    of: Arc<RwLock<dyn Component>>,
    length: Option<i32>
}

impl WidthSpring {
    pub fn new(of: Arc<RwLock<dyn Component>>) -> Spring {
        Spring::wrap(Self { of, length: None })
    }
}

impl Debug for WidthSpring {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        futures::executor::block_on(async {
            fmt.debug_struct("WidthSpring")
                .field("of", &self.of.read().await)
                .field("length", &self.length)
                .finish()
        })
    }
}

#[async_trait]
impl SpringImpl for WidthSpring {
    async fn get_requirements(&self) -> LengthRequirements {
        self.of.read().await.get_size_requirements().await.width & *MAX
    }

    async fn get_length(&self) -> i32 {
        if let Some(l) = self.length {
            l
        }
        else {
            self.get_requirements().await.preferred
        }
    }

    async fn set_length(&mut self, width: i32) {
        self.length = Some(width);
    }

    async fn is_cyclic(&self, _seen: &mut SpringSet) -> bool {
        false
    }
}

/// A spring whose requirements and length are defined by that of a
/// supplied component. The spring keeps track of changes in the
/// component.
#[derive(Clone)]
pub struct HeightSpring {
    of: Arc<RwLock<dyn Component>>,
    length: Option<i32>
}

impl HeightSpring {
    pub fn new(of: Arc<RwLock<dyn Component>>) -> Spring {
        Spring::wrap(Self { of, length: None })
    }
}

impl Debug for HeightSpring {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        futures::executor::block_on(async {
            fmt.debug_struct("HeightSpring")
                .field("of", &self.of.read().await)
                .field("length", &self.length)
                .finish()
        })
    }
}

#[async_trait]
impl SpringImpl for HeightSpring {
    async fn get_requirements(&self) -> LengthRequirements {
        self.of.read().await.get_size_requirements().await.height & *MAX
    }

    async fn get_length(&self) -> i32 {
        if let Some(l) = self.length {
            l
        }
        else {
            self.get_requirements().await.preferred
        }
    }

    async fn set_length(&mut self, height: i32) {
        self.length = Some(height);
    }

    async fn is_cyclic(&self, _seen: &mut SpringSet) -> bool {
        false
    }
}

#[derive(Clone, Debug)]
pub(crate) struct SumSpring {
    a: Spring,
    b: Spring
}

impl SumSpring {
    pub fn new(a: Spring, b: Spring) -> Spring {
        Spring::wrap(Self {a, b})
    }
}

#[async_trait]
impl SpringImpl for SumSpring {
    async fn get_requirements(&self) -> LengthRequirements {
        let (a, b) = tokio::join!(
            self.a.get_requirements(),
            self.b.get_requirements());
        a + b
    }

    async fn get_length(&self) -> i32 {
        let (a, b) = tokio::join!(
            self.a.get_length(),
            self.b.get_length());
        a + b
    }

    async fn set_length(&mut self, length: i32) {
        self.a.set_strain(self.get_strain(length).await).await;
        self.b.set_length(length - self.a.get_length().await).await;
    }

    async fn is_cyclic(&self, seen: &mut SpringSet) -> bool {
        seen.add(&self.a).await
            .add(&self.b).await
            .is_cyclic()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct NegativeSpring {
    s: Spring
}

impl NegativeSpring {
    pub fn new(s: Spring) -> Spring {
        Spring::wrap(Self {s})
    }
}

#[async_trait]
impl SpringImpl for NegativeSpring {
    async fn get_requirements(&self) -> LengthRequirements {
        -self.s.get_requirements().await
    }

    async fn get_length(&self) -> i32 {
        -self.s.get_length().await
    }

    async fn set_length(&mut self, length: i32) {
        self.s.set_length(-length).await;
    }

    async fn is_cyclic(&self, seen: &mut SpringSet) -> bool {
        seen.add(&self.s).await.is_cyclic()
    }
}

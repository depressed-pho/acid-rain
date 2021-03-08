mod key;
pub use key::*;

use std::fmt::{self, Debug};

pub trait InputEvent: Debug {
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) enum Modifier {
    Shift,
    Control,
    Alt
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub(crate) struct Modifiers {
    bits: u8
}

impl Modifiers {
    pub fn is_shift_down(&self) -> bool {
        self.bits & (1 << 0) != 0
    }

    pub fn is_control_down(&self) -> bool {
        self.bits & (1 << 1) != 0
    }

    pub fn is_alt_down(&self) -> bool {
        self.bits & (2 << 1) != 0
    }
}

impl Default for Modifiers {
    fn default() -> Self {
        Self {
            bits: 0
        }
    }
}

impl Debug for Modifiers {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut set = fmt.debug_set();
        if self.is_shift_down() {
            set.entry(&Modifier::Shift);
        }
        if self.is_control_down() {
            set.entry(&Modifier::Control);
        }
        if self.is_alt_down() {
            set.entry(&Modifier::Alt);
        }
        set.finish()
    }
}

impl From<Modifier> for Modifiers {
    fn from(m: Modifier) -> Self {
        let bits =
            match m {
                Modifier::Shift   => 1 << 0,
                Modifier::Control => 1 << 1,
                Modifier::Alt     => 1 << 2
            };
        Self { bits }
    }
}

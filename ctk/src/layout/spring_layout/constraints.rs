use crate::layout::spring_layout::Spring;
use fixed_map::{Key, Map as FixedMap};
use std::cell::{RefCell, RefMut};

#[derive(Eq, PartialEq, Clone, Copy, Key, Hash, Debug)]
pub enum Edge {
    Bottom,
    Left,
    Top,
    Right,
    HorizontalCenter,
    VerticalCenter,
    Width,
    Height
}

impl Edge {
    fn is_horizontal(self) -> bool {
        match self {
            Edge::Left   | Edge::Right | Edge::HorizontalCenter | Edge::Width  => true,
            Edge::Bottom | Edge::Top   | Edge::VerticalCenter   | Edge::Height => false
        }
    }
}

pub struct Constraints {
    /* Invariants:
     * - As long as an edge is in a history, it has an explicit
     *   spring.
     * - There are at most 2 explicit springs for each axis.
     */
    explicit:   FixedMap<Edge, Spring>,
    h_implicit: RefCell<FixedMap<Edge, Option<Spring>>>,
    v_implicit: RefCell<FixedMap<Edge, Option<Spring>>>,
    h_history:  Vec<Edge>,
    v_history:  Vec<Edge>
}

impl Constraints {
    /** Create an empty constraints object.
     */
    pub(crate) fn new() -> Self {
        Constraints {
            explicit:   FixedMap::new(),
            h_implicit: RefCell::new(FixedMap::new()),
            v_implicit: RefCell::new(FixedMap::new()),
            h_history:  Vec::with_capacity(2),
            v_history:  Vec::with_capacity(2)
        }
    }

    fn history_mut(&mut self, edge: Edge) -> &mut Vec<Edge> {
        if edge.is_horizontal() {
            &mut self.h_history
        }
        else {
            &mut self.v_history
        }
    }

    fn implicit_mut(&self, edge: Edge) -> RefMut<FixedMap<Edge, Option<Spring>>> {
        if edge.is_horizontal() {
            self.h_implicit.borrow_mut()
        }
        else {
            self.v_implicit.borrow_mut()
        }
    }

    fn push_spring(&mut self, edge: Edge) -> &mut Self {
        debug_assert!(self.explicit.get(edge).is_some());

        // Only retain the last two springs explicitly given for each
        // axis.
        let history    = self.history_mut(edge);
        let pushed_out =
            if let e@Some(_) = remove_item(history, &edge) {
                e
            }
            else if history.len() >= 2 {
                Some(history.remove(0))
            }
            else {
                None
            };
        history.push(edge);
        if let Some(e) = pushed_out {
            /* An old spring is being pushed out. This means there may
             * be some implicit springs in the cache which depend on
             * it. So we need to invalidate the cache.
             */
            self.explicit.remove(e);
            self.implicit_mut(edge).clear();
        }
        self
    }

    fn forget_spring(&mut self, edge: Edge) -> &mut Self {
        debug_assert!(self.explicit.get(edge).is_none());

        let history = self.history_mut(edge);
        remove_item(history, &edge);
        self
    }

    pub fn set_spring(&mut self, edge: Edge, spring: Option<Spring>) {
        if let Some(s) = spring {
            self.explicit.insert(edge, s);
            self.push_spring(edge);
        }
        else {
            self.explicit.remove(edge);
            self.forget_spring(edge);
        }
    }

    fn get_explicit_spring(&self, edge: Edge) -> Option<Spring> {
        self.explicit.get(edge).cloned()
    }

    fn get_explicit_pair(&self, e1: Edge, e2: Edge) -> Option<(Spring, Spring)> {
        self.get_explicit_spring(e1)
            .and_then(|s1| {
                self.get_explicit_spring(e2)
                    .map(|s2| (s1, s2))
            })
    }

    pub fn get_implicit_spring(&self, edge: Edge) -> Option<Spring> {
        match edge {
            Edge::Bottom =>
                self.get_spring(Edge::Top)
                    .and_then(|top| {
                        self.get_spring(Edge::Height)
                            .map(|height| top + height)
                    }),
            Edge::Left =>
                self.get_explicit_pair(Edge::Right, Edge::Width)
                    .map(|(right, width)| right - width)
                    .or_else(|| {
                        self.get_explicit_pair(Edge::HorizontalCenter, Edge::Width)
                            .map(|(h_center, width)| h_center - width * 0.5)
                    })
                    .or_else(|| {
                        self.get_explicit_pair(Edge::HorizontalCenter, Edge::Right)
                            .map(|(h_center, right)| h_center * 2.0 - right)
                    }),
            Edge::Top =>
                self.get_explicit_pair(Edge::Bottom, Edge::Height)
                    .map(|(bottom, height)| bottom - height)
                    .or_else(|| {
                        self.get_explicit_pair(Edge::VerticalCenter, Edge::Height)
                            .map(|(v_center, height)| v_center - height * 0.5)
                    })
                    .or_else(|| {
                        self.get_explicit_pair(Edge::VerticalCenter, Edge::Bottom)
                            .map(|(v_center, bottom)| v_center * 2.0 - bottom)
                    }),
            Edge::Right =>
                self.get_spring(Edge::Left)
                    .and_then(|left| {
                        self.get_spring(Edge::Width)
                            .map(|width| left + width)
                    }),
            Edge::HorizontalCenter =>
                self.get_spring(Edge::Left)
                    .and_then(|left| {
                        self.get_spring(Edge::Width)
                            .map(|width| left + width * 0.5)
                    }),
            Edge::VerticalCenter =>
                self.get_spring(Edge::Top)
                    .and_then(|top| {
                        self.get_spring(Edge::Height)
                            .map(|height| top + height * 0.5)
                    }),
            Edge::Width =>
                self.get_explicit_spring(Edge::Right)
                    .and_then(|right| {
                        self.get_spring(Edge::Left)
                            .map(|left| right - left)
                    })
                    .or_else(|| {
                        self.get_explicit_spring(Edge::HorizontalCenter)
                            .and_then(|h_center| {
                                self.get_spring(Edge::Left)
                                    .map(|left| (h_center - left) * 2.0)
                            })
                    }),
            Edge::Height =>
                self.get_explicit_spring(Edge::Bottom)
                    .and_then(|bottom| {
                        self.get_spring(Edge::Top)
                            .map(|top| bottom - top)
                    })
                    .or_else(|| {
                        self.get_explicit_spring(Edge::VerticalCenter)
                            .and_then(|v_center| {
                                self.get_spring(Edge::Top)
                                    .map(|top| (v_center - top) * 2.0)
                            })
                    })
        }
    }

    pub fn get_spring(&self, edge: Edge) -> Option<Spring> {
        self.get_explicit_spring(edge)
            .or_else(|| {
                // Interior mutability to cache the expensive
                // computation of get_implicit_spring().
                let mut cache = self.implicit_mut(edge);
                if let Some(spring) = cache.get(edge) {
                    // The result was cached, either positively or
                    // negatively.
                    spring.to_owned()
                }
                else {
                    let spring = self.get_implicit_spring(edge);
                    cache.insert(edge, spring.clone());
                    spring
                }
            })
    }
}

// THINKME: We want to use Vec::remove_item() but it's only in nightly.
fn remove_item<T: PartialEq>(v: &mut Vec<T>, a: &T) -> Option<T> {
    let i = v.iter().position(|b| *b == *a)?;
    Some(v.remove(i))
}

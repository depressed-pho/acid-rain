use crate::ctk::dimension::{
    Dimension,
    Point,
    Rectangle
};
use crate::ctk::layout::spring_layout::Spring;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
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

lazy_static! {
    static ref ALL_HORIZONTAL: Vec<Edge> =
        vec![Edge::Left, Edge::Right, Edge::HorizontalCenter, Edge::Width];

    static ref ALL_VERTICAL: Vec<Edge> =
        vec![Edge::Bottom, Edge::Top, Edge::VerticalCenter, Edge::Height];
}

impl Edge {
    fn is_horizontal(self) -> bool {
        ALL_HORIZONTAL.contains(&self)
    }

    fn all_horizontal() -> impl Iterator<Item = &'static Self> {
        ALL_HORIZONTAL.iter()
    }

    fn all_vertical() -> impl Iterator<Item = &'static Self> {
        ALL_VERTICAL.iter()
    }
}

pub struct Constraints {
    bounds: Rectangle<Option<Spring>>,
    bottom_right: Point<Option<Spring>>,
    center: Point<Option<Spring>>,
    h_history: Vec<Edge>,
    v_history: Vec<Edge>
}

impl Constraints {
    /** Create an empty constraints object.
     */
    pub(crate) fn new() -> Self {
        let mut h_history = Vec::new(); h_history.reserve_exact(2);
        let mut v_history = Vec::new(); v_history.reserve_exact(2);
        Constraints {
            bounds: Rectangle {
                pos: Point {
                    x: None,
                    y: None
                },
                size: Dimension {
                    width: None,
                    height: None
                }
            },
            bottom_right: Point {
                x: None,
                y: None
            },
            center: Point {
                x: None,
                y: None
            },
            h_history,
            v_history
        }
    }

    fn push_spring(&mut self, edge: Edge) {
        // Only retain the last two springs explicitly given for each
        // axis.
        let history =
            if edge.is_horizontal() {
                &mut self.h_history
            }
            else {
                &mut self.v_history
            };
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
            self.set_spring(e, None)
        }
    }

    pub fn set_spring(&mut self, edge: Edge, spring: Option<Spring>) {
        match edge {
            Edge::Bottom           => self.bottom_right.y     = spring.to_owned(),
            Edge::Left             => self.bounds.pos.x       = spring.to_owned(),
            Edge::Top              => self.bounds.pos.y       = spring.to_owned(),
            Edge::Right            => self.bottom_right.x     = spring.to_owned(),
            Edge::HorizontalCenter => self.center.x           = spring.to_owned(),
            Edge::VerticalCenter   => self.center.y           = spring.to_owned(),
            Edge::Width            => self.bounds.size.width  = spring.to_owned(),
            Edge::Height           => self.bounds.size.height = spring.to_owned()
        }
        if spring.is_some() {
            self.push_spring(edge);
        }
    }

    fn get_explicit_spring(&self, edge: Edge) -> Option<Spring> {
        match edge {
            Edge::Bottom           => self.bottom_right.y.to_owned(),
            Edge::Left             => self.bounds.pos.x.to_owned(),
            Edge::Top              => self.bounds.pos.y.to_owned(),
            Edge::Right            => self.bottom_right.x.to_owned(),
            Edge::HorizontalCenter => self.center.x.to_owned(),
            Edge::VerticalCenter   => self.center.y.to_owned(),
            Edge::Width            => self.bounds.size.width.to_owned(),
            Edge::Height           => self.bounds.size.height.to_owned()
        }
    }

    fn get_explicit_pair(&self, e1: Edge, e2: Edge) -> Option<(Spring, Spring)> {
        let s1 = self.get_explicit_spring(e1);
        let s2 = self.get_explicit_spring(e2);

        self.get_explicit_spring(e1)
            .and_then(|s1| {
                self.get_explicit_spring(e2)
                    .map(|s2| (s1, s2))
            })
    }

    pub fn get_spring(&self, edge: Edge) -> Option<Spring> {
        // THINKME: Perhaps we should be caching the result of this
        // computation for performance.
        self.get_explicit_spring(edge)
            .or_else(|| {
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
            })
    }
}

// THINKME: We want to use Vec::remove_item() but it's only in nightly.
fn remove_item<T: PartialEq>(v: &mut Vec<T>, a: &T) -> Option<T> {
    let i = v.iter().position(|b| *b == *a)?;
    Some(v.remove(i))
}

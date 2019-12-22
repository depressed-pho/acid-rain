mod constraints;
pub use constraints::*;

mod spring;
pub use spring::*;

use crate::{
    Component,
    Layout
};
use crate::dimension::{
    Dimension,
    LengthRequirements,
    Point,
    Rectangle,
    SizeRequirements
};
use std::cell::RefCell;
use std::fmt::{self, Debug};
use std::rc::{Rc, Weak};
use weak_table::PtrWeakKeyHashMap;

#[derive(Debug)]
pub struct SpringLayout {
    /* Invariants:
     *
     * - Springs are all acyclic, including proxies and implicit
     *   ones. This is VERY hard to validate.
     *
     * - For all elements in "components", there exists a value in
     *   "constraints".
     *
     * - For all keys in "constraints", there exists a component in
     *   "components".
     */
    components: Vec<Rc<RefCell<dyn Component>>>,
    constraints: PtrWeakKeyHashMap<Weak<RefCell<dyn Component>>, Rc<RefCell<Constraints>>>,
    parent_constr: Rc<RefCell<Constraints>>,
    is_valid: bool
}

impl SpringLayout {
    pub fn new() -> Self {
        let mut con = Constraints::new();
        con.set_spring(Edge::Left, Some(StaticSpring::new(LengthRequirements::exactly(0))));
        con.set_spring(Edge::Top , Some(StaticSpring::new(LengthRequirements::exactly(0))));
        Self {
            components: Vec::new(),
            constraints: PtrWeakKeyHashMap::new(),
            parent_constr: Rc::new(RefCell::new(con)),
            is_valid: true
        }
    }

    pub fn add(&mut self, c: Rc<RefCell<dyn Component>>) -> &mut Self {
        self.components.push(c.clone());

        /* The rules for default springs are as follows:
         *
         * - For edges that have no sensible defaults and always need
         *   explicit attention from users, they have no defaults even
         *   that would lead to panics at runtime if unattended. The
         *   Left and Top edges are such examples.
         *
         * - For edges that have a sensible default which makes sense
         *   to most users, they have a default spring. The width and
         *   height "edges" are examples.
         *
         * - We don't inject any default springs into Constraint
         *   objects after exposing them to users. That is, if a user
         *   removes the width spring and only leaves a left spring,
         *   the layout manager just panics as opposed to reinstalling
         *   a width spring.
         */
        let mut con = Constraints::new();
        con.set_spring(Edge::Width, Some(WidthSpring::new(c.clone())));
        con.set_spring(Edge::Height, Some(HeightSpring::new(c.clone())));

        self.constraints.insert(c, Rc::new(RefCell::new(con)));
        self.invalidate();
        self
    }

    pub fn remove(&mut self, c: Rc<RefCell<dyn Component>>) -> &mut Self {
        self.components.retain(|c_| !Rc::ptr_eq(&c, &c_));
        self.constraints.remove(&c);
        self.invalidate();
        self
    }

    /** Return the Constraints object associated with a given
     * Component, or panics if no such Component has been added to this
     * SpringLayout.
     *
     * This method is unsafe because callers can accidentally create
     * cycles in springs.
     */
    unsafe fn get_constraints(&self, of: EdgesOf) -> Rc<RefCell<Constraints>> {
        match &of {
            EdgesOf::Parent   => Some(self.parent_constr.clone()),
            EdgesOf::Child(c) => self.constraints.get(&c).cloned()
        }
        .unwrap_or_else(|| panic!("No such component exists: {:#?}", of))
    }

    /** Return the spring controlling the specified edge of a
     * component. This method, instead of returning the current
     * binding for the edge, returns a proxy that tracks the
     * characteristics of the edge even if the edge is subsequently
     * rebound.
     */
    pub fn get_spring(&self, edge: Edge, of: EdgesOf) -> Spring {
        let cc = unsafe { self.get_constraints(of.clone()) };
        SpringProxy::new(edge, of, cc)
    }

    /** Set a spring controlling the specified edge of a child
     * component. This method panics if setting the given spring would
     * create a cycle.
     */
    pub fn set_spring(&mut self, edge: Edge, of: EdgesOf, s: Spring) -> &mut Self {
        let cc = unsafe { self.get_constraints(of.clone()) };
        cc.borrow_mut().set_spring(edge, Some(s.clone()));
        if s.is_cyclic(&mut SpringSet::new()) {
            // Remove it. We don't want to enter into an infinite
            // loop by debug-formatting the cyclic spring.
            cc.borrow_mut().set_spring(edge, None);
            panic!("A cycle has been detected in spring on {:?} of {:?}: {:#?}", edge, of, s);
        }
        self
    }

    fn do_layout(&mut self, parent: &dyn Component) {
        let pc_    = unsafe { self.get_constraints(EdgesOf::Parent) };
        let pc     = pc_.borrow();
        let insets = parent.get_insets();
        let size   = parent.get_size();
        pc.get_spring(Edge::Left)
            .unwrap_or_else(|| panic!("No springs for the left edge of the parent"))
            .set_length(0);
        pc.get_spring(Edge::Top)
            .unwrap_or_else(|| panic!("No springs for the top edge of the parent"))
            .set_length(0);
        pc.get_spring(Edge::Width)
            .unwrap_or_else(|| panic!("No springs for the width of the parent"))
            .set_length(size.width - insets.left - insets.right);
        pc.get_spring(Edge::Height)
            .unwrap_or_else(|| panic!("No springs for the height of the parent"))
            .set_length(size.height - insets.top - insets.bottom);

        for (c, cc) in self.constraints.iter() {
            let bounds = Rectangle {
                pos: Point {
                    x: cc.borrow()
                        .get_spring(Edge::Left)
                        .unwrap_or_else(|| panic!("No springs for the left edge of the component: {:#?}", c))
                        .get_length(),
                    y: cc.borrow()
                        .get_spring(Edge::Top)
                        .unwrap_or_else(|| panic!("No springs for the top edge of the component: {:#?}", c))
                        .get_length()
                },
                size: Dimension {
                    width: cc.borrow()
                        .get_spring(Edge::Width)
                        .unwrap_or_else(|| panic!("No springs for the width of the component: {:#?}", c))
                        .get_length(),
                    height: cc.borrow()
                        .get_spring(Edge::Height)
                        .unwrap_or_else(|| panic!("No springs for the height of the component: {:#?}", c))
                        .get_length()
                }
            };
            c.borrow_mut().set_bounds(bounds);
        }
    }
}

impl Layout for SpringLayout {
    fn validate(&mut self, parent: &dyn Component) {
        if !self.is_valid {
            self.do_layout(parent);
            self.is_valid = true;
        }
        for child in self.components.iter() {
            child.borrow_mut().validate();
        }
    }

    fn invalidate(&mut self) {
        self.is_valid = false
    }

    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Rc<RefCell<dyn Component>>> + 'a> {
        Box::new(self.components.iter())
    }

    fn get_size_requirements(&self, parent: &dyn Component) -> SizeRequirements {
        let pc_ = unsafe { self.get_constraints(EdgesOf::Parent) };
        let pc  = pc_.borrow();
        let w   = pc.get_spring(Edge::Width)
                    .unwrap_or_else(|| panic!("No springs for the width of the parent"));
        let h   = pc.get_spring(Edge::Height)
                    .unwrap_or_else(|| panic!("No springs for the height of the parent"));
        let req = SizeRequirements {
            width: w.get_requirements(),
            height: h.get_requirements()
        };
        req + parent.get_insets()
    }
}

#[derive(Clone)]
pub enum EdgesOf {
    Parent,
    Child(Rc<RefCell<dyn Component>>)
    /* THINKME: Ideally this should be Child(&'a Rc<...>) but
     * coerce_unsized doesn't seem to work in this case.
     */
}

impl Debug for EdgesOf {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Parent    => fmt.write_str("Parent"),
            Self::Child(rc) => rc.borrow().fmt(fmt)
        }
    }
}

struct SpringProxy {
    edge: Edge,
    of: EdgesOf,
    constraints: Rc<RefCell<Constraints>>
}

impl SpringProxy {
    pub fn new(edge: Edge, of: EdgesOf, constraints: Rc<RefCell<Constraints>>) -> Spring {
        Spring::wrap(Self { edge, of, constraints })
    }

    fn deref(&self) -> Spring {
        self.constraints
            .borrow()
            .get_spring(self.edge)
            .unwrap_or_else(|| {
                panic!("The referenced edge of the proxy has no spring: {:?}", self.edge)
            })
    }
}

impl Debug for SpringProxy {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("SpringProxy")
            .field("edge", &self.edge)
            .field("of", &self.of)
            .field("proxying", &self.deref())
            .finish()
    }
}

impl SpringImpl for SpringProxy {
    fn get_requirements(&self) -> LengthRequirements {
        self.deref().get_requirements()
    }

    fn get_length(&self) -> i32 {
        self.deref().get_length()
    }

    fn set_length(&mut self, length: i32) {
        self.deref().set_length(length)
    }

    fn is_cyclic(&self, seen: &mut SpringSet) -> bool {
        self.deref().is_cyclic(seen)
    }
}

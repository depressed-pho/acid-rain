use crate::Component;
use self::Instruction::*;
use std::fmt;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

type Ctx = Rc<RefCell<dyn Component>>;

// A convenient type alias for use only in this module.
type EventListener<T> = dyn Fn(&Ctx, &T) -> Instruction + 'static;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Instruction {
    /// Continue propagating the event to other listeners that are
    /// listening to it.
    Proceed,
    /// Consume the event and don't let it be propagated any further.
    Stop
}

/// An event bus is a registry of event listeners which resides in a
/// [Component](crate::Component). The type parameter `T` denotes the
/// type of event.
///
/// Event listeners are functions taking a component and an event
/// object `T` and return either [Proceed] or [Stop].
///
#[derive(Clone)]
pub struct EventBus<T> {
    // Why listeners are Fn and not FnMut? Because they have to be
    // 'static (or technically, they must outlive busses) so allowing
    // them to capture &mut refs doesn't really give them much. On the
    // other hand, if we were to use FnMut we'd have to wrap listeners
    // in RefCell and pay the runtime cost for that.
    //
    // Rc is necessary to support deregistration.
    //
    listeners: Vec<Rc<EventListener<T>>>
}

impl<T> EventBus<T> {
    /// Create an empty event bus.
    pub fn new() -> Self {
        Self {
            listeners: Vec::new()
        }
    }

    /// Register a listener to the bus. Listeners have precedence in
    /// the reverse order they are registered. That is, The last
    /// listener registered to the bus will receive events first, and
    /// they have the right to [Stop] propagation. This method returns
    /// a token which can be used for removing the listener.
    pub fn add_listener<F>(&mut self, listener: F) -> ListenerToken<T>
    where F: Fn(&Ctx, &T) -> Instruction + 'static {

        // NOTE: I have absolutely no idea why removing this explicit
        // type coertion results in a compilation error.
        let rc    = Rc::new(listener) as Rc<EventListener<T>>;
        let token = ListenerToken::new(&rc);

        self.listeners.push(rc);
        token
    }

    /// Remove a listener from the bus. If the bus has no listener
    /// that corresponds to the given token, this method does nothing.
    pub fn remove_listener(&mut self, token: ListenerToken<T>) {
        // We could use PtrWeakKeyHashMap from weak_table to make this
        // amortized O(1), but the cost of maintaining the table would
        // not justify that.
        if let Some(i) = self.listeners.iter().position(
            |l| ListenerToken::new(l) == token) {

            self.listeners.remove(i);
        }
    }

    /// Dispatch an event to the bus. Returns [Stop] If any of the
    /// listener returns [Stop], otherwise it returns [Proceed].
    pub fn dispatch(&self, ctx: &Ctx, event: &T) -> Instruction {
        for l in self.listeners.iter().rev() {
            match l(ctx, event) {
                Stop => return Stop,
                _    => {}
            }
        }
        Proceed
    }
}

impl<T: 'static> fmt::Debug for EventBus<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(
            format_args!(
                "EventBus<{:?}> {{ .. }}",
                std::any::TypeId::of::<T>()))
    }
}

#[derive(Clone)]
pub struct ListenerToken<T> {
    ptr: Weak<EventListener<T>>
}

impl<T> ListenerToken<T> {
    fn new(listener: &Rc<EventListener<T>>) -> Self {
        Self {
            ptr: Rc::downgrade(listener)
        }
    }
}

impl<T> PartialEq for ListenerToken<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.ptr.ptr_eq(&rhs.ptr)
    }
}

impl<T> Eq for ListenerToken<T> {}

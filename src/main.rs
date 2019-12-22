#![allow(dead_code)]

mod world;

#[macro_use]
extern crate clap;

use clap::{Arg, ArgMatches};
use ctk::{
    HorizontalAlignment as HA
};
use ctk::component::{Button, Label, Panel};
use ctk::dimension::LengthRequirements;
use ctk::layout::GridLayout;
use ctk::layout::spring_layout::{
    Edge,
    EdgesOf,
    SpringLayout,
    StaticSpring
};
use std::cell::RefCell;
use std::rc::Rc;

fn opt_matches<'a>() -> ArgMatches<'a> {
    app_from_crate!()
        .get_matches()
}

fn main() {
    ctk::install_default_panic_hook();

    let matches = opt_matches();

    //let w = world::World::new();

    let layout = Rc::new(RefCell::new(GridLayout::new()));
    layout.borrow_mut().set_cols(1);

    let title = Rc::new(RefCell::new(Label::new("A c i d   R a i n")));
    title.borrow_mut().set_horizontal_alignment(HA::Center);
    layout.borrow_mut().add(title);

    let buttons_outer = {
        let layout  = Rc::new(RefCell::new(SpringLayout::new()));

        let buttons = {
            let layout = Rc::new(RefCell::new(GridLayout::new()));
            layout.borrow_mut().set_cols(1).set_vgap(1);

            let play = Rc::new(RefCell::new(Button::new("Play")));
            layout.borrow_mut().add(play);

            let quit = Rc::new(RefCell::new(Button::new("Quit")));
            layout.borrow_mut().add(quit);

            Rc::new(RefCell::new(Panel::new(layout)))
        };
        {
            let mut l    = layout.borrow_mut();
            let gap      = || StaticSpring::new(LengthRequirements::any().preferred(0));

            let p_left   = l.get_spring(Edge::Left  , EdgesOf::Parent);
            let p_top    = l.get_spring(Edge::Top   , EdgesOf::Parent);

            l.add(buttons.clone())
             .set_spring(Edge::Left, EdgesOf::Child(buttons.clone()), p_left + gap())
             .set_spring(Edge::Top , EdgesOf::Child(buttons.clone()), p_top  + gap());

            let b_right  = l.get_spring(Edge::Right , EdgesOf::Child(buttons.clone()));
            let b_bottom = l.get_spring(Edge::Bottom, EdgesOf::Child(buttons.clone()));

            l.set_spring(Edge::Right , EdgesOf::Parent, b_right  + gap())
             .set_spring(Edge::Bottom, EdgesOf::Parent, b_bottom + gap());
        }
        Rc::new(RefCell::new(Panel::new(layout)))
    };
    layout.borrow_mut().add(buttons_outer);

    let mut tk = ctk::Ctk::initiate(layout).unwrap();
    tk.main();
}

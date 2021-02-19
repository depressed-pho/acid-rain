#![allow(dead_code)]

pub mod builtin;
pub mod module;
pub mod world;

use crate::module::loader::ModuleLoader;

use clap::{Arg, ArgMatches, *};
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

    let mut btl = builtin::loader::BuiltinModuleLoader::new();
    btl.load_tiles();

    //ctk_main();
}

fn ctk_main() {
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

            l.add(buttons.clone())

             .take(Edge::Left, EdgesOf::Parent)
                .modify(|s| s + gap())
                .hook(Edge::Left, EdgesOf::Child(buttons.clone()))

             .take(Edge::Top, EdgesOf::Parent)
                .modify(|s| s + gap())
                .hook(Edge::Top, EdgesOf::Child(buttons.clone()))

             .take(Edge::Right, EdgesOf::Child(buttons.clone()))
                .modify(|s| s + gap())
                .hook(Edge::Right, EdgesOf::Parent)

             .take(Edge::Bottom, EdgesOf::Child(buttons.clone()))
                .modify(|s| s + gap())
                .hook(Edge::Bottom, EdgesOf::Parent);
        }
        Rc::new(RefCell::new(Panel::new(layout)))
    };
    layout.borrow_mut().add(buttons_outer);

    let mut tk = ctk::Ctk::initiate(layout).unwrap();
    tk.main();
}

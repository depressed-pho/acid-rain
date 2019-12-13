#![allow(dead_code)]

mod world;

#[macro_use]
extern crate clap;

use clap::{Arg, ArgMatches};
use ctk::{
    HorizontalAlignment as HA
};
use ctk::component::{Button, Label, Panel};
use ctk::layout::GridLayout;
use ctk::layout::SpringLayout;
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
        layout.borrow_mut().add(buttons);

        Rc::new(RefCell::new(Panel::new(layout)))
    };
    layout.borrow_mut().add(buttons_outer);

    let mut tk = ctk::Ctk::initiate(layout).unwrap();
    tk.main();
}

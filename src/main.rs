#![allow(dead_code)]

mod ctk;
mod world;

#[macro_use]
extern crate clap;

#[macro_use]
extern crate lazy_static;

use clap::{Arg, ArgMatches};
use std::cell::RefCell;
use std::rc::Rc;
use crate::ctk::{
    Component,
    HorizontalAlignment as HA
};
use crate::ctk::component::{Button, Label, Panel};
use crate::ctk::layout::GridLayout;

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

    let buttons = {
        let layout = Rc::new(RefCell::new(GridLayout::new()));
        layout.borrow_mut().set_cols(1);

        let play = Rc::new(RefCell::new(Button::new("Play")));
        layout.borrow_mut().add(play);

        let quit = Rc::new(RefCell::new(Button::new("Quit")));
        layout.borrow_mut().add(quit);

        Rc::new(RefCell::new(Panel::new(layout)))
    };
    layout.borrow_mut().add(buttons);

    let mut tk = ctk::Ctk::initiate(layout).unwrap();
    tk.main();
}

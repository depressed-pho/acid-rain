#![allow(dead_code)]

use rain_client::tui::view::world::WorldView;
use rain_core::world::World;

use clap::{/*Arg, */ArgMatches, *}; // "*" because its macros don't support the use syntax yet.
use ctk::{
    Component,
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
use std::sync::{Arc, RwLock};
use uuid::Uuid;

fn opt_matches<'a>() -> ArgMatches<'a> {
    app_from_crate!()
        .get_matches()
}

#[tokio::main]
async fn main() {
    ctk::install_default_panic_hook();

    let _matches = opt_matches();

    let world = Arc::new(RwLock::new(rain_server::world::LocalWorld::new()));
    let root_id = world.read().unwrap().get_root_player().uuid();

    ctk_main(world, root_id).await;
    //ctk_title().await;
}

async fn ctk_main(world: Arc<RwLock<impl World + 'static>>, player: Uuid) {
    let layout = Rc::new(RefCell::new(GridLayout::new()));
    layout.borrow_mut().set_cols(1);

    let view = WorldView::new(world, player).into_ref();
    layout.borrow_mut().add(view.unsize());

    let mut tk = ctk::Ctk::initiate(layout).unwrap();
    tk.step().await;
}

async fn ctk_title() {
    let layout = Rc::new(RefCell::new(GridLayout::new()));
    layout.borrow_mut().set_cols(1);

    let title = Label::new("A c i d   R a i n").into_ref();
    title.borrow_mut().set_horizontal_alignment(HA::Center);
    layout.borrow_mut().add(title.unsize());

    let buttons_outer = {
        let layout = Rc::new(RefCell::new(SpringLayout::new()));

        let buttons = {
            let layout = Rc::new(RefCell::new(GridLayout::new()));
            layout.borrow_mut().set_cols(1).set_vgap(1);

            let play = Button::new("Play").into_ref();
            layout.borrow_mut().add(play.unsize());

            let quit = Button::new("Quit").into_ref();
            layout.borrow_mut().add(quit.unsize());

            Panel::new(layout).into_ref()
        };
        {
            let gap = || StaticSpring::new(LengthRequirements::any().preferred(0));

            layout.borrow_mut().add(buttons.clone().unsize())

             .take(Edge::Left, EdgesOf::This)
                .modify(|s| s + gap())
                .hook(Edge::Left, EdgesOf::Child(buttons.clone().unsize()))

             .take(Edge::Top, EdgesOf::This)
                .modify(|s| s + gap())
                .hook(Edge::Top, EdgesOf::Child(buttons.clone().unsize()))

             .take(Edge::Right, EdgesOf::Child(buttons.clone().unsize()))
                .modify(|s| s + gap())
                .hook(Edge::Right, EdgesOf::This)

             .take(Edge::Bottom, EdgesOf::Child(buttons.clone().unsize()))
                .modify(|s| s + gap())
                .hook(Edge::Bottom, EdgesOf::This);
        }
        Panel::new(layout).into_ref()
    };
    layout.borrow_mut().add(buttons_outer.unsize());

    let mut tk = ctk::Ctk::initiate(layout).unwrap();
    tk.step().await;
    tk.step().await;
}

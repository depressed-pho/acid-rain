#![allow(dead_code)]

use rain_client::tui::view::world::WorldView;
use rain_core::world::World;

use clap::{/*Arg, */ArgMatches, *}; // "*" because its macros don't support the use syntax yet.
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
use std::sync::Arc;
use tokio::sync::RwLock;
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
    let root_id = world.read().await.get_root_player().uuid();

    ctk_main(world, root_id).await;
    //ctk_title().await;
}

async fn ctk_main(world: Arc<RwLock<impl World + Send + Sync + 'static>>, player: Uuid) {
    let layout = Arc::new(RwLock::new(GridLayout::new()));
    layout.write().await.set_cols(1);

    let view = Arc::new(RwLock::new(WorldView::new(world, player)));
    layout.write().await.add(view);

    let mut tk = ctk::Ctk::initiate(layout).unwrap();
    tk.step().await;
}

async fn ctk_title() {
    let layout = Arc::new(RwLock::new(GridLayout::new()));
    layout.write().await.set_cols(1);

    let title = Arc::new(RwLock::new(Label::new("A c i d   R a i n")));
    title.write().await.set_horizontal_alignment(HA::Center);
    layout.write().await.add(title);

    let buttons_outer = {
        let layout = Arc::new(RwLock::new(SpringLayout::new()));

        let buttons = {
            let layout = Arc::new(RwLock::new(GridLayout::new()));
            layout.write().await.set_cols(1).set_vgap(1);

            let play = Arc::new(RwLock::new(Button::new("Play")));
            layout.write().await.add(play);

            let quit = Arc::new(RwLock::new(Button::new("Quit")));
            layout.write().await.add(quit);

            Arc::new(RwLock::new(Panel::new(layout)))
        };
        {
            let gap = || StaticSpring::new(LengthRequirements::any().preferred(0));

            layout.write().await.add(buttons.clone())

             .take(Edge::Left, EdgesOf::Parent)
                .modify(|s| s + gap())
                .hook(Edge::Left, EdgesOf::Child(buttons.clone()))
                .await

             .take(Edge::Top, EdgesOf::Parent)
                .modify(|s| s + gap())
                .hook(Edge::Top, EdgesOf::Child(buttons.clone()))
                .await

             .take(Edge::Right, EdgesOf::Child(buttons.clone()))
                .modify(|s| s + gap())
                .hook(Edge::Right, EdgesOf::Parent)
                .await

             .take(Edge::Bottom, EdgesOf::Child(buttons.clone()))
                .modify(|s| s + gap())
                .hook(Edge::Bottom, EdgesOf::Parent)
                .await;
        }
        Arc::new(RwLock::new(Panel::new(layout)))
    };
    layout.write().await.add(buttons_outer);

    let mut tk = ctk::Ctk::initiate(layout).unwrap();
    tk.step().await;
    tk.step().await;
}

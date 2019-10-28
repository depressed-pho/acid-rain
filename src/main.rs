#[macro_use]
extern crate clap;

use clap::{Arg, ArgMatches};

mod ctk;
mod world;

fn opt_matches<'a>() -> ArgMatches<'a> {
    app_from_crate!()
        .get_matches()
}

fn main() {
    let matches = opt_matches();

    //let w = world::World::new();

    let tk = ctk::Ctk::initiate();
}

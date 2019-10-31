mod ctk;
mod world;

#[macro_use]
extern crate clap;

#[macro_use]
extern crate rental;

use clap::{Arg, ArgMatches};

fn opt_matches<'a>() -> ArgMatches<'a> {
    app_from_crate!()
        .get_matches()
}

fn main() {
    let matches = opt_matches();

    //let w = world::World::new();

    let tk = ctk::Ctk::initiate();
}

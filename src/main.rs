mod ctk;
mod world;

#[macro_use]
extern crate clap;

#[macro_use]
extern crate lazy_static;

use clap::{Arg, ArgMatches};

fn opt_matches<'a>() -> ArgMatches<'a> {
    app_from_crate!()
        .get_matches()
}

fn main() {
    let matches = opt_matches();

    //let w = world::World::new();

    let mut tk = ctk::Ctk::initiate().unwrap();
    tk.main();
}

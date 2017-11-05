#[macro_use] extern crate nom;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate termion;

mod data;
mod input;
mod parse;
mod editor;
mod environment;

use environment::Environment;

fn init_environment() -> Environment {
    let mut env = Environment::new();

    {
        let mut excl = env.exclusive();
    }

    env
}

fn main() {
    let env = init_environment();
    let mut term = match input::Terminal::new() {
        Ok(x)   => x,
        Err(e)  => unimplemented!()
    };

    loop {
        let cmd = match term.read() {
            Ok(x)  => x,
            Err(_) => unimplemented!()
        };
    }
}

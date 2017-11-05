#[macro_use] extern crate nom;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate termion;

mod data;
mod input;
mod parse;
mod environment;

use environment::Environment;

fn init_environment() -> Environment {
    let mut env = Environment::new();
    let mut excl = env.exclusive();
}

fn main() {
    let env = init_environment();
    let mut term = input::Terminal::new();
}

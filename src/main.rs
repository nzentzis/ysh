#[macro_use] extern crate nom;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate termion;

mod data;
mod input;
mod parse;

fn main() {
    let mut term = input::Terminal::new();
}

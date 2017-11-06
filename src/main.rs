#[macro_use] extern crate nom;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate termion;

mod data;
mod input;
mod parse;
mod editor;
mod evaluate;
mod environment;

use environment::Environment;
use evaluate::evaluate;
use data::{Value, Executable};

fn get_initial_paths() -> Value {
    use std::env;

    match env::var_os("PATH") {
        // TODO: add Bytes type and generate those here
        Some(paths) =>
            Value::List(env::split_paths(&paths)
                            .map(|p| p.to_str().unwrap().to_owned())
                            .map(Value::Str)
                            .collect()),
        None => Value::List(vec![Value::Str(String::from("/bin")),
                                 Value::Str(String::from("/usr/bin")),
                                 Value::Str(String::from("/usr/local/bin"))])
    }
}

fn locate_executable(env: &Environment, args: &[Value]) -> Value {
    use std::path::Path;

    if args.len() == 0 {
        return Value::empty();
    }

    let paths = if let Some(p) = env.get("path") { p }
                else { return Value::empty() };
    let paths = (*paths).to_owned().into_seq();
    let paths: Vec<_> = paths.into_iter().map(Value::into_str).collect();
    
    // search for the requested files
    let mut res = Vec::with_capacity(args.len());
    for f in args {
        let f = f.clone().into_str();
        for p in paths.iter() {
            let pth = Path::new(p).join(&f);
            if pth.exists() {
                // TODO: handle bytes conversion
                res.push(Value::Str(pth.to_str().unwrap().to_owned()));
                break;
            }
        }
    }
    Value::List(res)
}

fn init_environment() -> Environment {
    let mut env = Environment::new();
    let copy = env.clone();

    {
        let mut excl = env.exclusive();

        excl.set("test-function", Value::Function(copy.clone(),
            Executable::native(|_, args| {
                println!("test function! {:?}", args);
                Value::List(vec![])
            })));

        excl.set("print", Value::Function(copy.clone(),
            Executable::native(|_, args| {
                println!("{:?}", args);
                Value::List(vec![])
            })));

        excl.set("shell/locate", Value::Function(copy.clone(),
            Executable::native(locate_executable)));

        // set executable path
        excl.set("path", get_initial_paths());
    }

    env
}

fn main() {
    let mut env = init_environment();
    let mut term = match input::Terminal::new() {
        Ok(x)   => x,
        Err(e)  => unimplemented!()
    };

    loop {
        let cmd = match term.read() {
            Ok(x)  => x,
            Err(_) => unimplemented!()
        };
        println!("{:?}", cmd);
        evaluate(&mut env, cmd);
        break;
    }
}

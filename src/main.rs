#[macro_use] extern crate nom;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate termion;
extern crate libc;

#[allow(dead_code)] mod data;
mod input;
mod parse;
mod evaluate;
mod pipeline;
#[allow(dead_code)] mod editor;
#[allow(dead_code)] mod environment;

use std::sync::atomic::{AtomicBool, Ordering, ATOMIC_BOOL_INIT};

use environment::{Environment, global, empty};
use pipeline::execute_pipeline;
use data::{Value, Executable};

static RUN_SHELL: AtomicBool = ATOMIC_BOOL_INIT;

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

fn init_environment() {
    let env = global();
    env.set("print", Value::Function(empty(),
        Executable::native(|_, args| {
            println!("{:?}", args);
            Value::empty()
        })));

    env.set("shell/locate", Value::Function(empty(),
        Executable::native(locate_executable)));

    env.set("exit", Value::Function(empty(),
        Executable::native(|_,_| {
            RUN_SHELL.store(false, Ordering::Relaxed);
            Value::empty() })));

    // recovery function to restore the system environment in case something got
    // seriously borked
    env.set_immut("sys/recover", Value::Function(empty(),
        Executable::native(|_,_| {
            init_environment();
            Value::empty() })));

    // set executable path
    env.set("path", get_initial_paths());
}

fn main() {
    // become group/session leader
    let _sid = unsafe { libc::setsid() };

    RUN_SHELL.store(true, Ordering::Relaxed);

    init_environment();

    let mut term = match input::Terminal::new() {
        Ok(x)   => x,
        Err(_)  => unimplemented!()
    };

    while RUN_SHELL.load(Ordering::Relaxed) {
        let cmd = match term.read() {
            Ok(x)  => x,
            Err(_) => unimplemented!()
        };
        println!("\n{:?}", cmd);
        execute_pipeline(cmd);
        break;
    }
}

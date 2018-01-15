#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate termion;
extern crate libc;
extern crate nix;

// base modules
#[macro_use] mod util;
#[allow(dead_code)] mod data;
mod numeric;

// global namespace support and standard library
#[allow(dead_code)] mod globals;
#[allow(dead_code)] mod environment;
mod docs;
mod library;

// input & user interaction
mod reader;
#[allow(dead_code)] mod stream;
#[allow(dead_code)] mod span;
#[allow(dead_code)] mod editor;
mod input;

// runtime control and management
#[allow(dead_code)] mod jobs;
mod evaluate;
#[allow(dead_code)] mod pipeline;
mod history;

use std::io;
use std::process::exit;
use std::sync::atomic::{AtomicBool, Ordering, ATOMIC_BOOL_INIT};

use nix::unistd;
use nix::sys::signal;

use environment::{Environment, global};
use pipeline::{Plan, PlanningError};
use data::{Value, ValueLike, Executable, EvalResult, Eval};
use reader::ParseError;

static RUN_SHELL: AtomicBool = ATOMIC_BOOL_INIT;

fn get_initial_paths() -> Value {
    use std::env;

    match env::var_os("PATH") {
        // TODO: add Bytes type and generate those here
        Some(paths) =>
            Value::list(env::split_paths(&paths)
                            .map(|p| p.to_str().unwrap().to_owned())
                            .map(Value::str)),
        None => Value::list(vec![Value::str("/bin"),
                                      Value::str("/usr/bin"),
                                      Value::str("/usr/local/bin")])
    }
}

fn locate_executable(env: &Environment, args: &[Value]) -> EvalResult {
    use std::path::Path;

    if args.len() == 0 {
        // allow use as a transformer
        return Ok(Value::from(Executable::native(locate_executable)));
    }

    let paths = if let Some(p) = env.get("path") { p }
                else { return Ok(Value::empty()) };
    let paths: Vec<String> = paths.into_seq()?
                                  .into_iter()
                                  .map(|x| x.into_str())
                                  .collect::<Eval<Vec<String>>>()?;
    
    // search for the requested files
    let mut res = Vec::with_capacity(args.len());
    for f in args {
        let f = f.clone().into_str()?;
        for p in paths.iter() {
            let pth = Path::new(p).join(&f);
            if pth.exists() {
                // TODO: handle bytes conversion
                res.push(Value::str(pth.to_str().unwrap()));
                break;
            }
        }
    }
    Ok(Value::list(res))
}

struct EnvProxy(::std::sync::Mutex<Value>);

impl EnvProxy {
    fn new() -> Self {
        // TODO: switch this to a bytes type once one is available
        let v = Value::map(
            ::std::env::vars_os()
                .map(|(k,v)| {
                    let k = Value::str(k.to_string_lossy())
                                  .hash()
                                  .unwrap() // direct strings are hashable
                                  .unwrap();
                    let v = Value::str(v.to_string_lossy());
                    (k,v)}));
        EnvProxy(::std::sync::Mutex::new(v))
    }
}

impl environment::BindingProxy for EnvProxy {
    fn get(&self) -> Value {
        self.0.lock().unwrap().to_owned()
    }

    fn set(&self, val: Value) {
        let mut current_val = self.0.lock().unwrap();
        let current = &mut current_val.data;
        let current = if let &mut data::ValueData::Map(ref mut h) = current {h}
                      else {panic!("invalid environment storage")};

        // try to convert into a hashmap
        let mut val = if let data::ValueData::Map(h) = val.data {h}
                      else {return};

        // compute delta between it and our current value by removing any
        // differing keys
        val.retain(|k,v| current.get(&k) != Some(v));

        for (k,v) in val.iter() {
            // abort on conversion fail
            let k = if let Ok(k) = k.val.into_str() {k}
                    else {return};
            let v = if let Ok(v) = v.into_str() {v}
                    else {return};
            ::std::env::set_var(k, v);
        }
    }
}

fn init_environment() {
    library::initialize();

    let env = global();
    env.set("print", Value::from(Executable::native(|_, _| {
            //println!("{:?}", args);
            println!("value!");
            Ok(Value::empty())
        })));

    env.set("print-lines", Value::from(Executable::native(|_, args| {
            for a in args {
                for i in a.into_iter() {
                    println!("{}", i?.into_str()?);
                }
            }
            Ok(Value::empty())
        })));

    env.set("dbg", Value::from(Executable::native(|_, args| {
            for a in args {
                for i in a.into_iter() {
                    println!("{:?}", i?);
                }
            }
            Ok(Value::empty())
        })));

    env.set("shell/locate", Value::from(
        Executable::native(locate_executable)));

    /*
    env.set("try-read", Value::from(
        Executable::native(|_,_| reader::read(&mut ::std::io::stdin())
                                .map_err(ParseError::to_eval)
                           )));

    env.set("try-read-pl", Value::from(
        Executable::native(|_,_| reader::read_pipeline(&mut ::std::io::stdin())
                                .and_then(|x| {
                                    println!("{:?}", x);
                                    Ok(Value::empty())
                                })
                                .map_err(ParseError::to_eval)
                           )));
    */

    env.set("exit", Value::from(Executable::native(|_,_| {
            RUN_SHELL.store(false, Ordering::Relaxed);
            Ok(Value::empty()) })));

    // recovery function to restore the system environment in case something got
    // seriously borked
    env.set_immut("sys/recover", Value::from(Executable::native(|_,_| {
            init_environment();
            Ok(Value::empty()) })));

    // set executable path
    env.set("path", get_initial_paths());

    // create virtual environment map
    env.set_proxy("env", EnvProxy::new());

    // create virtual history list
    env.set_proxy("history", history::HistoryProxy{});
}

fn init_process_group() -> Result<(), nix::Error> {
    // handle process group initialization
    if termion::is_tty(&io::stdin()) {
        globals::enable_job_control();

        // move to the foreground
        loop {
            let pgrp = unistd::tcgetpgrp(libc::STDIN_FILENO)
                      .map_err(|e| {
                          eprintln!("ysh: failed to get process group: {}", e);
                          e
                      })?;
            let shell_pgid = unistd::getpgrp();

            if shell_pgid == pgrp { break; }
            signal::kill(shell_pgid, signal::SIGTTIN)
                   .map_err(|e| {
                       eprintln!("ysh: cannot send SIGTTIN: {}", e);
                       e
                   })?;
        }

        // Ignore job control signals
        //
        // this is safe since we're ignoring them rather than installing
        // handlers
        unsafe {
            let ignore = signal::SigAction::new(signal::SigHandler::SigIgn,
                                                signal::SA_RESTART,
                                                signal::SigSet::empty());
            signal::sigaction(signal::SIGINT,  &ignore)?;
            signal::sigaction(signal::SIGQUIT, &ignore)?;
            signal::sigaction(signal::SIGTSTP, &ignore)?;
            signal::sigaction(signal::SIGTTIN, &ignore)?;
            signal::sigaction(signal::SIGTTOU, &ignore)?;
            //signal::sigaction(signal::SIGCHLD, &ignore)?;
        }

        // take ownership of the process group
        let pgid = unistd::getpid();
        unistd::setpgid(pgid, pgid)
               .map_err(|e| {
                   eprintln!("ysh: failed to move to own process group: {}", e);
                   e
               })?;

        // and take control of the terminal
        unistd::tcsetpgrp(libc::STDIN_FILENO, pgid)?;
    }

    Ok(())
}

fn main() {
    if let Err(_) = init_process_group() {
        exit(1);
    }

    RUN_SHELL.store(true, Ordering::Relaxed);

    init_environment();

    if cfg!(debug_assertions) {
        for (k,v) in environment::global().listing() {
            if v.doc.is_none() {
                println!("warning: global item '{}' has no documentation", k);
            }
        }
    }

    let mut term = match input::Terminal::new() {
        Ok(x)   => x,
        Err(_)  => unimplemented!()
    };

    while RUN_SHELL.load(Ordering::Relaxed) {
        let cmd = match term.read() {
            Ok(x)  => x,
            Err(_) => unimplemented!()
        };

        history::db().record(&cmd);

        let plan = match Plan::plan_for(cmd) {
            Ok(r) => r,
            Err(e) => {
                match e {
                    PlanningError::MultipleInputs  =>
                        eprintln!("ysh: failed to plan job: too many inputs"),
                    PlanningError::MultipleOutputs =>
                        eprintln!("ysh: failed to plan job: too many outputs"),
                    PlanningError::NotFound => {},
                    PlanningError::FrozenPipeline =>
                        panic!("unexpected frozen pipeline error")
                }
                continue;
            }
        };
        //println!("\r{:?}", plan);

        match plan.launch(false) {
            Ok(x) => x.wait(),
            Err(e) => {
                eprintln!("ysh: pipeline launch error: {:?}", e)
            }
        }
    }
}

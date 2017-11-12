#[macro_use] extern crate nom;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate termion;
extern crate libc;
extern crate nix;

#[allow(dead_code)] mod data;
mod jobs;
mod globals;
mod input;
mod parse;
mod evaluate;
mod pipeline;
#[allow(dead_code)] mod editor;
#[allow(dead_code)] mod environment;

use std::io;
use std::process::exit;
use std::sync::atomic::{AtomicBool, Ordering, ATOMIC_BOOL_INIT};

use nix::unistd;
use nix::sys::signal;

use environment::{Environment, global, empty};
use pipeline::{Plan, PlanningError};
use data::{Value, ValueLike, Executable};

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
    let paths = (*paths).to_owned();
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
            signal::sigaction(signal::SIGCHLD, &ignore)?;
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

    let mut term = match input::Terminal::new() {
        Ok(x)   => x,
        Err(_)  => unimplemented!()
    };

    while RUN_SHELL.load(Ordering::Relaxed) {
        let cmd = match term.read() {
            Ok(x)  => x,
            Err(_) => unimplemented!()
        };

        let plan = match Plan::generate(cmd) {
            Ok(r) => r,
            Err(e) => {
                match e {
                    PlanningError::MultipleInputs  =>
                        eprintln!("ysh: failed to plan job: too many inputs"),
                    PlanningError::MultipleOutputs =>
                        eprintln!("ysh: failed to plan job: too many outputs"),
                    PlanningError::NotFound => {}
                }
                continue;
            }
        };
        println!("\n{:?}", plan);

        plan.launch(false).wait();
    }
}

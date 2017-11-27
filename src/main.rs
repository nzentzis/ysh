#[macro_use] extern crate nom;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate termion;
extern crate libc;
extern crate nix;

mod numeric;
#[allow(dead_code)] mod data;
#[allow(dead_code)] mod globals;
#[allow(dead_code)] mod environment;

mod library;

#[allow(dead_code)] mod stream;
#[allow(dead_code)] mod span;

#[allow(dead_code)] mod editor;
mod input;
mod parse;

#[allow(dead_code)] mod jobs;
mod evaluate;
#[allow(dead_code)] mod pipeline;

use std::io;
use std::process::exit;
use std::sync::atomic::{AtomicBool, Ordering, ATOMIC_BOOL_INIT};

use nix::unistd;
use nix::sys::signal;

use environment::{Environment, global, empty};
use pipeline::{Plan, PlanningError};
use data::{Value, ValueLike, BasicValue, Executable, EvalResult};

static RUN_SHELL: AtomicBool = ATOMIC_BOOL_INIT;

fn get_initial_paths() -> Value {
    use std::env;

    match env::var_os("PATH") {
        // TODO: add Bytes type and generate those here
        Some(paths) =>
            BasicValue::list(env::split_paths(&paths)
                            .map(|p| p.to_str().unwrap().to_owned())
                            .map(BasicValue::str)),
        None => BasicValue::list(vec![BasicValue::str("/bin"),
                                      BasicValue::str("/usr/bin"),
                                      BasicValue::str("/usr/local/bin")])
    }
}

fn locate_executable(env: &Environment, args: &[Value]) -> EvalResult {
    use std::path::Path;

    if args.len() == 0 {
        // allow use as a transformer
        return Ok(BasicValue::function(Executable::native(locate_executable)));
    }

    let paths = if let Some(p) = env.get("path") { p }
                else { return Ok(BasicValue::empty()) };
    let paths: Vec<_> = paths.into_iter().map(|x| x.into_str()).collect();
    
    // search for the requested files
    let mut res = Vec::with_capacity(args.len());
    for f in args {
        let f = f.clone().into_str();
        for p in paths.iter() {
            let pth = Path::new(p).join(&f);
            if pth.exists() {
                // TODO: handle bytes conversion
                res.push(BasicValue::str(pth.to_str().unwrap()));
                break;
            }
        }
    }
    Ok(BasicValue::list(res))
}

fn init_environment() {
    library::initialize();

    let env = global();
    env.set("print", BasicValue::function(Executable::native(|_, args| {
            //println!("{:?}", args);
            println!("value!");
            Ok(BasicValue::empty())
        })));

    env.set("print-lines", BasicValue::function(Executable::native(|_, args| {
            for a in args {
                for i in a.into_iter() {
                    println!("{}", i.into_str());
                }
            }
            Ok(BasicValue::empty())
        })));

    env.set("shell/locate", BasicValue::function(
        Executable::native(locate_executable)));

    env.set("exit", BasicValue::function(Executable::native(|_,_| {
            RUN_SHELL.store(false, Ordering::Relaxed);
            Ok(BasicValue::empty()) })));

    // recovery function to restore the system environment in case something got
    // seriously borked
    env.set_immut("sys/recover", BasicValue::function(Executable::native(|_,_| {
            init_environment();
            Ok(BasicValue::empty()) })));

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
        //println!("\r{:?}", plan);

        if let Some(x) = plan.launch(false) {
            x.wait();
        }
    }
}

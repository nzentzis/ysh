use std::path::PathBuf;

use data::*;
use environment::{Environment, run_fn};

/// Try to find a command using values from the active environment
pub fn find_command(cmd: &str) -> Option<Vec<PathBuf>> {
    let r = run_fn("shell/locate", &[Value::str(cmd)])
        .map(|r| r.and_then(|x| x.into_seq())
             .and_then(|x| x.into_iter()
                            .map(|o| o.into_str())
                            .collect::<Eval<Vec<String>>>())
             .map(|x| x.into_iter()
                       .map(PathBuf::from)
                       .collect()));
    if let Some(r) = r {
        match r {
            Ok(r) => Some(r),
            Err(e) => {
                eprintln!("ysh: shell/locate failed: {}", e);
                None
            }
        }
    } else {
        // they somehow unbound shell/locate
        eprintln!("ysh: shell/locate not bound. try running `sys/recover`");
        None
    }
}

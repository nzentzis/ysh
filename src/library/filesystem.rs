use std::path::{Path, PathBuf};
use std::sync::{atomic, mpsc, Arc};
use std::env;
use std::io;

use environment::*;
use data::*;
use evaluate::*;

use crossbeam::queue::MsQueue;
use walkdir::{self, WalkDir};

extern crate globset;
use self::globset::GlobSet;

/// An iterator over matching paths
struct Globs {
    set: GlobSet,
    root: PathBuf,
    walker: walkdir::IntoIter,
}

impl Iterator for Globs {
    type Item = Result<PathBuf, walkdir::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        // this could probably be more efficient if we only walked paths which
        // *might* match the pattern
        while let Some(x) = self.walker.next() {
            let item = match x {
                Err(e) => return Some(Err(e)),
                Ok(r) => r
            };

            if self.set.is_match(item.path().strip_prefix(&self.root).unwrap()) {
                return Some(Ok(item.path().to_owned()));
            }
        }
        None
    }
}

/// Perform globbing relative to a base directory
///
/// Returns an iterator of all patterns matching any of the given globs. If any
/// glob is invalid, it will be ignored.
fn glob_relative<P: AsRef<Path>>(patterns: Vec<String>, base: P) -> Globs {
    use self::globset::{GlobBuilder, GlobSetBuilder};
    use std::thread;

    // build the glob set
    let mut set = GlobSetBuilder::new();
    for glob in patterns.into_iter().filter_map(|p| GlobBuilder::new(&p)
                                                   .literal_separator(true)
                                                   .build().ok()) {
        set.add(glob);
    }
    let set = set.build().unwrap(); // TODO: handle error

    Globs {
        set,
        root: base.as_ref().to_owned(),
        walker: WalkDir::new(base).into_iter()
    }
}

/// Perform globbing based on a specified pattern
/// 
/// With 1 arg: listify, stringify items, then expand glob for each one and
///             return concatenated results as a list
/// With 2 arg: listify first, stringify items, then expand glob from each
///             relative to path from stringified second arg. Return the same
///             thing as the 1-arg form.
///
/// If any of the elements are not valid glob patterns, they will be ignored.
fn fn_glob(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        // build the glob set
        let items = args[0].into_seq().wait()?;
        let items = items.into_iter().map(|x| x.into_str().wait())
                         .collect::<EvalRes<Vec<_>>>()?;
        let cwd = env::current_dir()?;
        let results = glob_relative(items, cwd)
                     .collect::<Result<Vec<_>, walkdir::Error>>()
                     .map_err(|_| EvalError::Runtime(String::from(
                                 "failed to walk directory")))?;
        Ok(Value::list(results.into_iter()
                      .map(|x| Value::str(x.to_str().unwrap()))))
    } else if args.len() == 2 {
        // build the glob set
        let items = args[0].into_seq().wait()?;
        let items = items.into_iter().map(|x| x.into_str().wait())
                         .collect::<EvalRes<Vec<_>>>()?;

        let loc = args[1].into_str().wait()?;
        let results = glob_relative(items, loc)
                     .collect::<Result<Vec<_>, walkdir::Error>>()
                     .map_err(|_| EvalError::Runtime(String::from(
                                 "failed to walk directory")))?;
        Ok(Value::list(results.into_iter()
                      .map(|x| Value::str(x.to_str().unwrap()))))
    } else {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        })
    }
}

pub fn initialize() {
    let env = global();

    env.set("fs/glob", Value::from(Executable::native(fn_glob)));
}

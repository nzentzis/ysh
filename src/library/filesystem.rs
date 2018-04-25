use std::path::PathBuf;

use environment::*;
use data::*;
use evaluate::*;

extern crate glob;

/// Perform globbing based on a specified pattern
/// 
/// With 1 arg: listify, stringify items, then expand glob for each one and
///             return concatenated results as a list
/// With 2 arg: listify first, stringify items, then expand glob from each
///             relative to path from stringified second arg. Return the same
///             thing as the 1-arg form.
fn fn_glob(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        let items = args[0].into_seq().wait()?;
        let items = items.into_iter().map(|x| x.into_str().wait())
                         .collect::<EvalRes<Vec<_>>>()?;
        let globbed = items.into_iter()
                           .map(|i| glob::glob(&i)
                             .map(|itr| itr.collect::<Result<Vec<_>,
                                                      glob::GlobError>>()
                                           .unwrap_or(vec![PathBuf::from(i)])))
                           .collect::<Result<Vec<_>, glob::PatternError>>();
        globbed.map(|res|
                    Value::list(res.into_iter()
                                   .flat_map(|x| x) // concat vecs
                                   .map(|x| Value::str(x.to_str().unwrap()))))
               .map_err(|_| EvalError::InvalidOperation("glob failed"))
    } else if args.len() == 2 {
        // TODO: implement this
        unimplemented!()
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

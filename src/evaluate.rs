use std::path::PathBuf;
use std::sync::Arc;
use std::iter::FromIterator;

use data::*;
use environment::{run_fn, Environment};

/// Try to find a command using values from the active environment
pub fn find_command(cmd: &str) -> Option<Vec<PathBuf>> {
    let r = run_fn("shell/locate", &[Value::str(cmd)])
        .map(|r| r.wait().and_then(|x| x.into_seq().wait())
             .and_then(|x| x.into_iter()
                            .map(|o| o.into_str().wait())
                            .collect::<Result<Vec<String>, _>>())
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

#[derive(Clone)]
pub enum Executable {
    /// Native function which acts like an interpreted one
    /// 
    /// Arguments will be evaluated *prior* to running the function. The
    /// function accepts a reference to the lexical environment at call time.
    Native(Arc<Fn(&Environment, &[Value]) -> Eval<Value> + Send + Sync>),

    /// Interpreted function
    /// 
    /// The inner function accepts a reference to the stored environment
    Interpreted(Environment, Arc<Fn(&Environment, &[Value]) -> Eval<Value> + Send + Sync>),

    /// Native implementation of a core form
    /// 
    /// Similar to the `Native` type, but arguments are not evaluated prior to
    /// running it. The function accepts a reference to the lexical environment
    /// at call time.
    CoreFn(fn(&Environment, &[Value]) -> Eval<Value>),
}

impl Executable {
    pub fn native<F>(f: F) -> Self
        where F: Fn(&Environment, &[Value])
            -> Result<Value, EvalError> + Send + Sync + 'static {
        Executable::Native(Arc::new(move |a,b| Eval::from(f(a,b))))
    }

    pub fn run(&self, lexical: &Environment, args: &[Value]) -> Eval<Value> {
        match self {
            &Executable::Native(ref f) => {
                let vals: Result<Vec<_>, EvalError> =
                    args.iter().map(|x| x.evaluate(lexical).wait()).collect();
                match vals {
                    Ok(vals) => f(lexical, vals.as_slice()),
                    Err(e)   => Eval::Direct(Err(e))
                }
            },
            &Executable::CoreFn(ref f) => f(lexical, args),
            &Executable::Interpreted(ref env, ref f) => {
                let vals: Result<Vec<_>, EvalError> =
                    args.iter().map(|x| x.evaluate(lexical).wait()).collect();
                match vals {
                    Ok(vals) => f(env, vals.as_slice()),
                    Err(e)   => Eval::Direct(Err(e))
                }
            }
        }
    }
}

pub enum Eval<T> {
    Direct(Result<T, EvalError>),
    Indirect(Box<FnMut() -> Eval<T>>)
}

impl<T> Eval<T> {
    pub fn wait(self) -> Result<T, EvalError> {
        let mut current = self;
        loop {
            current = match current {
                Eval::Direct(r) => break r,
                Eval::Indirect(mut f) => f()
            };
        }
    }
}

impl<T> From<Result<T, EvalError>> for Eval<T> {
    fn from(r: Result<T, EvalError>) -> Self {Eval::Direct(r)}
}

impl<T> FromIterator<Eval<T>> for Eval<Vec<T>> {
    fn from_iter<I>(iter: I) -> Self where I: IntoIterator<Item=Eval<T>> {
        Eval::Direct(iter.into_iter().map(|x| x.wait()).collect())
    }
}

pub type EvalRes<T> = Result<T, EvalError>;

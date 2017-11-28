use std::path::PathBuf;

use data::*;
use environment::{Environment, empty, global};

/*
pub trait Evaluable {
    /// Try to evaluate this value in the given environment
    fn eval_in(self, env: &mut Environment) -> Value;
}

impl Evaluable for Value {
    fn eval_in(self, env: &mut Environment) -> Value {
        use std::ops::Deref;

        match self {
            Value::Boolean(x)   => Value::Boolean(x),
            Value::Number(x)    => Value::Number(x),
            Value::Str(x)       => Value::Str(x),
            Value::Symbol(x)    => {
                // look it up in the environment
                if let Some(v) = env.get(x.0.deref()) {
                    v.deref().to_owned()
                } else {
                    Value::Symbol(x)
                }
            },
            Value::List(mut xs) => {
                // evaluate () as ()
                if xs.is_empty() {
                    Value::List(xs)
                } else {
                    let args: Vec<_> =
                               xs.split_off(1)
                                 .into_iter()
                                 .map(|x| x.eval_in(env))
                                 .collect();
                    let exec = xs.pop().unwrap().eval_in(env);

                    execute(exec, args, env)
                }
            },
            Value::Function(e,x)=> Value::Function(e,x),
            Value::Wrapped(e)   => Value::Wrapped(e)
        }
    }
}
*/

/// Try to find a command using values from the active environment
pub fn find_command(cmd: &str) -> Option<Vec<PathBuf>> {
    if let Some(locate) = global().get("shell/locate") {
        let r = locate.execute(&empty(), &vec![BasicValue::str(cmd)])
                      .and_then(|x| x.into_seq())
                      .and_then(|x| x.into_iter()
                                     .map(|o| o.into_str())
                                     .collect::<Eval<Vec<String>>>()
                                     .map(|v| v.into_iter()
                                               .map(PathBuf::from)
                                               .collect()));
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

/*
/// Evalute a value in an executable context
pub fn execute(val: Value, args: Vec<Value>, env: &Environment) -> Value {
    // TODO: handle lexical scoping
    if val.is_executable() {
        val.execute(args).unwrap()
    } else {
        /*
        // stringify it and try finding it
        let search_obj = val.to_owned().into_str();
        let search_res = find_command(&search_obj);
        if search_res.len() > 1 {
            eprintln!("ysh: more than one candidate for '{}'", search_obj);
            return Value::empty();
        } else if search_res.len() == 0 {
            eprintln!("ysh: command not found: {}", search_obj);
            return Value::empty();
        } else {
            unimplemented!()
        }
        */
        let mut args = args;
        let mut l = vec![val];
        l.append(&mut args);
        Value::List(l)
    }
}
*/

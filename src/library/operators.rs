use environment::*;
use numeric::*;
use data::*;

fn fn_add(env: &Environment, args: &[Value]) -> EvalResult {
    // require that all args are numbers
    // TODO: handle results using exceptions
    let mut ns = Vec::new();
    for i in args.iter() {
        match i.get_basic() {
            Some(&BasicValue::Number(ref n)) => ns.push(n.to_owned()),
            r => return Err(EvalError::TypeError(String::from("non-numeric adds not yet implemented")))
        }
    }

    Ok(Value::new(BasicValue::Number(ns.into_iter().fold(Number::int(0), |a,b| a+b))))
}

pub fn initialize() {
    let env = global();
    env.set("+", BasicValue::function(empty(), Executable::native(fn_add)));
}

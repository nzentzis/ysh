use environment::*;
use numeric::*;
use data::*;

fn fn_add(env: &Environment, args: &[Value]) -> Value {
    // require that all args are numbers
    // TODO: use type trait system
    // TODO: handle results using exceptions
    let mut ns = Vec::new();
    for i in args.iter() {
        match i {
            &Value::Number(ref n) => ns.push(n.to_owned()),
            _ => panic!("type error")
        }
    }

    Value::Number(ns.into_iter().fold(Number::int(0), |a,b| a+b))
}

pub fn initialize() {
    let env = global();
    env.set("+", Value::Function(empty(), Executable::native(fn_add)));
}

use environment::*;
use numeric::*;
use data::*;

fn fn_add(env: &Environment, args: &[Value]) -> EvalResult {
    // require that all args are numbers
    let mut ns = Vec::new();
    for i in args.iter() {
        match i.into_num() {
            Some(n) => ns.push(n),
            r => return Err(EvalError::TypeError(String::from("non-numeric adds not yet implemented")))
        }
    }

    Ok(Value::new(BasicValue::Number(ns.into_iter().fold(Number::int(0), |a,b| a+b))))
}

fn fn_inc(env: &Environment, args: &[Value]) -> EvalResult {
    // require that all args are numbers
    let mut rs = Vec::new();
    let one = Number::int(1);
    for i in args.iter() {
        match i.into_num() {
            Some(n) => rs.push(n + one.clone()),
            r => return Err(EvalError::TypeError(String::from("non-numeric adds not yet implemented")))
        }
    }

    let rs: Vec<_> = rs.into_iter()
                       .map(BasicValue::Number)
                       .map(Value::new)
                       .collect();
    if rs.len() == 1 { Ok(rs.into_iter().next().unwrap()) }
    else { Ok(BasicValue::list(rs)) }
}

pub fn initialize() {
    let env = global();
    env.set("+", BasicValue::function(empty(), Executable::native(fn_add)));

    env.set("inc", BasicValue::function(empty(), Executable::native(fn_inc)));
}

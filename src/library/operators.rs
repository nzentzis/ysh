use environment::*;
use numeric::*;
use data::*;

fn fn_add(env: &Environment, args: &[Value]) -> EvalResult {
    // require that all args are numbers
    let mut ns = Vec::new();
    for i in args.iter() {
        match i.into_num()? {
            Some(n) => ns.push(n),
            r => return Err(EvalError::TypeError(String::from("non-numeric adds not yet implemented")))
        }
    }

    Ok(Value::new(BasicValue::Number(ns.into_iter().fold(Number::int(0), |a,b| a+b))))
}

/// If used with one arg, negate it
/// If more than one, subtract the second and on from the first
fn fn_sub(env: &Environment, args: &[Value]) -> EvalResult {
    if args.is_empty() {
        Err(EvalError::Arity {
            got: 0,
            expected: 1
        })
    } else if args.len() == 1 {
        let n = if let Some(n) = args[0].into_num()? { n }
                else { return Err(EvalError::TypeError(String::from("cannot negate non-numeric type"))) };
        Ok(Value::new(BasicValue::Number(-n)))
    } else {
        let mut n =
            if let Some(n) = args[0].into_num()? { n }
            else { return Err(EvalError::TypeError(
                        String::from("cannot subtract from non-numeric type"))) };

        for i in args[1..].iter() {
            if let Some(x) = i.into_num()? {
                n -= x;
            } else {
                return Err(EvalError::TypeError(String::from("cannot subtract non-numeric type")))
            }
        }

        Ok(Value::new(BasicValue::Number(n)))
    }
}

fn fn_inc(env: &Environment, args: &[Value]) -> EvalResult {
    // require that all args are numbers
    let mut rs = Vec::new();
    let one = Number::int(1);
    for i in args.iter() {
        match i.into_num()? {
            Some(n) => rs.push(n + &one),
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
    env.set("+", BasicValue::function(Executable::native(fn_add)));
    env.set("-", BasicValue::function(Executable::native(fn_sub)));

    env.set("inc", BasicValue::function(Executable::native(fn_inc)));
}

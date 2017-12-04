use environment::*;
use numeric::*;
use data::*;

fn fn_add(_: &Environment, args: &[Value]) -> EvalResult {
    // require that all args are numbers
    let mut ns = Vec::new();
    for i in args.iter() {
        match i.into_num()? {
            Some(n) => ns.push(n),
            _ => return Err(EvalError::TypeError(
                    String::from("non-numeric adds not yet implemented")))
        }
    }

    Ok(Value::from(ns.into_iter().fold(Number::int(0), |a,b| a+b)))
}

/// If used with one arg, negate it
/// If more than one, subtract the second and on from the first
fn fn_sub(_: &Environment, args: &[Value]) -> EvalResult {
    if args.is_empty() {
        Err(EvalError::Arity {
            got: 0,
            expected: 1
        })
    } else if args.len() == 1 {
        let n = if let Some(n) = args[0].into_num()? { n }
                else { return Err(EvalError::TypeError(String::from("cannot negate non-numeric type"))) };
        Ok(Value::from(-n))
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

        Ok(Value::from(n))
    }
}

fn fn_inc(_: &Environment, args: &[Value]) -> EvalResult {
    // require that all args are numbers
    let mut rs = Vec::new();
    let one = Number::int(1);
    for i in args.iter() {
        match i.into_num()? {
            Some(n) => rs.push(n + &one),
            _ => return Err(EvalError::TypeError(
                    String::from("non-numeric adds not yet implemented")))
        }
    }

    let rs: Vec<_> = rs.into_iter()
                       .map(Value::from)
                       .collect();
    if rs.len() == 1 { Ok(rs.into_iter().next().unwrap()) }
    else { Ok(Value::list(rs)) }
}

/// Compare two things for equality
/// 
/// With 1 arg: produce function which compares to the arg
/// With 2 args: compare arguments
fn fn_equal(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        let tgt = args[0].clone();
        Ok(Value::from(Executable::native(move |_,args| {
            if args.len() != 1 {
                Err(EvalError::Arity {got: args.len(), expected: 1})
            } else {
                Ok(Value::from(tgt == args[0]))
            }
        })))
    } else if args.len() == 2 {
        Ok(Value::from(args[0] == args[1]))
    } else {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 2
        })
    }
}

pub fn initialize() {
    let env = global();
    env.set("+", Value::from(Executable::native(fn_add)));
    env.set("-", Value::from(Executable::native(fn_sub)));
    env.set("=", Value::from(Executable::native(fn_equal)));

    env.set("inc", Value::from(Executable::native(fn_inc)));
}

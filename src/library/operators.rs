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

    Ok(Value::from(ns.into_iter().fold(Number::int(0), |a,b| a+b).simplify()))
}

fn fn_mul(_: &Environment, args: &[Value]) -> EvalResult {
    // require that all args are numbers
    let mut ns = Vec::new();
    for i in args.iter() {
        match i.into_num()? {
            Some(n) => ns.push(n),
            _ => return Err(EvalError::TypeError(
                    String::from("non-numeric multiply not yet implemented")))
        }
    }

    Ok(Value::from(ns.into_iter().fold(Number::int(1), |a,b| a*b).simplify()))
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
                else { return Err(EvalError::TypeError(
                            String::from("cannot negate non-numeric type"))) };
        Ok(Value::from(-n))
    } else {
        let mut n =
            if let Some(n) = args[0].into_num()? { n }
            else { return Err(EvalError::TypeError(
                        String::from("cannot subtract from non-numeric type")))};

        for i in args[1..].iter() {
            if let Some(x) = i.into_num()? {
                n -= x;
            } else {
                return Err(EvalError::TypeError(
                        String::from("cannot subtract non-numeric type")))
            }
        }

        Ok(Value::from(n.simplify()))
    }
}

/// Divide the first argument by the second
fn fn_div(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 2 {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 2
        })
    } else {
        let m = if let Some(n) = args[0].into_num()? { n }
                else { return Err(EvalError::TypeError(
                        String::from("cannot divide non-numeric type")))};
        let n = if let Some(n) = args[1].into_num()? { n }
                else { return Err(EvalError::TypeError(
                        String::from("cannot divide non-numeric type")))};

        Ok(Value::from((m/n).simplify()))
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
                       .map(|x| x.simplify())
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

/// Return whether all passed arguments are integers
fn int_q(_: &Environment, args: &[Value]) -> EvalResult {
    Ok(Value::from(args.iter().map(|x| x.into_num())
                       .collect::<Eval<Vec<_>>>()?.into_iter()
                       .collect::<Option<Vec<_>>>()
                       .map(|v| v.into_iter().all(|n| n.is_integer()))
                       .unwrap_or(false)))
}

/// Return whether all passed arguments are rationals
fn rational_q(_: &Environment, args: &[Value]) -> EvalResult {
    Ok(Value::from(args.iter().map(|x| x.into_num())
                       .collect::<Eval<Vec<_>>>()?.into_iter()
                       .collect::<Option<Vec<_>>>()
                       .map(|v| v.into_iter().all(|n| n.is_rational()))
                       .unwrap_or(false)))
}

/// Return whether all passed arguments are reals
fn real_q(_: &Environment, args: &[Value]) -> EvalResult {
    Ok(Value::from(args.iter().map(|x| x.into_num())
                       .collect::<Eval<Vec<_>>>()?.into_iter()
                       .collect::<Option<Vec<_>>>()
                       .map(|v| v.into_iter().all(|n| n.is_real()))
                       .unwrap_or(false)))
}

/// Return whether all passed arguments are complexs
fn complex_q(_: &Environment, args: &[Value]) -> EvalResult {
    Ok(Value::from(args.iter().map(|x| x.into_num())
                       .collect::<Eval<Vec<_>>>()?.into_iter()
                       .collect::<Option<Vec<_>>>()
                       .map(|v| v.into_iter().all(|n| n.is_complex()))
                       .unwrap_or(false)))
}

pub fn initialize() {
    let env = global();
    env.set("+", Value::from(Executable::native(fn_add)));
    env.set("-", Value::from(Executable::native(fn_sub)));
    env.set("*", Value::from(Executable::native(fn_mul)));
    env.set("/", Value::from(Executable::native(fn_div)));

    // comparisons
    env.set("=", Value::from(Executable::native(fn_equal)));

    // numeric type queries
    env.set("int?", Value::from(Executable::native(int_q)));
    env.set("rational?", Value::from(Executable::native(rational_q)));
    env.set("real?", Value::from(Executable::native(real_q)));
    env.set("complex?", Value::from(Executable::native(complex_q)));

    env.set("inc", Value::from(Executable::native(fn_inc)));
}

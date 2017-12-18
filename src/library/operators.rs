use environment::*;
use numeric::*;
use data::*;

lazy_static! {
    static ref DOC_ADD: Documentation = Documentation::new()
        .origin("ops")
        .form(&["exprs*"])
        .short("Return the sum of the passed values")
        .desc("Evaluate the passed exprs and convert the results to numbers. \
               Return the sum of all passed numbers, or 0 if called with no \
               arguments.");

    static ref DOC_MUL: Documentation = Documentation::new()
        .origin("ops")
        .form(&["exprs*"])
        .short("Return the product of the passed values")
        .desc("Evaluate the passed exprs and convert the results to numbers. \
               Return the product of all passed numbers, or 1 if called with \
               no arguments.");

    static ref DOC_SUB: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr"])
        .form(&["exprs+"])
        .short("Subtract or negate values")
        .desc("Evaluate the passed exprs and convert the results to numbers. \
               If one argument is given, negate the value and return it.\n \
               If more than one argument is given, subtract the later values \
               from the first one and return the result.");

    static ref DOC_DIV: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr expr"])
        .short("Divide the first numeric value by the second");

    static ref DOC_INC: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr"])
        .form(&["exprs+"])
        .short("Increment numeric value(s)")
        .desc("With 1 argument, increment the value and return it. Otherwise, \
               increment each argument and return them as a list.");

    static ref DOC_EQUAL: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr"])
        .form(&["expr expr"])
        .short("Compare two values for equality")
        .desc("With 1 argument, return a function of one argument which will \
               return whether its input is equal to the original argument.\n \
               With 2 arguments, return whether they are equal.");

    static ref DOC_INTQ: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr*"])
        .short("Return whether all arguments are integers");

    static ref DOC_RATIONALQ: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr*"])
        .short("Return whether all arguments are rationals");

    static ref DOC_REALQ: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr*"])
        .short("Return whether all arguments are reals");

    static ref DOC_COMPLEXQ: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr*"])
        .short("Return whether all arguments are complex numbers");

    static ref DOC_LT: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr*"])
        .short("Return whether values are in monotonically increasing order");

    static ref DOC_GT: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr*"])
        .short("Return whether values are in monotonically decreasing order");

    static ref DOC_LE: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr*"])
        .short("Return whether values are in monotonically non-decreasing order");

    static ref DOC_GE: Documentation = Documentation::new()
        .origin("ops")
        .form(&["expr*"])
        .short("Return whether values are in monotonically non-increasing order");

    static ref DOC_SET: Documentation = Documentation::new()
        .origin("ops")
        .form(&["object", "key", "val"])
        .form(&["object", "key0", "val0", "key1", "val1", "..."])
        .short("Add a mapping to an associative structure")
        .desc("Evaluate the inputs, then return an updated version of the given \
               associative structure with the given key-value mappings. If the \
               value given for object is not associative or if the key is not \
               valid, this function will fail. Maps can use any hashable type \
               as a key and vectors can use integers. Undefined spaces in lists \
               will be filled in with empty () values.");
}

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

/// Return whether all passed arguments are complex
fn complex_q(_: &Environment, args: &[Value]) -> EvalResult {
    Ok(Value::from(args.iter().map(|x| x.into_num())
                       .collect::<Eval<Vec<_>>>()?.into_iter()
                       .collect::<Option<Vec<_>>>()
                       .map(|v| v.into_iter().all(|n| n.is_complex()))
                       .unwrap_or(false)))
}

/// Utility for implmementing numeric comparison ops
fn ord_compare<F>(args: &[Value], f: F) -> EvalResult
        where F: Fn(&Number,&Number) -> bool {
    if args.is_empty() { return Ok(Value::from(true)) }
    let mut last = None;
    for i in args.into_iter() {
        let v = i.into_num()?
                 .ok_or(EvalError::TypeError(
                         String::from("value is not number-like")))?;
        if let &Some(ref l) = &last {
            if !f(l,&v) { return Ok(Value::from(false)) }
        }
        last = Some(v)
    }
    
    return Ok(Value::from(true))
}

/// Return whether values are in monotonically increasing order
/// 
/// If applied to no args, returns true
fn fn_lt(_: &Environment, args: &[Value]) -> EvalResult {
    ord_compare(args, |a,b| a < b) }

/// Return whether values are in monotonically decreasing order
/// 
/// If applied to no args, returns true
fn fn_gt(_: &Environment, args: &[Value]) -> EvalResult {
    ord_compare(args, |a,b| a > b) }

/// Return whether values are in monotonically non-decreasing order
/// 
/// If applied to no args, returns true
fn fn_le(_: &Environment, args: &[Value]) -> EvalResult {
    ord_compare(args, |a,b| a <= b) }

/// Return whether values are in monotonically non-increasing order
/// 
/// If applied to no args, returns true
fn fn_ge(_: &Environment, args: &[Value]) -> EvalResult {
    ord_compare(args, |a,b| a >= b) }

/// Associate values in a map-like structure
fn fn_set(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() < 3 {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 3
        })
    } else if (args.len() - 1) % 2 != 0{
        Err(EvalError::Arity {
            got: args.len(),
            expected: 3
        })
    } else {
        match args[0].data {
            ValueData::Map(ref m) => {
                let mut arg_pairs = Vec::with_capacity(args.len()-1 / 2);
                {
                    let mut s = None;
                    for a in args[1..].iter() {
                        if let Some(i) = s.take() {
                            arg_pairs.push((i,a));
                        } else {
                            s = Some(a);
                        }
                    }
                }
                let pairs = arg_pairs.into_iter()
                           .map(|(k,v)|
                                if let Some(h) = k.hash()? {Ok((h,v))}
                                else {Err(EvalError::TypeError(String::from(
                                                "set keys must be hashable")))})
                           .collect::<Eval<Vec<_>>>()?;

                let mut r = m.to_owned();
                for (h,v) in pairs {
                    r.insert(h,v.to_owned());
                }
                Ok(Value::from(ValueData::Map(r)))
            },
            ValueData::List(ref v) => {
                unimplemented!()
            },
            _ => Err(EvalError::TypeError(
                    String::from("Cannot set into non-associative structure")))
        }
    }
}

pub fn initialize() {
    let env = global();
    env.set("+", Value::from(Executable::native(fn_add)).document(&DOC_ADD));
    env.set("-", Value::from(Executable::native(fn_sub)).document(&DOC_SUB));
    env.set("*", Value::from(Executable::native(fn_mul)).document(&DOC_MUL));
    env.set("/", Value::from(Executable::native(fn_div)).document(&DOC_DIV));

    // comparisons
    env.set("=", Value::from(Executable::native(fn_equal)).document(&DOC_EQUAL));
    env.set("<", Value::from(Executable::native(fn_lt)).document(&DOC_LT));
    env.set(">", Value::from(Executable::native(fn_gt)).document(&DOC_GT));
    env.set("<=", Value::from(Executable::native(fn_le)).document(&DOC_LE));
    env.set(">=", Value::from(Executable::native(fn_ge)).document(&DOC_GE));

    // numeric type queries
    env.set("int?", Value::from(Executable::native(int_q)).document(&DOC_INTQ));
    env.set("rational?", Value::from(Executable::native(rational_q))
                               .document(&DOC_RATIONALQ));
    env.set("real?", Value::from(Executable::native(real_q))
                           .document(&DOC_REALQ));
    env.set("complex?", Value::from(Executable::native(complex_q))
                              .document(&DOC_COMPLEXQ));

    env.set("inc", Value::from(Executable::native(fn_inc)).document(&DOC_INC));

    env.set("set", Value::from(Executable::native(fn_set)).document(&DOC_SET));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn add() {
        let e = empty();
        assert_eq!(fn_add(&e, &[Value::from(Number::int(2)),
                                Value::from(Number::int(3))]).unwrap(),
                   Value::from(Number::int(5)));
        assert!(fn_add(&e, &[Value::from(Number::int(2)),
                             Value::str("test")]).is_err());
    }

    #[test]
    fn mul() {
        let e = empty();
        assert_eq!(fn_mul(&e, &[Value::from(Number::int(2)),
                                Value::from(Number::int(3))]).unwrap(),
                   Value::from(Number::int(6)));
        assert!(fn_mul(&e, &[Value::from(Number::int(2)),
                             Value::str("test")]).is_err());
    }

    #[test]
    fn sub() {
        let e = empty();
        assert_eq!(fn_sub(&e, &[Value::from(Number::int(3)),
                                Value::from(Number::int(2))]).unwrap(),
                   Value::from(Number::int(1)));
        assert_eq!(fn_sub(&e, &[Value::from(Number::int(3))]).unwrap(),
                   Value::from(Number::int(-3)));
        assert!(fn_sub(&e, &[]).is_err()); // arity
        assert!(fn_sub(&e, &[Value::str("test")]).is_err());
        assert!(fn_sub(&e, &[Value::str("test"), Value::str("foo")]).is_err());
        assert!(fn_sub(&e, &[Value::from(Number::int(1)),
                             Value::str("foo")]).is_err());
    }

    #[test]
    fn div() {
        let e = empty();
        assert_eq!(fn_div(&e, &[Value::from(Number::int(3)),
                                Value::from(Number::int(2))]).unwrap(),
                   Value::from(Number::rational(3,2)));
        assert!(fn_div(&e, &[Value::str("foo"),
                             Value::from(Number::int(2))]).is_err());
        assert!(fn_div(&e, &[Value::from(Number::int(2)),
                             Value::str("foo")]).is_err());
        assert!(fn_div(&e, &[Value::from(Number::int(2))]).is_err());
    }

    #[test]
    fn inc() {
        let e = empty();
        assert_eq!(fn_inc(&e, &[Value::from(Number::int(2))]).unwrap(),
                   Value::from(Number::int(3)));
        assert_eq!(fn_inc(&e, &[Value::from(Number::int(2)),
                                Value::from(Number::int(3))]).unwrap(),
                   Value::list(vec![Value::from(Number::int(3)),
                                    Value::from(Number::int(4))]));
        assert!(fn_inc(&e, &[Value::str("foo")]).is_err());
    }

    #[test]
    fn equality() {
        let e = empty();
        assert_eq!(fn_equal(&e, &[Value::from(Number::int(2)),
                                  Value::from(Number::int(2))]).unwrap(),
                   Value::from(true));
        assert_eq!(fn_equal(&e, &[Value::from(Number::int(2)),
                                  Value::from(Number::int(3))]).unwrap(),
                   Value::from(false));
        assert_eq!(fn_equal(&e, &[Value::str("test"),
                                  Value::from(Number::int(3))]).unwrap(),
                   Value::from(false));
        assert!(fn_equal(&e, &[]).is_err());

        let v = fn_equal(&e, &[Value::from(Number::int(2))]).unwrap();
        assert!(v.execute(&e, &[]).is_err());
        assert_eq!(v.execute(&e, &[Value::from(Number::int(2))]).unwrap(),
                   Value::from(true));
        assert_eq!(v.execute(&e, &[Value::from(Number::int(3))]).unwrap(),
                   Value::from(false));
        assert_eq!(v.execute(&e, &[Value::str("foo")]).unwrap(),
                   Value::from(false));
    }

    #[test]
    fn type_queries() {
        let e = empty();
        assert_eq!(int_q(&e, &[]).unwrap(),
                   Value::from(true));
        assert_eq!(int_q(&e, &[Value::from(Number::int(1))]).unwrap(),
                   Value::from(true));
        assert_eq!(int_q(&e, &[Value::from(Number::int(1)),
                               Value::from(Number::real(1.5))]).unwrap(),
                   Value::from(false));

        assert_eq!(rational_q(&e, &[]).unwrap(),
                   Value::from(true));
        assert_eq!(rational_q(&e, &[Value::from(Number::rational(1,2))]).unwrap(),
                   Value::from(true));
        assert_eq!(rational_q(&e, &[Value::from(Number::rational(1,2)),
                                    Value::from(Number::real(1.5))]).unwrap(),
                   Value::from(false));

        assert_eq!(real_q(&e, &[]).unwrap(),
                   Value::from(true));
        assert_eq!(real_q(&e, &[Value::from(Number::real(1.5))]).unwrap(),
                   Value::from(true));
        assert_eq!(real_q(&e, &[Value::from(Number::int(1)),
                               Value::from(Number::real(1.5))]).unwrap(),
                   Value::from(false));

        assert_eq!(complex_q(&e, &[]).unwrap(),
                   Value::from(true));
        assert_eq!(complex_q(&e, &[Value::from(Number::complex(1.,1.))]).unwrap(),
                   Value::from(true));
        assert_eq!(complex_q(&e, &[Value::from(Number::complex(1.,1.)),
                               Value::from(Number::real(1.5))]).unwrap(),
                   Value::from(false));
    }

    #[test]
    fn comparison() {
        let e = empty();
        assert_eq!(fn_lt(&e, &[]).unwrap(),
                   Value::from(true));
        assert_eq!(fn_gt(&e, &[Value::from(Number::int(1)),
                               Value::from(Number::int(2))]).unwrap(),
                   Value::from(false));
        assert_eq!(fn_gt(&e, &[Value::from(Number::int(2)),
                               Value::from(Number::int(2)),
                               Value::from(Number::int(1))]).unwrap(),
                   Value::from(false));
        assert_eq!(fn_le(&e, &[Value::from(Number::int(1)),
                               Value::from(Number::int(2))]).unwrap(),
                   Value::from(true));
        assert_eq!(fn_ge(&e, &[Value::from(Number::int(2)),
                               Value::from(Number::int(1))]).unwrap(),
                   Value::from(true));
    }
}

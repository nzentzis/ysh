use std::sync::Arc;

use environment::*;
use data::*;

/// if the first arg is truthy, evaluate and yield the second arg. Otherwise,
/// yield the third arg if present and () otherwise
fn core_if(lex: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 2 {
        if args[0].into_bool() {
            Ok(args[1].clone())
        } else {
            Ok(BasicValue::empty())
        }
    } else if args.len() == 3 {
        if args[0].into_bool() {
            Ok(args[1].clone())
        } else {
            Ok(args[2].clone())
        }
    } else {
        Err(EvalError::Arity {
            expected: 3,
            got: args.len()
        })
    }
}

/// Creates a new global symbol and initializes it to the specified value
/// 
/// If no value is specified, this will retrieve the given symbol from the
/// global environment or () if not present.
fn core_def(lex: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        if let Some(ident) = args[0].get_symbol() {
            Ok(global().get(ident).unwrap_or_else(|| BasicValue::empty()))
        } else {
            Err(EvalError::TypeError(
                    format!("argument to def cannot be converted to a symbol")))
        }
    } else if args.len() == 2 {
        let sym = args[0].get_symbol().ok_or(EvalError::TypeError(
                format!("argument to def cannot be converted to a symbol")));
        let sym = sym?;

        let body = args[1].evaluate(lex)?;

        global().set(sym, body);
        Ok(BasicValue::empty())
    } else {
        Err(EvalError::Arity {
            expected: 2,
            got: args.len()
        })
    }
}

/// Evaluate the argument forms in the order in which they were given
/// 
/// Return the output of the last argument form, or () if no arguments were
/// given.
fn core_do(lex: &Environment, args: &[Value]) -> EvalResult {
    let mut r = BasicValue::empty();
    for i in args.iter() {
        r = i.evaluate(lex)?;
    }
    Ok(r)
}

/// Evaluate the inner expressions a modified lexical environment
/// 
/// The first argument is a list of bindings of the form
/// `(symbol1 value1 symbol2 value2 ...)`. The remaining arguments are evaluated
/// using the same semantics as `do`, but with the given bindings applied.
/// 
/// Note that earlier bindings will apply to the value forms for later bindings.
fn core_let(lex: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 0 {
        return Err(EvalError::Arity {
            got: 0,
            expected: 1
        });
    }

    // evaluate bindings
    let bind_list = args[0].into_seq()?;
    if bind_list.len() % 2 != 0 {
        return Err(EvalError::InvalidOperation(
                "binding list for let has odd length"));
    }

    let mut e = lex.clone();
    for c in bind_list.chunks(2) {
        let sym = c[0].get_symbol().ok_or(EvalError::TypeError(format!(
                    "first argument in let binding cannot be converted to symbol")))?;
        let val = c[1].evaluate(&e)?;
        e.exclusive().set(sym, val);
    }

    core_do(&e, &args[1..])
}

/// Create an anonymous function
/// 
/// This has two basic forms. The first, which defines a fixed-arity function,
/// is as follows:
/// 
///     (fn (args*) body*)
/// 
/// In this form, the given arguments are bound based on the function's inputs
/// and the body forms are then evaluted using the same semantics as `do`.
/// 
/// The second, which defines a variable-arity function, is as follows:
/// 
///     (fn ((args1*) body1*)
///         ((args2*) body2*)
///         ...)
/// 
/// When the resulting function is called, the argument patterns will be tried
/// in order until one matches the values passed at runtime. That pattern's
/// variables will then be bound and its body forms will be evaluated.
/// 
/// The following syntaxes are allowed in argument lists:
/// 
///     (a1 a2 a3)          - binds a fixed number of required args
///     (a1 a2 a3 ?)        - binds some required args and some optional ones.
///                           Optional args default to ().
///     (a1 a2 & rest)      - binds a fixed number of args and stores the rest
///                           as a list in another variable
/// 
/// `rest` and `?` patterns may be mixed, but the matcher will give priority to
/// arguments which come first. All arguments must be consumed for a pattern to
/// match.
fn core_fn(lex: &Environment, args: &[Value]) -> EvalResult {
    enum PatternPiece {
        Required(Identifier),
        Optional(Identifier),
        Rest(Identifier)
    }

    if args.len() == 1 {
        return Err(EvalError::Arity {
            got: args.len(),
            expected: 2
        });
    }

    fn make_pattern(v: &Value) -> Eval<Vec<PatternPiece>> {
        let mut r = Vec::new();
        let mut next_is_rest = false;
        let mut should_end = false;
        for i in v.into_iter() {
            let i = i?;
            let s = i.get_symbol()
                     .ok_or(EvalError::TypeError(
                             format!("pattern element cannot be converted to symbol")))?;
            if s.as_ref() == "&" {
                if should_end || next_is_rest {
                    return Err(EvalError::InvalidOperation("invalid fn pattern"));
                }
                next_is_rest = true;
                continue;
            } else if s.as_ref() == "?" {
                if should_end || next_is_rest {
                    return Err(EvalError::InvalidOperation("invalid fn pattern"));
                }
                let ident = if let PatternPiece::Rest(x) = r.pop().unwrap() {x}
                            else { return Err(EvalError::InvalidOperation(
                                        "invalid fn pattern")); };
                r.push(PatternPiece::Optional(ident));
                continue;
            }

            if next_is_rest {
                r.push(PatternPiece::Rest(s));
                should_end = true;
                next_is_rest = false;
            } else {
                r.push(PatternPiece::Required(s));
            }
        }

        if next_is_rest {Err(EvalError::InvalidOperation("invalid fn pattern"))}
        else {Ok(r)}
    }

    fn match_pattern(pat: &[PatternPiece], args: &[Value])
            -> Option<Vec<(Identifier, Value)>> {
        let mut res = Vec::new();
        let mut idx = 0;
        let arg_len = args.len();
        for p in pat.iter() {
            match p {
                &PatternPiece::Required(ref i) => {
                    if idx < arg_len {
                        res.push((i.to_owned(), args[idx].clone()));
                    } else {
                        // missing required arg
                        return None;
                    }
                },
                &PatternPiece::Optional(ref i) => {
                    res.push((i.to_owned(),
                    if idx < arg_len { args[idx].clone() }
                    else { BasicValue::empty() }));
                },
                &PatternPiece::Rest(ref i) => {
                    res.push((i.to_owned(),
                    if idx >= arg_len { BasicValue::list(vec![]) }
                    else { BasicValue::list(args[idx..].iter().cloned()) }));
                    idx = arg_len;
                    break;
                },
            }
            idx += 1;
        }

        if idx < arg_len { None }
        else { Some(res) }
    }

    // it's single form if the first element of the first arg isn't a list
    // since no valid bindings are lists, and multi-form if it *is* a list
    let is_single_form = if let Some(x) = args[0].into_iter().next() {
            if let Some(&BasicValue::List(_)) = x?.get_basic() { false }
            else { true } }
        else { true };

    // build list of forms and patterns
    let mut variants: Vec<(Vec<PatternPiece>, Vec<Value>)> = Vec::new();
    if is_single_form {
        let pat = make_pattern(&args[0])?;
        let vals: Vec<_> = args[1..].iter().cloned().collect();
        variants.push((pat, vals));
    } else {
        for l in args.iter() {
            let l = l.into_seq()?;
            if l.is_empty() {
                return Err(EvalError::InvalidOperation("empty fn variant spec"));
            }
            let pat = make_pattern(&l[0])?;
            let body: Vec<_> = l.into_iter().skip(1).collect();
            variants.push((pat, body));
        }
    }

    Ok(BasicValue::function(Executable::Interpreted(lex.to_owned(),
        Arc::new(move |env, args| {
            // try matching each pattern
            for &(ref pat, ref body) in variants.iter() {
                if let Some(assignments) = match_pattern(pat, args) {
                    let mut e = env.clone();
                    {
                    let mut excl = e.exclusive();
                    for a in assignments { excl.set(a.0, a.1); }
                    }
                    return core_do(&e, body);
                } else {
                    continue;
                }
            }
            Err(EvalError::Arity {
                got: args.len(),
                expected: variants[0].0.len()
            })
        }))))
}

/// Return the literal arguments without interpretation or substitution
/// 
/// With one argument, return its quoted form. Otherwise, wrap its args in a
/// list.
fn core_quote(lex: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        Ok(args[0].to_owned())
    } else {
        Ok(BasicValue::list(args.iter().cloned()))
    }
}

/// Utility function combining `def` and `fn`
/// 
/// The first argument is interpreted as in `def`, and the remainder are
/// interpreted as arguments to `fn`.
fn core_defn(lex: &Environment, args: &[Value]) -> EvalResult {
    if args.len() < 1 {
        return Err(EvalError::Arity { got: 0, expected: 3 });
    }

    let v = core_fn(lex, &args[1..])?;
    let name = args[0].clone();
    core_def(lex, &[name, v])
}

pub fn initialize() {
    let env = global();

    // special forms
    env.set_immut("fn", BasicValue::function(Executable::CoreFn(core_fn)));
    env.set_immut("let", BasicValue::function(Executable::CoreFn(core_let)));
    env.set_immut("def", BasicValue::function(Executable::CoreFn(core_def)));
    env.set_immut("do", BasicValue::function(Executable::CoreFn(core_do)));
    env.set_immut("if", BasicValue::function(Executable::CoreFn(core_if)));
    env.set_immut("quote", BasicValue::function(Executable::CoreFn(core_quote)));

    // utilities
    env.set_immut("defn", BasicValue::function(Executable::CoreFn(core_defn)));
}

pub fn quote(vals: &[Value]) -> Value {
    let mut r = Vec::with_capacity(vals.len() + 1);
    r.push(BasicValue::function(Executable::CoreFn(core_quote)));
    r.extend(vals.iter().cloned());
    BasicValue::list(r)
}

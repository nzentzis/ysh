use std::sync::Arc;

use environment::*;
use data::*;

lazy_static! {
    static ref DOC_IF: Documentation = Documentation::new()
        .form(&["test", "then"])
        .form(&["test", "then", "else"])
        .desc("Evaluates test. If the result is truthy, evaluate then and \
               return the result. Otherwise, evaluate and return else if it's \
               provided and () if not.");

    static ref DOC_DEF: Documentation = Documentation::new()
        .form(&["sym"])
        .form(&["sym", "value"])
        .short("Create or retrieve global symbol bindings")
        .desc("With 1 argument, returns the value bound to sym if one exists \
               and returns () otherwise.\n \
               With 2 arguments, evaluates value and modifies the global \
               environment binding the result to sym.");

    static ref DOC_DO: Documentation = Documentation::new()
        .form(&["forms*"])
        .short("Evaluate the passed forms in order and return the last value");

    static ref DOC_LET: Documentation = Documentation::new()
        .form(&["(id0 val0 id1 val1...)", "forms*"])
        .short("Evaluate the passed forms in a modified lexical environment.")
        .desc("\
The first argument is a list of bindings which are applied in the order
provided. This means later bindings can reference the value of earlier ones. For
example:

    $ (let (x 1 y (+ 2 x)) (+ y x))
    4

If more than one inner form is given, they are evaluated in order using the same
semantics as 'do'.");

    static ref DOC_FN: Documentation = Documentation::new()
        .form(&["(args*)", "body*"])
        .form(&["((args0*) body0*)", "((args1*) body1*)", "..."])
        .short("Create an anonymous function.");
    
    static ref DOC_QUOTE: Documentation = Documentation::new()
        .form(&["form"])
        .form(&["(forms*)"])
        .short("Return passed forms without evaluating them")
        .desc("If more than one form is passed, a list will be created from \
               all passed forms and `quote` will return that.");

    static ref DOC_DEFN: Documentation = Documentation::new()
        .form(&["sym", "(args*)", "body*"])
        .form(&["sym", "((args0*) body0*)", "((args1*) body1*)", "..."])
        .short("Create a named function in the global namespace")
        .desc("Generates a function using all arguments after the first, then \
               stores it in the first symbol. Function creation works as with \
               `fn` and definition works as with `def`.");

    static ref DOC_SOURCE: Documentation = Documentation::new()
        .form(&["filelike", "filelike*"])
        .short("Read and execute code from files or streams")
        .desc("Convert arguments to streams (opening files by name as read-only \
               in the process if necessary) and repeatedly read forms from \
               them using the normal Lisp reader, evaluating each form as it \
               is read. Passed forms are sourced in order.");
}

/// if the first arg is truthy, evaluate and yield the second arg. Otherwise,
/// yield the third arg if present and () otherwise
fn core_if(lex: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 2 {
        let pred = args[0].evaluate(lex).and_then(|x| x.into_bool())?;
        if pred {
            args[1].evaluate(lex)
        } else {
            Ok(Value::empty())
        }
    } else if args.len() == 3 {
        let pred = args[0].evaluate(lex).and_then(|x| x.into_bool())?;
        if pred {
            args[1].evaluate(lex)
        } else {
            args[2].evaluate(lex)
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
        if let Some(ident) = args[0].get_symbol()? {
            Ok(global().get(ident).unwrap_or_else(|| Value::empty()))
        } else {
            Err(EvalError::TypeError(
                    format!("argument to def cannot be converted to a symbol")))
        }
    } else if args.len() == 2 {
        let sym = args[0].get_symbol()?.ok_or(EvalError::TypeError(
                format!("argument to def cannot be converted to a symbol")));
        let sym = sym?;

        let body = args[1].evaluate(lex)?;

        global().set(sym, body);
        Ok(Value::empty())
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
    let mut r = Value::empty();
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
        let sym = c[0].get_symbol()?.ok_or(EvalError::TypeError(format!(
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
            let s = i.get_symbol()?
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
                let ident = if let PatternPiece::Required(x) = r.pop().unwrap() {x}
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
                    else { Value::empty() }));
                },
                &PatternPiece::Rest(ref i) => {
                    res.push((i.to_owned(),
                    if idx >= arg_len { Value::list(vec![]) }
                    else { Value::list(args[idx..].iter().cloned()) }));
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
            if let &ValueData::List(_) = &x?.data { false }
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

    Ok(Value::from(Executable::Interpreted(lex.to_owned(),
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
fn core_quote(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        Ok(args[0].to_owned())
    } else {
        Ok(Value::list(args.iter().cloned()))
    }
}

// TODO: add local binding for external environment
/// Define a macro with the given name
/// 
/// This operates like to `defn`, but the constructed function is stored as a
/// macro rather than as a normal function.
fn core_defmacro(lex: &Environment, args: &[Value]) -> EvalResult {
    if args.len() < 1 {
        return Err(EvalError::Arity { got: 0, expected: 3 });
    }

    // convert the function into a macro
    let mut m = core_fn(lex, &args[1..])?;
    let d = if let ValueData::Function(e) = m.data.clone() { e }
            else { panic!("invalid value result from fn call") };
    m.data = ValueData::Macro(d);

    let name = args[0].get_symbol()?.ok_or(EvalError::TypeError(
            format!("argument to def cannot be converted to a symbol")));
    let name = name?;

    global().set(name, m);
    Ok(Value::empty())
}

/// Perform macro expansion on the passed form
fn core_macroexpand(lex: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 1 {
        Err(EvalError::Arity { got: args.len(), expected: 1})
    } else {
        args[0].evaluate(lex)?.macroexpand()
    }
}

// TODO: once hashmaps are a thing, allow custom environments
/// Evaluate the passed form in the current lexical environment
fn core_eval(lex: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 1 {
        Err(EvalError::Arity { got: args.len(), expected: 1})
    } else {
        args[0].evaluate(lex)
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

/// Render and display a documentation object
fn render_doc(d: &Documentation) {
    use std::io;
    use docs::DocFormat;
    let out = io::stdout();
    let mut out_lock = out.lock();

    let fmt = DocFormat::new();
    fmt.render(d, &mut out_lock).expect("failed to render docs");
}

/// Utility function to look up and format documentation
/// 
/// This should be renamed `man` and delegate to the system's man command when
/// support is available.
fn fn_man(env: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        let x = &args[0];
        if let Some(ref d) = x.doc {
            render_doc(d);
            return Ok(Value::empty());
        } else if let Some(id) = x.get_symbol()? {
            // try looking up the symbol to check docs for its value
            if let Some(ref d) = env.get(id).and_then(|o| o.doc) {
                render_doc(d);
                return Ok(Value::empty());
            }
        }
    }
    Err(EvalError::Runtime(String::from("documentation not found")))

    /*
    // find man command
    let cmd = ::evaluate::find_command("man");
    let cmd = if let Some(cmd) = cmd { cmd }
              else {
                  return Err(EvalError::Runtime(
                          String::from("cannot find system man command")));
              };
    if cmd.is_empty() {
        return Err(EvalError::Runtime(
                String::from("cannot find system man command")));
    } else if cmd.len() > 1 {
        return Err(EvalError::Runtime(
                String::from("multiple man commands found")));
    }

    let pth = &cmd[0];
    unimplemented!()
    */
}

/// Repeatedly read forms from input streams and evaluate them. Return ().
pub fn fn_source(env: &Environment, args: &[Value]) -> EvalResult {
    use ::reader::ParseError;

    for a in args.iter() {
        let f = a.into_raw_stream(true, false)?;
        loop {
            let m = ::reader::read(&f);
            match m {
                Ok(r) => {r.evaluate(env)?;},
                Err(e) => if let ParseError::UnexpectedEOF = e {break;}
                          else {return Err(EvalError::Runtime(
                                      format!("read error: {}", e)));}
            }
        }
    }

    Ok(Value::empty())
}

pub fn initialize() {
    let env = global();

    // special forms
    env.set_immut("fn", Value::from(Executable::CoreFn(core_fn))
                              .document(&DOC_FN));
    env.set_immut("let", Value::from(Executable::CoreFn(core_let))
                               .document(&DOC_LET));
    env.set_immut("def", Value::from(Executable::CoreFn(core_def))
                               .document(&DOC_DEF));
    env.set_immut("do", Value::from(Executable::CoreFn(core_do))
                              .document(&DOC_DO));
    env.set_immut("if", Value::from(Executable::CoreFn(core_if))
                              .document(&DOC_IF));

    // macros and quoting
    env.set_immut("quote", Value::from(Executable::CoreFn(core_quote))
                                     .document(&DOC_QUOTE));
    env.set_immut("defmacro", Value::from(Executable::CoreFn(core_defmacro)));
    env.set_immut("macroexpand", Value::from(Executable::CoreFn(core_macroexpand)));

    // utilities
    env.set("defn", Value::from(Executable::CoreFn(core_defn))
                          .document(&DOC_DEFN));
    env.set("eval", Value::from(Executable::native(core_eval)));
    env.set("doc", Value::from(Executable::native(fn_man)));
    env.set("source", Value::from(Executable::native(fn_source))
                            .document(&DOC_SOURCE));
}

/// Generate a value with the passed args wrapped in a call to `quote`
pub fn quote(vals: &[Value]) -> Value {
    let mut r = Vec::with_capacity(vals.len() + 1);
    r.push(Value::from(Executable::CoreFn(core_quote)));
    r.extend(vals.iter().cloned());
    Value::list(r)
}

#[cfg(test)]
mod test {
    use super::*;
    use numeric::Number;

    #[test]
    fn test_if() {
        let mut e = empty();
        {
            let mut x = e.exclusive();
            x.set("t", Value::from(true));
            x.set("f", Value::from(false));
        }
        let t = Value::from(Identifier::new("t"));
        let f = Value::from(Identifier::new("f"));
        let n1 = Value::from(Number::int(1));
        let n2 = Value::from(Number::int(2));

        assert_eq!(core_if(&e, &[t.clone(), n1.clone()]).unwrap(), n1);
        assert_eq!(core_if(&e, &[f.clone(), n1.clone()]).unwrap(),
                   Value::empty());
        assert_eq!(core_if(&e, &[t.clone(), n1.clone(), n2.clone()]).unwrap(), n1);
        assert_eq!(core_if(&e, &[f.clone(), n1.clone(), n2.clone()]).unwrap(), n2);
        assert!(core_if(&e, &[]).is_err());
    }

    #[test]
    fn test_def() {
        let e = empty();
        let test_sym = Value::from(Identifier::new("def_test_sym"));
        assert!(core_def(&e, &[]).is_err());
        assert!(core_def(&e, &[Value::from(true)]).is_err());
        assert!(core_def(&e, &[Value::from(true), Value::from(true)]).is_err());
        assert!(global().get("def_test_sym").is_none());
        assert_eq!(core_def(&e, &[test_sym.clone()]).unwrap(), Value::empty());
        assert_eq!(core_def(&e, &[test_sym.clone(), Value::from(true)]).unwrap(),
                   Value::empty());
        assert_eq!(global().get("def_test_sym"), Some(Value::from(true)));
        assert_eq!(core_def(&e, &[test_sym.clone()]).unwrap(),
                   Value::from(true));
    }

    #[test]
    fn test_let() {
        let e = empty();
        let test_sym = Value::from(Identifier::new("let_test_sym"));
        assert_eq!(test_sym.evaluate(&e).unwrap(), test_sym);
        assert_eq!(core_let(&e, &[Value::list(vec![test_sym.clone(),
                                                   Value::from(true)]),
                                  test_sym]).unwrap(), Value::from(true));
    }

    #[test]
    fn test_fn() {
        let e = empty();

        // single-variant tests
        let f1 = core_fn(&e, &[Value::list(vec![
                                           Value::from(Identifier::new("a"))]),
                               Value::from(Identifier::new("a"))]).unwrap();
        assert_eq!(f1.execute(&e, &[Value::from(true)]).unwrap(), Value::from(true));
        assert!(f1.execute(&e, &[]).is_err());

        let f2 = core_fn(&e, &[Value::list(vec![
                                           Value::from(Identifier::new("a")),
                                           Value::from(Identifier::new("?"))]),
                               Value::from(Identifier::new("a"))]).unwrap();
        assert_eq!(f2.execute(&e, &[Value::from(true)]).unwrap(), Value::from(true));
        assert_eq!(f2.execute(&e, &[]).unwrap(), Value::empty());

        let f3 = core_fn(&e, &[Value::list(vec![
                                           Value::from(Identifier::new("&")),
                                           Value::from(Identifier::new("rest"))]),
                               Value::from(Identifier::new("rest"))]).unwrap();
        assert_eq!(f3.execute(&e, &[Value::from(true)]).unwrap(),
                   Value::list(vec![Value::from(true)]));
        assert_eq!(f3.execute(&e, &[]).unwrap(), Value::empty());

        // multivariant test
        let f4 = core_fn(&e, &[
            Value::list(vec![Value::list(vec![Value::from(Identifier::new("a"))]),
                             Value::from(Identifier::new("a"))]),
            Value::list(vec![Value::list(vec![Value::from(Identifier::new("a")),
                                              Value::from(Identifier::new("?"))]),
                             Value::from(Identifier::new("a"))]),
            Value::list(vec![Value::list(vec![Value::from(Identifier::new("&")),
                                              Value::from(Identifier::new("rest"))]),
                             Value::from(Identifier::new("rest"))])]).unwrap();

        assert_eq!(f4.execute(&e, &[]).unwrap(), Value::empty());
        assert_eq!(f4.execute(&e, &[Value::from(true)]).unwrap(),
                   Value::from(true));
        assert_eq!(f4.execute(&e, &[Value::from(true),
                                    Value::from(false)]).unwrap(),
                   Value::list(vec![Value::from(true), Value::from(false)]));
    }
}

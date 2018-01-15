use environment::*;
use data::*;
use stream::*;

lazy_static! {
    static ref DOC_INTO_READ: Documentation = Documentation::new()
        .form(&["obj"])
        .short("Convert an object into a readable stream");
    static ref DOC_INTO_WRITE: Documentation = Documentation::new()
        .form(&["obj"])
        .short("Convert an object into a writable stream");
    static ref DOC_INTO_READWRITE: Documentation = Documentation::new()
        .form(&["obj"])
        .short("Convert an object into a read-write stream");
    static ref DOC_ADAPT_STREAM: Documentation = Documentation::new()
        .form(&["obj"])
        .form(&["obj", "delimiter"])
        .short("Parse a readable stream into a polymorphic structure")
        .desc("Convert the first argument into a readable stream, then parse it \
               using the normal polymorphic parsing infrastructure. If a custom \
               delimiter value is given, it will be stringified and the first \
               character will be used as the delimiter for parsing.");
    static ref DOC_PEEK: Documentation = Documentation::new()
        .form(&["stream"])
        .short("Return the next character in a stream without consuming it");
    static ref DOC_NEXT: Documentation = Documentation::new()
        .form(&["stream"])
        .short("Return the next character in a stream and consume it");
    static ref DOC_PUSH: Documentation = Documentation::new()
        .form(&["stream", "ch"])
        .short("Push a character back into a readable stream");
    static ref DOC_NEXT_LINE: Documentation = Documentation::new()
        .form(&["stream"])
        .short("Read from a stream up to the next newline")
        .desc("Read characters from the passed stream up to the next newline, \
               and return a string of the read characters. The newline will be \
               consumed and will not be returned as part of the result string.\n\n\
               On EOF, return the accumulated string.");
}

/// Convert something into a readable stream
fn fn_into_read(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        });
    }

    Ok(Value::raw_stream(args[0].into_raw_stream(true, false)?))
}

/// Convert something into a writable stream
fn fn_into_write(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        });
    }

    Ok(Value::raw_stream(args[0].into_raw_stream(false, true)?))
}


/// Convert something into a read-write stream
fn fn_into_rw(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        });
    }

    Ok(Value::raw_stream(args[0].into_raw_stream(true, true)?))
}

/// Peek at the next char in a stream
fn fn_peek(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        });
    }

    let s = args[0].into_raw_stream(true, false)?;
    let c = s.peek()?;
    Ok(Value::str(format!("{}", c)))
}

/// Read the next char in a stream
fn fn_next(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        });
    }

    let s = args[0].into_raw_stream(true, false)?;
    let c = s.next()?;
    Ok(Value::str(format!("{}", c)))
}

/// Push a character back into the given stream
fn fn_push(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 2 {
        return Err(EvalError::Arity {
            got: args.len(),
            expected: 2
        });
    }

    let s = args[0].into_raw_stream(true, false)?;
    let c = args[1].into_str()?;
    if c.len() != 1 {
        return Err(EvalError::TypeError(format!(
                    "expected char, found string of length {}", c.len())));
    }

    s.push(c.chars().next().unwrap());
    Ok(Value::empty())
}

/// Read the next line in a stream
fn fn_next_line(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        });
    }

    let strm = args[0].into_raw_stream(true, false)?;
    let mut s = String::new();

    loop {
        match strm.next() {
            Ok(c) => {
                if c == '\n' {break;}
                else {s.push(c);}
            },
            Err(e) => {
                if e.is_eof() { break; }
                else {return Err(EvalError::from(e));}
            }
        }
    }
    Ok(Value::str(s))
}

pub fn initialize() {
    let env = global();

    env.set("io/into-read", Value::from(Executable::native(fn_into_read))
                                  .document(&DOC_INTO_READ));
    env.set("io/into-write", Value::from(Executable::native(fn_into_write))
                                   .document(&DOC_INTO_WRITE));
    env.set("io/into-rw", Value::from(Executable::native(fn_into_rw))
                                .document(&DOC_INTO_READWRITE));

    // basic read functions
    env.set("io/peek", Value::from(Executable::native(fn_peek))
                             .document(&DOC_PEEK));
    env.set("io/next", Value::from(Executable::native(fn_next))
                             .document(&DOC_NEXT));
    env.set("io/push", Value::from(Executable::native(fn_push))
                             .document(&DOC_PUSH));

    // extended read functions
    env.set("io/next-line", Value::from(Executable::native(fn_next_line))
                                  .document(&DOC_NEXT_LINE));
    
    //env.set("io/adapt-stream", Value::from(Executable::native(fn_adapt_stream))
    //                                 .document(&DOC_ADAPT_STREAM));
}

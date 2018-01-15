use environment::*;
use data::*;

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

pub fn initialize() {
    let env = global();

    env.set("io/into-read", Value::from(Executable::native(fn_into_read))
                                  .document(&DOC_INTO_READ));
    env.set("io/into-write", Value::from(Executable::native(fn_into_write))
                                   .document(&DOC_INTO_WRITE));
    env.set("io/into-rw", Value::from(Executable::native(fn_into_rw))
                                .document(&DOC_INTO_READWRITE));
}

use environment::*;
use data::*;

fn check_result<T>(e: ::nix::Result<T>) -> Eval<T> {
    use nix::Error;

    e.map_err(|e| {
        match e {
            Error::Sys(e) => {
                EvalError::Runtime(format!("system call failed: {}", e.desc()))
            },
            Error::UnsupportedOperation => {
                EvalError::Runtime(String::from("unsupported operation"))
            },
            _ => EvalError::Unknown
        }
    })
}

fn fn_cd(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() != 1 {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        })
    } else {
        let s = args[0].into_str()?;
        check_result(::nix::unistd::chdir(&s[..]))
            .map(|_| Value::empty())
    }
}

pub fn initialize() {
    let env = global();

    env.set("cd", Value::from(Executable::native(fn_cd)))
}

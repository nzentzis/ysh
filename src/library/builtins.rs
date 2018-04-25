use environment::*;
use data::*;
use evaluate::*;

lazy_static! {
    static ref DOC_CD: Documentation = Documentation::new()
        .form(&["dir"])
        .short("Change the current working directory");

    static ref DOC_PWD: Documentation = Documentation::new()
        .form(&["arg*"])
        .short("Return the current working directory")
        .desc("Ignores any given arguments.");
}

fn check_result<T>(e: ::nix::Result<T>) -> EvalRes<T> {
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
        let s = args[0].into_str().wait()?;
        check_result(::nix::unistd::chdir(&s[..]))
            .map(|_| Value::empty())
    }
}

fn fn_pwd(_: &Environment, args: &[Value]) -> EvalResult {
    check_result(::nix::unistd::getcwd())
        .map(|p| p.into_os_string().into_string().unwrap())
        .map(|s| Value::str(s))
}

pub fn initialize() {
    let env = global();

    env.set("cd", Value::from(Executable::native(fn_cd)).document(&DOC_CD));
    env.set("pwd", Value::from(Executable::native(fn_pwd)).document(&DOC_PWD));
}

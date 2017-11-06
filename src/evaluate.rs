use data::*;
use environment::Environment;

trait Evaluable {
    /// Try to evaluate this value in the given environment
    fn eval_in(self, env: &mut Environment) -> Value;
}

impl Evaluable for Value {
    fn eval_in(self, env: &mut Environment) -> Value {
        use std::ops::Deref;

        match self {
            Value::Boolean(x)   => Value::Boolean(x),
            Value::Number(x)    => Value::Number(x),
            Value::Str(x)       => Value::Str(x),
            Value::Symbol(x)    => {
                // look it up in the environment
                if let Some(v) = env.get(x.0.deref()) {
                    v.deref().to_owned()
                } else {
                    Value::Symbol(x)
                }
            },
            Value::List(mut xs) => {
                let mut args: Vec<_> =
                           xs.split_off(1)
                             .into_iter()
                             .map(|x| x.eval_in(env))
                             .collect();
                let exec = xs.pop().unwrap().eval_in(env);

                execute(&exec, &args, env)
            },
            Value::Function(e,x)=> Value::Function(e,x)
        }
    }
}

/// Evalute a value in an executable context
fn execute(val: &Value, args: &Vec<Value>, env: &mut Environment) -> Value {
    // TODO: handle lexical scoping
    if let &Value::Function(_, ref exe) = val {
        exe.run(&env, &args)
    } else {
        // stringify it and try finding it
        let search_obj = val.to_owned().into_str();
        let search_res = if let Some(locate) = env.get("shell/locate") {
            execute(&*locate, &vec![Value::Str(search_obj.clone())], env)
        } else {
            // they somehow unbound shell/locate
            eprintln!("ysh: shell/locate not bound. try 'sys/recover'");
            return Value::empty();
        }.into_seq();

        if search_res.len() > 1 {
            eprintln!("ysh: more than one candidate for '{}'", search_obj);
            return Value::empty();
        } else if search_res.len() == 0 {
            eprintln!("ysh: command not found: {}", search_obj);
            return Value::empty();
        } else {
            unimplemented!()
        }
    }
}

/// Evaluate a pipeline in the given environment
pub fn evaluate(env: &mut Environment, pipe: Pipeline) {
    let Pipeline{elements, terminals} = pipe;

    for elem in elements {
        Value::List(elem.xform.0).eval_in(env);
    }
}

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
                    panic!()
                }
            },
            Value::List(mut xs) => {
                let args: Vec<_> =
                           xs.split_off(1)
                             .into_iter()
                             .map(|x| x.eval_in(env))
                             .collect();
                let exec = xs.pop().unwrap().eval_in(env);
                if let Value::Function(env, exe) = exec {
                    exe.run(&env, &args)
                } else {
                    panic!()
                }
            },
            Value::Function(e,x)=> Value::Function(e,x)
        }
    }
}

pub fn evaluate(env: &mut Environment, pipe: Pipeline) {
    let Pipeline{elements, terminals} = pipe;

    for elem in elements {
        Value::List(elem.xform.0).eval_in(env);
    }
}

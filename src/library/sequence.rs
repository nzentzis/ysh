use environment::*;
use data::*;

struct MapIterator {
    func: Value,
    env: Environment,
    iters: Vec<ValueIteratorBox>,
}

impl Iterator for MapIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        // pull items from param items
        let items: Option<Vec<_>> = self.iters
                                        .iter_mut()
                                        .map(|x| x.next())
                                        .collect();

        if let Some(itms) = items {
            match self.func.execute(&self.env, itms.as_slice()) {
                Ok(res) => Some(res),
                Err(e)  => {
                    // TODO: handle this failure better
                    None
                }
            }
        } else {
            None
        }
    }
}

fn map_impl(env: &Environment, func: &Value, args: &[Value]) -> EvalResult {
    let iters: Vec<_> = args.iter().cloned().map(|x| x.into_iter()).collect();

    let s: LazySequence<MapIterator> = LazySequence::<MapIterator>::new(MapIterator {
        func: func.to_owned(),
        env: env.to_owned(),
        iters: iters
    });

    // zip over args
    Ok(Value::new(s))
}

/// Map a function over an input sequence
fn fn_map(env: &Environment, args: &[Value]) -> EvalResult {
    // adjust behavior depending on args
    // on 1 arg: build transformer
    // on 2 args: perform basic map operation
    // on 3+ args: map after zipping - in other words, map f c0 c1 c2
    //             generates [(f (first c0) (first c1) (first c2))
    //                        (f (second c0) (second c1) (second c2)...]
    //             until one list runs out of elements
    if args.len() == 1 {
        let func = args[0].clone();
        return Ok(BasicValue::function(env.to_owned(),
            Executable::native(move |env, args| {
                map_impl(env, &func, args)
            })));
    } else if args.len() >= 2 {
        // just run the map operation
        map_impl(env, &args[0], &args[1..])
    } else {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 1 // TODO: improve arity error to handle variadics
        })
    }
}

/// Take the first element of an input sequence
/// 
/// If the input is empty, returns ()
fn fn_first(env: &Environment, args: &[Value]) -> EvalResult {
    // adjust behavior depending on args
    // on 1 arg: take first element of arg
    if args.len() == 1 {
        let itm = &args[0];
        match itm.into_iter().next() {
            Some(r) => Ok(r),
            None    => Ok(BasicValue::list(vec![]))
        }
    } else {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        })
    }
}

pub fn initialize() {
    let env = global();
    env.set("map", BasicValue::function(empty(), Executable::native(fn_map)));
    env.set("first", BasicValue::function(empty(), Executable::native(fn_first)));
}

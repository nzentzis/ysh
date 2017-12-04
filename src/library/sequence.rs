use environment::*;
use numeric::*;
use data::*;

struct MapIterator {
    func: Value,
    env: Environment,
    iters: Vec<ValueIteratorBox>,
}

impl Iterator for MapIterator {
    type Item = Eval<Value>;

    fn next(&mut self) -> Option<Eval<Value>> {
        // pull items from param items
        let items: Option<Vec<_>> = self.iters
                                        .iter_mut()
                                        .map(|x| x.next())
                                        .collect();
        if let Some(itms) = items {
            let itms: Eval<Vec<_>> = itms.into_iter().collect();
            let itms = match itms {
                Ok(i) => i,
                Err(e) => return Some(Err(e))
            };

            Some(self.func.execute(&self.env, itms.as_slice()))
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
        let env = env.to_owned();
        return Ok(Value::from(
                Executable::native(move |_, args| {
                    map_impl(&env, &func, args)
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

struct FilterIterator {
    inputs: Vec<ValueIteratorBox>,
    env: Environment,
    func: Value
}

impl Iterator for FilterIterator {
    type Item = Eval<Value>;

    fn next(&mut self) -> Option<Eval<Value>> {
        loop {
            if self.inputs.is_empty() { return None }
            if let Some(ref mut iter) = self.inputs.first_mut() {
                if let Some(r) = iter.next() {
                    let r = if let Ok(r) = r { r } else { return Some(r) };
                    let cond = self.func.execute(&self.env, &[r.clone()])
                                        .and_then(|x| x.into_bool());
                    let cond = if let Ok(c) = cond {c} else {
                        return Some(Err(cond.unwrap_err()))
                    };

                    if cond { return Some(Ok(r)); }
                    else { continue; }
                }
            }
            self.inputs.remove(0);
        }
    }
}

fn filter_impl(env: &Environment, func: &Value, args: &[Value]) -> EvalResult {
    Ok(Value::new(LazySequence::new(FilterIterator {
        inputs: args.iter().map(|x| x.into_iter()).collect(),
        env: env.to_owned(),
        func: func.to_owned()
    })))
}

/// Filter a sequence using a function
/// 
/// On 1 arg: build transformer
/// On 2 args: perform normal filter operation
/// On 3 args: filter over concatenated args
fn fn_filter(env: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        let env = env.to_owned();
        let func = args[0].clone();
        return Ok(Value::from(Executable::native(move |_, args| {
            filter_impl(&env, &func, args) })));
    } else if args.len() >= 2 {
        filter_impl(env, &args[0], &args[1..])
    } else {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        })
    }
}

fn reduce_impl(env: &Environment, func: &Value, init: &Value, args: &[Value]) -> EvalResult {
    let mut state = init.to_owned();

    let mut iters: Vec<_> = args.iter().cloned().map(|x| x.into_iter()).collect();

    loop {
        // pull one item from each
        let items: Option<Vec<_>> = iters.iter_mut()
                                         .map(|x| x.next())
                                         .collect();

        if let Some(items) = items {
            let mut items: Vec<Value> = items.into_iter()
                                             .collect::<Eval<Vec<_>>>()?;
            items.insert(0, state.clone());
            state = func.execute(env, items.as_slice())?;
        } else {
            return Ok(state);
        }
    }
}

/// Left-reduce a sequence using a function
/// 
/// On 2 args: build transformer reducing using arg 1 starting with seed from
///            arg 2
/// On 3 args: reduce normally
/// On 4+ args: reduce, passing each input element as arguments. For example,
///             (reduce f 0 (1 2) (3 4) (5 6)) is equivalent to
///             (f (f 0 1 3 5) 2 4 6)
fn fn_reduce(env: &Environment, args: &[Value]) -> EvalResult {
    if args.len() < 2 {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 2
        })
    } else if args.len() == 2 {
        let env = env.to_owned();
        let func = args[0].clone();
        let init = args[1].clone();
        Ok(Value::from(Executable::native(move |_, args|
                                          reduce_impl(&env, &func, &init, args))))
    } else {
        reduce_impl(env, &args[0], &args[1], &args[2..])
    }
}

/// Take the first element of an input sequence
/// 
/// If the input is empty, returns ()
fn fn_first(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        let itm = &args[0];
        match itm.into_iter().next() {
            Some(r) => r,
            None    => Ok(Value::list(vec![]))
        }
    } else {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        })
    }
}

/// Remove the first element of an input sequence
/// 
/// If the input is empty, returns ()
fn fn_rest(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        let itm = &args[0];
        let res = itm.into_iter().skip(1).collect::<Eval<Vec<_>>>()?;
        Ok(Value::list(res))
    } else {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        })
    }
}

/// Take a specific element of the input sequence(s)
/// 
///     (nth i)     -> return a function f such that (f ...) = (nth i ...)
///     (nth i xs)  -> take the i-th element of xs
///     (nth i xs ys...) -> get a list of i-th elements, excluding those for
///                         which no element is available.
/// 
/// If the given element isn't there, return ()
fn fn_nth(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 { // build transformer
        let idx = if let Some(i) = args[0].into_num()?.map(|x| x.round()) { i }
                  else { return Err(EvalError::TypeError(String::from("index must be numeric"))); };
        Ok(Value::from(
                Executable::native(move |_, args| {
                    let res = args.iter().filter_map(|v| v.into_iter()
                                         .nth(idx as usize))
                                         .collect::<Eval<Vec<_>>>()?;
                    if res.len() == 1 { Ok(res.into_iter().next().unwrap()) }
                    else { Ok(Value::list(res)) } })))
    } else if args.len() > 1 {
        let idx = if let Some(i) = args[0].into_num()?.map(|x| x.round()) { i }
                  else { return Err(EvalError::TypeError(String::from("index must be numeric"))); };
        let res = args[1..].iter().filter_map(|v| v.into_iter()
                                                   .nth(idx as usize))
                           .collect::<Eval<Vec<_>>>()?;
        if res.len() == 1 {
            Ok(res.into_iter().next().unwrap())
        } else {
            Ok(Value::list(res))
        }
    } else {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        })
    }
}

struct ConcatIterator {
    inner: Vec<ValueIteratorBox>
}

impl Iterator for ConcatIterator {
    type Item = Eval<Value>;

    fn next(&mut self) -> Option<Eval<Value>> {
        if self.inner.is_empty() { return None }

        if let Some(ref mut iter) = self.inner.first_mut() {
            if let Some(r) = iter.next() { return Some(r) }
        }
        self.inner.remove(0);
        self.next()
    }
}

/// Get the length of an input's sequential form
/// 
/// If more than one argument is given, return a list of lengths.
fn fn_length(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() == 1 {
        Ok(Value::from(Number::int(args[0].into_seq()?.len() as i64)))
    } else if args.len() > 1 {
        Ok(Value::list(args.iter()
                           .map(|x| x.into_seq())
                           .collect::<Eval<Vec<Vec<_>>>>()?
                           .into_iter()
                           .map(|v| Value::from(Number::int(v.len() as i64)))))
    } else {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 1
        })
    }
}

/// Check whether an input's sequential form is empty
/// 
/// If supplied with multiple arguments, returns whether all are empty.
fn fn_empty_q(_: &Environment, args: &[Value]) -> EvalResult {
    Ok(Value::from(args.iter().all(|x| x.into_iter().next().is_none())))
}

/// Concatenate the sequential forms of multiple inputs
///
///     (concat xs ys zs) = ((nth 0 xs) (nth 1 ys) (nth 2 zs) ...
///                          (nth 0 xs) (nth 1 ys) (nth 2 zs) ...
///                          (nth 0 xs) (nth 1 ys) (nth 2 zs) ...)
fn fn_concat(_: &Environment, args: &[Value]) -> EvalResult {
    Ok(Value::new(LazySequence::new(ConcatIterator {
        inner: args.iter().map(|x| x.into_iter()).collect()
    })))
}

struct ConsIterator {
    inner: ValueIteratorBox,
    items: Vec<Value>
}

impl Iterator for ConsIterator {
    type Item = Eval<Value>;

    fn next(&mut self) -> Option<Eval<Value>> {
        if !self.items.is_empty() { return Some(Ok(self.items.remove(0))) }
        self.inner.next()
    }
}

/// Add one or more items to the beginning of the provided sequence
/// 
/// All items after the first are added *in order* to the first element. For
/// example, `(cons (1 2 3) 4 5 6)` would return `(4 5 6 1 2 3)`.
fn fn_cons(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() < 2 {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 2
        })
    } else {
        Ok(Value::new(LazySequence::new(ConsIterator {
            inner: args[0].into_iter(),
            items: args[1..].iter().cloned().collect()
        })))
    }
}

struct ConjIterator {
    inner: Option<ValueIteratorBox>,
    items: Vec<Value>
}

impl Iterator for ConjIterator {
    type Item = Eval<Value>;

    fn next(&mut self) -> Option<Eval<Value>> {
        if let Some(ref mut iter) = self.inner.as_mut() {
            if let Some(i) = iter.next() { return Some(i); }
        }
        self.inner = None;
        
        if self.items.is_empty() { None }
        else { Some(Ok(self.items.remove(0))) }
    }
}

/// Add one or more items to the end of the provided sequence
fn fn_conj(_: &Environment, args: &[Value]) -> EvalResult {
    if args.len() < 2 {
        Err(EvalError::Arity {
            got: args.len(),
            expected: 2
        })
    } else {
        Ok(Value::new(LazySequence::new(ConjIterator {
            inner: Some(args[0].into_iter()),
            items: args[1..].iter().cloned().collect()
        })))
    }
}

pub fn initialize() {
    let env = global();
    
    // higher-order stuff
    env.set("map", Value::from(Executable::native(fn_map)));
    env.set("reduce", Value::from(Executable::native(fn_reduce)));
    env.set("filter", Value::from(Executable::native(fn_filter)));

    // queries
    env.set("length", Value::from(Executable::native(fn_length)));
    env.set("empty?", Value::from(Executable::native(fn_empty_q)));

    // taking subranges
    env.set("first", Value::from(Executable::native(fn_first)));
    env.set("rest", Value::from(Executable::native(fn_rest)));
    env.set("nth", Value::from(Executable::native(fn_nth)));

    // list-list ops
    env.set("concat", Value::from(Executable::native(fn_concat)));

    // list-item ops
    env.set("cons", Value::from(Executable::native(fn_cons)));
    env.set("conj", Value::from(Executable::native(fn_conj)));
}

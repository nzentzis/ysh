use environment::*;
use numeric::*;
use data::*;
use evaluate::*;

lazy_static! {
    static ref DOC_MAP: Documentation = Documentation::new()
        .form(&["fn"])
        .form(&["fn", "seq"])
        .form(&["fn", "seqs*"])
        .short("Map a function over one or more sequences")
        .desc("Applies a function over the elements of one or more input\
               sequences, and collects the results in a new sequence. With only\
               a function, build and return a transformer. If more than one\
               input sequence is provided, each sequence will be used for a\
               successive parameter value. For example:

    $ map concat (1 2 3) (4 5 6) (7 8 9)
    ((1 4 7) (2 5 8) (3 6 9))");
    static ref DOC_LENGTH: Documentation = Documentation::new()
        .form(&["as-seq*"])
        .short("Get the length of input's sequential form")
        .desc("If more than one input is provided, return a list of their\
               lengths");
    static ref DOC_EMPTY: Documentation = Documentation::new()
        .form(&["as-seq*"])
        .short("Check whether an input's sequential form is empty")
        .desc("If more than one input is provided, return whether all inputs'\
               sequential forms are empty.");
    static ref DOC_FIRST: Documentation = Documentation::new()
        .form(&["as-seq*"])
        .short("Take the first element of an input sequence")
        .desc("If the input is empty, returns ()");
    static ref DOC_REST: Documentation = Documentation::new()
        .form(&["as-seq*"])
        .short("Remove the first element of an input sequence")
        .desc("If the input is empty, returns ()");
    static ref DOC_NTH: Documentation = Documentation::new()
        .form(&["idx"])
        .form(&["idx", "seq"])
        .form(&["idx", "seq*"])
        .short("Take a specific element of the input sequence(s)")
        .desc("With only an index, build a transformer. With an index and one\
               or more sequences, take the requested element of the input\
               sequences. If more than one input sequence is provided, then\
               return a list of N-th elements. If the given element isn't\
               there, then either return () or omit it from the output\
               sequence.");
    static ref DOC_CONS: Documentation = Documentation::new()
        .form(&["seq", "item*"])
        .short("Prepend one or more items to a sequence")
        .desc("Accepts a sequence and some items, and produces a new lazy \
               sequence with the provided items prepended to the input \
               sequence.");
    static ref DOC_CONJ: Documentation = Documentation::new()
        .form(&["seq", "item*"])
        .short("Append one or more items to a sequence")
        .desc("Accepts a sequence and some items, and produces a new lazy \
               sequence with the provided items appended to the input \
               sequence.");
    static ref DOC_CONCAT: Documentation = Documentation::new()
        .form(&["seq*"])
        .short("Concatenate one or more sequences")
        .desc("Return a lazy sequence which is the concatenation of all input \
               sequences.");

    static ref DOC_REDUCE: Documentation = Documentation::new()
        .form(&["fn", "seed"])
        .form(&["fn", "seed", "seq"])
        .form(&["fn", "seed", "seq0", "seq1", "seq2", "..."])
        .short("Left-reduce a sequence using a provided function")
        .desc("Performs reduction: computing the result of applying f to a seed \
               and the first element in a sequence, then applying f to the \
               result and the second item in the sequence, and so on until all \
               elements of the sequence are exhausted.\n\n\

               With two arguments, build a transformer that reduces input \
               sequences using the provided seed and function. With three args, \
               reduce the input sequence as described. With more than three \
               arguments, zip the input sequences and use the result as \
               arguments to the reduction function.\n\n\
               
               For example:\n\
               $ reduce cons () (1 2 3) (4 5 6)\n\
               (1 4 2 5 3 6)");

    static ref DOC_FILTER: Documentation = Documentation::new()
        .form(&["fn"])
        .form(&["fn", "seq*"])
        .short("Return elements of a sequence for which a function returns true")
        .desc("Performs filtering: retaining only the elements of a sequence \
               for which the given function returns a truthy value.\n\n\
               
               With one argument, build a sequence transformer which filters \
               using the provided function. With two or more arguments, filter \
               the concatenation of the sequences using the provided function \
               and return the result.\n\n\
               
               This function produces a lazy sequence; filtering is performed \
               as needed.");
}

struct MapIterator {
    func: Value,
    env: Environment,
    iters: Vec<ValueIteratorBox>,
}

impl Iterator for MapIterator {
    type Item = EvalResult;

    fn next(&mut self) -> Option<EvalResult> {
        // pull items from param items
        let items: Option<Vec<_>> = self.iters
                                        .iter_mut()
                                        .map(|x| x.next())
                                        .collect();
        if let Some(itms) = items {
            let itms: EvalRes<Vec<_>> = itms.into_iter().collect();
            let itms = match itms {
                Ok(i) => i,
                Err(e) => return Some(Err(e))
            };

            Some(self.func.execute(&self.env, itms.as_slice()).wait())
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
    type Item = EvalResult;

    fn next(&mut self) -> Option<EvalResult> {
        loop {
            if self.inputs.is_empty() { return None }
            if let Some(ref mut iter) = self.inputs.first_mut() {
                if let Some(r) = iter.next() {
                    let r = if let Ok(r) = r { r } else { return Some(r) };
                    let cond = self.func.execute(&self.env, &[r.clone()]).wait()
                                        .and_then(|x| x.into_bool().wait());
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
                                             .collect::<EvalRes<Vec<_>>>()?;
            items.insert(0, state.clone());
            state = func.execute(env, items.as_slice()).wait()?;
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
        let res = itm.into_iter().skip(1).collect::<EvalRes<Vec<_>>>()?;
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
        let idx = if let Some(i) = args[0].into_num().wait()?
                                          .map(|x| x.round()) { i }
                  else { return Err(EvalError::TypeError(String::from("index must be numeric"))); };
        Ok(Value::from(
                Executable::native(move |_, args| {
                    let res = args.iter().filter_map(|v| v.into_iter()
                                         .nth(idx as usize))
                                         .collect::<EvalRes<Vec<_>>>()?;
                    if res.len() == 1 { Ok(res.into_iter().next().unwrap()) }
                    else { Ok(Value::list(res)) } })))
    } else if args.len() > 1 {
        let idx = if let Some(i) = args[0].into_num().wait()?
                                          .map(|x| x.round()) { i }
                  else { return Err(EvalError::TypeError(String::from("index must be numeric"))); };
        let res = args[1..].iter().filter_map(|v| v.into_iter()
                                                   .nth(idx as usize))
                           .collect::<EvalRes<Vec<_>>>()?;
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
    type Item = EvalResult;

    fn next(&mut self) -> Option<EvalResult> {
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
        Ok(Value::from(Number::int(args[0].into_seq().wait()?.len() as i64)))
    } else if args.len() > 1 {
        Ok(Value::list(args.iter()
                           .map(|x| x.into_seq())
                           .collect::<Eval<Vec<Vec<_>>>>().wait()?
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
    type Item = EvalRes<Value>;

    fn next(&mut self) -> Option<EvalRes<Value>> {
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
    type Item = EvalResult;

    fn next(&mut self) -> Option<EvalResult> {
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
    env.set("map", Value::from(Executable::native(fn_map))
                  .document(&DOC_MAP));
    env.set("reduce", Value::from(Executable::native(fn_reduce))
                     .document(&DOC_REDUCE));
    env.set("filter", Value::from(Executable::native(fn_filter))
                     .document(&DOC_FILTER));

    // queries
    env.set("length", Value::from(Executable::native(fn_length))
                     .document(&DOC_LENGTH));
    env.set("empty?", Value::from(Executable::native(fn_empty_q))
                     .document(&DOC_EMPTY));

    // taking subranges
    env.set("first", Value::from(Executable::native(fn_first))
                    .document(&DOC_FIRST));
    env.set("rest", Value::from(Executable::native(fn_rest))
                   .document(&DOC_FIRST));
    env.set("nth", Value::from(Executable::native(fn_nth))
                  .document(&DOC_FIRST));

    // list-list ops
    env.set("concat", Value::from(Executable::native(fn_concat))
                     .document(&DOC_CONCAT));

    // list-item ops
    env.set("cons", Value::from(Executable::native(fn_cons))
                   .document(&DOC_CONS));
    env.set("conj", Value::from(Executable::native(fn_conj))
                   .document(&DOC_CONJ));
}

use std::fmt;
use std::sync::{Arc, Mutex};
use std::boxed::Box;
use std::cell::RefCell;

use environment::Environment;
use numeric::*;

#[derive(Clone)]
pub enum Executable {
    Native(Arc<Fn(&Environment, &[Value]) -> EvalResult + Send + Sync>),
    //Interpreted()
}

impl Executable {
    pub fn native<F>(f: F) -> Self
        where F: Fn(&Environment, &[Value]) -> EvalResult + Send + Sync + 'static {
        Executable::Native(Arc::new(f))
    }

    pub fn run(&self, env: &Environment, args: &[Value]) -> EvalResult {
        match self {
            &Executable::Native(ref f) => f(env, args)
        }
    }
}

#[derive(Clone)]
pub enum BasicValue {
    Boolean(bool),
    Number(Number), // TODO: more arithmetic tower support
    Str(String),
    Symbol(Identifier),
    List(Vec<Value>),
    Function(Environment, Executable),
    Macro(Environment, Executable),
    // TODO: Lazy(Box<FnOnce()->Value>),
}

pub type ValueIteratorBox = Box<Iterator<Item=Value>+Send+Sync>;
pub type EvalResult = Result<Value, EvalError>;

#[derive(Clone)]
pub struct Value {
    inner: Arc<ValueLike + 'static>
}

impl Value {
    /// Generate a new value with the given content
    pub fn new<T: ValueLike + 'static>(x: T) -> Value {
        Value {
            inner: Arc::new(x)
        }
    }

    /// Perform macro expansion on the contained form
    pub fn macroexpand(self) -> EvalResult {
        match self.get_basic() {
            Some(&BasicValue::List(ref xs)) => {
                let m =
                    xs.first()
                      .and_then(|x| x.get_symbol())
                      .and_then(|sym| ::environment::global().get(&*(sym.0)))
                      .and_then(|val| match val.get_basic() {
                          Some(&BasicValue::Macro(ref env, ref exec)) =>
                              Some((env.clone(), exec.clone())),
                          _ => None
                      });

                // check the first element to see if we can resolve it
                if let Some((env, exec)) = m {
                    let body: Result<Vec<_>, EvalError> =
                        self.into_iter().skip(1)
                            .map(|v| v.macroexpand())
                            .collect();
                    return exec.run(&env, body?.as_slice());
                }
            },
            _ => {}
        }
        Ok(self)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if let Some(b) = self.get_basic() {
            write!(f, "{:?}", b)
        } else {
            write!(f, "<item: {}>", self.inner.into_str())
        }
    }
}

impl ValueLike for Value {
    fn get_basic(&self) -> Option<&BasicValue> { self.inner.get_basic() }
    fn get_symbol(&self) -> Option<Identifier> { self.inner.get_symbol() }
    fn get_string(&self) -> Option<String> { self.inner.get_string() }
    fn into_seq(&self) -> Vec<Value> { self.inner.into_seq() }
    fn into_iter(&self) -> ValueIteratorBox { self.inner.into_iter() }
    fn into_str(&self) -> String { self.inner.into_str() }
    fn into_num(&self) -> Option<Number> { self.inner.into_num() }
    fn into_bool(&self) -> bool { self.inner.into_bool() }
    fn into_args(&self) -> Vec<String> { self.inner.into_args() }
    fn is_executable(&self) -> bool { self.inner.is_executable() }
    fn evaluate(&self, env: &Environment) -> EvalResult {
        self.inner.evaluate(env)
    }
    fn execute(&self, env: &Environment, args: &[Value]) -> EvalResult {
        self.inner.execute(env, args)
    }
    fn first(&self) -> Option<&ValueLike> { self.inner.first() }
}

#[derive(Debug, Clone)]
pub enum EvalError {
    Unknown,
    InvalidOperation(&'static str),
    TypeError(String),
    Arity {
        got: usize,
        expected: usize
    }
}

impl ::std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            &EvalError::Unknown =>
                write!(f, "Unknown evaluation error"),
            &EvalError::InvalidOperation(ref s) =>
                write!(f, "Invalid operation: {}", s),
            &EvalError::TypeError(ref s) =>
                write!(f, "Type error: {}", s),
            &EvalError::Arity {got, expected} =>
                write!(f, "Got {} arguments, expected {}", got, expected),
        }
    }
}

impl ::std::error::Error for EvalError {
    fn description(&self) -> &str {
        match self {
            &EvalError::Unknown => &"unknown evaluation error",
            &EvalError::InvalidOperation(_) => &"invalid operation",
            &EvalError::TypeError(_) => &"type error",
            &EvalError::Arity{..} => &"arity mismatch",
        }
    }
}

/// Trait for basic behavior which applies to all value-like types. If possible,
/// make other code dependent on these rather than requiring `Value` structures
/// directly.
/// 
/// These functions may block, if the value is lazy and has not yet computed its
/// final content.
pub trait ValueLike : Send + Sync {
    /// Get the relevant basic value if available
    fn get_basic(&self) -> Option<&BasicValue> { None }
    
    /// Get the value's symbol (if applicable)
    /// 
    /// This should *not* perform any conversions -- just return existing data
    fn get_symbol(&self) -> Option<Identifier> { None }

    /// Get the value's string (if applicable)
    /// 
    /// Like `get_symbol`, this shouldn't do any conversions
    fn get_string(&self) -> Option<String> { None }

    /// Convert into a sequential form
    /// 
    /// This must be equivalent to calling `self.into_iter().collect()`
    fn into_seq(&self) -> Vec<Value> { self.into_iter().collect() }

    /// Generate an iterator over the element's values
    /// 
    /// This provides an opportunity for lazy sequences to delay evaluation and
    /// should be used when possible.
    fn into_iter(&self) -> ValueIteratorBox;

    /// Convert a value into string form
    fn into_str(&self) -> String;

    /// Convert a value into numeric form
    fn into_num(&self) -> Option<Number> { None }

    /// Check whether a value is truth-like
    fn into_bool(&self) -> bool { true }

    /// Convert into a vector of strings usable as command-line arguments
    fn into_args(&self) -> Vec<String> {
        self.into_iter().map(|x| x.into_str()).collect()
    }

    /// Evaluate this value-like object in a given context
    fn evaluate(&self, env: &Environment) -> EvalResult;

    /// Whether the value can be executed with arguments
    fn is_executable(&self) -> bool { false }

    /// Execute the value with the given arguments, and return the result
    /// 
    /// If the value is not executable, return Err() with the original
    /// arguments.
    fn execute(&self, env: &Environment, args: &[Value]) -> EvalResult {
        Err(EvalError::InvalidOperation("execution not supported"))
    }

    /// Get the first element of the object's sequential form
    fn first(&self) -> Option<&ValueLike>;
}

impl BasicValue {
    /// Generate an empty value
    pub fn empty() -> Value {
        Value::new(BasicValue::List(Vec::new()))
    }

    /// Generate a new function
    pub fn function(env: Environment, body: Executable) -> Value {
        Value::new(BasicValue::Function(env, body))
    }

    /// Build a new list value
    pub fn list<I: IntoIterator<Item=Value>>(i: I) -> Value {
        Value::new(BasicValue::List(i.into_iter().collect()))
    }

    /// Build a string value
    pub fn str<S: ::std::borrow::Borrow<str>>(s: S) -> Value {
        Value::new(BasicValue::Str(s.borrow().to_owned()))
    }
}

impl ValueLike for BasicValue {
    fn get_basic(&self) -> Option<&BasicValue> { Some(self) }

    fn get_symbol(&self) -> Option<Identifier> {
        if let &BasicValue::Symbol(ref id) = self { Some(id.to_owned()) }
        else { None }
    }

    fn get_string(&self) -> Option<String> {
        if let &BasicValue::Symbol(ref id) = self { Some((&*id.0).to_owned()) }
        else if let &BasicValue::Str(ref s) = self { Some(s.to_owned()) }
        else { None }
    }

    fn into_seq(&self) -> Vec<Value> {
        if let &BasicValue::List(ref l) = self {
            l.to_owned()
        } else {
            vec![Value::new(self.to_owned())]
        }
    }

    fn into_iter(&self) -> ValueIteratorBox {
        if let &BasicValue::List(ref l) = self {
            Box::new(l.to_owned().into_iter())
        } else {
            let itm: Value = Value::new(self.to_owned());
            Box::new(vec![itm].into_iter())
        }
    }

    fn into_str(&self) -> String {
        match self {
            &BasicValue::Boolean(true)       => String::from("true"),
            &BasicValue::Boolean(false)      => String::from("false"),
            &BasicValue::Number(ref n)       => format!("{}", n),
            &BasicValue::Str(ref s)          => s.to_owned(),
            &BasicValue::Symbol(ref id)      => (*(id.0)).to_owned(),
            &BasicValue::List(ref l)         => {
                let mut s = String::with_capacity(128);
                s.push('(');
                let xs: Vec<_> = l.into_iter().map(|x| x.into_str()).collect();
                s.push_str(&xs.join(" "));
                s.push(')');
                s
            },
            &BasicValue::Function(_,_)       => String::from("<function>"),
            &BasicValue::Macro(_,_)          => String::from("<macro>"),
        }
    }

    fn into_num(&self) -> Option<Number> {
        if let &BasicValue::Number(ref n) = self {
            Some(n.to_owned())
        } else {
            None
        }
    }

    fn into_bool(&self) -> bool {
        match self {
            &BasicValue::Boolean(b)          => b,
            &BasicValue::Number(ref n)       => true, // TODO: 0 -> false
            &BasicValue::Str(ref s)          => !s.is_empty(),
            &BasicValue::Symbol(ref id)      => true,
            &BasicValue::List(ref l)         => if l.is_empty() { false }
                                                else { l.iter().any(|x| x.into_bool()) },
            &BasicValue::Function(_,_)       => true,
            &BasicValue::Macro(_,_)          => true,
        }
    }

    fn into_args(&self) -> Vec<String> {
        match self {
            &BasicValue::Boolean(true)       => vec![self.into_str()],
            &BasicValue::Boolean(false)      => vec![self.into_str()],
            &BasicValue::Number(ref n)       => vec![self.into_str()],
            &BasicValue::Str(ref s)          => vec![self.into_str()],
            &BasicValue::Symbol(ref id)      => vec![self.into_str()],
            &BasicValue::List(ref l)         => l.into_iter()
                                           .flat_map(|x| x.into_args())
                                           .collect(),
            &BasicValue::Function(_,_)       => vec![self.into_str()],
            &BasicValue::Macro(_,_)          => vec![self.into_str()],
        }
    }

    fn evaluate(&self, env: &Environment) -> EvalResult {
        match self {
            &BasicValue::Symbol(ref s) => {
                // try looking it up
                if let Some(v) = env.get(&*(s.0)) {
                    Ok(v)
                } else {
                    Ok(Value::new(self.clone()))
                }
            },
            &BasicValue::List(ref xs) => {
                let evaluated: Result<Vec<_>, _> = xs.iter()
                                                     .map(|x| x.evaluate(env))
                                                     .collect();
                let xs = evaluated?;

                // evaluate () as ()
                if xs[0].is_executable() {
                    let mut xs = xs;
                    let args: Vec<_> = xs.split_off(1);
                    xs.pop().unwrap()
                      .evaluate(env)
                      .and_then(|e| e.execute(env, args.as_slice()))
                } else {
                    Ok(BasicValue::list(xs))
                }
            },
            &BasicValue::Macro(_,_) => panic!("illegal attempt to evaluate macro"),
            r => Ok(Value::new(r.to_owned()))
        }
    }

    fn is_executable(&self) -> bool {
        match self {
            &BasicValue::Boolean(_)       => false,
            &BasicValue::Number(_)        => false,
            &BasicValue::Str(_)           => false,
            &BasicValue::Symbol(_)        => false,
            &BasicValue::List(_)          => false,
            &BasicValue::Function(_,_)    => true,
            &BasicValue::Macro(_,_)       => false,
        }
    }

    fn execute(&self, env: &Environment, args: &[Value]) -> EvalResult {
        match self {
            // TODO: force evaluate args in outer environment
            &BasicValue::Function(ref e,ref f) => {
                // for functions, evaluate all args. macros won't do this.
                let vals: Result<Vec<_>, EvalError> =
                    args.iter().map(|x| x.evaluate(env)).collect();
                f.run(&e, vals?.as_slice())
            },
            &BasicValue::Macro(_,_) => panic!("illegal attempt to execute macro"),
            x => Err(EvalError::TypeError(format!("object '{:?}' is not executable", x))),
        }
    }

    fn first(&self) -> Option<&ValueLike> {
        match self {
            &BasicValue::Boolean(_)       => Some(self),
            &BasicValue::Number(_)        => Some(self),
            &BasicValue::Str(_)           => Some(self),
            &BasicValue::Symbol(_)        => Some(self),
            &BasicValue::List(ref v)      => v.first().map(|x| -> &ValueLike {&*x}),
            &BasicValue::Function(_,_)    => Some(self),
            &BasicValue::Macro(_,_)       => Some(self),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        unimplemented!()
        /*
        match (self, other) {
            (&Value::Boolean(x),     &Value::Boolean(y))    => x == y,
            (&Value::Number(ref x),  &Value::Number(ref y)) => x == y,
            (&Value::Str(ref x),     &Value::Str(ref y))    => x == y,
            (&Value::Symbol(ref x),  &Value::Symbol(ref y)) => x == y,
            (&Value::List(ref x),    &Value::List(ref y))   => x == y,

            // don't bother comparing functions or wrappers yet
            (&Value::Function(_,_),&Value::Function(_,_))   => false,
            _                                               => false
        }
        */
    }
}

struct LazySequenceIterator<I> where I: Iterator<Item=Value>+Send+Sync {
    item_offset: usize, // offset into items of current element
    items: Arc<Mutex<Vec<Value>>>,
    rest: Arc<Mutex<I>>
}

impl<I: Iterator<Item=Value>+Send+Sync> Iterator for LazySequenceIterator<I> {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        let mut items = self.items.lock().unwrap();
        if self.item_offset < items.len() {
            let r = items[self.item_offset].clone();
            self.item_offset += 1;
            return Some(r);
        }

        // no items left in vector - check iterator
        let mut rest = self.rest.lock().unwrap();
        match rest.next() {
            None => None,
            Some(itm) => {
                // add to the item vec
                items.push(itm.clone());
                self.item_offset += 1;
                return Some(itm)
            }
        }
    }
}

/// Utility type for converting lazy iterators into sequence types
/// 
/// Note that this assumes the given iterator ends when `next` returns `None`.
pub struct LazySequence<I> where I: Iterator<Item=Value>+Send+Sync {
    // always lock `items` first if locking both
    items: Arc<Mutex<Vec<Value>>>,
    rest: Arc<Mutex<I>>
}

impl<I: Iterator<Item=Value>+Send+Sync> LazySequence<I> {
    pub fn new(iter: I) -> Self {
        LazySequence {
            items: Arc::new(Mutex::new(Vec::new())),
            rest: Arc::new(Mutex::new(iter))
        }
    }
}

impl<I: Iterator<Item=Value>+Send+Sync+'static> ValueLike for LazySequence<I> {
    fn into_iter(&self) -> ValueIteratorBox {
        Box::new(LazySequenceIterator {
            item_offset: 0,
            items: Arc::clone(&self.items),
            rest: Arc::clone(&self.rest),
        })
    }

    fn into_str(&self) -> String {
        let mut s = String::new();
        s.push('(');
        let body: Vec<_> = self.into_iter().map(|x| x.into_str()).collect();
        s.push_str(&body.join(" "));
        s.push(')');

        s
    }

    fn evaluate(&self, env: &Environment) -> EvalResult {
        Ok(Value::new((*self).clone()))
    }

    fn first(&self) -> Option<&ValueLike> {
        // TODO: try to find an efficient way to do this in the future
        None
    }
}

impl fmt::Debug for BasicValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &BasicValue::Boolean(b)    => write!(f, "<bool:{}>", b),
            &BasicValue::Number(ref n) => write!(f, "<num:{}>", n),
            &BasicValue::Str(ref s)    => write!(f, "<str:\"{}\">", s),
            &BasicValue::Symbol(ref s) => write!(f, "<sym:{}>", s.0),
            &BasicValue::List(ref v)   => write!(f, "{:?}", v),
            &BasicValue::Function(_,_) => write!(f, "<function>"),
            &BasicValue::Macro(_,_)    => write!(f, "<macro>"),
        }
    }
}

impl<I: Iterator<Item=Value>+Send+Sync> Clone for LazySequence<I> {
    fn clone(&self) -> Self {
        LazySequence {
            items: Arc::clone(&self.items),
            rest: Arc::clone(&self.rest),
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum PipeMode {
    Pipe, // |
    DelimitedPipe(char), // |.> forall .
    PipeText // |> - pipe without semantic interpretation
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Identifier(pub Arc<String>);

impl Identifier {
    pub fn new<R: AsRef<str>>(s: R) -> Self {
        Identifier(Arc::new(s.as_ref().to_owned()))
    }

    pub fn from(s: String) -> Self {
        Identifier(Arc::new(s))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TerminalMode {
    ReplaceFile(String), // > [file]
    AppendFile(String), // >> [file]
    SetVariable(Identifier), // >= name
    AppendVariable(Identifier), // >>= name
    InputFile(String), // < [file]
    InputVar(Identifier), // <= name
}

#[derive(PartialEq, Clone, Debug)]
pub struct Transformer(pub Vec<Value>);

#[derive(PartialEq, Clone, Debug)]
pub struct PipelineComponent {
    /// The transformer to execute in this component
    pub xform: Transformer,

    /// The terminating element for this component. If `None`, then this is the
    /// last part of the pipeline.
    pub link: Option<PipeMode>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Pipeline {
    /// List of pipeline stages
    pub elements: Vec<PipelineComponent>,

    /// Pipeline-global routing components
    pub terminals: Vec<TerminalMode>
}

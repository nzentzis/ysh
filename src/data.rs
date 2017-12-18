use std::fmt;
use std::sync::{Arc, Mutex};
use std::boxed::Box;
use std::collections::HashMap;

use environment::Environment;
use numeric::*;

#[derive(Clone)]
pub enum Executable {
    /// Native function which acts like an interpreted one
    /// 
    /// Arguments will be evaluated *prior* to running the function. The
    /// function accepts a reference to the lexical environment at call time.
    Native(Arc<Fn(&Environment, &[Value]) -> EvalResult + Send + Sync>),

    /// Interpreted function
    /// 
    /// The inner function accepts a reference to the stored environment
    Interpreted(Environment, Arc<Fn(&Environment, &[Value]) -> EvalResult + Send + Sync>),

    /// Native implementation of a core form
    /// 
    /// Similar to the `Native` type, but arguments are not evaluated prior to
    /// running it. The function accepts a reference to the lexical environment
    /// at call time.
    CoreFn(fn(&Environment, &[Value]) -> EvalResult),
}

impl Executable {
    pub fn native<F>(f: F) -> Self
        where F: Fn(&Environment, &[Value]) -> EvalResult + Send + Sync + 'static {
        Executable::Native(Arc::new(f))
    }

    pub fn run(&self, lexical: &Environment, args: &[Value]) -> EvalResult {
        match self {
            &Executable::Native(ref f) => {
                let vals: Result<Vec<_>, EvalError> =
                    args.iter().map(|x| x.evaluate(lexical)).collect();
                f(lexical, vals?.as_slice())
            },
            &Executable::CoreFn(ref f) => f(lexical, args),
            &Executable::Interpreted(ref env, ref f) => f(env, args)
        }
    }
}

pub type ValueIteratorBox = Box<Iterator<Item=EvalResult>+Send+Sync>;
pub type EvalResult = Result<Value, EvalError>;
pub type Eval<T> = Result<T, EvalError>;

#[derive(Debug)]
/// An type representing the errors which can occur when evaluating a form
pub enum EvalError {
    Unknown,
    IO(::std::io::Error),
    Runtime(String),
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
            &EvalError::IO(ref e) =>
                write!(f, "I/O error: {}", e),
            &EvalError::InvalidOperation(ref s) =>
                write!(f, "Invalid operation: {}", s),
            &EvalError::TypeError(ref s) =>
                write!(f, "Type error: {}", s),
            &EvalError::Runtime(ref s) =>
                write!(f, "Runtime error: {}", s),
            &EvalError::Arity {got, expected} =>
                write!(f, "Got {} arguments, expected {}", got, expected),
        }
    }
}

impl ::std::error::Error for EvalError {
    fn description(&self) -> &str {
        match self {
            &EvalError::Unknown => &"unknown evaluation error",
            &EvalError::IO(_) => &"I/O error",
            &EvalError::InvalidOperation(_) => &"invalid operation",
            &EvalError::TypeError(_) => &"type error",
            &EvalError::Runtime(_) => &"runtime error",
            &EvalError::Arity{..} => &"arity mismatch",
        }
    }
}

impl From<::std::io::Error> for EvalError {
    fn from(e: ::std::io::Error) -> EvalError { EvalError::IO(e) }
}

/// Trait for basic behavior which applies to all value-like types. If possible,
/// make other code dependent on these rather than requiring `Value` structures
/// directly.
/// 
/// These functions may block, if the value is lazy and has not yet computed its
/// final content.
pub trait ValueLike : Send + Sync {
    /// Get the value's symbol (if applicable)
    /// 
    /// This should *not* perform any conversions -- just return existing data
    fn get_symbol(&self) -> Eval<Option<Identifier>> { Ok(None) }

    /// Get the value's string (if applicable)
    /// 
    /// Like `get_symbol`, this shouldn't do any conversions
    fn get_string(&self) -> Eval<Option<String>> { Ok(None) }

    /// Convert into a sequential form
    /// 
    /// This must be equivalent to calling `self.into_iter().collect()`
    fn into_seq(&self) -> Eval<Vec<Value>> { self.into_iter().collect() }

    /// Generate an iterator over the element's values
    /// 
    /// This provides an opportunity for lazy sequences to delay evaluation and
    /// should be used when possible.
    fn into_iter(&self) -> ValueIteratorBox;

    /// Convert a value into string form
    fn into_str(&self) -> Eval<String>;

    /// Convert a value into numeric form
    fn into_num(&self) -> Eval<Option<Number>> { Ok(None) }

    /// Check whether a value is truth-like
    fn into_bool(&self) -> Eval<bool> { Ok(true) }

    /// Convert into a vector of strings usable as command-line arguments
    fn into_args(&self) -> Eval<Vec<String>> {
        self.into_seq()?
            .into_iter()
            .map(|x| x.into_str()).collect()
    }

    /// Evaluate this value-like object in a given lexical environment
    fn evaluate(&self, env: &Environment) -> EvalResult;

    /// Whether the value can be executed with arguments
    fn is_executable(&self) -> bool { false }

    /// Execute the value with the given arguments, and return the result
    /// 
    /// If the value is not executable, return Err() with the original
    /// arguments.
    fn execute(&self, _env: &Environment, _args: &[Value]) -> EvalResult {
        Err(EvalError::InvalidOperation("execution not supported"))
    }

    /// Get the first element of the object's sequential form
    fn first(&self) -> Eval<Option<Value>>;
}

#[derive(Clone)]
pub enum ValueData {
    Boolean(bool),
    Number(Number),
    Str(String),
    Symbol(Identifier),
    Atom(Arc<String>), // TODO: add interning
    Map(HashMap<ValueHash, Value>),
    List(Vec<Value>),
    Function(Executable),
    Macro(Executable),
    Polymorphic(Arc<ValueLike + 'static>),
    // TODO: Lazy(Box<FnOnce()->Value>),
}

#[derive(Debug)]
pub struct Documentation {
    pub origin: Option<&'static str>,
    pub short_desc: Option<&'static str>,
    pub description: Option<&'static str>,
    pub forms: Vec<&'static [&'static str]>
}

impl Documentation {
    /// Generate a new, empty, documentation object
    pub fn new() -> Documentation {
        Documentation {
            origin: None,
            short_desc: None,
            description: None,
            forms: vec![]
        }
    }

    /// Modify the origin of a documentation object
    pub fn origin(mut self, origin: &'static str) -> Self {
        self.origin = Some(origin);
        self
    }

    /// Modify the description of a documentation object
    /// 
    /// The passed text may be word-wrapped at arbitrary boundaries.
    pub fn desc(mut self, description: &'static str) -> Self {
        self.description = Some(description);
        self
    }

    /// Modify the short description of a documentation object
    ///
    /// The passed text may be word-wrapped at arbitrary boundaries
    pub fn short(mut self, short: &'static str) -> Self {
        self.short_desc = Some(short);
        self
    }

    /// Add a new form
    pub fn form(mut self, form: &'static [&'static str]) -> Self {
        self.forms.push(form);
        self
    }
}

#[derive(Clone, Debug)]
/// Unified storage for a value and its hash
///
/// Since values cannot be reliably hashed (the operation may fail with an
/// `EvalError`) this wraps a value and stored hash to allow the use of values
/// as hashable objects.
pub struct ValueHash {
    pub hash: u64,
    pub val: Value
}

impl ::std::hash::Hash for ValueHash {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl ::std::ops::Deref for ValueHash {
    type Target = Value;
    fn deref(&self) -> &Value { &self.val }
}

impl PartialEq for ValueHash {
    fn eq(&self, other: &Self) -> bool { self.val == other.val }
}

impl Eq for ValueHash {}

#[derive(Clone, Debug)]
pub struct Value {
    pub data: ValueData,
    pub name: Option<Identifier>,
    pub doc: Option<&'static Documentation>
}

impl Value {
    /// Change the value's name
    pub fn rename(mut self, name: Identifier) -> Value {
        self.name = Some(name);
        self
    }

    /// Add documentation to a value
    pub fn document(mut self, doc: &'static Documentation) -> Value {
        self.doc = Some(doc);
        self
    }

    /// Generate an empty value
    pub fn empty() -> Value {
        Value {
            data: ValueData::List(Vec::new()),
            name: None,
            doc: None
        }
    }

    /// Generate an empty value
    pub fn new<T: ValueLike+'static>(x: T) -> Value {
        Value {
            data: ValueData::Polymorphic(Arc::new(x)),
            name: None,
            doc: None
        }
    }

    /// Build a new list value
    pub fn list<I: IntoIterator<Item=Value>>(i: I) -> Value {
        Value {
            data: ValueData::List(i.into_iter().collect()),
            name: None,
            doc: None
        }
    }

    /// Build a string value
    pub fn str<S: ::std::borrow::Borrow<str>>(s: S) -> Value {
        Value {
            data: ValueData::Str(s.borrow().to_owned()),
            name: None,
            doc: None
        }
    }

    /// Build an atom
    pub fn atom<S: AsRef<str>>(s: S) -> Value {
        Value {
            data: ValueData::Atom(Arc::new(String::from(s.as_ref()))),
            name: None,
            doc: None
        }
    }

    /// Build a hashmap
    pub fn map<I: IntoIterator<Item=(ValueHash, Value)>>(i: I) -> Value {
        Value::from(ValueData::Map(i.into_iter().collect::<HashMap<_,_>>()))
    }

    /// Perform macro expansion on the contained form
    pub fn macroexpand(self) -> EvalResult {
        match self.data {
            ValueData::List(ref xs) => {
                let macro_expr: Option<Value> =
                    if let Some(f) = xs.first() {
                        if f.is_macro() { Some(f.to_owned()) }
                        else { f.get_symbol()?
                                .and_then(|sym|
                                          ::environment::global().get(&*(sym.0))) }
                    } else { None };
                let macro_expr =
                    if let Some(m) = macro_expr {
                        match m.data {
                            ValueData::Macro(ref exec) => Some(exec.clone()),
                            _ => None
                        }
                    } else { None };

                // check the first element to see if we can resolve it
                if let Some(exec) = macro_expr {
                    let body = self.into_seq()?;
                    let r = exec.run(&::environment::empty(), &body[1..])?;
                    return r.macroexpand();
                }
            },
            _ => {}
        }
        Ok(self)
    }

    /// Check whether the value is a macro
    pub fn is_macro(&self) -> bool {
        if let &ValueData::Macro(_) = &self.data { true } else { false }
    }

    /// Try to compute a hashed version of the underlying values
    pub fn hash(&self) -> Eval<Option<ValueHash>> {
        use std::hash::{Hasher, Hash};

        let mut hasher = ::std::collections::hash_map::DefaultHasher::new();
        match &self.data {
            &ValueData::Boolean(x)    => x.hash(&mut hasher),
            &ValueData::Number(ref n) => n.hash(&mut hasher),
            &ValueData::Str(ref x)    => x.hash(&mut hasher),
            &ValueData::Symbol(ref x) => x.as_ref().hash(&mut hasher),
            &ValueData::Atom(ref x)   => x.hash(&mut hasher),
            &ValueData::Map(ref x) => {
                for (ref k, ref v) in x {
                    k.hash(&mut hasher);
                    match v.hash()? {
                        Some(v) => v.hash(&mut hasher),
                        None => return Ok(None)
                    }
                }
            },
            &ValueData::List(ref x) => {
                for i in x {
                    match i.hash()? {
                        Some(v) => v.hash(&mut hasher),
                        None => return Ok(None)
                    }
                }
            },
            _ => return Ok(None)
        }

        Ok(Some(ValueHash {
            hash: hasher.finish(),
            val: self.to_owned()
        }))
    }
}

impl From<Executable> for Value {
    fn from(e: Executable) -> Value {
        Value {
            data: ValueData::Function(e),
            name: None,
            doc: None
        }
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Value {
        Value {
            data: ValueData::Number(n),
            name: None,
            doc: None
        }
    }
}

impl From<Identifier> for Value {
    fn from(i: Identifier) -> Value {
        Value {
            data: ValueData::Symbol(i),
            name: None,
            doc: None
        }
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Value {
        Value {
            data: ValueData::Boolean(x),
            name: None,
            doc: None
        }
    }
}

impl From<ValueData> for Value {
    fn from(x: ValueData) -> Value {
        Value {data: x, name: None, doc: None}
    }
}

impl ValueLike for Value {
    fn get_symbol(&self) -> Eval<Option<Identifier>> {
        if let ValueData::Symbol(ref id) = self.data { Ok(Some(id.to_owned())) }
        else if let ValueData::Polymorphic(ref p) = self.data { p.get_symbol() }
        else { Ok(None) }
    }

    fn get_string(&self) -> Eval<Option<String>> {
        if let &ValueData::Symbol(ref id) = &self.data { Ok(Some((&*id.0).to_owned())) }
        else if let &ValueData::Str(ref s) = &self.data { Ok(Some(s.to_owned())) }
        else if let &ValueData::Polymorphic(ref p) = &self.data { p.get_string() }
        else { Ok(None) }
    }

    fn into_seq(&self) -> Eval<Vec<Value>> {
        if let &ValueData::List(ref l) = &self.data {
            Ok(l.to_owned())
        } else if let &ValueData::Polymorphic(ref p) = &self.data {
            p.into_seq()
        } else {
            Ok(vec![self.to_owned()])
        }
    }

    fn into_iter(&self) -> ValueIteratorBox {
        if let &ValueData::List(ref l) = &self.data {
            Box::new(l.to_owned().into_iter().map(Ok))
        } else if let &ValueData::Polymorphic(ref p) = &self.data {
            p.into_iter()
        } else {
            let itm = self.to_owned();
            Box::new(vec![itm].into_iter().map(Ok))
        }
    }

    fn into_str(&self) -> Eval<String> {
        match &self.data {
            &ValueData::Boolean(true)       => Ok(String::from("true")),
            &ValueData::Boolean(false)      => Ok(String::from("false")),
            &ValueData::Number(ref n)       => Ok(format!("{}", n)),
            &ValueData::Str(ref s)          => Ok(s.to_owned()),
            &ValueData::Symbol(ref id)      => Ok((*(id.0)).to_owned()),
            &ValueData::Atom(ref a)         => Ok(format!(":{}", a)),
            &ValueData::Map(ref m) => {
                let mut items = Vec::new();
                for (k,v) in m {
                    items.push(k.val.into_str()?);
                    items.push(v.into_str()?);
                }

                let mut s = String::with_capacity(128);
                s.push('{');
                s.push_str(&items.join(" "));
                s.push('}');
                Ok(s)
            },
            &ValueData::List(ref l)         => {
                let mut s = String::with_capacity(128);
                s.push('(');
                let xs: Vec<_> = l.into_iter()
                                  .map(|x| x.into_str())
                                  .collect::<Eval<Vec<_>>>()?;
                s.push_str(&xs.join(" "));
                s.push(')');
                Ok(s)
            },
            &ValueData::Function(_)         => Ok(match &self.name {
                &None        => String::from("<anonymous fn>"),
                &Some(ref s) => format!("<fn '{}'>", s.as_ref()),
            }),
            &ValueData::Macro(_)            => Ok(String::from("<macro>")),
            &ValueData::Polymorphic(ref v)  => v.into_str()
        }
    }

    fn into_num(&self) -> Eval<Option<Number>> {
        if let &ValueData::Number(ref n) = &self.data {
            Ok(Some(n.to_owned()))
        } else if let &ValueData::Polymorphic(ref p) = &self.data {
            p.into_num()
        } else {
            Ok(None)
        }
    }

    fn into_bool(&self) -> Eval<bool> {
        match &self.data {
            &ValueData::Boolean(b)          => Ok(b),
            &ValueData::Number(ref n)       => Ok(n.round() != 0),
            &ValueData::Str(ref s)          => Ok(!s.is_empty()),
            &ValueData::Symbol(_)           => Ok(true),
            &ValueData::Atom(_)             => Ok(true),
            &ValueData::Map(ref m)          => Ok(!m.is_empty()),
            &ValueData::List(ref l) => Ok(
                if l.is_empty() { false }
                else { l.iter().map(|x| x.into_bool())
                                         .collect::<Eval<Vec<bool>>>()?
                                         .into_iter()
                                         .any(|x| x) }),
            &ValueData::Function(_)         => Ok(true),
            &ValueData::Macro(_)            => Ok(true),
            &ValueData::Polymorphic(ref v)  => v.into_bool()
        }
    }

    fn into_args(&self) -> Eval<Vec<String>> {
        match &self.data {
            &ValueData::List(ref l) => l.into_iter()
                                         .map(|x| x.into_args())
                                         .collect::<Eval<Vec<_>>>()
                                         .map(|r| r.into_iter()
                                                   .flat_map(|x| x)
                                                   .collect()),
            _ => self.into_str().map(|r| vec![r])
        }
    }

    fn evaluate(&self, env: &Environment) -> EvalResult {
        match &self.data {
            &ValueData::Symbol(ref s) => {
                // try looking it up
                if let Some(v) = env.get(&*(s.0)) {
                    Ok(v)
                } else {
                    Ok(self.clone())
                }
            },
            &ValueData::List(ref xs) => {
                // evaluate () as ()
                if xs.is_empty() { return Ok(Value::list(vec![])); }
                let first = xs[0].evaluate(env)?;

                if first.is_executable() {
                    first.execute(env, &xs[1..])
                } else {
                    // evaluate elements when using bare list
                    let xs: Vec<_> = xs.into_iter()
                                       .map(|v| v.evaluate(env))
                                       .collect::<Result<Vec<_>, _>>()?;
                    Ok(Value::list(xs))
                }
            },
            &ValueData::Polymorphic(ref p) => p.evaluate(env),
            _ => Ok(self.to_owned())
        }
    }

    fn is_executable(&self) -> bool {
        match &self.data {
            &ValueData::Function(_)        => true,
            &ValueData::Polymorphic(ref v) => v.is_executable(),
            _                              => false
        }
    }

    fn execute(&self, env: &Environment, args: &[Value]) -> EvalResult {
        match &self.data {
            // TODO: force evaluate args in outer environment
            // delegate to inner evaluation function
            &ValueData::Function(ref f) => f.run(env, args),
            &ValueData::Macro(_)        => panic!("illegal attempt to execute macro"),
            &ValueData::Polymorphic(ref v) => v.execute(env, args),
            x => Err(EvalError::TypeError(format!("object '{:?}' is not executable", x))),
        }
    }

    fn first(&self) -> Eval<Option<Value>> {
        match &self.data {
            &ValueData::List(ref v)        => Ok(v.first().map(|x| x.clone())),
            &ValueData::Polymorphic(ref v) => v.first(),
            _                              => Ok(Some(self.clone()))
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (&self.data, &other.data) {
            (&ValueData::Boolean(x),     &ValueData::Boolean(y))    => x == y,
            (&ValueData::Number(ref x),  &ValueData::Number(ref y)) => x == y,
            (&ValueData::Str(ref x),     &ValueData::Str(ref y))    => x == y,
            (&ValueData::Symbol(ref x),  &ValueData::Symbol(ref y)) => x == y,
            (&ValueData::Atom(ref x),    &ValueData::Atom(ref y))   => x == y,
            (&ValueData::Map(ref m),     &ValueData::Map(ref n))    => m == n,
            (&ValueData::List(ref x),    &ValueData::List(ref y))   => x == y,

            // don't bother comparing functions or wrappers yet
            (&ValueData::Function(_),    &ValueData::Function(_))   => false,
            _                                                       => false
        }
    }
}

struct LazySequenceIterator<I> where I: Iterator<Item=Eval<Value>>+Send+Sync {
    item_offset: usize, // offset into items of current element
    items: Arc<Mutex<Vec<Value>>>,
    rest: Arc<Mutex<I>>
}

impl<I: Iterator<Item=Eval<Value>>+Send+Sync> Iterator for LazySequenceIterator<I> {
    type Item = Eval<Value>;

    fn next(&mut self) -> Option<Eval<Value>> {
        let mut items = self.items.lock().unwrap();
        if self.item_offset < items.len() {
            let r = items[self.item_offset].clone();
            self.item_offset += 1;
            return Some(Ok(r));
        }

        // no items left in vector - check iterator
        let mut rest = self.rest.lock().unwrap();
        match rest.next() {
            None => None,
            Some(itm) => {
                let itm = match itm {
                    Ok(r) => r,
                    Err(e) => return Some(Err(e))
                };

                // add to the item vec
                items.push(itm.clone());
                self.item_offset += 1;
                return Some(Ok(itm))
            }
        }
    }
}

/// Utility type for converting lazy iterators into sequence types
/// 
/// Note that this assumes the given iterator ends when `next` returns `None`.
pub struct LazySequence<I> where I: Iterator<Item=Eval<Value>>+Send+Sync {
    // always lock `items` first if locking both
    items: Arc<Mutex<Vec<Value>>>,
    rest: Arc<Mutex<I>>
}

impl<I: Iterator<Item=Eval<Value>>+Send+Sync> LazySequence<I> {
    pub fn new(iter: I) -> Self {
        LazySequence {
            items: Arc::new(Mutex::new(Vec::new())),
            rest: Arc::new(Mutex::new(iter))
        }
    }
}

impl<I: Iterator<Item=Eval<Value>>+Send+Sync+'static> ValueLike for LazySequence<I> {
    fn into_iter(&self) -> ValueIteratorBox {
        Box::new(LazySequenceIterator {
            item_offset: 0,
            items: Arc::clone(&self.items),
            rest: Arc::clone(&self.rest),
        })
    }

    fn into_str(&self) -> Eval<String> {
        let mut s = String::new();
        s.push('(');
        let body: Vec<_> = self.into_iter()
                               .map(|x| x.and_then(|x| x.into_str()))
                               .collect::<Eval<Vec<_>>>()?;
        s.push_str(&body.join(" "));
        s.push(')');

        Ok(s)
    }

    fn evaluate(&self, env: &Environment) -> EvalResult {
        // evaluate first so iterators act like lists
        if let Some(first) = self.first()? {
            let first = first.evaluate(env)?;
            if first.is_executable() {
                let s = self.into_iter().collect::<Eval<Vec<_>>>()?;
                return first.execute(env, &s[1..]);
            }
        }

        // otherwise evaluate every entry
        let env = env.to_owned();
        let iter: ValueIteratorBox = Box::new(LazySequenceIterator {
                item_offset: 0,
                items: Arc::clone(&self.items),
                rest: Arc::clone(&self.rest),
            }.map(move |i| i.and_then(|x| x.evaluate(&env))));
        Ok(Value::new(LazySequence::new(iter)))
    }

    fn first(&self) -> Eval<Option<Value>> {
        let mut itms = self.items.lock().unwrap();
        if itms.is_empty() {
            // try to pull more
            let mut itr = self.rest.lock().unwrap();
            match itr.next() {
                None => return Ok(None),
                Some(itm) => {
                    let itm = itm?;
                    itms.push(itm);
                    return Ok(itms.first().map(|x| x.clone()))
                }
            }
        } else {
            Ok(itms.first().map(|x| x.clone()))
        }
    }
}

impl fmt::Debug for ValueData {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &ValueData::Boolean(b)     => write!(f, "<bool:{}>", b),
            &ValueData::Number(ref n)  => write!(f, "<num:{}>", n),
            &ValueData::Str(ref s)     => write!(f, "<str:\"{}\">", s),
            &ValueData::Symbol(ref s)  => write!(f, "<sym:{}>", s.0),
            &ValueData::Atom(ref s)    => write!(f, "<atom:{}>", s),
            &ValueData::Map(_)         => write!(f, "<map>"),
            &ValueData::List(ref v)    => write!(f, "{:?}", v),
            &ValueData::Function(_)    => write!(f, "<function>"),
            &ValueData::Macro(_)       => write!(f, "<macro>"),
            &ValueData::Polymorphic(_) => write!(f, "<polymorphic>"),
        }
    }
}

impl<I: Iterator<Item=Eval<Value>>+Send+Sync> Clone for LazySequence<I> {
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

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        &(self.0)
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

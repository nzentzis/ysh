use std::fmt;
use std::sync::{Arc, Weak, Mutex, RwLock};
use std::boxed::Box;
use std::collections::HashMap;

use environment::Environment;
use stream::{StreamWrapper, StreamError};
use numeric::*;
use evaluate::*;
pub use evaluate::Eval;

/// Trait for types which can be converted to/from values
pub trait ValueConvert : Sized {
    /// Convert the object into a value representation
    fn into_obj(&self) -> Value;

    /// Try to convert a value back into the object
    ///
    /// The output of `into_obj` must be accepted by this function.
    fn from_obj(val: &Value) -> EvalRes<Self>;
}

pub type ValueIteratorBox = Box<Iterator<Item=EvalResult>+Send+Sync>;
pub type EvalResult = EvalRes<Value>;

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

impl From<StreamError> for EvalError {
    fn from(e: StreamError) -> EvalError {
        match e {
            StreamError::IO(e) => EvalError::IO(e),
            StreamError::ModeDenied =>
                EvalError::Runtime(String::from(
                        "operation unsupported by stream"))
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
    fn get_symbol(&self) -> Eval<Option<Identifier>> { Eval::from(Ok(None)) }

    /// Get the value's string (if applicable)
    /// 
    /// Like `get_symbol`, this shouldn't do any conversions
    fn get_string(&self) -> Eval<Option<String>> { Eval::from(Ok(None)) }

    /// Try converting into a `StreamWrapper` with the given parameters
    ///
    /// The `r` and `w` parameters control whether the stream should be opened
    /// in read or write mode.
    fn into_raw_stream(&self, r: bool, w: bool) -> Eval<StreamWrapper> {
        Eval::from(Err(EvalError::TypeError(
                    String::from("cannot convert object into stream"))))
    }

    /// Convert into a sequential form
    /// 
    /// This must be equivalent to calling `self.into_iter().collect()`
    fn into_seq(&self) -> Eval<Vec<Value>> {
        Eval::from(self.into_iter().collect::<Result<_, _>>())
    }

    /// Generate an iterator over the element's values
    /// 
    /// This provides an opportunity for lazy sequences to delay evaluation and
    /// should be used when possible.
    fn into_iter(&self) -> ValueIteratorBox;

    /// Convert a value into string form
    fn into_str(&self) -> Eval<String>;

    /// Convert value into readable representation
    ///
    /// This is similar to `into_str`, but returns a string which suitable for
    /// display to the user. This should be as close as possible to the value
    /// specified by the user to *create* the value. For example:
    ///
    ///     (into_str "foobar") -> "foobar"
    ///     (into_repr "foobar") -> "\"foobar\""
    fn into_repr(&self) -> Eval<String> { self.into_str() }

    /// Convert a value into numeric form
    fn into_num(&self) -> Eval<Option<Number>> { Eval::from(Ok(None)) }

    /// Check whether a value is truth-like
    fn into_bool(&self) -> Eval<bool> { Eval::from(Ok(true)) }

    /// Convert into a vector of strings usable as command-line arguments
    fn into_args(&self) -> Eval<Vec<String>> {
        Eval::from(self.into_seq().wait()
                   .and_then(|s| s.into_iter()
                                  .map(|x| x.into_str().wait())
                                  .collect()))
    }

    /// Evaluate this value-like object in a given lexical environment
    fn evaluate(&self, env: &Environment) -> Eval<Value>;

    /// Whether the value can be executed with arguments
    fn is_executable(&self) -> bool { false }

    /// Execute the value with the given arguments, and return the result
    /// 
    /// If the value is not executable, return Err() with the original
    /// arguments.
    fn execute(&self, _env: &Environment, _args: &[Value]) -> Eval<Value> {
        Eval::from(Err(EvalError::InvalidOperation("execution not supported")))
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
    RawStream(StreamWrapper),
    Polymorphic(Arc<ValueLike + 'static>),
    // TODO: Lazy(Box<FnOnce()->Value>),
}

#[derive(Clone)]
/// Wrapper around strings which doesn't duplicate static values but allows
/// dynamic creation
pub enum ConstString {
    Static(&'static str),
    Owned(Arc<String>),
}

impl ConstString {
    fn from_static(s: &'static str) -> Self {
        ConstString::Static(s)
    }

    fn from_str(s: String) -> Self {
        ConstString::Owned(Arc::new(s))
    }
}

impl AsRef<str> for ConstString {
    fn as_ref(&self) -> &str {
        match self {
            ConstString::Static(r) => r,
            ConstString::Owned(ref s) => s.as_str()
        }
    }
}

impl fmt::Debug for ConstString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstString::Static(r) => write!(f, "&{:?}", r),
            ConstString::Owned(r)  => write!(f, "{:?}", &*r),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Documentation {
    pub origin: Option<ConstString>,
    pub short_desc: Option<ConstString>,
    pub description: Option<ConstString>,
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
        self.origin = Some(ConstString::from_static(origin));
        self
    }

    /// Modify the origin of a documentation object with a dynamic string
    pub fn origin_str(mut self, origin: String) -> Self {
        self.origin = Some(ConstString::from_str(origin));
        self
    }

    /// Modify the description of a documentation object
    /// 
    /// The passed text may be word-wrapped at arbitrary boundaries.
    pub fn desc(mut self, description: &'static str) -> Self {
        self.description = Some(ConstString::from_static(description));
        self
    }

    /// Modify the description of a documentation object with a dynamic string
    /// 
    /// The passed text may be word-wrapped at arbitrary boundaries.
    pub fn desc_str(mut self, description: String) -> Self {
        self.description = Some(ConstString::from_str(description));
        self
    }

    /// Modify the short description of a documentation object
    ///
    /// The passed text may be word-wrapped at arbitrary boundaries
    pub fn short(mut self, short: &'static str) -> Self {
        self.short_desc = Some(ConstString::from_static(short));
        self
    }

    /// Modify the short description of a documentation object with a dynamic string
    ///
    /// The passed text may be word-wrapped at arbitrary boundaries
    pub fn short_str(mut self, short: String) -> Self {
        self.short_desc = Some(ConstString::from_str(short));
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SourcePoint {
    line: usize,
    col: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SourceRegion {
    start: SourcePoint,
    end: SourcePoint
}

#[derive(Clone, Debug)]
pub struct Value {
    pub loc: Option<SourceRegion>,
    pub data: ValueData,
    pub name: Option<Identifier>,
    pub doc: Option<Documentation>
}

impl Value {
    /// Change the value's name
    pub fn rename(mut self, name: Identifier) -> Value {
        self.name = Some(name);
        self
    }

    /// Add documentation to a value
    pub fn document(mut self, doc: &Documentation) -> Value {
        self.doc = Some(doc.to_owned());
        self
    }

    /// Set the location of a value
    pub fn locate(mut self, l: SourceRegion) -> Value {
        self.loc = Some(l);
        self
    }

    /// Generate an empty value
    pub fn empty() -> Value {
        Value {
            loc: None,
            data: ValueData::List(Vec::new()),
            name: None,
            doc: None
        }
    }

    /// Generate an empty value
    pub fn new<T: ValueLike+'static>(x: T) -> Value {
        Value {
            loc: None,
            data: ValueData::Polymorphic(Arc::new(x)),
            name: None,
            doc: None
        }
    }

    /// Build a new list value
    pub fn list<I: IntoIterator<Item=Value>>(i: I) -> Value {
        Value {
            loc: None,
            data: ValueData::List(i.into_iter().collect()),
            name: None,
            doc: None
        }
    }

    /// Build a stream value
    pub fn raw_stream(strm: StreamWrapper) -> Value {
        Value {
            loc: None,
            data: ValueData::RawStream(strm),
            name: None,
            doc: None
        }
    }

    /// Build a string value
    pub fn str<S: ::std::borrow::Borrow<str>>(s: S) -> Value {
        Value {
            loc: None,
            data: ValueData::Str(s.borrow().to_owned()),
            name: None,
            doc: None
        }
    }

    /// Build an atom
    pub fn atom<S: AsRef<str>>(s: S) -> Value {
        Value {
            loc: None,
            data: ValueData::Atom(Arc::new(String::from(s.as_ref()))),
            name: None,
            doc: None
        }
    }

    /// Build a hashed atom
    pub fn atom_hash<S: AsRef<str>>(s: S) -> ValueHash {
        Value {
            loc: None,
            data: ValueData::Atom(Arc::new(String::from(s.as_ref()))),
            name: None,
            doc: None
        }.hash().wait().unwrap().unwrap()
    }

    /// Build a hashmap
    pub fn map<I: IntoIterator<Item=(ValueHash, Value)>>(i: I) -> Value {
        Value::from(ValueData::Map(i.into_iter().collect::<HashMap<_,_>>()))
    }

    /// Utility function to build a hashmap with atom keys
    pub fn atom_map<S, I>(i: I) -> Value 
            where S: AsRef<str>,
                  I: IntoIterator<Item=(S, Value)> {
        Value::map(i.into_iter()
                    .map(|(s,v)| (Value::atom(s).hash().wait()
                                        .unwrap().unwrap(), v)))
    }

    /// Quote the form
    ///
    /// This just returns a new form wrapping this one in `(quote some-form)`.
    pub fn quote(&self) -> Value {
        Value::list(vec![Value::from(Identifier::new("quote")), self.to_owned()])
    }

    /// Perform macro expansion on the contained form, repeatedly if necessary,
    /// until no expansions remain to be performed.
    pub fn macroexpand(self) -> Eval<Value> {
        match self.data {
            ValueData::List(ref xs) => {
                // might be a macro call, but could still be a normal function
                // call
                //
                // check whether the first element is a resolvable symbol
                let macro_expr: Option<Value> =
                    if let Some(f) = xs.first() {
                        if f.is_macro() { Some(f.to_owned()) }
                        else {
                            let r = match f.get_symbol().wait() {
                                Ok(x) => x,
                                Err(e) => return Eval::from(Err(e))
                            };
                            r.and_then(|sym|
                                       ::environment::global().get(&*(sym.0)))
                        }
                    } else { None };

                // if so, check to make sure it's a macro
                let macro_expr =
                    if let Some(m) = macro_expr {
                        match m.data {
                            ValueData::Macro(ref exec) => Some(exec.clone()),
                            _ => None
                        }
                    } else { None };

                // We have an executable macro, so this is a macro form. Go and
                // run the macro.
                if let Some(exec) = macro_expr {
                    let body = match self.into_seq().wait() {
                        Ok(x) => x,
                        Err(e) => return Eval::from(Err(e))
                    };
                    
                    // we have to quote the body forms so that when they're
                    // evaluated by the macro function it doesn't run them
                    let quoted_body = body[1..].iter()
                                      .map(|form| form.quote())
                                      .collect::<Vec<_>>();
                    let r = match exec.run(&::environment::empty(), &quoted_body)
                                      .wait() {
                        Ok(x) => x,
                        Err(e) => return Eval::from(Err(e))
                    };
                    return r.macroexpand();
                }
            },
            _ => {}
        }
        Eval::from(Ok(self))
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
                    match v.hash().wait() {
                        Ok(Some(v)) => v.hash(&mut hasher),
                        Ok(None) => return Eval::from(Ok(None)),
                        Err(e) => return Eval::from(Err(e))
                    };
                }
            },
            &ValueData::List(ref x) => {
                for i in x {
                    match i.hash().wait() {
                        Ok(Some(v)) => v.hash(&mut hasher),
                        Ok(None) => return Eval::from(Ok(None)),
                        Err(e) => return Eval::from(Err(e))
                    };
                }
            },
            _ => return Eval::from(Ok(None))
        }

        Eval::from(Ok(Some(ValueHash {
            hash: hasher.finish(),
            val: self.to_owned()
        })))
    }
}

pub trait ToValueHash {
    fn to_value_hash(self) -> ValueHash;
}

impl ToValueHash for bool {
    fn to_value_hash(self) -> ValueHash {
        Value::from(self).hash().wait().unwrap().unwrap()
    }
}

impl ToValueHash for Number {
    fn to_value_hash(self) -> ValueHash {
        Value::from(self).hash().wait().unwrap().unwrap()
    }
}

impl<'a> ToValueHash for &'a str {
    fn to_value_hash(self) -> ValueHash {
        Value::str(self).hash().wait().unwrap().unwrap()
    }
}

impl<'a> ToValueHash for &'a String {
    fn to_value_hash(self) -> ValueHash {
        Value::str(self.as_ref()).hash().wait().unwrap().unwrap()
    }
}

impl ToValueHash for Identifier {
    fn to_value_hash(self) -> ValueHash {
        Value::from(self).hash().wait().unwrap().unwrap()
    }
}

impl<V: ToValueHash> ToValueHash for Vec<V> {
    fn to_value_hash(self) -> ValueHash {
        Value::list(self.into_iter().map(|x| x.to_value_hash().val))
              .hash().wait().unwrap().unwrap()
    }
}

impl From<Executable> for Value {
    fn from(e: Executable) -> Value {
        Value {
            loc: None,
            data: ValueData::Function(e),
            name: None,
            doc: None
        }
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Value {
        Value {
            loc: None,
            data: ValueData::Number(n),
            name: None,
            doc: None
        }
    }
}

impl From<Identifier> for Value {
    fn from(i: Identifier) -> Value {
        Value {
            loc: None,
            data: ValueData::Symbol(i),
            name: None,
            doc: None
        }
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Value {
        Value {
            loc: None,
            data: ValueData::Boolean(x),
            name: None,
            doc: None
        }
    }
}

impl From<ValueData> for Value {
    fn from(x: ValueData) -> Value {
        Value {loc: None, data: x, name: None, doc: None}
    }
}

impl ValueLike for Value {
    fn get_symbol(&self) -> Eval<Option<Identifier>> {
        if let ValueData::Symbol(ref id) = self.data {
            Eval::from(Ok(Some(id.to_owned())))
        } else if let ValueData::Polymorphic(ref p) = self.data {
            p.get_symbol()
        } else { Eval::from(Ok(None)) }
    }

    fn get_string(&self) -> Eval<Option<String>> {
        if let &ValueData::Symbol(ref id) = &self.data {
            Eval::from(Ok(Some((&*id.0).to_owned())))
        } else if let &ValueData::Str(ref s) = &self.data {
            Eval::from(Ok(Some(s.to_owned())))
        } else if let &ValueData::Polymorphic(ref p) = &self.data {
            p.get_string()
        } else {
            Eval::from(Ok(None))
        }
    }

    fn into_raw_stream(&self, r: bool, w: bool) -> Eval<StreamWrapper> {
        if !r && !w {
            panic!("Cannot generate stream without r or w ability");
        }

        let fname = match &self.data {
            &ValueData::RawStream(ref s) => {
                // make sure the permissions are compatible
                if (r & s.is_readable() != r) || (w & s.is_writable() != w) {
                    return Eval::from(Err(EvalError::TypeError(String::from(
                                "incompatible stream capabilities"))));
                } else {
                    return Eval::from(Ok(s.to_owned()));
                }
            },
            &ValueData::Str(ref s) => s.to_owned(),
            &ValueData::Symbol(ref id) => id.as_ref().to_owned(),
            &ValueData::Atom(ref id) => id.as_ref().to_owned(),
            _ => return Eval::from(Err(EvalError::TypeError(String::from(
                        "cannot convert type to raw stream"))))
        };

        // try opening the file
        use std::fs::OpenOptions;
        let f = match OpenOptions::new().read(r).write(w).create(w).open(fname) {
            Ok(r) => r,
            Err(e) => return Eval::from(Err(EvalError::from(e)))
        };
        Eval::from(match (r,w) {
            (true,false) => Ok(StreamWrapper::new_read(f)),
            (false,true) => Ok(StreamWrapper::new_write(f)),
            (true,true) => Ok(StreamWrapper::new_rw(f)),
            _ => unreachable!()
        })
    }

    fn into_seq(&self) -> Eval<Vec<Value>> {
        if let &ValueData::List(ref l) = &self.data {
            Eval::from(Ok(l.to_owned()))
        } else if let &ValueData::Polymorphic(ref p) = &self.data {
            p.into_seq()
        } else {
            Eval::from(Ok(vec![self.to_owned()]))
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
            &ValueData::Boolean(true)       => Eval::from(Ok(String::from("true"))),
            &ValueData::Boolean(false)      => Eval::from(Ok(String::from("false"))),
            &ValueData::Number(ref n)       => Eval::from(Ok(format!("{}", n))),
            &ValueData::Str(ref s)          => Eval::from(Ok(s.to_owned())),
            &ValueData::Symbol(ref id)      => Eval::from(Ok((*(id.0)).to_owned())),
            &ValueData::Atom(ref a)         => Eval::from(Ok(format!(":{}", a))),
            &ValueData::Map(ref m) => {
                let mut items = Vec::new();
                for (k,v) in m {
                    let a = k.val.into_repr().wait();
                    let a = match a {
                        Err(e) => return Eval::from(Err(e)),
                        Ok(x) => x,
                    };
                    let b = v.into_repr().wait();
                    let b = match b {
                        Err(e) => return Eval::from(Err(e)),
                        Ok(x) => x,
                    };
                    items.push(a);
                    items.push(b);
                }

                let mut s = String::with_capacity(128);
                s.push('{');
                s.push_str(&items.join(" "));
                s.push('}');
                Eval::from(Ok(s))
            },
            &ValueData::List(ref l)         => {
                let mut s = String::with_capacity(128);
                s.push('(');
                let xs = l.into_iter()
                          .map(|x| x.into_str())
                          .collect::<Eval<Vec<_>>>()
                          .wait();
                let xs = match xs {
                    Err(e) => return Eval::from(Err(e)),
                    Ok(r)  => r,
                };
                s.push_str(&xs.join(" "));
                s.push(')');
                Eval::from(Ok(s))
            },
            &ValueData::Function(_)         => Eval::from(Ok(match &self.name {
                &None        => String::from("<anonymous fn>"),
                &Some(ref s) => format!("<fn '{}'>", s.as_ref()),
            })),
            &ValueData::Macro(_)            => Eval::from(Ok(String::from("<macro>"))),
            &ValueData::RawStream(ref s) => {
                // TODO: use a more meaningful representation. maybe add to the
                //       stream API?
                Eval::from(Ok(String::from("<raw stream object>")))
            }
            &ValueData::Polymorphic(ref v)  => v.into_str()
        }
    }

    fn into_repr(&self) -> Eval<String> {
        match &self.data {
            &ValueData::Str(ref s) => Eval::from(Ok(format!("\"{}\"", s))),
            _ => self.into_str()
        }
    }

    fn into_num(&self) -> Eval<Option<Number>> {
        if let &ValueData::Number(ref n) = &self.data {
            Eval::from(Ok(Some(n.to_owned())))
        } else if let &ValueData::Polymorphic(ref p) = &self.data {
            p.into_num()
        } else {
            Eval::from(Ok(None))
        }
    }

    fn into_bool(&self) -> Eval<bool> {
        match &self.data {
            &ValueData::Boolean(b)          => Eval::from(Ok(b)),
            &ValueData::Number(ref n)       => Eval::from(Ok(n.round() != 0)),
            &ValueData::Str(ref s)          => Eval::from(Ok(!s.is_empty())),
            &ValueData::Symbol(_)           => Eval::from(Ok(true)),
            &ValueData::Atom(_)             => Eval::from(Ok(true)),
            &ValueData::Map(ref m)          => Eval::from(Ok(!m.is_empty())),
            &ValueData::List(ref l) =>
                if l.is_empty() { Eval::from(Ok(false)) } else {
                    let obj = l.iter().map(|x| x.into_bool())
                               .collect::<Eval<Vec<bool>>>().wait();
                    Eval::from(obj.map(|x| x.into_iter().any(|x| x)))
                },
            &ValueData::Function(_)         => Eval::from(Ok(true)),
            &ValueData::Macro(_)            => Eval::from(Ok(true)),
            &ValueData::RawStream(_)        => Eval::from(Ok(true)),
            &ValueData::Polymorphic(ref v)  => v.into_bool()
        }
    }

    fn into_args(&self) -> Eval<Vec<String>> {
        match &self.data {
            &ValueData::List(ref l) =>
                Eval::from(l.into_iter()
                            .map(|x| x.into_args())
                            .collect::<Eval<Vec<_>>>().wait()
                            .map(|r| r.into_iter()
                                      .flat_map(|x| x)
                                      .collect())),
            _ => Eval::from(self.into_str().wait().map(|r| vec![r]))
        }
    }

    fn evaluate(&self, env: &Environment) -> Eval<Value> {
        match &self.data {
            &ValueData::Symbol(ref s) => {
                // try looking it up
                Eval::from(if let Some(v) = env.get(&*(s.0)) {
                    Ok(v)
                } else {
                    Ok(self.clone())
                })
            },
            &ValueData::List(ref xs) => {
                // evaluate () as ()
                if xs.is_empty() { return Eval::from(Ok(Value::list(vec![]))); }
                let first = match xs[0].evaluate(env).wait() {
                    Ok(r) => r,
                    Err(e) => return Eval::from(Err(e))
                };

                if first.is_executable() {
                    first.execute(env, &xs[1..])
                } else {
                    // evaluate elements when using bare list
                    let xs = xs.into_iter()
                               .map(|v| v.evaluate(env).wait())
                               .collect::<Result<Vec<_>, _>>();
                    let xs = match xs {
                        Ok(r) => r,
                        Err(e) => return Eval::from(Err(e))
                    };
                    Eval::from(Ok(Value::list(xs)))
                }
            },
            &ValueData::Polymorphic(ref p) => p.evaluate(env),
            _ => Eval::from(Ok(self.to_owned()))
        }
    }

    fn is_executable(&self) -> bool {
        match &self.data {
            &ValueData::Function(_)        => true,
            &ValueData::Polymorphic(ref v) => v.is_executable(),
            _                              => false
        }
    }

    fn execute(&self, env: &Environment, args: &[Value]) -> Eval<Value> {
        match &self.data {
            // TODO: force evaluate args in outer environment
            // delegate to inner evaluation function
            &ValueData::Function(ref f) => f.run(env, args),
            &ValueData::Macro(_)        => panic!("illegal attempt to execute macro"),
            &ValueData::Polymorphic(ref v) => v.execute(env, args),
            x => Eval::from(Err(EvalError::TypeError(
                        format!("object '{:?}' is not executable", x)))),
        }
    }

    fn first(&self) -> Eval<Option<Value>> {
        match &self.data {
            &ValueData::List(ref v)        => Eval::from(Ok(v.first().map(|x| x.clone()))),
            &ValueData::Polymorphic(ref v) => v.first(),
            _                              => Eval::from(Ok(Some(self.clone())))
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
            (&ValueData::Function(ref a),&ValueData::Function(ref b))=> a == b,
            _                                                       => false
        }
    }
}

struct LazySequenceIterator<I> where I: Iterator<Item=EvalResult>+Send+Sync {
    item_offset: usize, // offset into items of current element
    items: Arc<Mutex<Vec<Value>>>,
    rest: Arc<Mutex<I>>
}

impl<I: Iterator<Item=EvalResult>+Send+Sync> Iterator for LazySequenceIterator<I> {
    type Item = EvalResult;

    fn next(&mut self) -> Option<EvalResult> {
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
                Some(Ok(itm))
            }
        }
    }
}

/// Utility type for converting lazy iterators into sequence types
/// 
/// Note that this assumes the given iterator ends when `next` returns `None`.
pub struct LazySequence<I> where I: Iterator<Item=EvalRes<Value>>+Send+Sync {
    // always lock `items` first if locking both
    items: Arc<Mutex<Vec<Value>>>,
    rest: Arc<Mutex<I>>
}

impl<I: Iterator<Item=EvalResult>+Send+Sync> LazySequence<I> {
    pub fn new(iter: I) -> Self {
        LazySequence {
            items: Arc::new(Mutex::new(Vec::new())),
            rest: Arc::new(Mutex::new(iter))
        }
    }
}

impl<I: Iterator<Item=EvalResult>+Send+Sync+'static> ValueLike for LazySequence<I> {
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
        let body = self.into_iter()
                       .map(|x| x.and_then(|x| x.into_str().wait()))
                       .collect::<EvalRes<Vec<_>>>();
        let body = match body {
            Ok(r) => r,
            Err(e) => return Eval::from(Err(e))
        };
        s.push_str(&body.join(" "));
        s.push(')');

        Eval::from(Ok(s))
    }

    fn evaluate(&self, env: &Environment) -> Eval<Value> {
        // evaluate first so iterators act like lists
        let first = match self.first().wait() {
            Ok(r) => r,
            Err(e) => return Eval::from(Err(e))
        };
        if let Some(first) = first {
            let first = match first.evaluate(env).wait() {
                Ok(r) => r,
                Err(e) => return Eval::from(Err(e))
            };
            if first.is_executable() {
                let s = match self.into_iter().collect::<EvalRes<Vec<_>>>() {
                    Ok(r) => r,
                    Err(e) => return Eval::from(Err(e))
                };
                return first.execute(env, &s[1..]);
            }
        }

        // otherwise evaluate every entry
        let env = env.to_owned();
        let iter: ValueIteratorBox = Box::new(LazySequenceIterator {
                item_offset: 0,
                items: Arc::clone(&self.items),
                rest: Arc::clone(&self.rest),
            }.map(move |i| i.and_then(|x| x.evaluate(&env).wait())));
        Eval::from(Ok(Value::new(LazySequence::new(iter))))
    }

    fn first(&self) -> Eval<Option<Value>> {
        let mut itms = self.items.lock().unwrap();
        if itms.is_empty() {
            // try to pull more
            let mut itr = self.rest.lock().unwrap();
            match itr.next() {
                None => return Eval::from(Ok(None)),
                Some(itm) => {
                    let itm = match itm {
                        Ok(r) => r,
                        Err(e) => return Eval::from(Err(e))
                    };
                    itms.push(itm);
                    return Eval::from(Ok(itms.first().map(|x| x.clone())))
                }
            }
        } else {
            Eval::from(Ok(itms.first().map(|x| x.clone())))
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
            &ValueData::RawStream(_)   => write!(f, "<raw stream>"),
            &ValueData::Polymorphic(_) => write!(f, "<polymorphic>"),
        }
    }
}

impl<I: Iterator<Item=EvalRes<Value>>+Send+Sync> Clone for LazySequence<I> {
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

lazy_static! {
    static ref IDENT_INTERN_TBL: RwLock<HashMap<String, Weak<String>>>
        = RwLock::new(HashMap::new());
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Identifier(pub Arc<String>);

impl Identifier {
    pub fn new<R: AsRef<str>>(s: R) -> Self {
        // reuse existing atoms?
        if let Some(w) = IDENT_INTERN_TBL.read().unwrap().get(s.as_ref()) {
            if let Some(a) = w.upgrade() {
                return Identifier(a);
            }
        }

        let mut tbl = IDENT_INTERN_TBL.write().unwrap();
        if let Some(a) = tbl.get(s.as_ref()).and_then(|x| x.upgrade()) {
            Identifier(a)
        } else {
            let arc = Arc::new(s.as_ref().to_owned());
            tbl.insert(s.as_ref().to_owned(), Arc::downgrade(&arc));
            Identifier(arc)
        }
    }

    pub fn from(s: String) -> Self {
        // reuse existing atoms?
        if let Some(w) = IDENT_INTERN_TBL.read().unwrap().get(&s) {
            if let Some(a) = w.upgrade() {
                return Identifier(a);
            }
        }

        let mut tbl = IDENT_INTERN_TBL.write().unwrap();
        if let Some(a) = tbl.get(&s).and_then(|x| x.upgrade()) {
            Identifier(a)
        } else {
            let arc = Arc::new(s.clone());
            tbl.insert(s, Arc::downgrade(&arc));
            Identifier(arc)
        }
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

impl PipelineComponent {
    pub fn into_repr(&self) -> String {
        let mut s = self.xform.0.iter()
                        .map(|x| x.into_repr().wait().unwrap())
                        .collect::<Vec<_>>()
                        .join(" ");
        match self.link {
            Some(PipeMode::Pipe) => s.push_str(" | "),
            Some(PipeMode::DelimitedPipe(c)) => s.push_str(&format!(" |{}> ", c)),
            Some(PipeMode::PipeText) => s.push_str(" |> "),
            None => {}
        }

        s
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Pipeline {
    /// List of pipeline stages
    pub elements: Vec<PipelineComponent>,

    /// Pipeline-global routing components
    pub terminals: Vec<TerminalMode>
}

impl Pipeline {
    pub fn into_repr(&self) -> String {
        let mut r = self.elements.iter()
                        .map(|x| x.into_repr())
                        .collect::<Vec<_>>()
                        .join("");
        for t in self.terminals.iter() {
            match t {
                &TerminalMode::ReplaceFile(ref s) =>
                    r.push_str(&format!(" > {}", s)),
                &TerminalMode::AppendFile(ref s) =>
                    r.push_str(&format!(" >> {}", s)),
                &TerminalMode::SetVariable(ref id) =>
                    r.push_str(&format!(" >= {}", id.as_ref())),
                &TerminalMode::AppendVariable(ref id) =>
                    r.push_str(&format!(" >>= {}", id.as_ref())),
                &TerminalMode::InputFile(ref s) =>
                    r.push_str(&format!(" < {}", s)),
                &TerminalMode::InputVar(ref id) =>
                    r.push_str(&format!(" <= {}", id.as_ref())),
            }
        }

        r
    }

    pub fn into_obj(&self) -> Value {
        let mut v = Vec::new();
        for e in self.elements.iter() {
            v.push(Value::list(e.xform.0.iter().cloned()));
            match e.link {
                None => {},
                Some(PipeMode::Pipe) => v.push(Value::atom("pipe")),
                Some(PipeMode::PipeText) => v.push(Value::atom("pipe-text")),
                Some(PipeMode::DelimitedPipe(c)) =>
                    v.push(Value::list(vec![Value::atom("pipe-delim"),
                                            Value::str(format!("{}", c))])),
            }
        }

        for t in self.terminals.iter() {
            v.push(match t {
                &TerminalMode::ReplaceFile(ref s) =>
                    Value::list(vec![Value::atom("replace-file"),
                                     Value::str(s.as_ref())]),
                &TerminalMode::AppendFile(ref s) =>
                    Value::list(vec![Value::atom("into-file"),
                                     Value::str(s.as_ref())]),
                &TerminalMode::SetVariable(ref id) =>
                    Value::list(vec![Value::atom("replace-var"),
                                     Value::from(id.clone())]),
                &TerminalMode::AppendVariable(ref id) =>
                    Value::list(vec![Value::atom("into-var"),
                                     Value::from(id.clone())]),
                &TerminalMode::InputFile(ref s) =>
                    Value::list(vec![Value::atom("input-file"),
                                     Value::str(s.as_ref())]),
                &TerminalMode::InputVar(ref id) =>
                    Value::list(vec![Value::atom("input-var"),
                                     Value::from(id.clone())]),
            });
        }

        Value::list(v)
    }
}

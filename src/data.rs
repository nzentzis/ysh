use std::fmt;
use std::sync::Arc;
use std::boxed::Box;

use environment::Environment;

#[derive(Clone)]
pub enum Executable {
    Native(Arc<Fn(&Environment, &[Value]) -> Value + Send + Sync>),
    //Interpreted()
}

impl Executable {
    pub fn native<F>(f: F) -> Self
        where F: Fn(&Environment, &[Value]) -> Value + Send + Sync + 'static {
        Executable::Native(Arc::new(f))
    }

    pub fn run(&self, env: &Environment, args: &[Value]) -> Value {
        match self {
            &Executable::Native(ref f) => f(env, args)
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Boolean(bool),
    Number(i64), // TODO: more arithmetic tower support
    Str(String),
    Symbol(Identifier),
    List(Vec<Value>),
    Function(Environment, Executable),
    // TODO: Lazy(Box<FnOnce()->Value>),
    // polyobject
}

/// Trait for basic behavior which applies to all value-like types. If possible,
/// make other code dependent on these rather than requiring `Value` structures
/// directly.
pub trait ValueLike : IntoIterator {
    /// Convert into a sequential form
    /// 
    /// This must be equivalent to calling `self.into_iter().collect()`
    fn into_seq(self) -> Vec<Value>;

    /// Convert a value into string form
    fn into_str(self) -> String;

    /// Convert into a vector of strings usable as command-line arguments
    fn into_args(self) -> Vec<String>;
}

impl Value {
    /// Generate an empty value
    pub fn empty() -> Value {
        Value::List(Vec::new())
    }
}

pub struct ValueIterator {
    iter: ::std::vec::IntoIter<Value>
}

impl Iterator for ValueIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Value> { self.iter.next() }
}

// Values are strict, so just build an iterator directly. No need to be lazy
// about it.
impl IntoIterator for Value {
    type Item = Value;
    type IntoIter = ValueIterator;

    fn into_iter(self) -> ValueIterator {
        ValueIterator {
            iter: self.into_seq().into_iter()
        }
    }
}

impl ValueLike for Value {
    fn into_seq(self) -> Vec<Value> {
        if let Value::List(l) = self {
            l
        } else {
            vec![self]
        }
    }

    fn into_str(self) -> String {
        match self {
            Value::Boolean(true)       => String::from("true"),
            Value::Boolean(false)      => String::from("false"),
            Value::Number(n)           => format!("{}", n),
            Value::Str(s)              => s,
            Value::Symbol(id)          => (*(id.0)).to_owned(),
            Value::List(l)             => {
                let mut s = String::with_capacity(128);
                s.push('(');
                let xs: Vec<_> = l.into_iter().map(|x| x.into_str()).collect();
                s.push_str(&xs.join(" "));
                s.push(')');
                s
            },
            Value::Function(_,_)       => String::from("<function>")
        }
    }

    fn into_args(self) -> Vec<String> {
        match self {
            Value::Boolean(true)       => vec![String::from("true")],
            Value::Boolean(false)      => vec![String::from("false")],
            Value::Number(n)           => vec![format!("{}", n)],
            Value::Str(s)              => vec![s],
            Value::Symbol(id)          => vec![(*(id.0)).to_owned()],
            Value::List(l)             => l.into_iter()
                                           .flat_map(|x| x.into_args())
                                           .collect(),
            Value::Function(_,_)       => vec![String::from("<function>")]
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (&Value::Boolean(x),     &Value::Boolean(y))    => x == y,
            (&Value::Number(x),      &Value::Number(y))     => x == y,
            (&Value::Str(ref x),     &Value::Str(ref y))    => x == y,
            (&Value::Symbol(ref x),  &Value::Symbol(ref y)) => x == y,
            (&Value::List(ref x),    &Value::List(ref y))   => x == y,

            // don't bother comparing functions
            (&Value::Function(_,_),&Value::Function(_,_))   => false,
            _                                               => false
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Value::Boolean(b)    => write!(f, "<bool:{}>", b),
            &Value::Number(n)     => write!(f, "<num:{}>", n),
            &Value::Str(ref s)    => write!(f, "<str:\"{}\">", s),
            &Value::Symbol(ref s) => write!(f, "<sym:{}>", s.0),
            &Value::List(ref v)   => write!(f, "{:?}", v),
            &Value::Function(_,_) => write!(f, "<function>"),
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

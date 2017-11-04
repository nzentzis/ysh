use std::sync::Arc;

pub enum Value {
    Boolean(bool),
    Number(i64), // TODO: more arithmetic tower support
    Str(String),
    Symbol(Arc<String>),
    Char(char),
    List(Vec<Value>),
    // function
    // polyobject
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

pub enum Transformer {
    Command,
    FunctionExpr
}

pub struct PipelineComponent {
    /// The transformer to execute in this component
    xform: Transformer,

    /// The terminating element for this component. If `None`, then this is the
    /// last part of the pipeline.
    link: Option<PipeMode>,
}

pub struct Pipeline {
    /// List of pipeline stages
    elements: Vec<PipelineComponent>,

    /// Pipeline-global routing components
    terminals: Vec<TerminalMode>
}

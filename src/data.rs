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

pub enum Transformer {
    Command,
    FunctionExpr
}

pub struct Pipeline {
    // list of pipeline stages
    elements: Vec<(Transformer, Option<PipeMode>)>
}

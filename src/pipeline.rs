use std::path::PathBuf;

use data::*;
use evaluate::{execute, find_command};
use environment::global;

#[derive(Debug)]
enum AdapterType {
    StreamToPoly,     // full semantic stream parsing
    StreamToString,   // stream to object conversion without any semantic stuff
    PolyToStream,     // object to stream conversion
    StreamDelim(char),// semantic parsing with specified delimiter
}

impl AdapterType {
    /// Get the input and output types for this adapter
    fn io_types(&self) -> (PipeType, PipeType) {
        match self {
            &AdapterType::StreamToPoly  => (PipeType::Stream, PipeType::Object),
            &AdapterType::StreamToString=> (PipeType::Stream, PipeType::Object),
            &AdapterType::PolyToStream  => (PipeType::Object, PipeType::Stream),
            &AdapterType::StreamDelim(_)=> (PipeType::Stream, PipeType::Object),
        }
    }

    /// Generate an adapter fitting a given I/O pair
    /// 
    /// # Panics
    /// This panics if adapting the two types is impossible
    fn fit(i: PipeType, o: PipeType) -> Self {
        match (i,o) {
            (PipeType::Stream, PipeType::Object) => AdapterType::StreamToPoly,
            (PipeType::Object, PipeType::Stream) => AdapterType::PolyToStream,
            _                                   => panic!("cannot adapt stream")
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum PipeType { Stream, Object, Cap, Either }

impl PipeType {
    fn accepts(&self, input: &PipeType) -> bool {
        match (self, input) {
            (&PipeType::Stream, &PipeType::Stream) => true,
            (&PipeType::Object, &PipeType::Object) => true,
            (&PipeType::Either, &PipeType::Stream) => true,
            (&PipeType::Either, &PipeType::Object) => true,
            _                                      => false,
        }
    }
}

#[derive(Debug)]
enum PlanElement {
    Expression(Value),      // obj -> obj
    Command {               // stream -> stream
        exec: PathBuf,
        invoked_name: String,
        args: Vec<Value>
    },
    Adapter(AdapterType),   // a -> b
    FromFile(String),       // x -> stream
    FromVar(Identifier),    // x -> obj
    Stdin,                  // x -> stream
    Stdout,                 // obj/stream -> x (pretty-print if object)
    ToFile(String),         // stream -> x
    AppendFile(String),     // stream -> x
    IntoVar(Identifier),    // obj -> x
    AppendVar(Identifier),  // obj -> x
}

impl PlanElement {
    fn io_types(&self) -> (PipeType, PipeType) {
        match self {
            &PlanElement::Expression(_)  => (PipeType::Object, PipeType::Object),
            &PlanElement::Command{..}    => (PipeType::Stream, PipeType::Stream),
            &PlanElement::Adapter(ref a) => a.io_types(),
            &PlanElement::FromFile(_)    => (PipeType::Cap,    PipeType::Stream),
            &PlanElement::FromVar(_)     => (PipeType::Cap,    PipeType::Object),
            &PlanElement::Stdin          => (PipeType::Cap,    PipeType::Stream),
            &PlanElement::Stdout         => (PipeType::Either, PipeType::Cap),
            &PlanElement::ToFile(_)      => (PipeType::Stream, PipeType::Cap),
            &PlanElement::AppendFile(_)  => (PipeType::Stream, PipeType::Cap),
            &PlanElement::IntoVar(_)     => (PipeType::Object, PipeType::Cap),
            &PlanElement::AppendVar(_)   => (PipeType::Object, PipeType::Cap),
        }
    }
}

#[derive(Debug)]
enum PlanningError {
    MultipleInputs,
    MultipleOutputs,
    NotFound
}

/// Find the plan element for an actual transformation
fn plan_transform(mut xform: Transformer)
        -> Result<PlanElement, PlanningError> {
    let first = xform.0[0].clone();
    if let Value::Symbol(ref s) = first {
        // try looking it up
        let r = global().get(&*(s.0));

        // if we find it, use that
        if let Some(r) = r {
            if let &Value::Function(_,_) = &*r {
                return Ok(PlanElement::Expression(Value::List(xform.0)));
            }
        }
    }

    // try looking up as a command
    let cmd = first.into_str();
    let mut opts = find_command(&cmd);

    if opts.len() > 1 {
        eprintln!("ysh: more than one candidate for '{}'", cmd);
        return Err(PlanningError::NotFound);
    } else if opts.len() == 0 {
        eprintln!("ysh: command not found: {}", cmd);
        return Err(PlanningError::NotFound);
    } else {
        Ok(PlanElement::Command {
            exec: opts.pop().unwrap(),
            invoked_name: cmd,
            args: xform.0.split_off(1)
        })
    }
}

/// Generate a plan for evaluating the given pipeline
/// 
/// We need a plan for pipeline evaluation since in any given pipeline we should
/// do the fewest conversions possible. As a side effect, this also means we can
/// accomodate commands that need to be plugged directly into stdin/stdout like
/// vim or 
fn build_plan(pipe: Pipeline)
        -> Result<Vec<PlanElement>, PlanningError> {
    let Pipeline {elements, terminals} = pipe;

    let mut res = Vec::new();

    // add stdin or a file if needed
    let mut first = None;
    let mut last = None;
    for t in terminals {
        match t {
            TerminalMode::ReplaceFile(s) => {
                if last.is_some() {
                    return Err(PlanningError::MultipleOutputs);
                }
                last = Some(PlanElement::ToFile(s));
            },
            TerminalMode::AppendFile(s) => {
                if last.is_some() {
                    return Err(PlanningError::MultipleOutputs);
                }
                last = Some(PlanElement::AppendFile(s));
            },
            TerminalMode::SetVariable(id) => {
                if last.is_some() {
                    return Err(PlanningError::MultipleOutputs);
                }
                last = Some(PlanElement::IntoVar(id));
            },
            TerminalMode::AppendVariable(id) => {
                if last.is_some() {
                    return Err(PlanningError::MultipleOutputs);
                }
                last = Some(PlanElement::AppendVar(id));
            },
            TerminalMode::InputFile(s) => {
                if first.is_some() {
                    return Err(PlanningError::MultipleInputs);
                }
                first = Some(PlanElement::FromFile(s));
            },
            TerminalMode::InputVar(id) => {
                if first.is_some() {
                    return Err(PlanningError::MultipleInputs);
                }
                first = Some(PlanElement::FromVar(id));
            },
        }
    }
    res.push(first.unwrap_or(PlanElement::Stdin));
    let last = last.unwrap_or(PlanElement::Stdout);

    // run through the pipeline, inserting converters as needed
    let mut last_output = res[0].io_types().1;
    for elem in elements {
        let PipelineComponent {xform, link} = elem;

        let cmd = plan_transform(xform)?;
        let cmd_io = cmd.io_types();

        // insert an adapter before if needed
        if !cmd_io.0.accepts(&last_output) {
            // generate an adapter
            res.push(PlanElement::Adapter(
                    AdapterType::fit(last_output, cmd_io.0)));
        }
        res.push(cmd);
        last_output = cmd_io.1;

        // add the user's requested pipe
        if let Some(mode) = link {
            let elem = match mode {
                PipeMode::DelimitedPipe(c) =>
                    Some(PlanElement::Adapter(AdapterType::StreamDelim(c))),
                PipeMode::Pipe => None,
                PipeMode::PipeText =>
                    Some(PlanElement::Adapter(AdapterType::StreamToString))
            };

            if let Some(e) = elem {
                // don't add adapter if it wouldn't help
                if e.io_types().1 != last_output {
                    last_output = e.io_types().1;
                    res.push(e);
                }
            }
        }
    }

    // attach output
    if !last.io_types().0.accepts(&last_output) {
        // adapt I/O
        res.push(PlanElement::Adapter(
                AdapterType::fit(last_output, last.io_types().0)));
    }
    res.push(last);

    Ok(res)
}

/// Evaluate a pipeline in the given environment
pub fn execute_pipeline(pipe: Pipeline) {
    let plan = build_plan(pipe);
    println!("{:?}", plan);
}

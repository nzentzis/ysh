use std::io;
use std::path::PathBuf;
use std::os::unix::prelude::*;
use std::process;

use nix::fcntl;
use nix::unistd;

use data::*;
use globals::job_control;
use evaluate::{execute, find_command};
use environment::global;
use jobs::{Command, Job, IoChannel};

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PlanningError {
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

#[derive(Debug)]
pub struct Plan(Vec<PlanElement>);

impl Plan {
    /// Generate a plan for evaluating the given pipeline
    /// 
    /// We need a plan for pipeline evaluation since in any given pipeline we
    /// should do the fewest conversions possible. As a side effect, this also
    /// means we can accomodate commands that need to be plugged directly into
    /// stdin/stdout like vim or 
    pub fn generate(pipe: Pipeline) -> Result<Self, PlanningError> {
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

        Ok(Plan(res))
    }

    /// Construct a file descriptor from the first input element
    fn get_input_fd(&self) -> RawFd {
        match &self.0[0] {
            &PlanElement::Stdin          => io::stdin().as_raw_fd(),
            &PlanElement::FromFile(ref fname)=> {
                unimplemented!("file inputs not yet ready")
            },
            &PlanElement::FromVar(ref id)    => {
                unimplemented!("variable inputs not yet ready")
            },
            _ => panic!("plan has invalid beginning element")
        }
    }

    /// Launch the relevant processes and actually start running the pipeline
    /// 
    /// Pipelines are executed by joining all contiguous shell components
    /// together and launching each block of contiguous operations in a thread.
    pub fn launch(mut self, background: bool) -> ActivePipeline {
        // fd of the last output
        let mut last_output = self.get_input_fd();

        // remove the first element so we don't process it twice
        self.0.remove(0);

        // pull info of the last element out beforehand
        let last_elem = self.0.pop().unwrap();
        let arr_len = self.0.len();

        // execute all plan elements
        let mut job = Job::new();
        let mut transforms = Vec::new();
        let mut plan_buffer = Vec::with_capacity(self.0.len());
        for (idx, element) in self.0.into_iter().enumerate() {
            match element {
                PlanElement::Expression(v) =>
                    plan_buffer.push(PlanElement::Expression(v)),
                PlanElement::Adapter(t) =>
                    plan_buffer.push(PlanElement::Adapter(t)),
                PlanElement::Command {exec, invoked_name, args} => {
                    // we have to flush the plan buffer if there's anything
                    // there
                    if !plan_buffer.is_empty() {
                        println!("buffer: {:?}", plan_buffer);
                        // generate a pipe
                        let (i,o) = unistd::pipe2(fcntl::O_CLOEXEC)
                                           .expect("cannot generate pipes");
                        let eval = TransformEvaluation::launch(
                                        last_output, plan_buffer.drain(..),
                                        EvalOutput::Descriptor(i));
                        transforms.push(eval);
                        last_output = o;
                    }

                    // check whether to link the output to stdout
                    let fwd_stdout = idx == (arr_len-1) &&
                                     last_elem == PlanElement::Stdout;

                    // TODO: redirect or handle stderr
                    // TODO: handle spawn failure
                    let cmd = Command::new(exec)
                                      .invoked_using(invoked_name)
                                      .args(args.into_iter()
                                                .flat_map(|x| x.into_args()))
                                      .stdin(if last_output == 0 { IoChannel::Inherited }
                                             else {IoChannel::Specific(last_output)})
                                      .stdout(if fwd_stdout { IoChannel::Inherited }
                                              else { IoChannel::Pipe });
                    let cmd = if background { cmd }
                              else { cmd.foreground() };
                    let process = job.launch(cmd).expect("cannot spawn child");

                    if !fwd_stdout {
                        last_output = process.stdout().unwrap().into_raw_fd();
                    }
                },
                _ => {}
            }
        }

        // if there's a plan in the buffer, we need to flush it
        //
        // NOTE: we don't need to worry about having the output already
        // terminated here. The plan buffer will have items iff the previous
        // element wasn't a command. If it was, the buffer would have been
        // flushed already as part of launching it.
        if !plan_buffer.is_empty() {
            // figure out what output to use
            let out = match last_elem {
                PlanElement::Stdout         => EvalOutput::PrettyStdout,
                PlanElement::AppendFile(f)  => unimplemented!(),
                PlanElement::AppendVar(id)  => unimplemented!(),
                PlanElement::ToFile(f)      => unimplemented!(),
                PlanElement::IntoVar(id)    => unimplemented!(),
                _                           => panic!("invalid plan terminator")
            };

            let eval = TransformEvaluation::launch(
                last_output, plan_buffer.drain(..), out);
            transforms.push(eval);
        }

        ActivePipeline { job,
            xforms: transforms
        }
    }
}

enum EvalOutput {
    PrettyStdout,
    Descriptor(RawFd)
}

/// Representation of a transformer evaluation which is running as part of a
/// pipeline
struct TransformEvaluation {
}

impl TransformEvaluation {
    fn launch<I>(input: RawFd, elements: I, output: EvalOutput) -> Self
            where I: IntoIterator<Item=PlanElement> {
        unimplemented!()
    }
}

/// Representation of an active pipeline.
pub struct ActivePipeline {
    /// OS processes
    job: Job,

    /// Asynchronous transformer evaluations
    xforms: Vec<TransformEvaluation>
}

impl ActivePipeline {
    /// Wait until all the processes in the pipeline have terminated
    pub fn wait(mut self) {
        self.job.wait().expect("wait failed");
    }
}

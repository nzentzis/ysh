use std::io;
use std::path::PathBuf;
use std::os::unix::prelude::*;
use std::error::Error;

use nix::fcntl;
use nix::unistd;

use data::*;
use evaluate::find_command;
use environment::{global, empty};
use jobs::{Command, Job, IoChannel};
use stream::*;

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
/// 
/// # Panics
/// This will panic if passed a transformer containing values which generate
/// evaluation errors in response to any method but `evaluate` or `execute`.
fn plan_transform(mut xform: Transformer)
        -> Result<PlanElement, PlanningError> {
    let first = xform.0[0].clone();
    if let Some(s) = first.get_symbol().unwrap() {
        // try looking it up
        let r = global().get(&*(s.0));

        // if we find it, use that
        if let Some(r) = r {
            if r.is_executable() | r.is_macro() {
                // only convert to a list when there's more than one element
                if xform.0.len() == 1 {
                    return Ok(PlanElement::Expression(r));
                } else {
                    return Ok(PlanElement::Expression(Value::list(xform.0)));
                }
            } else {
                return Ok(PlanElement::Expression(r));
            }
        }
    }

    // if it's not a string or symbol, just return it
    let cmd: String =
        if let Some(s) = first.get_string().unwrap() { s }
        else {
            if xform.0.len() == 1 {
                return Ok(PlanElement::Expression(first));
            } else {
                return Ok(PlanElement::Expression(Value::list(xform.0)));
            }
        };

    // try looking up as a command
    let mut opts = if let Some(cmds) = find_command(&cmd) { cmds }
                   else {
                       eprintln!("ysh: failed to locate command");
                       return Err(PlanningError::NotFound);
                   };

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
            &PlanElement::FromFile(ref _fname)=> {
                unimplemented!("file inputs not yet ready")
            },
            &PlanElement::FromVar(ref _id)    => {
                unimplemented!("variable inputs not yet ready")
            },
            _ => panic!("plan has invalid beginning element")
        }
    }

    /// Launch the relevant processes and actually start running the pipeline
    /// 
    /// Pipelines are executed by joining all contiguous shell components
    /// together and launching each block of contiguous operations in a thread.
    pub fn launch(mut self, background: bool) -> Option<ActivePipeline> {
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
                        // generate a pipe
                        let (i,o) = unistd::pipe2(fcntl::O_CLOEXEC)
                                           .expect("cannot generate pipes");
                        let eval = TransformEvaluation::launch(
                                        last_output, plan_buffer.drain(..),
                                        EvalOutput::Descriptor(i));

                        if let Some(eval) = eval {
                            transforms.push(eval);
                        } else {
                            // handle launch failure
                            // currently just close FDs and abort
                            // TODO: close non-stdin FDs here
                            return None;
                        }
                        last_output = o;
                    }

                    // check whether to link the output to stdout
                    let fwd_stdout = idx == (arr_len-1) &&
                                     last_elem == PlanElement::Stdout;

                    // evaluate args
                    let args = args.into_iter()
                                   .map(|a| a.evaluate(&empty()))
                                   .collect::<Eval<Vec<_>>>()
                                   .and_then(|a| a.into_iter()
                                                  .map(|x| x.into_args())
                                                  .collect::<Eval<Vec<_>>>())
                                   .map(|a| a.into_iter()
                                             .flat_map(|x| x)
                                             .collect::<Vec<_>>());
                    let args = match args {
                        Ok(r) => r,
                        Err(e) => {
                            eprintln!("ysh: argument evaluation failed: {}", e);
                            return None;
                        }
                    };

                    // TODO: redirect or handle stderr
                    // TODO: handle spawn failure
                    // TODO: handle arg evaluation failure
                    let cmd = Command::new(exec)
                                      .invoked_using(invoked_name)
                                      .args(args)
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
                PlanElement::AppendFile(_f)  => unimplemented!(),
                PlanElement::AppendVar(_id)  => unimplemented!(),
                PlanElement::ToFile(_f)      => unimplemented!(),
                PlanElement::IntoVar(_id)    => unimplemented!(),
                _                           => panic!("invalid plan terminator")
            };

            let eval = TransformEvaluation::launch(
                last_output, plan_buffer.drain(..), out);
            if let Some(eval) = eval {
                transforms.push(eval);
            } else {
                // TODO close output FDs here
                return None;
            }
        }

        Some(ActivePipeline { job,
            xforms: transforms
        })
    }
}

enum EvalOutput {
    PrettyStdout,
    Descriptor(RawFd)
}

/// Representation of a transformer evaluation which is running as part of a
/// pipeline.
/// 
/// Each transformer runs in its own thread, and is responsible for driving a
/// collection of Lisp functions. The TransformEvaluation is restricted to the
/// remote thread, but it generates a TransformHandle that can be used to manage
/// its execution.
struct TransformEvaluation {
}

impl TransformEvaluation {
    fn build_innermost_elem(input: RawFd, first: PlanElement) -> PolyStream {
        let config = match first {
            PlanElement::Adapter(AdapterType::StreamToPoly)   => StreamOptions::new(),
            PlanElement::Adapter(AdapterType::StreamToString) => StreamOptions::basic(),
            PlanElement::Adapter(AdapterType::StreamDelim(c)) => {
                let mut o = StreamOptions::new();
                o.delimiter(c);
                o
            },
            _ => panic!("unhandled plan element")
        };

        if input == io::stdin().as_raw_fd() {
            PolyStream::from_stdin(config).expect("cannot construct stream")
        } else {
            PolyStream::from_fd(input, config).expect("cannot construct stream")
        }
    }

    /// Generate a transformed expression from a basic one using the following
    /// rules:
    /// 
    /// 1. If the transform value is executable:
    ///     a. Convert it to a sequence (if this fails, return the value)
    ///     b. Append the inner value
    ///     c. Return the result
    /// 2. Evaluate the transform. If the result is executable, perform steps
    ///    a, b, and c on the result.
    /// 3. If the evaluation succeeded, return a quoted evaluation result and
    ///    ignore the input
    /// 4. If the evaluation failed, use the original value
    fn apply_value_xform(xform: Value, inner: Value) -> Value {
        use std::ops::Deref;
        if xform.is_executable() {
            return Value::list(vec![xform, inner])
        } else if let Ok(Some(s)) = xform.get_symbol() {
            // try looking up the symbol to get an executable result
            let val = global().get(s.0.deref());
            match val {
                Some(ref x) if x.is_executable() => {
                    let mut arr = vec![x.deref().to_owned()];
                    let args = if let Ok(r) = xform.into_seq() {r}
                               else {return xform;};
                    arr.extend(args.into_iter().skip(1));
                    arr.push(inner);
                    return Value::list(arr);
                },
                _ => {}
            }
        }

        let modified_xform = xform.clone()
                                  .macroexpand()
                                  .and_then(|e| e.evaluate(&::environment::empty()));

        if let Ok(m) = modified_xform {
            if m.is_executable() {
                return Value::list(vec![m.clone(), inner]);
            } else {
                return ::library::core::quote(&[m])
            }
        } else {
            xform
        }
    }

    fn launch<I>(input: RawFd, elements: I, output: EvalOutput)
            -> Option<TransformHandle>
            where I: IntoIterator<Item=PlanElement> {
        use std::thread;
        use std::io::prelude::*;

        let mut elements = elements.into_iter();
        let first = elements.next().unwrap();

        // build the innermost (first generated) value from the input
        // since we the pipeline gets built from the inside (left element) out
        // (toward the right) we need to keep track of the current element at
        // all times.
        let mut innermost =
            Value::new(TransformEvaluation::build_innermost_elem(input, first));

        // TODO: Support for killing active transforms
        for elem in elements {
            // turn it into a viable transform
            innermost = match elem {
                PlanElement::Expression(v) =>
                    TransformEvaluation::apply_value_xform(v, innermost),
                _ => unimplemented!()
            };
        }

        // macroexpand the expression before evaluating it
        let expr = match innermost.macroexpand() {
            Ok(r) => r,
            Err(e) => {
                println!("ysh: runtime error while expanding macro: {}",
                         e.description());
                return None;
            }
        };

        // launch the transform
        let out = output;
        let hdl = thread::spawn(move || {
            // TODO: error handling
            let res = expr.evaluate(&::environment::empty());
            let res = match res {
                Ok(r) => r,
                Err(e) => {
                    eprintln!("ysh: {:?} - {}", expr, e);
                    return;
                }
            };
            match out {
                EvalOutput::PrettyStdout => {
                    match res.into_str() {
                        Ok(s) => {
                            println!("{}", s);
                        },
                        Err(e) => {
                            println!("ysh: cannot convert to string: {}", e);
                        }
                    }
                },
                EvalOutput::Descriptor(fd) => {
                    let mut f = unsafe {::std::fs::File::from_raw_fd(fd)};
                    match res.into_str() {
                        Ok(r) => {write!(f, "{}", r).unwrap();},
                        Err(e) =>
                            eprintln!("ysh: cannot convert to string: {}", e),
                    }
                }
            }
        });

        Some(TransformHandle {
            handle: hdl
        })
    }
}

struct TransformHandle {
    handle: ::std::thread::JoinHandle<()>
}

/// Representation of an active pipeline.
pub struct ActivePipeline {
    /// OS processes
    job: Job,

    /// Asynchronous transformer evaluations
    xforms: Vec<TransformHandle>
}

impl ActivePipeline {
    /// Wait until all the processes in the pipeline have terminated
    pub fn wait(mut self) {
        self.job.wait().expect("wait failed");
        for x in self.xforms {
            x.handle.join().unwrap();
        }
    }
}

use std::io;
use std::path::PathBuf;
use std::os::unix::prelude::*;
use std::error::Error;

use nix::fcntl;
use nix::unistd;

use data::*;
use evaluate::find_command;
use environment::{global, empty, run_fn};
use jobs::{Command, Job, IoChannel};
use stream::*;

#[derive(Copy, Clone, Debug, PartialEq)]
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
            (_,                 &PipeType::Either) => true,
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

    /// Build an element adapting between the given types
    pub fn adapt(i: PipeType, o: PipeType) -> Self {
        PlanElement::Adapter(AdapterType::fit(i, o))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PlanningError {
    MultipleInputs,
    MultipleOutputs,
    FrozenPipeline,
    NotFound
}

/// Errors which can arise from attempting to launch a pipeline
#[derive(Debug)]
pub enum LaunchError {
    Evaluation(EvalError),
    JobLaunch(::nix::Error),
    Unknown
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

#[derive(PartialEq, Eq)]
enum FDWrapper {
    Stdin, Stdout,
    Pipe(RawFd)
}

// TODO: adapt this to use the terminal type
impl FDWrapper {
    fn as_fd(&self) -> RawFd {
        match self {
            &FDWrapper::Stdin   => io::stdin().as_raw_fd(),
            &FDWrapper::Stdout  => io::stdout().as_raw_fd(),
            &FDWrapper::Pipe(f) => f,
        }
    }

    fn into_fd(self) -> RawFd {
        let r = match self {
            FDWrapper::Stdin   => io::stdin().as_raw_fd(),
            FDWrapper::Stdout  => io::stdout().as_raw_fd(),
            FDWrapper::Pipe(f) => f,
        };

        // avoid dropping self so we don't close FDs
        ::std::mem::forget(self);

        r
    }
}

impl Drop for FDWrapper {
    fn drop(&mut self) {
        match self {
            &mut FDWrapper::Pipe(f) => {
                unistd::close(f).expect("failed to close wrapped FD");
            },
            _ => {}
        }
    }
}

#[derive(Debug)]
/// Structure to plan execution operations for a pipeline
///
/// This accumulates and generates a set of pipeline evaluation ops for the
/// given pipeline so that the resulting chain will perform as few conversions
/// as possible.
///
/// As a side effect of this planning process, we can accomodate commands that
/// need to be plugged directly into stdin/stdout like `vim` or `htop`.
///
/// To generate a plan, create an empty pipeline plan with `new`, then add each
/// component using the `push` and `add_terminal` methods. When done, call
/// `freeze`. Once frozen, the pipeline can be used to launch instances of the
/// described job.
pub struct Plan {
    /// The inner components of the plan which don't directly link to the input
    /// or output
    elems: Vec<PlanElement>,

    /// The plan's input element
    input: Option<PlanElement>,

    /// The plan's output element
    output: Option<PlanElement>,

    /// The current type at the head of the pipeline
    ///
    /// Set to `Either` by default; when finalizing, elements will be inserted
    /// to convert the input type into the first element's input.
    output_type: PipeType,

    /// Whether the plan is frozen
    ///
    /// When frozen, no extra elements may be pushed.
    frozen: bool
}

impl Plan {
    /// Create a new empty plan
    ///
    /// By default, this will use standard input as input to the pipeline and
    /// standard output as the output. To change this, apply terminal modes.
    pub fn new() -> Self {
        Plan {
            elems: Vec::new(),
            input: None,
            output: None,
            output_type: PipeType::Stream,
            frozen: false
        }
    }

    /// Create a plan from the given pipeline
    ///
    /// The resulting plan will be frozen if generated successfully
    pub fn plan_for(p: Pipeline) -> Result<Self, PlanningError> {
        let Pipeline {elements, terminals} = p;
        let mut plan = Plan::new();

        for t in terminals { plan.add_terminal(t)?; }
        for e in elements { plan.push(e)?; }

        Ok(plan)
    }
    
    /// Modify the input or output based on a given terminal mode
    ///
    /// If the required terminal point (i.e. input or output) has already been
    /// specified by another call, this function may fail with a corresponding
    /// error code.
    pub fn add_terminal(&mut self,
                        term: TerminalMode) -> Result<(), PlanningError> {
        let (f,l) = match term {
            TerminalMode::ReplaceFile(s) => (None, Some(PlanElement::ToFile(s))),
            TerminalMode::AppendFile(s) => (None, Some(PlanElement::AppendFile(s))),
            TerminalMode::SetVariable(id) => (None, Some(PlanElement::IntoVar(id))),
            TerminalMode::AppendVariable(id) => (None, Some(PlanElement::AppendVar(id))),
            TerminalMode::InputFile(s) => (Some(PlanElement::FromFile(s)), None),
            TerminalMode::InputVar(id) => (Some(PlanElement::FromVar(id)), None),
        };

        if let Some(f) = f { // it's an output mode
            if self.output.is_none() {
                self.output = Some(f);
                Ok(())
            } else {
                Err(PlanningError::MultipleOutputs)
            }
        } else if let Some(l) = l { // it's an input mode
            if !self.input.is_none() {
                return Err(PlanningError::MultipleOutputs);
            }

            self.input = Some(l);
            Ok(())
        } else {
            panic!("cannot interpret terminal mode. this is a bug.")
        }
    }

    /// Add a pipeline component to the plan
    pub fn push(&mut self, c: PipelineComponent) -> Result<(), PlanningError> {
        if self.frozen { return Err(PlanningError::FrozenPipeline); }
        let PipelineComponent { xform, link } = c;

        let cmd = plan_transform(xform)?;
        let cmd_io = cmd.io_types();

        // add the element to the plan
        self.adapt_head(cmd_io.0);
        self.elems.push(cmd);
        self.output_type = cmd_io.1;

        // tack on the requested pipe
        if let Some(link) = link {
            let elem = match link {
                PipeMode::DelimitedPipe(c) =>
                    Some(PlanElement::Adapter(AdapterType::StreamDelim(c))),
                PipeMode::PipeText =>
                    Some(PlanElement::Adapter(AdapterType::StreamToString)),
                PipeMode::Pipe => None
            };

            if let Some(e) = elem {
                // don't add an adapter if it wouldn't help
                if e.io_types().1 != self.output_type {
                    self.adapt_head(e.io_types().0);
                    self.output_type = e.io_types().1;
                    self.elems.push(e);
                }
            }
        } else {
            self.freeze();
        }

        Ok(())
    }

    /// Add adapter elements to the front of the plan in order to make the head
    /// compatible with the given target
    fn adapt_head(&mut self, tgt: PipeType) {
        if tgt.accepts(&self.output_type) { return; }

        let elem = PlanElement::adapt(self.output_type, tgt);
        self.output_type = elem.io_types().1;
        self.elems.push(elem);
    }

    /// Freeze the plan
    ///
    /// This prevents any future modifications to the pipeline and prepares it
    /// for use in launching jobs.
    pub fn freeze(&mut self) -> &Self {
        if self.frozen { return self; }
        self.frozen = true;

        // generate adapter sequences for inputs and outputs
        let input = self.input.take().unwrap_or(PlanElement::Stdin);
        let output = self.input.take().unwrap_or(PlanElement::Stdout);

        if self.elems.is_empty() {
            // special case - we can't rely on io_types normally here
            if !output.io_types().0.accepts(&input.io_types().1) {
                self.elems.push(PlanElement::adapt(output.io_types().0,
                                                   input.io_types().1));
            }

            return self;
        }

        {
            if !self.elems[0].io_types().0.accepts(&input.io_types().1) {
                let in_iotype = self.elems[0].io_types().0;
                self.elems.insert(0,
                        PlanElement::adapt(input.io_types().1, in_iotype));
            }
            self.elems.insert(0, input);
        }

        {
            let last_type = self.elems.last().unwrap().io_types().1;
            if !output.io_types().0.accepts(&last_type) {
                self.elems.push(PlanElement::adapt(last_type,
                                                   output.io_types().0));
            }
            self.elems.push(output);
        }

        self
    }

    /// Construct a file descriptor from the first input element
    fn get_input_fd(&self) -> FDWrapper {
        match &self.elems[0] {
            &PlanElement::Stdin => FDWrapper::Stdin,
            &PlanElement::FromFile(ref _fname)=> {
                unimplemented!("file inputs not yet ready")
            },
            &PlanElement::FromVar(ref _id)    => {
                unimplemented!("variable inputs not yet ready")
            },
            _ => panic!("plan has invalid beginning element")
        }
    }

    /// Create an active instance of the pipeline
    /// 
    /// Pipelines are executed by joining all contiguous shell components
    /// together and launching each block of contiguous operations in a thread.
    ///
    /// # Panics
    ///
    /// This function will panic if the plan has not been frozen
    pub fn launch(&self, background: bool) -> Result<ActivePipeline, LaunchError> {
        if !self.frozen { panic!("cannot launch unfrozen plan"); }

        // fd of the last output
        let mut last_output = Some(self.get_input_fd());

        // drop the first element so we don't process it twice
        let mut elems_iter = self.elems.iter()
                                       .skip(1)
                                       .peekable();

        // execute all plan elements
        let mut job = Job::new();
        let mut plan_buffer = Vec::with_capacity(self.elems.len());
        while let Some(element) = elems_iter.next() {
            match element {
                &PlanElement::Expression(ref v) =>
                    plan_buffer.push(PlanElement::Expression(v.to_owned())),
                &PlanElement::Adapter(ref t) =>
                    plan_buffer.push(PlanElement::Adapter(*t)),
                &PlanElement::Command {ref exec, ref invoked_name, ref args} => {
                    // we have to flush the plan buffer if there's anything
                    // there
                    if !plan_buffer.is_empty() {
                        // generate a pipe
                        let (i,o) = unistd::pipe2(fcntl::O_CLOEXEC)
                                           .expect("cannot generate pipes");
                        let i = FDWrapper::Pipe(i);
                        let o = FDWrapper::Pipe(o);
                        let eval = TransformEvaluation::launch(&mut job,
                                        last_output.take().unwrap(),
                                        plan_buffer.drain(..),
                                        EvalOutput::Descriptor(i));

                        if eval.is_none() {
                            // handle launch failure
                            // currently just close FDs and abort
                            // TODO: specific error code
                            return Err(LaunchError::Unknown);
                        }
                        last_output = Some(o);
                    }

                    // check whether to link the output to stdout
                    let fwd_stdout = elems_iter.peek() == Some(&&PlanElement::Stdout);

                    // evaluate args
                    let args = args.into_iter()
                                   .map(|a| a.evaluate(&empty()))
                                   .collect::<Eval<Vec<_>>>()
                                   .and_then(|a|
                                         a.into_iter()
                                          .map(|x| match &x.data {
                                              // don't expand fns or macros
                                              &ValueData::Function(_) =>
                                                  x.name.clone()
                                                        .map(|x| Value::from(x))
                                                        .unwrap_or(x),
                                              &ValueData::Macro(_) =>
                                                  x.name.clone()
                                                        .map(|x| Value::from(x))
                                                        .unwrap_or(x),
                                              _ => x
                                          })
                                          .map(|x| match &x.data {
                                              &ValueData::Symbol(_) =>
                                                  run_fn("fs/glob", &[x.clone()])
                                                 .unwrap_or(Ok(x.clone()))
                                                 .map(|l|
                                                      if l.into_seq().unwrap().len() == 0 {
                                                          x.clone()
                                                      } else {
                                                          l
                                                      }),
                                              &ValueData::Str(_) =>
                                                  run_fn("fs/glob", &[x.clone()])
                                                 .unwrap_or(Ok(x.clone()))
                                                 .map(|l|
                                                      if l.into_seq().unwrap().len() == 0 {
                                                          x.clone()
                                                      } else {
                                                          l
                                                      }),
                                              _ => Ok(x)
                                          })
                                          .collect::<Eval<Vec<Value>>>())
                                   .and_then(|a| a.into_iter()
                                                  .map(|x| x.into_args())
                                                  .collect::<Eval<Vec<_>>>())
                                   .map(|a| a.into_iter()
                                             .flat_map(|x| x)
                                             .collect::<Vec<_>>());
                    let args = match args {
                        Ok(r) => r,
                        Err(e) => {
                            // TODO: kill existing components
                            return Err(LaunchError::Evaluation(e));
                        }
                    };

                    // TODO: redirect or handle stderr
                    let out_fd = last_output.as_ref().unwrap().as_fd();
                    let cmd = Command::new(exec)
                                      .invoked_using(invoked_name)
                                      .args(args)
                                      .stdin(
                                          if last_output == Some(FDWrapper::Stdin) {
                                              IoChannel::Inherited }
                                          else {IoChannel::Specific(last_output
                                                                   .take()
                                                                   .unwrap()
                                                                   .into_fd())})
                                      .stdout(
                                          if fwd_stdout { IoChannel::Inherited }
                                          else { IoChannel::Pipe });
                    let cmd = if background { cmd }
                              else { cmd.foreground() };
                    let process = match job.launch(cmd) {
                        Ok(p) => p,
                        Err(e) => {
                            // retake ownership of output FD
                            FDWrapper::Pipe(out_fd);
                            return Err(LaunchError::JobLaunch(e));
                        }
                    };

                    if !fwd_stdout {
                        last_output = Some(FDWrapper::Pipe(
                                process.stdout()
                                       .unwrap()
                                       .into_raw_fd()));
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
            let out = match self.elems.last().unwrap() {
                &PlanElement::Stdout         => EvalOutput::PrettyStdout,
                &PlanElement::AppendFile(_)  => unimplemented!(),
                &PlanElement::AppendVar(_ )  => unimplemented!(),
                &PlanElement::ToFile(_)      => unimplemented!(),
                &PlanElement::IntoVar(_)     => unimplemented!(),
                _                            => panic!("invalid plan terminator")
            };

            let eval = TransformEvaluation::launch(&mut job,
                last_output.take().unwrap(), plan_buffer.drain(..), out);
            if eval.is_none() {
                // TODO close output FDs here
                // TODO: specific error code
                return Err(LaunchError::Unknown);
            }
        }

        Ok(ActivePipeline { job })
    }
}

enum EvalOutput {
    PrettyStdout,
    Descriptor(FDWrapper)
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
    fn build_innermost_elem(input: FDWrapper, first: PlanElement) -> PolyStream {
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

        if input == FDWrapper::Stdin {
            PolyStream::from_stdin(config).expect("cannot construct stream")
        } else {
            PolyStream::from_fd(input.into_fd(),
                                config).expect("cannot construct stream")
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

    fn launch<I>(job: &mut Job,
                 input: FDWrapper,
                 elements: I,
                 output: EvalOutput) -> Option<()>
            where I: IntoIterator<Item=PlanElement> {
        use std::io::prelude::*;
        use terminal;

        let mut elements = elements.into_iter();
        let first = elements.next().unwrap();

        // build the innermost (first generated) value from the input
        // since we the pipeline gets built from the inside (left element) out
        // (toward the right) we need to keep track of the current element at
        // all times.
        let mut innermost = Value::new(
            TransformEvaluation::build_innermost_elem(input, first));

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
        job.spawn(move || {
            // TODO: error handling
            let res = expr.evaluate(&::environment::empty());
            let mut term = terminal::acquire();
            let res = match res {
                Ok(r) => r,
                Err(e) => {
                    writeln!(term.stderr(), "ysh: {:?} - {}", expr, e).unwrap();
                    return;
                }
            };
            match out {
                EvalOutput::PrettyStdout => {
                    // don't bother printing if they return null
                    if res != Value::empty() {
                        match res.into_str() {
                            Ok(s) => {
                                writeln!(term.stdout(), "{}", s).unwrap();
                            },
                            Err(e) => {
                                writeln!(term.stderr(),
                                    "ysh: cannot convert to string: {}", e)
                                    .unwrap();
                            }
                        }
                    }
                },
                EvalOutput::Descriptor(fd) => {
                    let mut f = unsafe {
                        ::std::fs::File::from_raw_fd(fd.into_fd())
                    };
                    match res.into_str() {
                        Ok(r) => {write!(f, "{}", r).unwrap();},
                        Err(e) => {
                            writeln!(term.stderr(),
                                "ysh: cannot convert to string: {}", e)
                                .unwrap();
                        }
                    }
                }
            }
        });
        Some(())
    }
}

/// Representation of an active pipeline.
pub struct ActivePipeline {
    /// OS processes
    job: Job,
}

impl ActivePipeline {
    /// Wait until all the processes in the pipeline have terminated
    pub fn wait(mut self) {
        self.job.wait().expect("wait failed");
    }
}
